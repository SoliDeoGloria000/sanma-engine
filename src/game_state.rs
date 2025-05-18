use crate::{Hand, Tile, Wall};
use std::convert::TryFrom;

/// Type of win: self-draw (Tsumo) or Ron (discard win, with winning tile)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WinType {
    Tsumo,
    Ron(Tile),
}

/// Scoring structure for han and fu calculation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Score {
    pub han: u8,
    pub fu: u8,
    pub points: u32,
}

pub struct GameState {
    pub wall: Wall,
    pub hands: [Hand; 3],
    pub turn: u8,

    // ─── Riichi / Ippatsu ───
    pub riichi_declared: [bool; 3],
    pub ippatsu_eligible: [bool; 3],

    // ─── Dora Indicators ───
    pub dora_indicators: Vec<Tile>,
    pub ura_dora_indicators: Vec<Tile>, // Revealed on Riichi win

    // ─── Red 5 (“Aka‐dora”) ───
    pub red_fives: Vec<Tile>,


    // ─── Round & Seat Winds ───
    pub round_wind: Tile,
    pub seat_winds: [Tile; 3],
}


impl GameState {
    /// Initialize game: deal 13 tiles each
    pub fn new(seed: u64) -> Self {
        let mut wall = Wall::new(seed); 
        let mut hands = [Hand::default(), Hand::default(), Hand::default()];
        for _ in 0..13 {
            for seat_idx in 0..3 {
                if let Some(t) = wall.draw() {
                    hands[seat_idx].add(t);
                }
            }
        }
        
        let dora_indicators = if wall.remaining_raw_count() > 14 { vec![wall.draw().unwrap_or(Tile::Man1)] } else { vec![] };
        let ura_dora_indicators = if wall.remaining_raw_count() > 14 { vec![wall.draw().unwrap_or(Tile::Pin1)] } else { vec![] };
        
        let red_fives = vec![Tile::Man5, Tile::Pin5, Tile::Sou5]; 


        Self {
            wall,
            hands,
            turn: 0,
            riichi_declared: [false; 3],
            ippatsu_eligible: [false; 3],
            dora_indicators,
            ura_dora_indicators,
            red_fives,
            round_wind: Tile::East,
            seat_winds: [Tile::East, Tile::South, Tile::West], 
        }
    }

    /// Advance to the next player
    pub fn next_turn(&mut self) {
        self.turn = (self.turn + 1) % 3;
    }

    /// Current player draws a tile and adds it to their hand
    pub fn draw_for_current(&mut self) -> Option<Tile> {
        self.wall.draw().map(|t| {
            self.hands[self.turn as usize].add(t);
            t
        })
    }

    /// Current player discards a tile from their hand
    pub fn discard_current(&mut self, tile: Tile) -> bool {
        self.hands[self.turn as usize].remove(tile)
    }

    /// Check if a win occurs by Tsumo on the current hand
    pub fn check_tsumo(&self) -> bool {
        let seat = self.turn as usize;
        let (counts, total_tiles) = get_final_hand_counts(&self.hands[seat], WinType::Tsumo); 

        if total_tiles != 14 { 
            return false; 
        }

        if counts.iter().filter(|&&c| c == 2).count() == 7 { // Seven pairs
            return true;
        }

        for i in 0..34 { 
            if counts[i] >= 2 { 
                let mut c2 = counts; 
                c2[i] -= 2; 
                if can_form_melds(&mut c2) { 
                    return true;
                }
            }
        }
        false
    }

    /// Check if a win occurs by Ron on the given tile
    pub fn check_ron(&self, ron_tile: Tile, _discarder: usize) -> bool {
        let seat = self.turn as usize; 
        let (counts, total_tiles) = get_final_hand_counts(&self.hands[seat], WinType::Ron(ron_tile));
                                                                    
        if total_tiles != 14 { 
            return false; 
        }

        if counts.iter().filter(|&&c| c == 2).count() == 7 { // Seven pairs
            return true;
        }
        
        for i in 0..34 { 
            if counts[i] >= 2 { 
                let mut c2 = counts; 
                c2[i] -= 2; 
                if can_form_melds(&mut c2) { 
                    return true;
                }
            }
        }
        false
    }
	
    /// Score a winning hand (han, fu, points)
    pub fn score_win(&self, seat: usize, win_type: WinType) -> Score {
        // Initial validation: If the hand isn't a winning structure, return 0.
        match win_type {
            WinType::Tsumo => {
                if !self.check_tsumo() { // Check if the current player's hand is a Tsumo win
                    return Score { han: 0, fu: 0, points: 0 };
                }
            }
            WinType::Ron(ron_tile) => {
                // For Ron, check_ron needs the discarder's seat, which isn't available here.
                // We assume if score_win is called for Ron, the win was already validated externally.
                // A more robust system might pass the discarder or have check_ron take fewer params.
                // For now, we'll proceed for Ron, but this is a point of attention.
                // A simple structural check can be done:
                let ( _, temp_total_tiles) = get_final_hand_counts(&self.hands[seat], WinType::Ron(ron_tile));
                if temp_total_tiles != 14 && count_kokushi_muso(&get_final_hand_counts(&self.hands[seat], WinType::Ron(ron_tile)).0) == 0 {
                     return Score { han: 0, fu: 0, points: 0 };
                }
            }
        }

        let winning_hand_before_ron = &self.hands[seat]; 
        let (final_counts, total_tiles_in_winning_hand) = get_final_hand_counts(winning_hand_before_ron, win_type);
        
        // This check is now mostly covered by the initial validation, but kept for safety.
        if total_tiles_in_winning_hand != 14 && count_kokushi_muso(&final_counts) == 0 {
            return Score { han: 0, fu: 0, points: 0 };
        }
        
        let is_chii_toitsu = final_counts.iter().filter(|&&c| c == 2).count() == 7 && total_tiles_in_winning_hand == 14;
        
        let mut han = 0;
        let mut is_yakuman = false;
        let mut yakuman_han_val = 0;

        let kokushi_val = count_kokushi_muso(&final_counts); 
        if kokushi_val > 0 { is_yakuman = true; yakuman_han_val = yakuman_han_val.max(kokushi_val); }

        if !is_yakuman { 
            let suuanko_val = count_suuanko(&final_counts, win_type); 
            if suuanko_val > 0 { is_yakuman = true; yakuman_han_val = yakuman_han_val.max(suuanko_val); }
        }
        if !is_yakuman {
            let daisangen_val = count_daisangen(&final_counts);
            if daisangen_val > 0 { is_yakuman = true; yakuman_han_val = yakuman_han_val.max(daisangen_val); }
        }

        if is_yakuman {
            han = yakuman_han_val; 
        } else {
            if self.riichi_declared[seat] {
                han += 1; 
                if self.ippatsu_eligible[seat] {
                    han += 1; 
                }
            }
            // TODO: Menzen Tsumo

            han += count_tanyao(&final_counts);
            han += count_pinfu(&final_counts, winning_hand_before_ron, win_type, self.seat_winds[seat], self.round_wind); 
            han += count_chanta(&final_counts);
            han += count_junchan(&final_counts);
            han += count_sanshoku_doujun(&final_counts);
            han += count_sanshoku_doukou(&final_counts);
            han += count_toitoi(&final_counts);
            han += count_sananko(&final_counts, win_type); 
            han += count_yakuhai(&final_counts, self.seat_winds[seat], self.round_wind);
            
            han += count_dora(&final_counts, &self.dora_indicators);
            if self.riichi_declared[seat] { 
                han += count_dora(&final_counts, &self.ura_dora_indicators);
            }
            han += count_red_fives(&final_counts, &self.red_fives);

            if han >= 13 { 
                han = 13; 
                is_yakuman = true; 
            }
        }

        let fu = if is_yakuman {
            0 
        } else if is_chii_toitsu {
            25 
        } else {
            let mut fu_calc = 20; 
            if let WinType::Tsumo = win_type {
                if !is_pinfu_candidate(&final_counts, winning_hand_before_ron, win_type, self.seat_winds[seat], self.round_wind) {
                     fu_calc += 2; 
                }
            } else if let WinType::Ron(_tile) = win_type { 
            }
            ((fu_calc + 9) / 10) * 10 
        };
        
        let final_fu = if is_yakuman { 0 } else { fu }; 
        let is_dealer = self.seat_winds[seat] == Tile::East; 
        let points = calculate_points(han, final_fu, is_dealer, win_type, is_yakuman);

        Score { han, fu: final_fu, points }
    }
}

fn get_final_hand_counts(hand_obj: &Hand, win_type: WinType) -> ([u8; 34], u8) {
    let mut counts = [0u8; 34];
    let mut total_tiles = 0;
    for (t, c) in hand_obj.iter() {
        counts[t as usize] = c;
        total_tiles += c;
    }
    if let WinType::Ron(tile) = win_type {
        counts[tile as usize] += 1;
        total_tiles += 1;
    }
    (counts, total_tiles)
}

fn can_form_melds(counts: &mut [u8; 34]) -> bool {
    if counts.iter().all(|&c| c == 0) { return true; }
    let i = match counts.iter().position(|&c| c > 0) { Some(idx) => idx, None => return true, };

    if counts[i] >= 3 { 
        counts[i] -= 3;
        if can_form_melds(counts) { return true; }
        counts[i] += 3; 
    }

    let suit = i / 9;
    let idx_in_suit = i % 9; 
    if suit < 3 && idx_in_suit <= 6 { 
        let i1 = i + 1; let i2 = i + 2;
        if counts[i] > 0 && counts[i1] > 0 && counts[i2] > 0 { 
            counts[i] -= 1; counts[i1] -= 1; counts[i2] -= 1;
            if can_form_melds(counts) { return true; }
            counts[i] += 1; counts[i1] += 1; counts[i2] += 1;
        }
    }
    false
}

fn count_dora(final_hand_counts: &[u8; 34], indicators: &[Tile]) -> u8 {
    let mut dora_count = 0;
    for &indicator_tile in indicators {
        let dora_value_tile = indicator_tile.next_in_series(); 
        dora_count += final_hand_counts[dora_value_tile as usize];
    }
    dora_count
}

fn count_red_fives(final_hand_counts: &[u8; 34], red_five_tile_types: &[Tile]) -> u8 {
    let mut red_five_count = 0;
    for (tile_idx, &count_in_hand) in final_hand_counts.iter().enumerate() {
        if count_in_hand > 0 {
            let tile_type = Tile::try_from(tile_idx as u8).unwrap();
            if red_five_tile_types.contains(&tile_type) {
                red_five_count += count_in_hand;
            }
        }
    }
    red_five_count
}

fn count_tanyao(final_hand_counts: &[u8; 34]) -> u8 {
    if final_hand_counts.iter().sum::<u8>() < 13 { return 0; } 
    for i in 0..34 {
        if final_hand_counts[i] > 0 { 
            let tile = Tile::try_from(i as u8).unwrap();
            let suit = tile as usize / 9;
            let idx_in_suit  = tile as usize % 9; 
            if suit == 3 || idx_in_suit == 0 || idx_in_suit == 8 { 
                return 0; 
            }
        }
    }
    1 
}

fn count_yakuhai(final_hand_counts: &[u8; 34], seat_wind: Tile, round_wind: Tile) -> u8 {
    let mut han = 0;
    for &dragon in &[Tile::White, Tile::Green, Tile::Red] {
        if final_hand_counts[dragon as usize] >= 3 { han += 1; }
    }
    if final_hand_counts[seat_wind as usize] >= 3 { han += 1; }
    if seat_wind != round_wind && final_hand_counts[round_wind as usize] >= 3 {
        han += 1;
    }
    han
}

fn is_pinfu_candidate(final_hand_counts: &[u8;34], _original_hand_before_win: &Hand, win_type: WinType, seat_wind: Tile, round_wind: Tile) -> bool {
    if final_hand_counts.iter().sum::<u8>() != 14 { return false; }

    for pair_idx in 0..34 { 
        if final_hand_counts[pair_idx] >= 2 {
            let pair_tile = Tile::try_from(pair_idx as u8).unwrap();
            if pair_tile == Tile::White || pair_tile == Tile::Green || pair_tile == Tile::Red ||
               pair_tile == seat_wind || pair_tile == round_wind { 
                continue; 
            }
            let mut counts_no_pair = *final_hand_counts; 
            counts_no_pair[pair_idx] -= 2;
            if can_form_sequences(&mut counts_no_pair) { 
                if let WinType::Ron(_ron_tile) = win_type { 
                    return true; 
                } else if let WinType::Tsumo = win_type {
                    return true; 
                }
            }
        }
    }
    false
}

fn count_pinfu(final_hand_counts: &[u8;34], original_hand_before_win: &Hand, win_type: WinType, seat_wind: Tile, round_wind: Tile) -> u8 {
    if is_pinfu_candidate(final_hand_counts, original_hand_before_win, win_type, seat_wind, round_wind) {
        1
    } else {
        0
    }
}

fn count_chanta(final_hand_counts: &[u8; 34]) -> u8 {
    if final_hand_counts.iter().sum::<u8>() < 13 { return 0; } 
    let mut has_simple = false;
    let mut has_terminal_or_honor = false;

    for i in 0..34 {
        if final_hand_counts[i] > 0 {
            let tile = Tile::try_from(i as u8).unwrap();
            let suit = tile as usize / 9;
            let idx = tile as usize % 9; 
            
            if suit < 3 && (idx > 0 && idx < 8) { 
                has_simple = true;
            } else { 
                has_terminal_or_honor = true;
            }
        }
    }
    if has_terminal_or_honor && !has_simple { return 1; }
    0 
}

fn count_junchan(final_hand_counts: &[u8; 34]) -> u8 {
    if final_hand_counts.iter().sum::<u8>() < 13 { return 0; }
    let mut has_simple = false;
    let mut has_honor = false;
    let mut has_terminal = false;

    for i in 0..34 {
        if final_hand_counts[i] > 0 {
            let tile = Tile::try_from(i as u8).unwrap();
            let suit = tile as usize / 9;
            let idx = tile as usize % 9;

            if suit == 3 { has_honor = true; }
            else if suit < 3 && (idx > 0 && idx < 8) { has_simple = true; }
            else if suit < 3 && (idx == 0 || idx == 8) { has_terminal = true; }
        }
    }
    if has_terminal && !has_honor && !has_simple { return 2; }
    0
}

fn count_sanshoku_doujun(final_hand_counts: &[u8; 34]) -> u8 {
    for base_idx in 0..7 { 
        if final_hand_counts[base_idx] > 0 && final_hand_counts[base_idx+1] > 0 && final_hand_counts[base_idx+2] > 0 && 
           final_hand_counts[base_idx+9] > 0 && final_hand_counts[base_idx+10] > 0 && final_hand_counts[base_idx+11] > 0 && 
           final_hand_counts[base_idx+18] > 0 && final_hand_counts[base_idx+19] > 0 && final_hand_counts[base_idx+20] > 0
        { return 1; }
    }
    0
}

fn count_sanshoku_doukou(final_hand_counts: &[u8; 34]) -> u8 {
    for i in 0..9 { 
        if final_hand_counts[i] >= 3 && final_hand_counts[i+9] >= 3 && final_hand_counts[i+18] >= 3 {
            return 2;
        }
    }
    0
}

fn count_toitoi(final_hand_counts: &[u8; 34]) -> u8 {
    if final_hand_counts.iter().sum::<u8>() != 14 { return 0; } 
    let mut pair_found = false;
    let mut triplet_kan_count = 0;
    for &count in final_hand_counts.iter() {
        match count {
            0 => {} 
            2 => { if pair_found { return 0; } pair_found = true; }
            3 | 4 => { triplet_kan_count += 1; }
            _ => { return 0; } 
        }
    }
    if pair_found && triplet_kan_count == 4 { 2 } else { 0 }
}

fn count_sananko(final_hand_counts: &[u8; 34], _win_type: WinType) -> u8 { 
    if final_hand_counts.iter().sum::<u8>() != 14 { return 0; } 
    let mut anko_like_groups = 0;
    for &count in final_hand_counts.iter() {
        if count >= 3 { anko_like_groups += 1; }
    }
    if anko_like_groups >= 3 { 2 } else { 0 }
}

fn count_daisangen(final_hand_counts: &[u8; 34]) -> u8 { 
    if final_hand_counts[Tile::White as usize] >= 3 &&
       final_hand_counts[Tile::Green as usize] >= 3 &&
       final_hand_counts[Tile::Red as usize] >= 3 {
        13 
    } else { 0 }
}

fn count_kokushi_muso(final_hand_counts: &[u8; 34]) -> u8 {
    if final_hand_counts.iter().sum::<u8>() != 14 { return 0; }
    let terminals_and_honors = [
        Tile::Man1, Tile::Man9, Tile::Pin1, Tile::Pin9, Tile::Sou1, Tile::Sou9,
        Tile::East, Tile::South, Tile::West, Tile::North,
        Tile::White, Tile::Green, Tile::Red,
    ];
    let mut pair_found = false;
    let mut missing_any_required = false;
    for &required_tile_type in &terminals_and_honors {
        match final_hand_counts[required_tile_type as usize] {
            0 => { missing_any_required = true; break; }
            1 => {} 
            2 => { if pair_found { missing_any_required = true; break; } pair_found = true; }
            _ => { missing_any_required = true; break; } 
        }
    }
    if missing_any_required || !pair_found { return 0; }
    for i in 0..34 { 
        let tile_is_required = terminals_and_honors.iter().any(|&t| t as usize == i);
        if !tile_is_required && final_hand_counts[i] > 0 {
            return 0; 
        }
    }
    13 
}

fn count_suuanko(final_hand_counts: &[u8; 34], _win_type: WinType) -> u8 { 
    if final_hand_counts.iter().sum::<u8>() != 14 { return 0; }
    let actual_triplet_groups = final_hand_counts.iter().filter(|&&c| c >= 3).count();
    let actual_pair_groups = final_hand_counts.iter().filter(|&&c| c == 2).count();

    if actual_triplet_groups == 4 && actual_pair_groups == 1 {
        return 13; 
    }
    0
}

fn calculate_points(han: u8, fu: u8, is_dealer: bool, win_type: WinType, is_yakuman: bool) -> u32 {
    if han == 0 { return 0; }

    if is_yakuman {
        let yakuman_multiplier = match han {
            13 => 1, 26 => 2, 39 => 3, 52 => 4, 
            _ => 1,  
        };
        let base_yakuman_unit = 8000; 
        
        let total_points = match win_type {
            WinType::Ron(_) => {
                if is_dealer { base_yakuman_unit * 6 * yakuman_multiplier } 
                else { base_yakuman_unit * 4 * yakuman_multiplier }
            }
            WinType::Tsumo => { // Sanma Tsumo payments
                if is_dealer { base_yakuman_unit * 4 * yakuman_multiplier } 
                else { base_yakuman_unit * 3 * yakuman_multiplier } 
            }
        };
        return total_points; 
    }
    
    let base_points = match han {
        _ if han >= 11 => 6000, 
        _ if han >= 8 => 4000,  
        _ if han >= 6 => 3000,  
        _ if han >= 5 => 2000,  
        4 => if fu >= 40 { 2000 } else { (fu as u32) * (1 << (han + 2)) }, 
        3 => if fu >= 70 { 2000 } else { (fu as u32) * (1 << (han + 2)) },
        _ => (fu as u32) * (1 << (han + 2)),
    };

    let capped_base_points = if han < 5 && base_points > 2000 { 2000 } else { base_points };
    let final_base = if capped_base_points > 8000 { 8000 } else { capped_base_points}; 

    let round_100 = |x: u32| ((x + 99) / 100) * 100;

    match win_type {
        WinType::Ron(_) => {
            let payment = if is_dealer { final_base * 6 } else { final_base * 4 };
            round_100(payment)
        }
        WinType::Tsumo => { // Sanma Tsumo payments
            if is_dealer { 
                let each_payment = round_100(final_base * 2);
                each_payment * 2 // Total from 2 non-dealers
            } else { 
                let dealer_payment = round_100(final_base * 2);
                let other_non_dealer_payment = round_100(final_base * 1);
                dealer_payment + other_non_dealer_payment 
            }
        }
    }
}

fn can_form_sequences(counts: &mut [u8; 34]) -> bool {
    if counts.iter().all(|&c| c == 0) { return true; }
    let i = match counts.iter().position(|&c| c > 0) { Some(idx) => idx, None => return true,  };
    let suit = i / 9;
    let idx_in_suit = i % 9;
    if suit < 3 && idx_in_suit <= 6 { 
        let i1 = i + 1; let i2 = i + 2;
        if counts[i] > 0 && counts[i1] > 0 && counts[i2] > 0 { 
            counts[i]  -= 1; counts[i1] -= 1; counts[i2] -= 1;
            if can_form_sequences(counts) { return true; }
            counts[i]  += 1; counts[i1] += 1; counts[i2] += 1;
        }
    }
    false 
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*; 

    fn hand_from_tiles(tiles: &[Tile]) -> Hand {
        let mut hand = Hand::default();
        for &tile in tiles { hand.add(tile); }
        hand
    }
    
    // gs_with_hand is used by tests
    fn gs_with_hand(tiles: &[Tile], _win_type: WinType) -> GameState { 
        let mut gs = GameState::new(0); 
        gs.hands[0] = hand_from_tiles(tiles);
        gs
    }

    #[test]
    fn initial_hand_size() {
        let gs = GameState::new(7);
        let mut count0: u8 = 0;
        for (_, c) in gs.hands[0].iter() { count0 += c; }
        assert_eq!(count0, 13);
    }

    #[test]
    fn discard_works() {
        let mut gs = GameState::new(42);
        let initial_tile_to_discard = gs.hands[0].iter().next().map(|(t,_)| t); 
        if let Some(tile) = initial_tile_to_discard {
            let mut count_before: u8 = 0;
            for (_,c) in gs.hands[0].iter() { count_before += c; }
            assert!(gs.discard_current(tile));
            let mut count_after: u8 = 0;
            for (_,c) in gs.hands[0].iter() { count_after += c; }
            assert_eq!(count_after, count_before - 1, "Hand size should decrease by 1");
        } else {
            panic!("Hand was unexpectedly empty.");
        }
    }

    #[test]
    fn score_win_default() { 
        let mut non_winning_gs = GameState::new(1);
        let hand_14_no_yaku_tiles = &[
            Man1, Man2, Man3, Man4, // Has terminal, so not Tanyao
            Pin2, Pin3, Pin4, Pin6, // No Man5 or Pin5 (default red fives)
            Sou2, Sou3, Sou4, Sou6, Sou7, // No obvious yaku here
            East // Single honor, not a yaku by itself as triplet
        ];
        non_winning_gs.hands[0] = hand_from_tiles(hand_14_no_yaku_tiles); 
        non_winning_gs.dora_indicators = vec![]; 
        non_winning_gs.red_fives = vec![]; 
        non_winning_gs.ippatsu_eligible[0] = false;
        non_winning_gs.riichi_declared[0] = false;

        // Ensure this hand is indeed not a Tsumo win according to check_tsumo
        assert!(!non_winning_gs.check_tsumo(), "Test hand for score_win_default should not be a Tsumo win");
        
        let score_non_win = non_winning_gs.score_win(0, WinType::Tsumo); 
        assert_eq!(score_non_win.han, 0, "Non-winning hand with no dora/red-fives should have 0 han");
        assert_eq!(score_non_win.points, 0, "Non-winning hand with no dora/red-fives should have 0 points");
    }

    #[test] 
    fn check_tsumo_defaults_to_false() {
        let gs = GameState::new(0); 
        assert!(!gs.check_tsumo(), "Default random hand should not be Tsumo");
    }

    #[test]
    fn check_ron_defaults_to_false() {
        let gs = GameState::new(0); 
        assert!(!gs.check_ron(Man1, 1), "Default random hand should not be Ron on Man1");
    }
    
    #[test]
    fn test_calculate_points_nondealer_ron() {
        let p = calculate_points(1, 30, false, WinType::Ron(Man1), false);
        assert_eq!(p, 1000);
    }

    #[test]
    fn test_calculate_points_dealer_tsumo() {
        let p_orig_test = calculate_points(2, 20, true, WinType::Tsumo, false);
        assert_eq!(p_orig_test, 1400); 
    }

    #[test]
    fn test_pinfu_ron_basic() { 
        let _hand_tiles = &[ Man2, Man3, Pin2, Pin3, Pin4, Sou5, Sou6, Sou7, East, East, West, West, West, ];
        let _pinfu_hand = &[ Man2, Man3, Man4, Pin3, Pin4, Pin5, Sou6, Sou7, Sou8, West, West, West, ];
        let _actual_pinfu_tiles_before_ron = &[ Man2, Man3, Pin2, Pin3, Pin4, Sou5, Sou6, Sou7, North, North, North, White, White ];
        let pinfu_example_pre_ron = hand_from_tiles(&[ Man1, Man2, Man3, Pin4, Pin5, Pin6, Sou7, Sou8, Sou9, Sou2, Sou3, East, East ]);
        let mut gs = GameState::new(0);
        gs.hands[0] = pinfu_example_pre_ron;
        gs.round_wind = Tile::South; 
        gs.seat_winds = [Tile::South, Tile::West, Tile::North]; 
        let _score = gs.score_win(0, WinType::Ron(Sou4)); 
        
        let mut gs_orig_pinfu = GameState::new(0);
        gs_orig_pinfu.hands[0] = hand_from_tiles(&[ Man2, Man3, Man4, Pin3, Pin4, Pin5, Sou4, Sou5, Sou6, Man6, Man7, Man9, Man9, ]);
         gs_orig_pinfu.round_wind = Tile::South;
         gs_orig_pinfu.seat_winds = [Tile::East, Tile::West, Tile::North]; 
        let hand_for_pinfu_ron = hand_from_tiles(&[ Man2, Man3, Man4, Pin3, Pin4, Pin5, Sou4, Sou5, Sou6, Man6, Man7, Pin9, Pin9 ]);
        gs_orig_pinfu.hands[0] = hand_for_pinfu_ron;
        let _score_pinfu = gs_orig_pinfu.score_win(0, WinType::Ron(Man5)); 
    }

    fn get_test_final_counts(tiles: &[Tile]) -> [u8; 34] {
        let hand = hand_from_tiles(tiles);
        let (counts, total_tiles) = get_final_hand_counts(&hand, WinType::Tsumo); 
        assert_eq!(total_tiles, 14, "Test hand for yaku count should have 14 tiles");
        counts
    }

    #[test]
    fn test_count_chanta() {
        let chanta_hand_counts = get_test_final_counts(&[Man1,Man1,Man1, Sou9,Sou9,Sou9, East,East,East, West,West,West, Pin1,Pin1]);
        assert_eq!(count_chanta(&chanta_hand_counts), 1, "Chanta with only terminals/honors");
        
        let non_chanta_strict_counts = get_test_final_counts(&[Man1,Man2,Man3, Sou9,Sou9,Sou9, East,East,East, West,West,West, Pin1,Pin1]);
        assert_eq!(count_chanta(&non_chanta_strict_counts), 0, "Not Chanta (strict): contains Man2, Man3");
    }

    #[test]
    fn test_count_junchan() {
        let junchan_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin9,Pin9,Pin9, Sou1,Sou1,Sou1, Man9,Man9,Man9, Pin1,Pin1]);
        assert_eq!(count_junchan(&junchan_counts), 2, "Junchan with only terminals");

        let non_junchan_honor_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin9,Pin9,Pin9, East,East,East, Sou1,Sou1,Sou1, Man9,Man9]);
        assert_eq!(count_junchan(&non_junchan_honor_counts), 0, "Not Junchan: has East wind");
        
        let non_junchan_simple_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou1,Sou1,Sou1, Man9,Man9,Man9, Pin7,Pin7]);
        assert_eq!(count_junchan(&non_junchan_simple_counts), 0, "Not Junchan: has Pin2");
    }

    #[test]
    fn test_count_sanshoku_doujun() {
        let sd_counts = get_test_final_counts(&[Man1,Man2,Man3, Pin1,Pin2,Pin3, Sou1,Sou2,Sou3, East,East, West,West,West]);
        assert_eq!(count_sanshoku_doujun(&sd_counts), 1, "Sanshoku Doujun: 123m 123p 123s EE WWW");
        let non_sd_counts = get_test_final_counts(&[Man1,Man2,Man3, Pin1,Pin2,Pin3, Sou4,Sou5,Sou6, East,East, West,West,West]);
        assert_eq!(count_sanshoku_doujun(&non_sd_counts), 0, "Not Sanshoku Doujun");
    }

    #[test]
    fn test_count_sanshoku_doukou() {
        let sk_counts = get_test_final_counts(&[Man2,Man2,Man2, Pin2,Pin2,Pin2, Sou2,Sou2,Sou2, East,East, West,West,West]);
        assert_eq!(count_sanshoku_doukou(&sk_counts), 2, "Sanshoku Doukou: 222m 222p 222s EE WWW");
        let non_sk_counts = get_test_final_counts(&[Man2,Man2,Man2, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,West,West,West]);
        assert_eq!(count_sanshoku_doukou(&non_sk_counts), 0, "Not Sanshoku Doukou");
    }

    #[test]
    fn test_count_toitoi() {
        let toitoi_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West,West]);
        assert_eq!(count_toitoi(&toitoi_counts), 2, "Toitoi: 111m 222p 333s EEE WW");
        let non_toitoi_counts = get_test_final_counts(&[Man1,Man2,Man3, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West,West]);
        assert_eq!(count_toitoi(&non_toitoi_counts), 0, "Not Toitoi: has 123m");
    }

    #[test]
    fn test_count_sananko() {
        let sananko_4_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West,West]);
        assert_eq!(count_sananko(&sananko_4_counts, WinType::Tsumo), 2, "Sananko with 4 anko-like groups");
        
        let sananko_3_seq_pair_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, Pin7,Pin8,Pin9, East,East]);
        assert_eq!(count_sananko(&sananko_3_seq_pair_counts, WinType::Tsumo), 2, "Sananko with 3 anko-like, 1 seq, 1 pair");

        let non_sananko_2_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou4,Sou5, Pin3,Pin4,Pin5, East,East]);
        assert_eq!(count_sananko(&non_sananko_2_counts, WinType::Tsumo), 0, "Not Sananko (only 2 anko-like groups)");
    }

    #[test]
    fn test_count_daisangen() {
        let daisangen_counts = get_test_final_counts(&[White,White,White, Green,Green,Green, Red,Red,Red, Man1,Man2,Man3, East,East]);
        assert_eq!(count_daisangen(&daisangen_counts), 13, "Daisangen (Big Three Dragons)");
        let non_daisangen_counts = get_test_final_counts(&[White,White,White, Green,Green,Green, Red,Red,Man1, Man1,Man2,Man3,Man4, East]);
        assert_eq!(count_daisangen(&non_daisangen_counts), 0, "Not Daisangen (missing Red triplet)");
    }

    #[test]
    fn test_count_kokushi_muso() {
        let kokushi_counts = get_test_final_counts(&[
            Man1, Man9, Pin1, Pin9, Sou1, Sou9, East, South, West, North, White, Green, Red, Man1 
        ]);
        assert_eq!(count_kokushi_muso(&kokushi_counts), 13, "Kokushi Muso");

        let non_kokushi_missing_counts = get_test_final_counts(&[
            Man1, Pin1, Pin9, Sou1, Sou9, East, South, West, North, White, Green, Red, Man1, Sou2 
        ]);
        assert_eq!(count_kokushi_muso(&non_kokushi_missing_counts), 0, "Not Kokushi (missing Man9)");

        let _non_kokushi_two_pair_counts = get_test_final_counts(&[ 
            Man1, Man9, Pin1, Pin9, Sou1, Sou9, East, South, West, North, White, Green, Red,Red 
        ]);
        let mut hand_for_two_pair_kokushi = Hand::default();
        for tile in &[Man1, Man9, Pin1, Pin9, Sou1, Sou9, East, South, West, North, White, Green, Red, Red] { 
            hand_for_two_pair_kokushi.add(*tile);
        }
        let (final_counts_two_pair,_) = get_final_hand_counts(&hand_for_two_pair_kokushi, WinType::Ron(Man1));
        assert_eq!(count_kokushi_muso(&final_counts_two_pair), 0, "Not Kokushi (two pairs after Ron)");
    }

    #[test]
    fn test_count_suuanko() {
        let suuanko_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, Man4,Man4,Man4, East,East]);
        assert_eq!(count_suuanko(&suuanko_counts, WinType::Tsumo), 13, "Suuanko (Four Concealed Triplets)");
        let non_suuanko_counts = get_test_final_counts(&[Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, Man4,Man5,Man6, East,East]);
        assert_eq!(count_suuanko(&non_suuanko_counts, WinType::Tsumo), 0, "Not Suuanko (only 3 triplets)");
    }

    #[test]
    fn test_kazoe_yakuman_scoring() {
        let score_11_non_dealer_ron = calculate_points(11, 30, false, WinType::Ron(Man1), false); 
        assert_eq!(score_11_non_dealer_ron, 24000, "11 han non-dealer Ron (Sanbaiman) should be 24000");

        let score_kazoe_ron_non_dealer = calculate_points(13, 0, false, WinType::Ron(Man1), true); 
        assert_eq!(score_kazoe_ron_non_dealer, 32000, "Kazoe Yakuman (13h) non-dealer Ron should be 32000");
        
        let score_kazoe_ron_dealer = calculate_points(13, 0, true, WinType::Ron(Man1), true);
        assert_eq!(score_kazoe_ron_dealer, 48000, "Kazoe Yakuman (13h) dealer Ron should be 48000");

        let score_named_yakuman_dealer_tsumo = calculate_points(13, 0, true, WinType::Tsumo, true); 
        assert_eq!(score_named_yakuman_dealer_tsumo, 32000, "Named Yakuman (13h) Dealer Tsumo should be 32000 (Sanma)");
        
        let score_named_yakuman_non_dealer_tsumo = calculate_points(13, 0, false, WinType::Tsumo, true);
        assert_eq!(score_named_yakuman_non_dealer_tsumo, 24000, "Named Yakuman (13h) Non-Dealer Tsumo should be 24000 (Sanma)");
    }
}
