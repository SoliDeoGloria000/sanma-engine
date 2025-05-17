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
    pub ura_dora_indicators: Vec<Tile>,

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
            for seat in 0..3 {
                if let Some(t) = wall.draw() {
                    hands[seat].add(t);
                }
            }
        }
        Self {
            wall,
            hands,
            turn: 0,
            riichi_declared: [false; 3],
            ippatsu_eligible: [false; 3],
            dora_indicators: Vec::new(),
            ura_dora_indicators: Vec::new(),
            red_fives: Vec::new(),
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
        let mut counts = [0u8; 34];
        for (t, c) in self.hands[seat].iter() {
            counts[t as usize] = c;
        }
        if counts.iter().sum::<u8>() != 14 {
            return false;
        }
        // Seven pairs
        if counts.iter().filter(|&&c| c == 2).count() == 7 {
            return true;
        }
        // Standard 4 melds + pair
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
    pub fn check_ron(&self, tile: Tile, _discarder: usize) -> bool {
        let seat = self.turn as usize;
        let mut counts = [0u8; 34];
        for (t, c) in self.hands[seat].iter() {
            counts[t as usize] = c;
        }
        counts[tile as usize] += 1;
        if counts.iter().sum::<u8>() != 14 {
            return false;
        }
        // Seven pairs
        if counts.iter().filter(|&&c| c == 2).count() == 7 {
            return true;
        }
        // Standard 4 melds + pair
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
        // Only bail out if they asked for a Tsumo but it isn’t really a Tsumo:
        if let WinType::Tsumo = win_type {
            if !self.check_tsumo() {
                return Score { han: 0, fu: 0, points: 0 };
            }
        }
        // (No bail-out for Ron here; we assume caller has already validated check_ron)

        // Seven pairs?
        let is_chii_toitsu = {
            let mut counts = [0u8; 34];
            for (t, c) in self.hands[seat].iter() {
                counts[t as usize] = c;
            }
            counts.iter().filter(|&&c| c == 2).count() == 7
        };

        // Count han
        let mut han = 0;
        if self.riichi_declared[seat] {
            han += 1;
            if self.ippatsu_eligible[seat] {
                han += 1;
            }
        }
        han += count_dora(&self.hands[seat], &self.dora_indicators);
        if self.riichi_declared[seat] {
            han += count_dora(&self.hands[seat], &self.ura_dora_indicators);
        }
        han += count_red_fives(&self.hands[seat], &self.red_fives);
        han += count_tanyao(&self.hands[seat]);
        han += count_yakuhai(&self.hands[seat], self.seat_winds[seat], self.round_wind);
        han += count_pinfu(&self.hands[seat], self.seat_winds[seat], self.round_wind);

        // Calculate fu
        let mut fu = if is_chii_toitsu { 25 } else { 20 };
        if matches!(win_type, WinType::Tsumo) {
            fu += 2;
        }
        fu = ((fu + 9) / 10) * 10;

        // Final points
        let is_dealer = seat == 0;
        let points = calculate_points(han, fu, is_dealer, win_type);

        Score { han, fu, points }
    }
}

/// Recursively try to peel off 4 melds (triplets or sequences).
fn can_form_melds(counts: &mut [u8; 34]) -> bool {
    if counts.iter().all(|&c| c == 0) {
        return true;
    }
    let i = counts.iter().position(|&c| c > 0).unwrap();
    // Triplet
    if counts[i] >= 3 {
        counts[i] -= 3;
        if can_form_melds(counts) { return true; }
        counts[i] += 3;
    }
    // Sequence
    let suit = i / 9;
    let idx = i % 9;
    if suit < 3 && idx <= 6 {
        let i1 = i + 1;
        let i2 = i + 2;
        if counts[i1] > 0 && counts[i2] > 0 {
            counts[i] -= 1;
            counts[i1] -= 1;
            counts[i2] -= 1;
            if can_form_melds(counts) { return true; }
            counts[i] += 1;
            counts[i1] += 1;
            counts[i2] += 1;
        }
    }
    false
}

/// Count how many dora (including kan-dora and ura-dora) you have.
fn count_dora(hand: &Hand, indicators: &[Tile]) -> u8 {
    indicators.iter().map(|&t| hand.count(t)).sum()
}

/// Count red‐five bonus tiles in hand
fn count_red_fives(hand: &Hand, red_fives: &[Tile]) -> u8 {
    red_fives.iter().map(|&t| hand.count(t)).sum()
}

/// Count Tanyao (all simples). Returns 1 if every tile is 2–8 in suits.
fn count_tanyao(hand: &Hand) -> u8 {
    for (tile, _cnt) in hand.iter() {
        let suit = tile as usize / 9;
        let idx  = tile as usize % 9;
        if suit == 3 || idx == 0 || idx == 8 {
            return 0;
        }
    }
    1
}

/// Count Yakuhai: one han per dragon triplet, seat‐wind triplet, round‐wind triplet.
/// Avoid double‐count if seat and round wind are the same.
fn count_yakuhai(hand: &Hand, seat_wind: Tile, round_wind: Tile) -> u8 {
    let mut han = 0;
    for &dragon in &[Tile::White, Tile::Green, Tile::Red] {
        if hand.count(dragon) >= 3 { han += 1; }
    }
    if seat_wind == round_wind {
        if hand.count(seat_wind) >= 3 { han += 1; }
    } else {
        if hand.count(seat_wind) >= 3 { han += 1; }
        if hand.count(round_wind) >= 3 { han += 1; }
    }
    han
}

/// Full Pinfu detector
fn count_pinfu(hand: &Hand, seat_wind: Tile, round_wind: Tile) -> u8 {
    let mut counts = [0u8; 34];
    for (t, c) in hand.iter() {
        counts[t as usize] = c;
    }
    for i in 0..34 {
        if counts[i] >= 2 {
            let tile = Tile::try_from(i as u8).unwrap();
            // ignore honors and winds
            match tile {
                Tile::East | Tile::South | Tile::West | Tile::North
                | Tile::White | Tile::Green | Tile::Red => continue,
                _ => {}
            }
            if tile == seat_wind || tile == round_wind {
                continue;
            }
            let mut clone = counts;
            clone[i] -= 2;
            if can_form_sequences(&mut clone) {
                return 1;
            }
        }
    }
    0
}

/// Given total han & fu, dealer flag and win type, compute final rounded points.
fn calculate_points(han: u8, fu: u8, is_dealer: bool, win_type: WinType) -> u32 {
    let base = {
        let is_mangan = han >= 5
            || (han == 4 && fu >= 40)
            || (han == 3 && fu >= 70);
        if is_mangan { 2000 }
        else       { (fu as u32) * 2u32.pow((han + 2) as u32) }
    };
    let round_100 = |x: u32| ((x + 99) / 100) * 100;
    match win_type {
        WinType::Ron(_) => {
            let raw = if is_dealer { base * 6 } else { base * 4 };
            round_100(raw)
        }
        WinType::Tsumo => {
            if is_dealer {
                let each = round_100(base * 2);
                each * 2
            } else {
                let pay_dealer = round_100(base * 2);
                let pay_others = round_100(base * 1);
                pay_dealer + pay_others * 2
            }
        }
    }
}

/// Recursively remove *only* sequences from counts. Returns true if
/// you can peel off everything as 4 sequences.
fn can_form_sequences(counts: &mut [u8; 34]) -> bool {
    if counts.iter().all(|&c| c == 0) {
        return true;
    }
    let i = counts.iter().position(|&c| c > 0).unwrap();
    let suit = i / 9;
    let idx = i % 9;
    if suit < 3 && idx <= 6 {
        let i1 = i + 1;
        let i2 = i + 2;
        if counts[i] > 0 && counts[i1] > 0 && counts[i2] > 0 {
            counts[i]  -= 1;
            counts[i1] -= 1;
            counts[i2] -= 1;
            if can_form_sequences(counts) {
                return true;
            }
            counts[i]  += 1;
            counts[i1] += 1;
            counts[i2] += 1;
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*;

    #[test]
    fn initial_hand_size() {
        let gs = GameState::new(7);
        let count0: usize = gs.hands[0].iter().map(|(_, c)| c as usize).sum();
        assert_eq!(count0, 13);
    }

    #[test]
    fn discard_works() {
        let mut gs = GameState::new(42);
        if let Some(tile) = gs.draw_for_current() {
            assert!(gs.hands[0].iter().any(|(t, _)| t == tile));
            let before = gs.hands[0].iter().count();
            assert!(gs.discard_current(tile));
            let after = gs.hands[0].iter().count();
            assert_eq!(after, before - 1);
        }
    }

    #[test]
    fn score_win_default() {
        let gs = GameState::new(0);
        let score = gs.score_win(0, WinType::Tsumo);
        assert_eq!(score.han, 0);
        assert_eq!(score.fu, 0);
        assert_eq!(score.points, 0);
    }

    #[test]
    fn check_tsumo_defaults_to_false() {
        let gs = GameState::new(0);
        assert!(!gs.check_tsumo());
    }

    #[test]
    fn check_ron_defaults_to_false() {
        let gs = GameState::new(0);
        assert!(!gs.check_ron(Man1, 0));
    }

    #[test]
    fn test_calculate_points_nondealer_ron() {
        // 1 han, 30 fu, non-dealer Ron = 1000 points
        let p = calculate_points(1, 30, false, WinType::Ron(Man1));
        assert_eq!(p, 1000);
    }

    #[test]
    fn test_calculate_points_dealer_tsumo() {
        // 2 han, 20 fu, dealer Tsumo = 1400 points each
        let p = calculate_points(2, 20, true, WinType::Tsumo);
        assert_eq!(p, 1400);
    }

    #[test]
    fn test_full_pinfu_ron() {
        let mut gs = GameState::new(0);
        // overwrite hand with a perfect pinfu + pair (14 tiles total)
        gs.hands[0] = Hand::default();
        let tiles = [
            Man2, Man3, Man4,
            Pin3, Pin4, Pin5,
            Sou4, Sou5, Sou6,
            Man6, Man7, Man8,
            Man9, Man9,
        ];
        for &t in &tiles {
            gs.hands[0].add(t);
        }
        let han = gs.score_win(0, WinType::Ron(Man1)).han;
        assert_eq!(han, 1, "Full Pinfu should give 1 han");
    }
}
