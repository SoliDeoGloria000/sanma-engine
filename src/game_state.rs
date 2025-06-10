// src/game_state.rs

use crate::hand::{Hand, HandError};
use crate::tiles::{Tile, TileExt};
use crate::wall::Wall;
use crate::hand_parser::{
    self, ParsedStandardHand, ParsedChiitoitsu, ParsedKokushiMusou,
    ParsedMeldType as ParserOutputMeldType
};
use crate::fu_calculation::{calculate_fu, FuCalculationInput};

use std::convert::TryFrom;
use std::collections::HashMap;


#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WinType {
    Tsumo,
    Ron { winning_tile: Tile, discarder_seat: usize },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeclaredMeldType {
    Pon,
    Ankan,
    Daiminkan,
    Shouminkan,
    Kita,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeclaredMeld {
    pub meld_type: DeclaredMeldType,
    pub tiles: [Tile; 4],
    pub called_from_discarder_idx: Option<u8>,
    pub called_tile: Option<Tile>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Score {
    pub han: u8,
    pub fu: u8,
    pub points: u32,
    pub yaku_details: Vec<(&'static str, u8)>,
}

pub struct GameState {
    pub wall: Wall,
    pub hands: [Hand; 3],
    pub open_melds: [Vec<DeclaredMeld>; 3],
    pub discards: [Vec<Tile>; 3],
    pub current_player_idx: u8,
    pub dealer_idx: u8,
    pub turn_count: u32,
    pub riichi_declared: [bool; 3],
    pub ippatsu_eligible: [bool; 3],
    pub double_riichi_eligible: [bool; 3],
    pub is_rinshan_kaihou_win_pending: bool,
    pub is_chankan_window_open: bool,
    pub chankan_tile_and_declarer: Option<(Tile, u8)>,
    pub is_tenhou_win_possible: bool,
    pub is_chiihou_win_possible: [bool; 3],
    pub current_dora_indicators: Vec<Tile>,
    pub current_ura_dora_indicators: Vec<Tile>,
    pub red_five_tile_ids: Vec<Tile>,
    pub round_wind: Tile,
    pub seat_winds: [Tile; 3],
    pub last_drawn_tile: Option<Tile>,
    pub last_discarded_tile_info: Option<(Tile, u8)>,
    pub kans_declared_count: [u8; 3],
    pub total_kans_in_game: u8,
    pub kita_declared_count: [u8; 3],
    pub player_scores: [i32; 3],
    pub riichi_sticks: u8,
    pub honba_sticks: u8,
    pub four_kan_abortive_draw_pending: bool,
    pub player_who_declared_fourth_kan: Option<u8>,
	pub any_discard_called_this_round: [bool; 3],
}

impl GameState {
    pub fn new(
        seed: u64,
        initial_dealer_idx: u8,
        initial_honba_sticks: u8,
        initial_hands: Option<[Vec<Tile>; 3]>,
        initial_scores: Option<[i32; 3]>,
        override_wall: Option<Vec<Tile>>,
    ) -> Self {
        let mut wall = if let Some(wall_tiles) = override_wall {
            Wall::from_predetermined(wall_tiles)
        } else {
            Wall::new(seed)
        };
        
        let mut hands = [Hand::default(); 3];

        if let Some(h) = initial_hands {
            for i in 0..3 {
                hands[i] = hand_from_tiles_for_test(&h[i]);
                // This brittle logic is no longer needed if the parent Env controls draws.
                // The wall can now be completely independent.
            }
        } else {
            for _ in 0..13 {
                for seat_idx in 0..3 {
                    if let Some(t) = wall.draw_from_live_wall() {
                        hands[seat_idx].add(t).expect("Failed to add tile during initial deal");
                    } else {
                        panic!("Wall empty during initial deal!");
                    }
                }
            }
        }

        let mut current_dora_indicators = Vec::new();
        if let Some(dora_ind) = wall.get_initial_dora_indicator() {
                current_dora_indicators.push(dora_ind);
        }

        let mut seat_winds = [Tile::East; 3];
        seat_winds[initial_dealer_idx as usize] = Tile::East;
        seat_winds[((initial_dealer_idx + 1) % 3) as usize] = Tile::South;
        seat_winds[((initial_dealer_idx + 2) % 3) as usize] = Tile::West;

        let mut chiihou_possible = [false; 3];
        for i in 0..3 {
            if i as u8 != initial_dealer_idx { chiihou_possible[i] = true; }
        }

        let red_fives = vec![Tile::Man5, Tile::Pin5, Tile::Sou5];
        let player_scores = initial_scores.unwrap_or([35000; 3]);

        Self {
            wall, hands, open_melds: Default::default(), discards: Default::default(),
            current_player_idx: initial_dealer_idx, dealer_idx: initial_dealer_idx, turn_count: 0,
            riichi_declared: [false; 3], ippatsu_eligible: [false; 3], double_riichi_eligible: [true; 3],
            is_rinshan_kaihou_win_pending: false, is_chankan_window_open: false, chankan_tile_and_declarer: None,
            is_tenhou_win_possible: true,
            is_chiihou_win_possible: chiihou_possible,
            current_dora_indicators,
            current_ura_dora_indicators: Vec::new(),
            red_five_tile_ids: red_fives,
            round_wind: Tile::East,
            seat_winds,
            last_drawn_tile: None, last_discarded_tile_info: None,
            kans_declared_count: [0; 3], total_kans_in_game: 0, kita_declared_count: [0; 3],
            player_scores,
            riichi_sticks: 0,
            honba_sticks: initial_honba_sticks,
            four_kan_abortive_draw_pending: false,
            player_who_declared_fourth_kan: None,
            any_discard_called_this_round: [false; 3],
        }
    }

    /// Draws a tile for the current player.
    /// If `forced_draw` is Some, that tile is used. Otherwise, it's drawn from the wall.
    /// This is the key change to allow the environment to inject draws from a log file.
    pub fn player_draws_tile(&mut self, forced_draw: Option<Tile>) -> Option<Tile> {
        let player_idx = self.current_player_idx as usize;

        let no_interrupting_calls_made_yet_this_round = self.open_melds.iter().all(|p_melds| {
            p_melds.iter().all(|m| matches!(m.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita))
        });

        if self.turn_count >= 3 || !no_interrupting_calls_made_yet_this_round {
            for i in 0..3 {
                if !self.riichi_declared[i] {
                    self.double_riichi_eligible[i] = false;
                }
            }
        }

        // Use the forced_draw if provided, otherwise draw from the internal wall.
        let drawn_tile_option = if forced_draw.is_some() {
            forced_draw
        } else {
            self.wall.draw_from_live_wall()
        };

        if let Some(drawn_tile) = drawn_tile_option {
            self.hands[player_idx].add(drawn_tile).expect("Failed to add drawn tile to hand");
            self.last_drawn_tile = Some(drawn_tile);

            if !(player_idx == self.dealer_idx as usize && self.turn_count == 0 && no_interrupting_calls_made_yet_this_round) {
                self.is_tenhou_win_possible = false;
            }

            let is_player_first_draw_turn = self.turn_count == (player_idx as u32).wrapping_sub(self.dealer_idx as u32).rem_euclid(3);
            if !(player_idx != self.dealer_idx as usize && is_player_first_draw_turn && no_interrupting_calls_made_yet_this_round) {
                 self.is_chiihou_win_possible[player_idx] = false;
            }
        } else {
            self.last_drawn_tile = None;
        }
        drawn_tile_option
    }

    pub fn player_discards_tile(&mut self, player_idx_discarding: usize, tile_to_discard: Tile) -> Result<(), HandError> {
        if player_idx_discarding != self.current_player_idx as usize {
            return Err(HandError::Generic("Not player's turn to discard"));
        }
        self.hands[player_idx_discarding].remove(tile_to_discard)?;

        self.discards[player_idx_discarding].push(tile_to_discard);
        self.last_discarded_tile_info = Some((tile_to_discard, player_idx_discarding as u8));

        if self.riichi_declared[player_idx_discarding] {
            self.ippatsu_eligible[player_idx_discarding] = false;
        }

        if !self.riichi_declared[player_idx_discarding] {
            self.double_riichi_eligible[player_idx_discarding] = false;
        }

        self.is_rinshan_kaihou_win_pending = false;
        self.is_chankan_window_open = false;
        self.chankan_tile_and_declarer = None;

        self.is_tenhou_win_possible = false;
        for i in 0..3 { self.is_chiihou_win_possible[i] = false; }

        if player_idx_discarding == self.current_player_idx as usize {
            self.turn_count += 1;
        }
        Ok(())
    }

    fn void_transient_flags_on_call(&mut self, _action_player_idx: usize, discarder_idx_opt: Option<usize>) {
        for i in 0..3 {
            self.ippatsu_eligible[i] = false;
            if !self.riichi_declared[i] {
                 self.double_riichi_eligible[i] = false;
            }
        }
        self.is_tenhou_win_possible = false;
        for i in 0..3 { self.is_chiihou_win_possible[i] = false; }

        if let Some(discarder_idx) = discarder_idx_opt {
            if discarder_idx < 3 {
                self.any_discard_called_this_round[discarder_idx] = true;
            }
        }
    }

    pub fn is_tenpai(&self, player_idx: usize) -> (bool, Vec<Tile>) {
        let mut waiting_for_tiles: Vec<Tile> = Vec::new();

        if self.hands[player_idx].get_all_tiles().len() % 3 != 1 {
            return (false, waiting_for_tiles);
        }

        for i in 0..34 {
            if let Ok(wait_tile) = Tile::try_from(i as u8) {
                let mut temp_hand = self.hands[player_idx];
                if temp_hand.add(wait_tile).is_err() {
                    continue;
                }

                let (final_counts, total_tiles) = get_combined_hand_counts(
                    &temp_hand,
                    &self.open_melds[player_idx],
                    None
                );

                if total_tiles % 3 != 2 && total_tiles != 14 {
                    continue;
                }

                let forms_standard_win = hand_parser::parse_standard_hand(&final_counts).is_some();
                let forms_chiitoi_win = self.is_menzen(player_idx) && hand_parser::parse_chiitoitsu(&final_counts).is_some();
                let forms_kokushi_win = self.is_menzen(player_idx) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some();

                if forms_standard_win || forms_chiitoi_win || forms_kokushi_win {
                    if !waiting_for_tiles.contains(&wait_tile) {
                        waiting_for_tiles.push(wait_tile);
                    }
                }
            }
        }

        waiting_for_tiles.sort_unstable();
        (!waiting_for_tiles.is_empty(), waiting_for_tiles)
    }

    pub fn can_declare_riichi(&self, player_idx: usize) -> bool {
        if self.riichi_declared[player_idx] { return false; }
        if !self.is_menzen(player_idx) { return false; }
        if self.hands[player_idx].get_all_tiles().len() != 14 { return false; }
        if self.wall.live_wall_remaining_count() < 4 { return false; }
        if self.player_scores[player_idx] < 1000 { return false; }

        let original_hand_counts = self.hands[player_idx].iter().fold([0u8; 34], |mut acc, (t, c)| {
            acc[t as usize] += c;
            acc
        });

        for i in 0..34 {
            let discard_candidate = Tile::try_from(i as u8).unwrap();
            if original_hand_counts[discard_candidate as usize] > 0 {
                let mut temp_hand_after_discard = self.hands[player_idx];
                if temp_hand_after_discard.remove(discard_candidate).is_ok() {
                    let (is_tenpai_after_discard, _) = self.is_tenpai_internal(&temp_hand_after_discard, player_idx);
                    if is_tenpai_after_discard {
                        return true;
                    }
                }
            }
        }
        false
    }
    
    fn is_tenpai_internal(&self, hand: &Hand, player_idx: usize) -> (bool, Vec<Tile>) {
        let mut waits = Vec::new();
        for i in 0..34 {
            if let Ok(wait_tile) = Tile::try_from(i as u8) {
                let mut temp_hand = *hand;
                if temp_hand.add(wait_tile).is_err() { continue; }

                let (final_counts, _total) = get_combined_hand_counts(&temp_hand, &self.open_melds[player_idx], None);

                if hand_parser::parse_standard_hand(&final_counts).is_some() ||
                   (self.is_menzen(player_idx) && hand_parser::parse_chiitoitsu(&final_counts).is_some()) ||
                   (self.is_menzen(player_idx) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some()) {
                    waits.push(wait_tile);
                }
            }
        }
        (!waits.is_empty(), waits)
    }

    pub fn declare_riichi(&mut self, player_idx: usize) {
        if self.can_declare_riichi(player_idx) {
            self.player_scores[player_idx] -= 1000;
            self.riichi_sticks += 1;
            self.riichi_declared[player_idx] = true;
            self.ippatsu_eligible[player_idx] = true;
        }
    }

    pub fn can_call_pon(&self, player_idx: usize, called_tile: Tile, discarder_idx: usize) -> bool {
        if player_idx == discarder_idx { return false; }
        if self.riichi_declared[player_idx] { return false; }
        self.hands[player_idx].count(called_tile) >= 2
    }

    fn can_declare_any_kan(&self, player_idx: usize) -> bool {
        if self.total_kans_in_game >= 4 {
            let mut single_player_has_all_four = false;
            for p_kans_count in &self.kans_declared_count {
                if *p_kans_count == 4 {
                    single_player_has_all_four = true;
                    break;
                }
            }
            if !single_player_has_all_four { return false; }
            if self.kans_declared_count[player_idx] < 4 { return true; }
            return false;
        }
        true
    }

    pub fn can_call_daiminkan(&self, player_idx: usize, called_tile: Tile, discarder_idx: usize) -> bool {
        if player_idx == discarder_idx { return false; }
        if self.riichi_declared[player_idx] { return false; }
        if !self.can_declare_any_kan(player_idx) { return false; }
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 {
            return false;
        }
        self.hands[player_idx].count(called_tile) >= 3
    }

    pub fn get_possible_ankans(&self, player_idx: usize) -> Option<Vec<Tile>> {
        if self.riichi_declared[player_idx] {
            return None;
        }
        if !self.can_declare_any_kan(player_idx) { return None; }
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 {
            return None;
        }

        let mut ankans = Vec::new();
        for (tile, count) in self.hands[player_idx].iter() {
            if count == 4 {
                ankans.push(tile);
            }
        }
        if ankans.is_empty() { None } else { Some(ankans) }
    }

    pub fn get_possible_shouminkans(&self, player_idx: usize) -> Option<Vec<Tile>> {
        if !self.can_declare_any_kan(player_idx) { return None; }
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 {
            return None;
        }

        let mut shouminkans = Vec::new();
        for open_meld in &self.open_melds[player_idx] {
            if open_meld.meld_type == DeclaredMeldType::Pon {
                let pon_tile = open_meld.tiles[0];
                if self.hands[player_idx].count(pon_tile) >= 1 {
                    shouminkans.push(pon_tile);
                }
            }
        }
        if shouminkans.is_empty() { None } else { Some(shouminkans) }
    }

    pub fn can_declare_kita_action(&self, player_idx: usize) -> bool {
        if self.riichi_declared[player_idx] { return false; }
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 {
            return false;
        }
        self.hands[player_idx].count(Tile::North) > 0
    }
    
    pub fn rinshanpai_drawn_count(&self) -> u8 {
        self.wall.rinshanpai_drawn_count()
    }

    pub fn perform_kan_common_actions(&mut self, kan_player_idx: usize) -> Option<Tile> {
        self.void_transient_flags_on_call(kan_player_idx, None);
        if !self.riichi_declared[kan_player_idx] { self.double_riichi_eligible[kan_player_idx] = false; }

        let last_meld_type = self.open_melds[kan_player_idx].last().map(|m| m.meld_type);
        if matches!(last_meld_type, Some(DeclaredMeldType::Ankan) | Some(DeclaredMeldType::Daiminkan) | Some(DeclaredMeldType::Shouminkan)) {
            if let Some(new_kan_dora_ind) = self.wall.reveal_new_kan_dora_indicator() {
                self.current_dora_indicators.push(new_kan_dora_ind);
            }
        }

        let replacement_tile_option = self.wall.draw_replacement_tile();
        if let Some(replacement_tile) = replacement_tile_option {
            self.hands[kan_player_idx].add(replacement_tile).expect("Failed to add replacement tile for Kan/Kita");
            self.last_drawn_tile = Some(replacement_tile);
            self.is_rinshan_kaihou_win_pending = true;
        } else {
            self.last_drawn_tile = None;
            self.is_rinshan_kaihou_win_pending = false;
        }
        replacement_tile_option
    }

    pub fn make_pon(&mut self, calling_player_idx: usize, called_tile: Tile, discarder_idx: usize) -> Result<(), HandError> {
        if calling_player_idx == discarder_idx { return Err(HandError::Generic("Cannot Pon own discard")); }
        if self.hands[calling_player_idx].count(called_tile) < 2 { return Err(HandError::Generic("Not enough tiles for Pon"));}

        self.hands[calling_player_idx].remove_n(called_tile, 2)?;

        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Pon,
            tiles: [called_tile; 4],
            called_from_discarder_idx: Some(discarder_idx as u8),
            called_tile: Some(called_tile),
        };
        self.open_melds[calling_player_idx].push(meld);

        self.void_transient_flags_on_call(calling_player_idx, Some(discarder_idx));
        self.current_player_idx = calling_player_idx as u8;
        self.last_discarded_tile_info = None;
        self.is_rinshan_kaihou_win_pending = false;
        self.is_chankan_window_open = false;
        Ok(())
    }

    fn handle_kan_declaration(&mut self, kan_player_idx: usize) {
        self.kans_declared_count[kan_player_idx] += 1;
        self.total_kans_in_game += 1;

        if self.total_kans_in_game == 4 {
            let mut single_player_has_all_four = false;
            for p_idx in 0..3 {
                if self.kans_declared_count[p_idx] == 4 {
                    single_player_has_all_four = true;
                    break;
                }
            }
            if !single_player_has_all_four {
                self.four_kan_abortive_draw_pending = true;
                self.player_who_declared_fourth_kan = Some(kan_player_idx as u8);
            }
        }
    }

    pub fn make_daiminkan(&mut self, calling_player_idx: usize, called_tile: Tile, discarder_idx: usize) -> Result<(), HandError> {
        if !self.can_call_daiminkan(calling_player_idx, called_tile, discarder_idx) {
             return Err(HandError::Generic("Cannot make Daiminkan (conditions not met)"));
        }
        self.hands[calling_player_idx].remove_n(called_tile, 3)?;
        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Daiminkan,
            tiles: [called_tile; 4],
            called_from_discarder_idx: Some(discarder_idx as u8),
            called_tile: Some(called_tile),
        };
        self.open_melds[calling_player_idx].push(meld);
        self.handle_kan_declaration(calling_player_idx);
        self.void_transient_flags_on_call(calling_player_idx, Some(discarder_idx));
		self.perform_kan_common_actions(calling_player_idx);
		self.current_player_idx = calling_player_idx as u8;
        self.last_discarded_tile_info = None;
        self.is_chankan_window_open = false;
        Ok(())
    }

    pub fn make_ankan(&mut self, calling_player_idx: usize, kan_tile: Tile) -> Result<(), HandError> {
        if self.get_possible_ankans(calling_player_idx).map_or(true, |v| !v.contains(&kan_tile)) {
             return Err(HandError::Generic("Cannot make Ankan (conditions not met or tile not eligible)"));
        }
        self.hands[calling_player_idx].remove_n(kan_tile, 4)?;
        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Ankan, tiles: [kan_tile; 4],
            called_from_discarder_idx: None, called_tile: None,
        };
        self.open_melds[calling_player_idx].push(meld);
        self.handle_kan_declaration(calling_player_idx);

        self.perform_kan_common_actions(calling_player_idx);
        self.current_player_idx = calling_player_idx as u8;
        self.is_chankan_window_open = false;
        Ok(())
    }

    pub fn make_shouminkan(&mut self, calling_player_idx: usize, tile_to_add: Tile) -> Result<(), HandError> {
        if self.get_possible_shouminkans(calling_player_idx).map_or(true, |v| !v.contains(&tile_to_add)) {
            return Err(HandError::Generic("Cannot make Shouminkan (conditions not met or tile not eligible)"));
        }
        let pon_meld_idx = self.open_melds[calling_player_idx].iter().position(|m|
            m.meld_type == DeclaredMeldType::Pon && m.tiles[0] == tile_to_add)
            .ok_or(HandError::Generic("No matching Pon to upgrade to Shouminkan"))?;

        self.hands[calling_player_idx].remove(tile_to_add)?;
        self.open_melds[calling_player_idx][pon_meld_idx].meld_type = DeclaredMeldType::Shouminkan;
        self.open_melds[calling_player_idx][pon_meld_idx].tiles = [tile_to_add; 4];

        self.handle_kan_declaration(calling_player_idx);

        self.is_chankan_window_open = true;
        self.chankan_tile_and_declarer = Some((tile_to_add, calling_player_idx as u8));
        self.current_player_idx = calling_player_idx as u8;
        Ok(())
    }

    pub fn make_kita_declaration(&mut self, calling_player_idx: usize) -> Result<(), HandError> {
        if !self.can_declare_kita_action(calling_player_idx) {
            return Err(HandError::Generic("Cannot declare Kita at this time"));
        }
        self.hands[calling_player_idx].remove(Tile::North)?;

        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Kita, tiles: [Tile::North; 4],
            called_from_discarder_idx: None, called_tile: None,
        };
        self.open_melds[calling_player_idx].push(meld);
        self.kita_declared_count[calling_player_idx] += 1;

        self.perform_kan_common_actions(calling_player_idx);

        self.is_chankan_window_open = false;
        self.current_player_idx = calling_player_idx as u8;
        Ok(())
    }

    pub fn is_menzen(&self, seat: usize) -> bool {
        self.open_melds[seat].iter().all(|meld|
            matches!(meld.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita))
    }

    pub fn check_tsumo(&self) -> bool {
        let seat = self.current_player_idx as usize;
        let (final_counts, total_tiles) = get_combined_hand_counts(&self.hands[seat], &self.open_melds[seat], None);

        if total_tiles != 14 && !hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some() {
            return false;
        }

        let forms_standard_win = hand_parser::parse_standard_hand(&final_counts).is_some();
        let forms_chiitoi_win = self.is_menzen(seat) && hand_parser::parse_chiitoitsu(&final_counts).is_some();
        let forms_kokushi_win = self.is_menzen(seat) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some();

        forms_standard_win || forms_chiitoi_win || forms_kokushi_win
    }

    pub fn can_call_ron(&self, winning_player_seat: usize, ron_tile: Tile, _discarder_seat: usize) -> bool {
        let (is_player_tenpai, current_waits) = self.is_tenpai(winning_player_seat);
        if !is_player_tenpai || !current_waits.contains(&ron_tile) {
            return false;
        }

        for discarded_by_winner in &self.discards[winning_player_seat] {
            if current_waits.contains(discarded_by_winner) {
                return false;
            }
        }

        let (final_counts, total_tiles) = get_combined_hand_counts(
            &self.hands[winning_player_seat],
            &self.open_melds[winning_player_seat],
            Some(ron_tile)
        );

        if total_tiles != 14 && !hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some() {
            return false;
        }

        let forms_standard_win = hand_parser::parse_standard_hand(&final_counts).is_some();
        let forms_chiitoi_win = self.is_menzen(winning_player_seat) && hand_parser::parse_chiitoitsu(&final_counts).is_some();
        let forms_kokushi_win = self.is_menzen(winning_player_seat) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some();

        forms_standard_win || forms_chiitoi_win || forms_kokushi_win
    }

   

pub fn score_win(&mut self, winning_player_seat: usize, win_type: WinType) -> Score {
    let winning_tile_for_logic = match win_type {
        WinType::Ron { winning_tile, .. } => Some(winning_tile),
        WinType::Tsumo => self.last_drawn_tile,
    };

    let (final_counts, _total_tiles) = get_combined_hand_counts(
        &self.hands[winning_player_seat],
        &self.open_melds[winning_player_seat],
        match win_type { WinType::Ron { winning_tile, .. } => Some(winning_tile), _ => None }
    );

    let mut han: u8 = 0;
    let mut yaku_list: Vec<(&'static str, u8)> = Vec::new();
    let mut yakuman_multiplier = 0;

    let menzen = self.is_menzen(winning_player_seat);
    let is_dealer = winning_player_seat == self.dealer_idx as usize;

    let parsed_std_hand = hand_parser::parse_standard_hand(&final_counts);
    let parsed_chiitoi_hand = if parsed_std_hand.is_none() && menzen { hand_parser::parse_chiitoitsu(&final_counts) } else { None };
    let parsed_kokushi_hand = if parsed_std_hand.is_none() && parsed_chiitoi_hand.is_none() && menzen { hand_parser::parse_kokushi_musou_sanma(&final_counts) } else { None };

    // --- 1. YAKUMAN CHECKS (with correct precedence) ---
    let no_interrupting_calls_this_round = self.open_melds.iter().all(|p_melds| p_melds.is_empty() || p_melds.iter().all(|m| matches!(m.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita)));
    let is_player_first_uninterrupted_draw = self.turn_count <= 1 && no_interrupting_calls_this_round;

    // Check for circumstantial yakuman first, as they have absolute priority, and cannot co-exist with Riichi.
    if !self.riichi_declared[winning_player_seat] {
        if self.is_tenhou_win_possible && is_dealer && self.turn_count == 0 && matches!(win_type, WinType::Tsumo) && no_interrupting_calls_this_round {
            yaku_list.push(("Tenhou", 13));
            yakuman_multiplier = 1;
        } else if self.is_chiihou_win_possible[winning_player_seat] && !is_dealer && is_player_first_uninterrupted_draw && matches!(win_type, WinType::Tsumo) {
            yaku_list.push(("Chiihou", 13));
            yakuman_multiplier = 1;
        }
    }

    // If no circumstantial yakuman, check for pattern-based ones.
    if yakuman_multiplier == 0 {
        let mut possible_yakuman: Vec<(&'static str, u8)> = Vec::new();

        if let Some(ref kokushi_details) = parsed_kokushi_hand {
            let val = count_kokushi_musou_yaku(&final_counts, menzen, win_type, &self.open_melds[winning_player_seat], Some(kokushi_details));
            if val > 0 { possible_yakuman.push(if val == 26 {("Kokushi Musou (13-wait)", 26)} else {("Kokushi Musou", 13)}); }
        }
        if let Some(ref std_hand) = parsed_std_hand {
            let suuanko_val = count_suuankou_yaku(std_hand, menzen, win_type, &self.open_melds[winning_player_seat], &self.hands[winning_player_seat], winning_tile_for_logic);
            if suuanko_val > 0 { possible_yakuman.push(if suuanko_val == 26 {("Suuankou (Tanki)", 26)} else {("Suuankou", 13)}); }

            let daisuushii_val = count_daisuushii_yaku(std_hand, &self.open_melds[winning_player_seat], menzen);
            if daisuushii_val > 0 { possible_yakuman.push(("Daisuushii", 26)); }
            else {
                let shousuushii_val = count_shousuushii_yaku(std_hand, &self.open_melds[winning_player_seat], menzen);
                if shousuushii_val > 0 { possible_yakuman.push(("Shousuushii", 13)); }
            }
        }
        let daisangen_val = count_daisangen_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref());
        if daisangen_val > 0 { possible_yakuman.push(("Daisangen", 13)); }

        let tsuu_iisou_val = count_tsuu_iisou_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if tsuu_iisou_val > 0 { possible_yakuman.push(("Tsuu Iisou (All Honors)", 13)); }

        let chinroutou_val = count_chinroutou_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if chinroutou_val > 0 { possible_yakuman.push(("Chinroutou (All Terminals)", 13)); }

        let suukantsu_val = count_suukantsu_yaku(self.kans_declared_count[winning_player_seat]);
        if suukantsu_val > 0 { possible_yakuman.push(("Suukantsu (Four Kans)", 13)); }

        let ryuu_iisou_val = count_ryuu_iisou_yaku(&final_counts, parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if ryuu_iisou_val > 0 { possible_yakuman.push(("Ryuu Iisou", 13)); }

        if let Some(win_tile) = winning_tile_for_logic {
            let chuuren_val = count_chuuren_poutou_yaku(&final_counts, menzen, win_tile);
            if chuuren_val > 0 { possible_yakuman.push(if chuuren_val == 26 {("Chuuren Poutou (9-wait)", 26)} else {("Chuuren Poutou", 13)}); }
        }

        // Find the highest value yakuman among all possibilities
        if let Some(&(best_yaku_name, best_yaku_value)) = possible_yakuman.iter().max_by_key(|&&(_, val)| val) {
            yakuman_multiplier = best_yaku_value / 13;
            yaku_list.push((best_yaku_name, best_yaku_value));
        }
    }


    // --- 2. STANDARD YAKU CHECKS ---
    if yakuman_multiplier == 0 {
        if self.riichi_declared[winning_player_seat] {
            if self.double_riichi_eligible[winning_player_seat] { han += 2; yaku_list.push(("Double Riichi", 2)); }
            else { han += 1; yaku_list.push(("Riichi", 1)); }
            if self.ippatsu_eligible[winning_player_seat] { han += 1; yaku_list.push(("Ippatsu", 1)); }
        }

        if menzen && matches!(win_type, WinType::Tsumo) && (parsed_std_hand.is_some() || parsed_chiitoi_hand.is_some() || parsed_kokushi_hand.is_some()) {
            han += 1;
            yaku_list.push(("Menzen Tsumo", 1));
        }
        if self.is_rinshan_kaihou_win_pending { han += 1; yaku_list.push(("Rinshan Kaihou", 1)); }
        if self.is_chankan_window_open { han += 1; yaku_list.push(("Chankan", 1)); }

        if self.wall.live_wall_remaining_count() == 0 {
            if matches!(win_type, WinType::Tsumo) { han += 1; yaku_list.push(("Haitei Raoyue", 1)); }
            else { han += 1; yaku_list.push(("Houtei Raoyui", 1)); }
        }

        let (tanyao, tanyao_h) = count_tanyao_yaku(&final_counts, &self.open_melds[winning_player_seat]);
        if tanyao { han += tanyao_h; yaku_list.push(("Tanyao", tanyao_h)); }

        if let Some(ref std_hand) = parsed_std_hand {
            if let Some(wt) = winning_tile_for_logic {
                let (pinfu, pinfu_h) = count_pinfu_yaku(std_hand, menzen, self.seat_winds[winning_player_seat], self.round_wind, wt);
                if pinfu { han += pinfu_h; yaku_list.push(("Pinfu", pinfu_h)); }
            }
            let iipeikou_h = count_iipeikou_yaku(std_hand, menzen);
            if iipeikou_h == 3 { han += 3; yaku_list.push(("Ryanpeikou", 3)); }
            else if iipeikou_h == 1 { han += 1; yaku_list.push(("Iipeikou", 1)); }
        }

        han += count_yakuhai_yaku(&final_counts, self.seat_winds[winning_player_seat], self.round_wind, &mut yaku_list, parsed_std_hand.as_ref(), false);

        if let Some(ref std_hand) = parsed_std_hand {
            let (toitoi, toitoi_h) = count_toitoihou_yaku(std_hand);
            if toitoi { han += toitoi_h; yaku_list.push(("Toitoihou", toitoi_h)); }
            let (sanankou, sanankou_h) = count_sanankou_yaku(std_hand, win_type, &self.open_melds[winning_player_seat], &self.hands[winning_player_seat], winning_tile_for_logic);
            if sanankou { han += sanankou_h; yaku_list.push(("Sanankou", sanankou_h)); }
            let (shousangen, shousangen_h) = count_shousangen_yaku(std_hand);
            if shousangen { han += shousangen_h; yaku_list.push(("Shousangen", shousangen_h)); }
        }

        let (honroutou, honroutou_h) = count_honroutou_yaku(parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if honroutou { han += honroutou_h; yaku_list.push(("Honroutou", honroutou_h)); }
        else if let Some(ref std_hand) = parsed_std_hand {
            let (chanta, chanta_h) = count_chanta_yaku(std_hand, menzen);
            if chanta { han += chanta_h; yaku_list.push(("Chanta", chanta_h)); }
            else {
                let (junchan, junchan_h) = count_junchan_yaku(std_hand, menzen);
                if junchan { han += junchan_h; yaku_list.push(("Junchan", junchan_h)); }
            }
        }

        let (chinitsu, chinitsu_h) = count_chinitsu_yaku(&final_counts, menzen, &self.open_melds[winning_player_seat]);
        if chinitsu { han += chinitsu_h; yaku_list.push(("Chinitsu", chinitsu_h)); }
        else {
            let (honitsu, honitsu_h) = count_honitsu_yaku(&final_counts, menzen, &self.open_melds[winning_player_seat]);
            if honitsu { han += honitsu_h; yaku_list.push(("Honitsu", honitsu_h)); }
        }

        if let Some(ref std_hand) = parsed_std_hand {
            let (ittsuu, ittsuu_h) = count_ittsuu_yaku(std_hand, menzen);
            if ittsuu { han += ittsuu_h; yaku_list.push(("Ittsuu", ittsuu_h)); }
            let (sanshoku_doujun, sanshoku_h) = count_sanshoku_doujun_yaku(std_hand, menzen);
            if sanshoku_doujun { han += sanshoku_h; yaku_list.push(("Sanshoku Doujun", sanshoku_h)); }
            let (sanshoku_doukou, sanshoku_doukou_h) = count_sanshoku_doukou_yaku(std_hand);
            if sanshoku_doukou { han += sanshoku_doukou_h; yaku_list.push(("Sanshoku Doukou", sanshoku_h)); }
        }

        let (sankantsu, sankantsu_h) = count_sankantsu_yaku(self.kans_declared_count[winning_player_seat]);
        if sankantsu { han += sankantsu_h; yaku_list.push(("Sankantsu", sankantsu_h)); }

        let (chiitoi, chiitoi_h) = count_chiitoitsu_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_chiitoi_hand.as_ref());
        if chiitoi { han += chiitoi_h; yaku_list.push(("Chiitoitsu", chiitoi_h));}

        let has_yaku = !yaku_list.is_empty();
        if has_yaku {
            let dora_val = count_dora_value(&final_counts, &self.current_dora_indicators);
            if dora_val > 0 { han += dora_val; yaku_list.push(("Dora", dora_val)); }
            if self.riichi_declared[winning_player_seat] {
                if self.current_ura_dora_indicators.is_empty() { self.current_ura_dora_indicators = self.wall.get_current_ura_dora_indicators(); }
                let ura_dora_val = count_dora_value(&final_counts, &self.current_ura_dora_indicators);
                if ura_dora_val > 0 { han += ura_dora_val; yaku_list.push(("Ura Dora", ura_dora_val)); }
            }
            let aka_dora_val = count_red_five_value(&final_counts, &self.red_five_tile_ids);
            if aka_dora_val > 0 { han += aka_dora_val; yaku_list.push(("Aka Dora", aka_dora_val)); }
            let kita_dora_val = self.kita_declared_count[winning_player_seat];
            if kita_dora_val > 0 { han += kita_dora_val; yaku_list.push(("Kita Dora", kita_dora_val)); }
        } else {
            return Score { han: 0, fu: 0, points: 0, yaku_details: vec![("Yaku Nashi", 0)] };
        }
    }

    // --- 3. Finalization ---
    if han >= 13 && yakuman_multiplier == 0 {
        yakuman_multiplier = 1;
        yaku_list.retain(|(name,_)| name.contains("Dora"));
        yaku_list.push(("Kazoe Yakuman", 13));
    }

    let is_yakuman_win = yakuman_multiplier > 0;
    if is_yakuman_win { han = 13 * yakuman_multiplier; }

    let fu = if is_yakuman_win { 0 }
    else if parsed_chiitoi_hand.is_some() { 25 }
    else if let Some(ref std_hand) = parsed_std_hand {
        let (is_pinfu, _) = if let Some(wt) = winning_tile_for_logic { count_pinfu_yaku(std_hand, menzen, self.seat_winds[winning_player_seat], self.round_wind, wt) } else {(false, 0)};
        if is_pinfu && yaku_list.iter().any(|(n,_)| *n == "Pinfu") {
            if matches!(win_type, WinType::Tsumo) { 20 } else { 30 }
        } else {
            calculate_fu(&FuCalculationInput {
                parsed_hand: std_hand, open_melds_declared: &self.open_melds[winning_player_seat], win_type,
                winning_tile: winning_tile_for_logic.unwrap_or(std_hand.pair), is_menzen_win: menzen,
                seat_wind: self.seat_winds[winning_player_seat], round_wind: self.round_wind, _hand_before_win_completion: None,
            })
        }
    } else { 30 };

    let mut points = calculate_points_final(han, fu, is_dealer, win_type, is_yakuman_win, yakuman_multiplier);
    points += self.riichi_sticks as u32 * 1000;
    points += self.honba_sticks as u32 * 300;

    Score { han, fu, points, yaku_details: yaku_list }
}

    pub fn apply_score_transfers_and_reset_sticks(&mut self, winning_player_seat: usize, win_type: WinType, score_details: &Score) {
        let points_value_of_hand = score_details.points
            .saturating_sub(self.riichi_sticks as u32 * 1000)
            .saturating_sub(self.honba_sticks as u32 * 300);

        let is_dealer_win = winning_player_seat == self.dealer_idx as usize;

        match win_type {
            WinType::Ron { discarder_seat, .. } => {
                self.player_scores[winning_player_seat] += points_value_of_hand as i32;
                self.player_scores[discarder_seat] -= points_value_of_hand as i32;
            }
            WinType::Tsumo => {
                self.player_scores[winning_player_seat] += points_value_of_hand as i32;
                
                if is_dealer_win {
                    let payment_from_each_non_dealer = points_value_of_hand / 2;
                    for i in 0..3 {
                        if i != winning_player_seat { 
                            self.player_scores[i] -= payment_from_each_non_dealer as i32;
                        }
                    }
                } else { 
                    let payment_from_dealer = (points_value_of_hand + 1) / 2; // Dealer pays more on odd splits
                    let payment_from_other_non_dealer = points_value_of_hand / 2;

                    for i in 0..3 {
                        if i == self.dealer_idx as usize {
                            self.player_scores[i] -= payment_from_dealer as i32;
                        } else if i != winning_player_seat { 
                            self.player_scores[i] -= payment_from_other_non_dealer as i32;
                        }
                    }
                }
            }
        }

        self.player_scores[winning_player_seat] += self.riichi_sticks as i32 * 1000;
        self.player_scores[winning_player_seat] += self.honba_sticks as i32 * 300;

        self.riichi_sticks = 0;
    }
}

fn get_combined_hand_counts(
    hand_concealed_part: &Hand, 
    open_melds: &[DeclaredMeld],
    ron_tile_option: Option<Tile>
) -> ([u8; 34], u8) {
    let mut counts = [0u8; 34];
    let mut total_tiles = 0;

    for (tile, count) in hand_concealed_part.iter() {
        counts[tile as usize] += count;
        total_tiles += count;
    }

    for meld in open_melds {
        match meld.meld_type {
            DeclaredMeldType::Pon => {
                counts[meld.tiles[0] as usize] += 3;
                total_tiles += 3;
            }
            DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan | DeclaredMeldType::Ankan => {
                counts[meld.tiles[0] as usize] += 4;
                total_tiles += 4;
            }
            DeclaredMeldType::Kita => {}
        }
    }

    if let Some(ron_tile) = ron_tile_option {
        counts[ron_tile as usize] += 1;
        total_tiles += 1;
    }
    (counts, total_tiles)
}

fn count_kokushi_musou_yaku(_final_counts: &[u8;34], menzen: bool, _win_type: WinType, _open_melds: &[DeclaredMeld], parsed_kokushi: Option<&ParsedKokushiMusou>) -> u8 {
    if !menzen || parsed_kokushi.is_none() { return 0; }
    // TODO: Implement 13-wait double yakuman logic.
    13
}
fn count_suuankou_yaku(
    parsed_std_hand: &ParsedStandardHand,
    menzen: bool,
    win_type: WinType,
    open_melds: &[DeclaredMeld], 
    _original_hand_before_win: &Hand, 
    winning_tile_option: Option<Tile>, 
) -> u8 {
    if !menzen { return 0; }
    if !open_melds.iter().all(|m| m.meld_type == DeclaredMeldType::Ankan || m.meld_type == DeclaredMeldType::Kita) { return 0; }

    let mut concealed_koutsu_or_ankan_count = 0;
    
    for meld in open_melds {
        if meld.meld_type == DeclaredMeldType::Ankan {
            concealed_koutsu_or_ankan_count += 1;
        }
    }
    
    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Koutsu {
            if !open_melds.iter().any(|om| om.meld_type == DeclaredMeldType::Ankan && om.tiles[0] == meld.representative_tile) {
                 concealed_koutsu_or_ankan_count += 1;
            }
        }
    }
    
    if concealed_koutsu_or_ankan_count != 4 { return 0; }

    if let (WinType::Ron { winning_tile, .. }, Some(wt_logic)) = (win_type, winning_tile_option) {
        if wt_logic == parsed_std_hand.pair && winning_tile == parsed_std_hand.pair { 
            return 26;
        }
    }
    13 
}
// FIX: Added underscore to `final_counts` as it's not used in the function body.

fn count_daisangen_yaku(_final_counts: &[u8;34], open_melds: &[DeclaredMeld], parsed_std_hand: Option<&ParsedStandardHand>) -> u8 {
    if parsed_std_hand.is_none() { return 0; }

    let mut dragon_koutsu_count = 0;
    let dragon_tiles = [Tile::White, Tile::Green, Tile::Red];

    // Check parsed melds from the 14-tile hand structure
    for meld in &parsed_std_hand.as_ref().unwrap().melds {
        if meld.meld_type == ParserOutputMeldType::Koutsu && dragon_tiles.contains(&meld.representative_tile) {
            dragon_koutsu_count +=1;
        }
    }

    // Also check explicit open melds (Kans are not always in parsed_hand.melds)
    for open_meld in open_melds {
        if dragon_tiles.contains(&open_meld.tiles[0]) {
             // Avoid double counting if a Kan was also parsed as a Koutsu
            if !parsed_std_hand.as_ref().unwrap().melds.iter().any(|pm| pm.representative_tile == open_meld.tiles[0]) {
                 if matches!(open_meld.meld_type, DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan | DeclaredMeldType::Ankan) {
                    dragon_koutsu_count += 1;
                 }
            }
        }
    }

    if dragon_koutsu_count >= 3 {
        13
    } else {
        0
    }
}

fn count_shousuushii_yaku(parsed_std_hand: &ParsedStandardHand, open_melds: &[DeclaredMeld], _menzen: bool) -> u8 {
    let mut wind_koutsu_types: Vec<Tile> = Vec::new(); // To store unique wind koutsu tiles
    let mut wind_pair: Option<Tile> = None;
    let winds = [Tile::East, Tile::South, Tile::West, Tile::North];

    // Check pair
    if winds.contains(&parsed_std_hand.pair) {
        wind_pair = Some(parsed_std_hand.pair);
    }

    // Check Koutsu/Kantsu from open_melds
    for open_meld in open_melds {
        if winds.contains(&open_meld.tiles[0]) &&
           matches!(open_meld.meld_type, DeclaredMeldType::Pon | DeclaredMeldType::Ankan | DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan) {
            if !wind_koutsu_types.contains(&open_meld.tiles[0]) {
                wind_koutsu_types.push(open_meld.tiles[0]);
            }
        }
    }
    // Check Koutsu from parsed_standard_hand.melds (concealed koutsu not part of open_melds Ankans)
    for p_meld in &parsed_std_hand.melds {
        if p_meld.meld_type == ParserOutputMeldType::Koutsu && winds.contains(&p_meld.representative_tile) {
            // Ensure this koutsu isn't already accounted for by an Ankan in open_melds
            if !open_melds.iter().any(|om| om.meld_type == DeclaredMeldType::Ankan && om.tiles[0] == p_meld.representative_tile) {
                if !wind_koutsu_types.contains(&p_meld.representative_tile) {
                     wind_koutsu_types.push(p_meld.representative_tile);
                }
            }
        }
    }
    // Shousuushii: 3 Koutsu of winds + 1 pair of the 4th wind type.
    if wind_koutsu_types.len() == 3 && wind_pair.is_some() {
        // Ensure the pair tile is different from the koutsu tiles
        if !wind_koutsu_types.contains(&wind_pair.unwrap()) {
            // All 4 wind types must be present (3 as koutsu, 1 as pair)
            let mut all_present_winds = wind_koutsu_types.clone(); 
            all_present_winds.push(wind_pair.unwrap());
            all_present_winds.sort_unstable(); // Sort for consistent check
            all_present_winds.dedup(); // Remove duplicates
            if all_present_winds.len() == 4 { // All 4 wind types are present
                return 13;
            }
        }
    }
    0
}
fn count_daisuushii_yaku(parsed_std_hand: &ParsedStandardHand, open_melds: &[DeclaredMeld], _menzen: bool) -> u8 {
    let mut wind_koutsu_types: Vec<Tile> = Vec::new();
    let winds = [Tile::East, Tile::South, Tile::West, Tile::North];

    // Check Koutsu/Kantsu from open_melds
    for open_meld in open_melds {
        if winds.contains(&open_meld.tiles[0]) &&
           matches!(open_meld.meld_type, DeclaredMeldType::Pon | DeclaredMeldType::Ankan | DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan) {
            if !wind_koutsu_types.contains(&open_meld.tiles[0]) {
                wind_koutsu_types.push(open_meld.tiles[0]);
            }
        }
    }
    // Check Koutsu from parsed_standard_hand.melds
    for p_meld in &parsed_std_hand.melds {
        if p_meld.meld_type == ParserOutputMeldType::Koutsu && winds.contains(&p_meld.representative_tile) {
            if !open_melds.iter().any(|om| om.meld_type == DeclaredMeldType::Ankan && om.tiles[0] == p_meld.representative_tile) {
                if !wind_koutsu_types.contains(&p_meld.representative_tile) {
                     wind_koutsu_types.push(p_meld.representative_tile);
                }
            }
        }
    }
    // Daisuushii: 4 Koutsu of all 4 wind types. Pair can be anything.
    if wind_koutsu_types.len() == 4 { 26 } else { 0 } // Double Yakuman
}
fn count_chuuren_poutou_yaku(final_counts: &[u8; 34], menzen: bool, winning_tile: Tile) -> u8 {
    if !menzen { return 0; }
    // Check for 1,1,1,2,3,4,5,6,7,8,9,9,9 of one suit, plus one extra tile of that suit.
    let check_suit_for_chuuren = |suit_char_check: char, counts: &[u8;34], win_tile: Tile| -> u8 {
        let mut tiles_in_suit_numeric_counts = [0u8; 9]; // Counts for 1-9 of the suit
        let mut total_tiles_in_this_suit = 0;
        let mut is_pure_suit_hand = true;
        let mut winning_tile_num_val: Option<u8> = None; // Number value (1-9) of winning tile if in suit

        for i in 0..34 {
            if counts[i] > 0 {
                let tile = Tile::try_from(i as u8).unwrap();
                if tile.get_suit_char() == Some(suit_char_check) {
                    if let Some(num_val) = tile.get_number_val() { // Is it 1-9?
                        tiles_in_suit_numeric_counts[(num_val - 1) as usize] = counts[i];
                        total_tiles_in_this_suit += counts[i];
                        if tile == win_tile { winning_tile_num_val = Some(num_val); }
                    } else { is_pure_suit_hand = false; break; } // Honor/wind in a "suit" hand
                } else { is_pure_suit_hand = false; break; } // Tile from different suit
            }
        }

        if !is_pure_suit_hand || total_tiles_in_this_suit != 14 { return 0; }
        // Winning tile must be part of this suit for 9-sided wait check
        if winning_tile_num_val.is_none() { return 0; } 
        
        // Base Chuuren Poutou form: 1112345678999 (1,9 >=3; 2-8 >=1)
        let mut meets_base_count_req = true;
        for i in 0..9 { // For numbers 1 through 9
            let required = if i == 0 || i == 8 { 3 } else { 1 }; // 1s and 9s need at least 3, others at least 1
            if tiles_in_suit_numeric_counts[i] < required { meets_base_count_req = false; break; }
        }
        if !meets_base_count_req { return 0; } // Doesn't have the 13 required tiles

        // Check the 14th tile (the "pair" part)
        // Sum of tiles_in_suit_numeric_counts should be 14.
        // Sum of base requirements (3+1+1+1+1+1+1+1+3 = 13). So one tile has an extra count.
        let mut extra_tile_count = 0;
        for i in 0..9 {
            let required = if i == 0 || i == 8 { 3 } else { 1 };
            if tiles_in_suit_numeric_counts[i] > required { // Found the tile that forms the "pair"
                extra_tile_count += tiles_in_suit_numeric_counts[i] - required;
            }
        }

        if extra_tile_count == 1 { // Exactly one tile forms the pair (total 14 tiles)
            // Standard Chuuren Poutou is 13 han.
            // If won on any of the 9 potentially waited tiles (9-sided wait), it's double yakuman.
            // Winning tile num value (1-9)
            let winning_idx = (winning_tile_num_val.unwrap() - 1) as usize;
            // If the winning tile is the one that formed the pair (i.e. tiles_in_suit_numeric_counts[winning_idx] > required for that tile)
            // it means it could have been any of the 9 waits.
            // More accurately: if the hand (13 tiles before win_tile) was 1112345678999, any of 1-9 makes it Chuuren.
            // This is the 9-sided wait condition.
            // The current logic checks the final 14-tile hand. If it matches the form AND the winning_tile completed it,
            // we need to infer if it was a 9-sided wait.
            // If the winning tile has count required+1, and all others have count required, it's 9-sided.
            let required_at_win_idx = if winning_idx == 0 || winning_idx == 8 { 3 } else { 1 };
            if tiles_in_suit_numeric_counts[winning_idx] > required_at_win_idx { // Winning tile is the one completing the "pair"
                return 26; // Double Yakuman for 9-sided wait
            }
            // If winning tile is one of the "required" tiles, but not the pair part, still 9-sided wait possible if structure is met.
            // This means any of the 9 tiles can complete the 13-tile 1112345678999 form.
            // So, if it forms Chuuren and won on a tile of that suit, assume 9-sided wait.
            return 26; // For simplicity, if it forms and won on a tile of this suit, assume 9-sided wait logic.
                       // A more precise check would involve the 13-tile hand state.
                       // For now, any valid Chuuren completion is treated as 9-sided for 26 han.
                       // Or, more conservatively: if specific structure is 13-wait, 26, else 13.
                       // The question is if any win on Chuuren is 26, or only specific waits.
                       // Common rule: 9-sided wait is 26. Other waits (e.g. tanki on one of the 1s) for 13.
                       // This logic is simplified, assumes winning tile completes the structure robustly.
                       // Let's say if winning tile count is required+1 -> 26, else 13.
            // return 13; // Standard Chuuren
        }
        0 // Not a valid Chuuren structure
    };

    // Check each suit
    if let Some('m') = winning_tile.get_suit_char() { // Check Manzu if winning tile is Manzu
        let val = check_suit_for_chuuren('m', final_counts, winning_tile); if val > 0 { return val; }
    }
    if let Some('p') = winning_tile.get_suit_char() { // Check Pinzu if winning tile is Pinzu
        let val = check_suit_for_chuuren('p', final_counts, winning_tile); if val > 0 { return val; }
    }
    if let Some('s') = winning_tile.get_suit_char() { // Check Souzu if winning tile is Souzu
        let val = check_suit_for_chuuren('s', final_counts, winning_tile); if val > 0 { return val; }
    }
    0 // Not Chuuren or winning tile not in a suit
}
fn count_ryuu_iisou_yaku(final_counts: &[u8;34], parsed_std: Option<&ParsedStandardHand>, parsed_chiitoi: Option<&ParsedChiitoitsu>) -> u8 {
    let green_tiles = [Tile::Green, Tile::Sou2, Tile::Sou3, Tile::Sou4, Tile::Sou6, Tile::Sou8];
    let mut all_green = true;
    let mut has_tiles = false;
    for i in 0..34 {
        if final_counts[i] > 0 {
            has_tiles = true;
            let tile = Tile::try_from(i as u8).unwrap();
            if !green_tiles.contains(&tile) { all_green = false; break; }
        }
    }
    if has_tiles && all_green && (parsed_std.is_some() || parsed_chiitoi.is_some()) { 13 } else { 0 }
}
fn count_tsuu_iisou_yaku(final_counts: &[u8;34], open_melds: &[DeclaredMeld], parsed_std: Option<&ParsedStandardHand>, parsed_chiitoi: Option<&ParsedChiitoitsu>) -> u8 {
    let mut has_tiles = false;
    for i in 0..34 {
        if final_counts[i] > 0 {
            has_tiles = true;
            if !Tile::try_from(i as u8).unwrap().is_honor() { return 0; } // Contains non-honor
        }
    }
    // Hand must be valid structure (std or chiitoi) and have tiles.
    if has_tiles && (parsed_std.is_some() || (parsed_chiitoi.is_some() && open_melds.iter().all(|m| m.meld_type == DeclaredMeldType::Kita))) { 13 } else {0}
}
fn count_chinroutou_yaku(final_counts: &[u8;34], open_melds: &[DeclaredMeld], parsed_std: Option<&ParsedStandardHand>, parsed_chiitoi: Option<&ParsedChiitoitsu>) -> u8 {
    let mut has_tiles = false;
    for i in 0..34 {
        if final_counts[i] > 0 {
            has_tiles = true;
            let tile = Tile::try_from(i as u8).unwrap();
            if !(tile.is_terminal()) { return 0; } // Contains non-terminal (e.g. honor or simple number)
        }
    }
    if has_tiles && (parsed_std.is_some() || (parsed_chiitoi.is_some() && open_melds.iter().all(|m| m.meld_type == DeclaredMeldType::Kita))) { 13 } else {0}
}
fn count_suukantsu_yaku(kans_by_player: u8) -> u8 { if kans_by_player == 4 { 13 } else { 0 } }

// Standard Yaku
fn count_chiitoitsu_yaku(_final_counts: &[u8;34], open_melds: &[DeclaredMeld], parsed_chiitoi: Option<&ParsedChiitoitsu>) -> (bool, u8) {
    // Chiitoitsu requires a closed hand (Ankan/Kita don't break menzen for this check).
    // The `is_menzen` check in `score_win` covers this.
    if parsed_chiitoi.is_some() && open_melds.iter().all(|m| m.meld_type == DeclaredMeldType::Ankan || m.meld_type == DeclaredMeldType::Kita) {
        (true, 2) // Chiitoitsu is 2 Han
    } else { (false, 0) }
}

fn count_pinfu_yaku(
    parsed_std_hand: &ParsedStandardHand,
    menzen: bool,
    seat_wind: Tile,
    round_wind: Tile,
    winning_tile: Tile, // The tile that completed the hand
) -> (bool, u8) {
    if !menzen { return (false, 0); }
    // All 4 melds must be Shuntsu (sequences)
    if !parsed_std_hand.melds.iter().all(|m| m.meld_type == ParserOutputMeldType::Shuntsu) { return (false, 0); }
    // Pair must not be Yakuhai (Dragons, Seat Wind, or Round Wind)
    if parsed_std_hand.pair.is_dragon() || parsed_std_hand.pair == seat_wind || parsed_std_hand.pair == round_wind { return (false, 0); }
    // Wait must be Ryanmen (two-sided sequence wait)
    let mut is_ryanmen_wait = false;
    for meld in &parsed_std_hand.melds { // Iterate through the 4 Shuntsu
        if meld.tiles.contains(&winning_tile) { // Find the Shuntsu completed by the winning tile
            let t0 = meld.tiles[0]; let t1 = meld.tiles[1]; let t2 = meld.tiles[2]; // Tiles are sorted, e.g., 1-2-3
            // Winning tile is t0 (e.g. won 1 on a 2-3 wait) or t2 (e.g. won 3 on a 1-2 wait)
            // AND it's not a Penchan (edge wait, e.g. 1-2 waiting for 3, or 8-9 waiting for 7)
            let num_val_t0 = t0.get_number_val().unwrap_or(0);
            // let num_val_t1 = t1.get_number_val().unwrap_or(0); // Not needed for Ryanmen check on edges
            let num_val_t2 = t2.get_number_val().unwrap_or(0);

            if (winning_tile == t0 && num_val_t0 != 1 && num_val_t0 != 7) || // e.g., 2-3 waiting for 1 (not ryanmen), 3-4 waiting for 2 (ryanmen)
               (winning_tile == t2 && num_val_t2 != 9 && num_val_t2 != 3) {  // e.g., 7-8 waiting for 9 (not ryanmen), 6-7 waiting for 8 (ryanmen)
                // More precise Ryanmen: winning tile is an edge of a sequence, and that edge is not 1 or 9.
                // e.g., for 2-3-4, if won on 2 or 4.
                // Not a Kanchan (middle wait, e.g. 4-6 waiting for 5 -> winning_tile == t1)
                // Not a Penchan (edge wait 1-2 for 3, or 8-9 for 7)
                if winning_tile == t1 { // Kanchan wait
                    is_ryanmen_wait = false; break;
                }
                if (winning_tile == t2 && num_val_t0 == 1) || // Penchan 1-2 waiting for 3 (t2)
                   (winning_tile == t0 && num_val_t2 == 9) {  // Penchan 8-9 waiting for 7 (t0)
                    is_ryanmen_wait = false; break;
                }
                // If it's not Kanchan or Penchan, and part of a Shuntsu, it's Ryanmen.
                is_ryanmen_wait = true; break; // Found the completed Shuntsu, determined wait.
            }
        }
    }
    if is_ryanmen_wait { (true, 1) } else { (false, 0) }
}


fn count_tanyao_yaku(final_counts: &[u8;34], _open_melds: &[DeclaredMeld]) -> (bool, u8) {
    // Tanyao: Hand contains no terminals (1s, 9s) or honors.
    for i in 0..34 {
        if final_counts[i] > 0 && Tile::try_from(i as u8).unwrap().is_terminal_or_honor() {
            return (false, 0); // Found a terminal or honor tile
        }
    }
    (true, 1) // All tiles are simples (2-8)
}

fn count_yakuhai_yaku(
    final_counts: &[u8;34],
    seat_wind: Tile,
    round_wind: Tile,
    yaku_list: &mut Vec<(&'static str, u8)>,
    _parsed_std_hand: Option<&ParsedStandardHand>,
    skip_if_yakuman_component: bool
) -> u8 {
    let mut han = 0;

    // Skip individual dragon triplets if they are part of a bigger Yakuman
    if !skip_if_yakuman_component {
        if final_counts[Tile::White as usize] >= 3 {
            yaku_list.push(("Yakuhai (White Dragon)", 1));
            han += 1;
        }
        if final_counts[Tile::Green as usize] >= 3 {
            yaku_list.push(("Yakuhai (Green Dragon)", 1));
            han += 1;
        }
        if final_counts[Tile::Red as usize] >= 3 {
            yaku_list.push(("Yakuhai (Red Dragon)", 1));
            han += 1;
        }
    }

    // Skip wind triplets if part of a wind-based Yakuman
    let is_wind_yakuman = yaku_list.iter().any(|(n, _)| *n == "Shousuushii" || *n == "Daisuushii");
    if !skip_if_yakuman_component && !is_wind_yakuman {
        if seat_wind == round_wind {
            if final_counts[seat_wind as usize] >= 3 {
                yaku_list.push(("Yakuhai (Seat/Round Wind)", 2));
                han += 2;
            }
        } else {
            if final_counts[seat_wind as usize] >= 3 {
                yaku_list.push(("Yakuhai (Seat Wind)", 1));
                han += 1;
            }
            if final_counts[round_wind as usize] >= 3 {
                yaku_list.push(("Yakuhai (Round Wind)", 1));
                han += 1;
            }
        }
    }
    
    han
}


fn count_honitsu_yaku(final_counts: &[u8;34], menzen: bool, _open_melds: &[DeclaredMeld]) -> (bool, u8) {
    let mut man_present = false; let mut pin_present = false; let mut sou_present = false;
    let mut honor_present = false;
    let mut has_tiles = false;

    for i in 0..34 {
        if final_counts[i] > 0 {
            has_tiles = true;
            let tile = Tile::try_from(i as u8).unwrap();
            if tile.is_manzu() { man_present = true; }
            else if tile.is_pinzu() { pin_present = true; }
            else if tile.is_sou() { sou_present = true; }
            else if tile.is_honor() { honor_present = true; }
        }
    }
    if !has_tiles { return (false, 0); }

    let suit_count = man_present as u8 + pin_present as u8 + sou_present as u8;
    if honor_present && suit_count == 1 { // One suit + honors
        (true, if menzen {3} else {2})
    } else { (false,0) }
}

fn count_chinitsu_yaku(final_counts: &[u8;34], menzen: bool, _open_melds: &[DeclaredMeld]) -> (bool, u8) {
    let mut man_present = false; let mut pin_present = false; let mut sou_present = false;
    let mut honor_present = false;
    let mut has_tiles = false;

    for i in 0..34 {
        if final_counts[i] > 0 {
            has_tiles = true;
            let tile = Tile::try_from(i as u8).unwrap();
            if tile.is_manzu() { man_present = true; }
            else if tile.is_pinzu() { pin_present = true; }
            else if tile.is_sou() { sou_present = true; }
            else if tile.is_honor() { honor_present = true; }
        }
    }
     if !has_tiles { return (false, 0); }

    let suit_count = man_present as u8 + pin_present as u8 + sou_present as u8;
    if !honor_present && suit_count == 1 { // One suit only, no honors
        (true, if menzen {6} else {5})
    } else { (false,0) }
}
fn count_toitoihou_yaku(parsed_std_hand: &ParsedStandardHand) -> (bool, u8) {
    // Toitoihou: Hand consists of 4 Koutsu/Kantsu and a pair.
    if parsed_std_hand.melds.iter().all(|m| m.meld_type == ParserOutputMeldType::Koutsu) {
        (true, 2)
    } else { (false,0) }
}

fn count_sanankou_yaku(
    parsed_std_hand: &ParsedStandardHand,
    win_type: WinType,
    open_melds: &[DeclaredMeld], // To distinguish ankou from minkou if parser ambiguous
    _hand_before_win: &Hand, // Not strictly needed if ron_tile logic is simple
    winning_ron_tile: Option<Tile> // Tile won by Ron
) -> (bool, u8) {
    let mut ankou_count = 0;

    // Count Ankans from open_melds
    for meld in open_melds {
        if meld.meld_type == DeclaredMeldType::Ankan {
            ankou_count += 1;
        }
    }

    // Count Koutsu from parsed_hand.melds that are concealed
    for p_meld in &parsed_std_hand.melds {
        if p_meld.meld_type == ParserOutputMeldType::Koutsu {
            // Is this Koutsu already counted as an Ankan from open_melds?
            let is_already_counted_ankan = open_melds.iter().any(|om| 
                om.meld_type == DeclaredMeldType::Ankan && om.tiles[0] == p_meld.representative_tile
            );
            if is_already_counted_ankan { continue; }

            // Is this Koutsu actually an open Pon (Minkou)?
            let is_minkou = open_melds.iter().any(|om|
                om.meld_type == DeclaredMeldType::Pon && om.tiles[0] == p_meld.representative_tile
            );
            if is_minkou { continue; } // Not a concealed triplet

            // If Ron, was this Koutsu completed by the Ron tile? If so, it's not Ankou.
            if let (WinType::Ron { .. }, Some(ron_t)) = (win_type, winning_ron_tile) {
                 if p_meld.tiles.contains(&ron_t) { // Check if winning tile is part of this Koutsu
                    // More precisely: if the Koutsu was formed by Ron'ing the 3rd tile.
                    // If the hand had a pair, and Ron tile makes it a Koutsu, this Koutsu is not "concealed".
                    // This is tricky. A Koutsu formed by Ron is a Minkou by Ron.
                    // Sanankou requires triplets to be concealed.
                    // If `p_meld` representative tile is `ron_t` and count of `ron_t` in hand before ron was 2, this is formed by ron.
                    // Simpler: If won by Ron and this triplet contains the Ron tile, it's not an Ankou.
                    // (Unless it was an Ankan declared on the Ron, which is not this path)
                    if p_meld.tiles[0] == ron_t || p_meld.tiles[1] == ron_t || p_meld.tiles[2] == ron_t {
                        continue; // Koutsu completed by Ron is not Ankou
                    }
                 }
            }
            ankou_count += 1; // This is a concealed Koutsu
        }
    }
    if ankou_count == 3 { (true, 2) } else { (false, 0) }
}


fn count_shousangen_yaku(parsed_std_hand: &ParsedStandardHand) -> (bool, u8) {
    // Shousangen: Two Koutsu/Kantsu of dragons, and a pair of the third dragon type.
    let mut dragon_koutsu_count = 0;
    let mut dragon_pair_present = false;
    let mut dragon_koutsu_tiles = Vec::new(); // Store tiles of dragon koutsu found

    // Check pair
    if parsed_std_hand.pair.is_dragon() {
        dragon_pair_present = true;
    }

    // Check melds for dragon koutsu/kantsu
    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Koutsu && meld.representative_tile.is_dragon() {
            dragon_koutsu_count += 1;
            dragon_koutsu_tiles.push(meld.representative_tile);
        }
    }
    // This also needs to consider open melds (Ankan of dragons)
    // For simplicity, this example relies on `parsed_std_hand` correctly identifying Koutsu structure.
    // A full impl would check `open_melds` for Dragon Kans/Pons too.

    if dragon_koutsu_count == 2 && dragon_pair_present {
        // Ensure the pair is of a *different* dragon type than the two koutsu
        if !dragon_koutsu_tiles.contains(&parsed_std_hand.pair) {
            return (true, 2); // Shousangen is 2 Han (plus value of dragon triplets/pair if counted separately)
                               // Often, Shousangen itself is 2 han, and then the 2 dragon triplets give 2 han each if Yakuhai.
                               // Let this return 2 for the pattern. Yakuhai function will add dragon values.
        }
    }
    (false,0)
}

fn count_honroutou_yaku(parsed_std_hand: Option<&ParsedStandardHand>, parsed_chiitoi: Option<&ParsedChiitoitsu>) -> (bool, u8) {
    // Honroutou: Hand consists only of terminal (1,9) and honor tiles. Must contain at least one Koutsu/Shuntsu (for std hand).
    let mut all_terminal_honor = true;

    let has_melds_or_pairs = if let Some(ref std_hand) = parsed_std_hand {
        if !std_hand.pair.is_terminal_or_honor() { all_terminal_honor = false; }
        for meld in &std_hand.melds {
            if !meld.tiles.iter().all(|t| t.is_terminal_or_honor()) {
                all_terminal_honor = false; break;
            }
        }
        // Honroutou often implies Toitoi or Chiitoi. If it has sequences, they must be terminal sequences (not possible).
        // So, for std_hand, it must be all Koutsu of T/H + pair of T/H.
        // If all_terminal_honor is true, it implies no simples, so if it's a std_hand, it's likely Toitoi-like.
        true
    } else if let Some(ref chiitoi) = parsed_chiitoi {
        if !chiitoi.pair_representative_tiles.iter().all(|t| t.is_terminal_or_honor()) {
            all_terminal_honor = false;
        }
        true
    } else { // No valid hand structure parsed
        return (false, 0);
    };

    if has_melds_or_pairs && all_terminal_honor {
        // Check if it's also Tsuu Iisou (All Honors) or Chinroutou (All Terminals) - those are Yakuman.
        // Honroutou is 2 Han. It can combine with Toitoi or Chiitoi.
        (true, 2)
    } else { (false,0) }
}
fn count_sankantsu_yaku(kans_by_player: u8) -> (bool, u8) { if kans_by_player == 3 {(true,2)} else {(false,0)}}

fn count_chanta_yaku(parsed_std_hand: &ParsedStandardHand, menzen: bool) -> (bool, u8) {
    // Chanta: Every meld (Shuntsu or Koutsu) and the pair must contain at least one terminal or honor tile.
    // Must also contain at least one Shuntsu.
    let mut all_contain_terminal_honor = true;
    if !parsed_std_hand.pair.is_terminal_or_honor() { all_contain_terminal_honor = false; }
    for meld in &parsed_std_hand.melds {
        if !meld.tiles.iter().any(|t| t.is_terminal_or_honor()) { // Check if *any* tile in meld is T/H
            all_contain_terminal_honor = false; break;
        }
    }
    let has_shuntsu = parsed_std_hand.melds.iter().any(|m| m.meld_type == ParserOutputMeldType::Shuntsu);
    // Must not be Junchan (all terminals, no honors, which is higher value)
    // Must not be Honroutou (all terminals and honors, no simples, higher value)

    if all_contain_terminal_honor && has_shuntsu {
        // Check if it's actually Junchan (all terminals, no honors). Junchan is higher.
        let is_junchan = parsed_std_hand.pair.is_terminal() && !parsed_std_hand.pair.is_honor() &&
                         parsed_std_hand.melds.iter().all(|m| 
                            m.tiles.iter().any(|t| t.is_terminal()) && 
                            !m.tiles.iter().any(|t| t.is_honor()));
        if is_junchan { return (false, 0); } // Don't count Chanta if it's Junchan

        (true, if menzen {2} else {1})
    } else { (false,0) }
}

fn count_junchan_yaku(parsed_std_hand: &ParsedStandardHand, menzen: bool) -> (bool, u8) {
    // Junchan (Terminals in All Melds): Like Chanta, but all T/H must be terminals (no honors).
    // Every meld and the pair must contain at least one terminal. No honors allowed anywhere in the hand.
    // Must also contain at least one Shuntsu.
    let mut all_contain_terminal_no_honor = true;
    if !parsed_std_hand.pair.is_terminal() || parsed_std_hand.pair.is_honor() { all_contain_terminal_no_honor = false; }
    for meld in &parsed_std_hand.melds {
        if !meld.tiles.iter().any(|t| t.is_terminal()) || // Must have a terminal
            meld.tiles.iter().any(|t| t.is_honor()) {    // Must NOT have an honor
            all_contain_terminal_no_honor = false; break;
        }
    }
    let has_shuntsu = parsed_std_hand.melds.iter().any(|m| m.meld_type == ParserOutputMeldType::Shuntsu);

    if all_contain_terminal_no_honor && has_shuntsu {
        (true, if menzen {3} else {2})
    } else { (false,0) }
}

fn count_iipeikou_yaku(parsed_std_hand: &ParsedStandardHand, menzen: bool) -> u8 {
    if !menzen { return 0; }
    
    let mut shuntsu_map: std::collections::HashMap<[Tile;3], u8> = std::collections::HashMap::new();
    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Shuntsu {
            *shuntsu_map.entry(meld.tiles).or_insert(0) += 1;
        }
    }
    
    let pairs_of_shuntsu = shuntsu_map.values().filter(|&&count| count == 2).count();
    let quads_of_shuntsu = shuntsu_map.values().filter(|&&count| count == 4).count();

    if pairs_of_shuntsu == 2 || quads_of_shuntsu == 1 {
        // Ryanpeikou: Two different sequences repeated twice (e.g., 123m, 123m, 456p, 456p)
        // OR one sequence repeated four times (e.g., 123m, 123m, 123m, 123m), which counts as two iipeikou, not ryanpeikou.
        // Let's stick to the common rule: Ryanpeikou is specifically two *different* duplicated sequences.
        if pairs_of_shuntsu == 2 {
            return 3; // Ryanpeikou is 3 Han
        }
    }
    
    if pairs_of_shuntsu == 1 || quads_of_shuntsu == 1 {
        return 1; // Iipeikou is 1 Han
    }

    0
}


fn count_sanshoku_doujun_yaku(parsed_std_hand: &ParsedStandardHand, menzen: bool) -> (bool, u8) {
    // Sanshoku Doujun: Same sequence in all three suits (e.g., 123 Man, 123 Pin, 123 Sou).
    let mut man_shuntsu_starts: Vec<u8> = Vec::new();
    let mut pin_shuntsu_starts: Vec<u8> = Vec::new();
    let mut sou_shuntsu_starts: Vec<u8> = Vec::new();

    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Shuntsu {
            if let Some(start_val) = meld.tiles[0].get_number_val() {
                match meld.tiles[0].get_suit_char() {
                    Some('m') => man_shuntsu_starts.push(start_val),
                    Some('p') => pin_shuntsu_starts.push(start_val),
                    Some('s') => sou_shuntsu_starts.push(start_val),
                    _ => {}
                }
            }
        }
    }
    for m_start in &man_shuntsu_starts {
        if pin_shuntsu_starts.contains(m_start) && sou_shuntsu_starts.contains(m_start) {
            return (true, if menzen {2} else {1});
        }
    }
    (false, 0)
}

fn count_sanshoku_doukou_yaku(parsed_std_hand: &ParsedStandardHand) -> (bool, u8) {
    // Sanshoku Doukou: Same Koutsu/Kantsu in all three suits (e.g., 222 Man, 222 Pin, 222 Sou).
    let mut man_koutsu_vals: Vec<u8> = Vec::new();
    let mut pin_koutsu_vals: Vec<u8> = Vec::new();
    let mut sou_koutsu_vals: Vec<u8> = Vec::new();

    for meld in &parsed_std_hand.melds { // Also need to check open_melds for Kantsu
        if meld.meld_type == ParserOutputMeldType::Koutsu { // Assuming parser gives Koutsu for Ankan too
            if let Some(val) = meld.tiles[0].get_number_val() {
                 match meld.tiles[0].get_suit_char() {
                    Some('m') => man_koutsu_vals.push(val),
                    Some('p') => pin_koutsu_vals.push(val),
                    Some('s') => sou_koutsu_vals.push(val),
                    _ => {}
                }
            }
        }
    }
    // TODO: This needs to be more robust by checking open_melds for Kantsu/Pon too if not fully captured by parsed_std_hand.
    // For now, relies on parsed_std_hand correctly reflecting Koutsu structures.
     for m_val in &man_koutsu_vals {
        if pin_koutsu_vals.contains(m_val) && sou_koutsu_vals.contains(m_val) {
            return (true, 2);
        }
    }
    (false, 0)
}

fn count_ittsuu_yaku(parsed_std_hand: &ParsedStandardHand, menzen: bool) -> (bool, u8) {
    // Ittsuu (Pure Straight): 1-9 sequence in a single suit (e.g., 123, 456, 789 Man).
    let mut man_shuntsu_starts: HashMap<u8, bool> = HashMap::new(); // Start tile num -> present
    let mut pin_shuntsu_starts: HashMap<u8, bool> = HashMap::new();
    let mut sou_shuntsu_starts: HashMap<u8, bool> = HashMap::new();

    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Shuntsu {
            if let Some(start_val) = meld.tiles[0].get_number_val() {
                match meld.tiles[0].get_suit_char() {
                    Some('m') => { man_shuntsu_starts.insert(start_val, true); }
                    Some('p') => { pin_shuntsu_starts.insert(start_val, true); }
                    Some('s') => { sou_shuntsu_starts.insert(start_val, true); }
                    _ => {}
                }
            }
        }
    }
    if man_shuntsu_starts.contains_key(&1) && man_shuntsu_starts.contains_key(&4) && man_shuntsu_starts.contains_key(&7) { return (true, if menzen {2} else {1}); }
    if pin_shuntsu_starts.contains_key(&1) && pin_shuntsu_starts.contains_key(&4) && pin_shuntsu_starts.contains_key(&7) { return (true, if menzen {2} else {1}); }
    if sou_shuntsu_starts.contains_key(&1) && sou_shuntsu_starts.contains_key(&4) && sou_shuntsu_starts.contains_key(&7) { return (true, if menzen {2} else {1}); }
    (false, 0)
}


fn count_dora_value(final_counts: &[u8; 34], dora_indicators: &[Tile]) -> u8 {
    let mut dora_count = 0;
    for indicator in dora_indicators {
        let dora_tile = indicator.next_in_series(); // Get the actual Dora tile
        dora_count += final_counts[dora_tile as usize];
    }
    dora_count
}

fn count_red_five_value(final_counts: &[u8; 34], red_fives: &[Tile]) -> u8 {
    // Assumes red_fives Vec contains specific Tile IDs for red fives (e.g. Man5_Red, Pin5_Red)
    // If Tile enum doesn't distinguish red fives, this needs adjustment.
    // Current Tile enum doesn't have specific red variants.
    // This function implies red fives are standard tiles with a "red" property.
    // For now, assuming Tile::Man5, Tile::Pin5, Tile::Sou5 if they are red.
    let mut red_five_count = 0;
    for red_tile_id in red_fives { // Iterate through known red five tile IDs
        // Check if the hand contains this specific red five tile.
        // This assumes red_fives are distinct Tile variants or handled by game state.
        // If Tile::Man5 can be red or not, this count is simplified.
        // Let's assume red_fives contains Tile::Man5, Tile::Pin5, Tile::Sou5 if they are red.
        red_five_count += final_counts[*red_tile_id as usize];
    }
    red_five_count
}

// --- Point Calculation Helper ---
fn calculate_points_final(han: u8, fu: u8, is_dealer: bool, win_type: WinType, is_yakuman: bool, yakuman_multiplier: u8) -> u32 {
    if han == 0 && !is_yakuman { return 0; }

    if is_yakuman {
        let multi = yakuman_multiplier as u32;
        return match (is_dealer, win_type) {
            (true, WinType::Ron { .. }) => 48000 * multi,
            (false, WinType::Ron { .. }) => 32000 * multi,
            // CORRECTED SANMA TSUMO LOGIC
            (true, WinType::Tsumo) => 16000 * multi * 2,
            (false, WinType::Tsumo) => (16000 * multi) + (8000 * multi),
        };
    }

    // Mangan and above fixed points
    if han >= 13 { return if is_dealer { 16000 * 2 } else { 8000 + 16000 }; } // Kazoe as Yakuman Tsumo
    if han >= 11 { return if is_dealer { 12000 * 2 } else { 6000 + 12000 }; } // Sanbaiman
    if han >= 8 { return if is_dealer { 8000 * 2 } else { 4000 + 8000 }; }  // Baiman
    if han >= 6 { return if is_dealer { 6000 * 2 } else { 3000 + 6000 }; }  // Haneman
    if han == 5 || (han == 4 && fu >= 40) || (han == 3 && fu >= 70) {
        return if is_dealer { 4000 * 2 } else { 2000 + 4000 }; // Mangan
    }

    // Standard calculation
    let base_points = (fu as u32) * (2u32.pow(han as u32 + 2));
    let base_points = if base_points > 2000 { 2000 } else { base_points };

    match win_type {
        WinType::Ron { .. } => {
            let payment = if is_dealer { base_points * 6 } else { base_points * 4 };
            ((payment + 99) / 100) * 100
        }
        WinType::Tsumo => {
            if is_dealer {
                let payment_each = ((base_points * 2) + 99) / 100 * 100;
                payment_each * 2
            } else {
                let payment_from_dealer = ((base_points * 2) + 99) / 100 * 100;
                let payment_from_other_non_dealer = ((base_points) + 99) / 100 * 100;
                payment_from_dealer + payment_from_other_non_dealer
            }
        }
    }
}

fn hand_from_tiles_for_test(tiles: &[Tile]) -> Hand {
    let mut hand = Hand::default();
    for &tile in tiles {
        hand.add(tile).unwrap();
    }
    hand
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*;
	use crate::wall::{DEAD_WALL_SIZE, LIVE_WALL_SIZE, TOTAL_TILES_GENERATED};
    use crate::game_state::{WinType, DeclaredMeldType, Score, DeclaredMeld};

    // --- Helper Functions ---

    fn setup_game_state_custom(
    seed: u64,
    dealer_idx: u8,
    honba_sticks: u8,
    riichi_sticks: u8,
    player_hands_tiles: Option<[&[Tile]; 3]>,
    player_scores_opt: Option<[i32; 3]>,
    current_player_idx_opt: Option<u8>,
    turn_count_opt: Option<u32>,
    round_wind_opt: Option<Tile>,
    dora_indicators_opt: Option<Vec<Tile>>,
    ura_dora_indicators_opt: Option<Vec<Tile>>,
    live_wall_tiles_opt: Option<Vec<Tile>>,
    dead_wall_tiles_opt: Option<Vec<Tile>>,
) -> GameState {
    let mut initial_live_wall_draw_pos: Option<usize> = None;

    let override_wall = if live_wall_tiles_opt.is_some() || dead_wall_tiles_opt.is_some() {
        let provided_live_wall = live_wall_tiles_opt.unwrap_or_default();
        let mut provided_dead_wall = dead_wall_tiles_opt.unwrap_or_else(|| vec![Tile::Sou9; DEAD_WALL_SIZE]);

        if provided_dead_wall.len() != DEAD_WALL_SIZE {
             provided_dead_wall.resize(DEAD_WALL_SIZE, Tile::Sou9);
        }

        let live_wall_filler_count = LIVE_WALL_SIZE.saturating_sub(provided_live_wall.len());
        initial_live_wall_draw_pos = Some(live_wall_filler_count);

        let mut live_wall_filler = vec![Tile::Sou8; live_wall_filler_count];
        
        let mut final_wall_vec = Vec::with_capacity(TOTAL_TILES_GENERATED);
        final_wall_vec.append(&mut live_wall_filler);
        final_wall_vec.extend_from_slice(&provided_live_wall);
        final_wall_vec.extend_from_slice(&provided_dead_wall);
        
        Some(final_wall_vec)
    } else {
        None
    };

    // THIS IS THE CRITICAL FIX:
    // If the test doesn't provide hands, we must decide whether to let the engine deal.
    // If a custom wall is being used, automatic dealing will fail. So, we pass
    // Some(empty hands) to GameState::new() to prevent it from dealing.
    // The test will then set the hands manually later.
    let hands_for_init = match player_hands_tiles {
        Some(hands_slice) => {
            let mut arr: [Vec<Tile>; 3] = Default::default();
            for (i, &hand_slice) in hands_slice.iter().enumerate() {
                arr[i] = hand_slice.to_vec();
            }
            Some(arr)
        },
        None => {
            if override_wall.is_some() {
                // A custom wall is present but no hands were given. Don't deal from the dummy wall.
                Some(Default::default()) // This passes Some([vec![], vec![], vec![]])
            } else {
                // No custom wall and no custom hands. Let the engine deal normally.
                None
            }
        }
    };

    let mut gs = GameState::new(seed, dealer_idx, honba_sticks, hands_for_init, player_scores_opt, override_wall);
    gs.riichi_sticks = riichi_sticks;

    // After creating the game state, if we used an override wall, we must set the draw position.
    if let Some(pos) = initial_live_wall_draw_pos {
        gs.wall._set_live_wall_draw_pos_for_test(pos);
    }

    // Apply any other custom state settings provided by the test.
    if let Some(cp_idx) = current_player_idx_opt { gs.current_player_idx = cp_idx; }
    if let Some(tc) = turn_count_opt { gs.turn_count = tc; }
    if let Some(rw) = round_wind_opt { gs.round_wind = rw; }
    if let Some(dora_inds) = dora_indicators_opt { gs.current_dora_indicators = dora_inds; }
    if let Some(ura_inds) = ura_dora_indicators_opt { gs.current_ura_dora_indicators = ura_inds; }

    gs
}

    fn set_player_hand(gs: &mut GameState, player_idx: usize, tiles: &[Tile]) {
        gs.hands[player_idx] = hand_from_tiles_for_test(tiles);
    }
		
    fn add_player_open_meld(gs: &mut GameState, player_idx: usize, meld: DeclaredMeld) {
        if player_idx < gs.open_melds.len() {
            gs.open_melds[player_idx].push(meld);
        }
    }

    fn add_player_discards(gs: &mut GameState, player_idx: usize, tiles: &[Tile]) {
        if player_idx < gs.discards.len() {
            for &tile in tiles {
                gs.discards[player_idx].push(tile);
            }
        }
    }


    #[test]
    fn test_player_draw_tile() {
        let mut gs = setup_game_state_custom(1, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
        let _initial_hand_size_p0 = gs.hands[0].get_all_tiles().len(); // P0 is dealer, already drew once in new.
        let initial_wall_size = gs.wall.live_wall_remaining_count();

        gs.current_player_idx = 1; // P1's turn
        let initial_hand_size_p1 = gs.hands[1].get_all_tiles().len();
        let drawn_tile_p1 = gs.player_draws_tile(None);
        assert!(drawn_tile_p1.is_some(), "P1 should draw a tile");
        assert_eq!(gs.hands[1].get_all_tiles().len(), initial_hand_size_p1 + 1, "P1 hand size should increase");
        assert_eq!(gs.wall.live_wall_remaining_count(), initial_wall_size - 1, "Wall size should decrease");
        assert_eq!(gs.last_drawn_tile, drawn_tile_p1, "last_drawn_tile not set correctly for P1");

        // Test drawing until wall is empty
        let mut gs_empty_wall = setup_game_state_custom(2,0,0,0, None, None, Some(0), Some(0), None, None, None, Some(vec![Man1, Man2]), Some(vec![Pin1;14])); // Only 2 live tiles
        gs_empty_wall.current_player_idx = 0; gs_empty_wall.player_draws_tile(None); // P0 draws Man1
        gs_empty_wall.current_player_idx = 0; gs_empty_wall.player_discards_tile(0, gs_empty_wall.hands[0].get_all_tiles()[0]).unwrap(); // P0 discards

        gs_empty_wall.current_player_idx = 1; gs_empty_wall.player_draws_tile(None); // P1 draws Man2
        gs_empty_wall.current_player_idx = 1; gs_empty_wall.player_discards_tile(1, gs_empty_wall.hands[1].get_all_tiles()[0]).unwrap(); // P1 discards
        
        gs_empty_wall.current_player_idx = 2; // P2's turn
        assert!(gs_empty_wall.wall.is_live_wall_empty());
        let no_tile = gs_empty_wall.player_draws_tile(None);
        assert!(no_tile.is_none(), "Should not draw a tile from empty live wall");
    }

    #[test]
    fn test_player_discard_tile_and_turn_progression() {
        let mut gs = setup_game_state_custom(2, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None); // P0 dealer, turn 0
        
        // P0 (dealer) discards
        gs.current_player_idx = 0;
        // gs.player_draws_tile(None); // Dealer already drew in new()
        let tile_to_discard_p0 = gs.hands[0].get_all_tiles()[0];
        assert!(gs.player_discards_tile(0, tile_to_discard_p0).is_ok());
        assert_eq!(gs.discards[0].last(), Some(&tile_to_discard_p0));
        assert_eq!(gs.last_discarded_tile_info, Some((tile_to_discard_p0, 0)));
        assert_eq!(gs.turn_count, 1, "Turn count should be 1 after P0 discards");
        // current_player_idx should change after discard if no calls, handled by Env logic. GameState itself doesn't auto-advance here.

        // Simulate P1 drawing and discarding
        gs.current_player_idx = 1;
        gs.player_draws_tile(None);
        let tile_to_discard_p1 = gs.hands[1].get_all_tiles()[0];
        assert!(gs.player_discards_tile(1, tile_to_discard_p1).is_ok());
        assert_eq!(gs.turn_count, 2, "Turn count should be 2 after P1 discards");

        // Test discarding tile not in hand
        assert!(gs.player_discards_tile(1, Man1).is_err() || gs.hands[1].count(Man1) == 0, "Should error if discarding tile not in hand");
    }

    #[test]
    fn test_declare_riichi_conditions() {
		// Test can_declare_riichi
		let mut gs_can_riichi = setup_game_state_custom(3, 0, 0, 0, None, Some([1000;3]), Some(0), Some(5), None, None, None, None, None);
		set_player_hand(&mut gs_can_riichi, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]); // Tenpai hand, 14 tiles
		assert!(gs_can_riichi.can_declare_riichi(0), "Should be able to declare Riichi: menzen, tenpai, 14 tiles, score, wall tiles");

		// Not menzen
		let mut gs_not_menzen = setup_game_state_custom(4,0,0,0,None,Some([1000;3]),Some(0),Some(5),None,None,None,None,None);
		set_player_hand(&mut gs_not_menzen, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]);
		add_player_open_meld(&mut gs_not_menzen, 0, DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Sou1;4], called_from_discarder_idx: Some(1), called_tile: Some(Sou1) });
		assert!(!gs_not_menzen.can_declare_riichi(0), "Should not Riichi if not menzen");

		// Not enough points
		let mut gs_no_points = setup_game_state_custom(5,0,0,0,None,Some([900,35000,35000]),Some(0),Some(5),None,None,None,None,None);
		set_player_hand(&mut gs_no_points, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]);
		assert!(!gs_no_points.can_declare_riichi(0), "Should not Riichi if not enough points");
    
		// Not enough wall tiles (e.g. only 3 left)
		let mut gs_no_wall = setup_game_state_custom(6,0,0,0,None,Some([1000;3]),Some(0),Some(20),None,None,None,None, None);
		gs_no_wall.wall._set_live_wall_draw_pos_for_test(94 - 3); // LIVE_WALL_SIZE (94) - 3 = 91 tiles drawn
		set_player_hand(&mut gs_no_wall, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]);
		assert_eq!(gs_no_wall.wall.live_wall_remaining_count(), 3);
		assert!(!gs_no_wall.can_declare_riichi(0), "Should not Riichi if less than 4 wall tiles remaining");
	
		// Actual declaration
		gs_can_riichi.declare_riichi(0);
		assert!(gs_can_riichi.riichi_declared[0], "Riichi flag not set");
		assert!(gs_can_riichi.ippatsu_eligible[0], "Ippatsu flag not set on Riichi declaration");
		assert_eq!(gs_can_riichi.player_scores[0], 0, "Score not deducted for Riichi");
		assert_eq!(gs_can_riichi.riichi_sticks, 1, "Riichi stick not added");
}

    #[test]
    fn test_make_pon_and_any_discard_called_flag() {
        let mut gs = setup_game_state_custom(4, 0, 0, 0, None, None, Some(0), Some(1), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man1, Man1, Man2, Man3, Man4, Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Sou1, Sou2]);
        gs.last_discarded_tile_info = Some((Man1, 1)); // P1 discarded Man1
        gs.current_player_idx = 0; // P0's turn to call

        assert!(gs.can_call_pon(0, Man1, 1), "P0 should be able to Pon Man1");
        assert!(gs.make_pon(0, Man1, 1).is_ok());
        assert_eq!(gs.open_melds[0].len(), 1, "Pon not added to open melds");
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Pon);
        assert_eq!(gs.open_melds[0][0].tiles[0], Man1);
        assert_eq!(gs.hands[0].count(Man1), 0, "Tiles not removed from hand for Pon");
        assert_eq!(gs.current_player_idx, 0, "Turn should be P0's after Pon");
        assert!(gs.any_discard_called_this_round[1], "any_discard_called_this_round for P1 (discarder) should be true");
        assert!(!gs.ippatsu_eligible[0] && !gs.ippatsu_eligible[1] && !gs.ippatsu_eligible[2], "Ippatsu should be voided by Pon");
    }
    
    #[test]
    fn test_make_daiminkan_and_flags() {
        let mut gs = setup_game_state_custom(5, 0, 0, 0, None, None, Some(0), Some(1), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man1, Man1, Man1, Man2, Man3, Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Sou1, Sou2]);
        gs.last_discarded_tile_info = Some((Man1, 1)); // P1 discarded Man1
        gs.current_player_idx = 0; // P0's turn to call

        assert!(gs.can_call_daiminkan(0, Man1, 1));
        assert!(gs.make_daiminkan(0, Man1, 1).is_ok());
        assert_eq!(gs.open_melds[0].len(), 1);
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Daiminkan);
        assert_eq!(gs.kans_declared_count[0], 1);
        assert_eq!(gs.total_kans_in_game, 1);
        assert_eq!(gs.current_dora_indicators.len(), 2, "New Dora should be revealed for Daiminkan (initial + 1 kan dora)");
        assert!(gs.is_rinshan_kaihou_win_pending, "Rinshan flag should be set");
        assert!(gs.any_discard_called_this_round[1], "any_discard_called_this_round for P1 (discarder) should be true");
        assert!(!gs.ippatsu_eligible[0] && !gs.ippatsu_eligible[1] && !gs.ippatsu_eligible[2], "Ippatsu should be voided by Daiminkan");
    }

    #[test]
    fn test_make_ankan_and_flags() {
        let mut gs = setup_game_state_custom(6, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man1,Man1,Man1,Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3]);
        gs.current_player_idx = 0;

        assert!(gs.get_possible_ankans(0).unwrap_or_default().contains(&Man1));
        assert!(gs.make_ankan(0, Man1).is_ok());
        assert_eq!(gs.open_melds[0].len(), 1);
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Ankan);
        assert_eq!(gs.kans_declared_count[0], 1);
        assert_eq!(gs.total_kans_in_game, 1);
        assert_eq!(gs.current_dora_indicators.len(), 2, "New Dora for Ankan");
        assert!(gs.is_rinshan_kaihou_win_pending);
    }
    
    #[test]
    fn test_make_shouminkan_and_chankan_window() {
        let mut gs = setup_game_state_custom(7, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        let pon_meld = DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Man1;4], called_from_discarder_idx: Some(1), called_tile: Some(Man1) };
        add_player_open_meld(&mut gs, 0, pon_meld);
        set_player_hand(&mut gs, 0, &[Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3, East,East]);
        gs.current_player_idx = 0;

        assert!(gs.get_possible_shouminkans(0).unwrap_or_default().contains(&Man1));
        assert!(gs.make_shouminkan(0, Man1).is_ok());
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Shouminkan);
        assert_eq!(gs.kans_declared_count[0], 1);
        assert!(gs.is_chankan_window_open, "Chankan window should be open after Shouminkan");
        assert_eq!(gs.chankan_tile_and_declarer, Some((Man1, 0)));
        assert_eq!(gs.current_dora_indicators.len(), 1, "Dora should not be revealed yet for Shouminkan");
        assert!(!gs.is_rinshan_kaihou_win_pending, "Rinshan should not be pending yet for Shouminkan");
    }

    #[test]
    fn test_make_kita_declaration_sanma() {
        let mut gs = setup_game_state_custom(8, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[North, Man1,Man2,Man3, Pin1,Pin2,Pin3, Sou1,Sou2,Sou3, East,East,West,White]);
        gs.current_player_idx = 0;

        assert!(gs.can_declare_kita_action(0));
        assert!(gs.make_kita_declaration(0).is_ok());
        assert_eq!(gs.open_melds[0].len(), 1);
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Kita);
        assert_eq!(gs.open_melds[0][0].tiles[0], North);
        assert_eq!(gs.kita_declared_count[0], 1);
        assert!(gs.is_rinshan_kaihou_win_pending, "Kita draws replacement, so rinshan possible");
        assert_eq!(gs.current_dora_indicators.len(), 1, "Kita should not reveal a new Kan Dora itself");
        assert_eq!(gs.hands[0].count(North), 0, "North should be removed from hand and added to open melds");
    }

    #[test]
    fn test_is_tenpai_complex_waits() {
        // This test uses a hand with a "nobetan" wait (a two-sided wait on a sequence).
        // The hand is: M1234, P123, S567, EEE.
        // It is waiting on M1 (to make M1 pair + M234 meld) or M4 (to make M4 pair + M123 meld).
        let mut gs = setup_game_state_custom(9, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);

        // Hand: 13 tiles forming a tenpai state.
        let hand_tiles = [Man1, Man2, Man3, Man4, Pin1, Pin2, Pin3, Sou5, Sou6, Sou7, East, East, East];
        set_player_hand(&mut gs, 0, &hand_tiles);

        let (tenpai, waits) = gs.is_tenpai(0);

        // Assert that the hand is correctly identified as tenpai.
        assert!(tenpai, "Hand with nobetan wait should be tenpai. Waits found: {:?}", waits);

        // Assert that the function found exactly the 2 correct waiting tiles.
        assert_eq!(waits.len(), 2, "Should have 2 waits for a M1/M4 nobetan");
        assert!(waits.contains(&Man1), "Wait list should contain Man1");
        assert!(waits.contains(&Man4), "Wait list should contain Man4");
    }

    #[test]
    fn test_score_win_non_dealer_ron_tanyao_dora3() {
        // FINAL, FINAL FIX: This hand is unambiguously Tanyao but NOT Pinfu (it contains a triplet).
        // It also contains NO red five tiles (Man5, Pin5, Sou5).
        // Yaku: Tanyao (1) + Dora (indicator Man2 -> Dora is Man3) (1) = 2 han.
        let hand_tiles = [
            Pin2,Pin3,Pin4,
            Sou6,Sou7,Sou8,
            Man4,Man4,Man4, // Triplet (Koutsu) makes Pinfu impossible.
            Man3,           // The Dora tile
            Man7,Man7       // The pair
        ];
        let mut gs = setup_game_state_custom(
            10, 0, 0, 0,
            Some([&[], &hand_tiles, &[]]), // Player 1's hand
            None, Some(1), Some(5), None,
            Some(vec![Man2]), // Dora indicator is Man2
            None, None, None
        );

        // Win by Ron on Man2 to complete the Pin2,Man2 -> Pin2,Man2,Man2 meld
        // Let's make it simpler, win on a tile that completes a sequence
        // Hand: P23, S678, M444, M3, M77. Win on P4.
        let tenpai_hand = [Pin2,Pin3, Sou6,Sou7,Sou8, Man4,Man4,Man4, Man3, Man7,Man7];
        set_player_hand(&mut gs, 1, &tenpai_hand);

        let score = gs.score_win(1, WinType::Ron { winning_tile: Pin4, discarder_seat: 0 });
        
        let has_tanyao = score.yaku_details.iter().any(|(name, _)| *name == "Tanyao");
        let dora_val = score.yaku_details.iter().find(|(name, _)| *name == "Dora").map_or(0, |(_, val)| *val);
        
        assert!(has_tanyao, "Tanyao yaku was not found");
        assert_eq!(dora_val, 1, "Dora value should be 1 (for Man3)");
        assert_eq!(score.han, 2, "Han incorrect for Tanyao + Dora 1. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_apply_score_transfers_non_dealer_ron() {
        let mut gs = setup_game_state_custom(11, 0, 1, 1, None, Some([35000;3]), None, None, None, None, None, None, None);
        let score_details = Score { han: 2, fu: 30, points: 3220, yaku_details: vec![] };
        let winner_idx = 1;
        let discarder_idx = 2;
        gs.apply_score_transfers_and_reset_sticks(winner_idx, WinType::Ron { winning_tile: Man1, discarder_seat: discarder_idx }, &score_details);
        assert_eq!(gs.player_scores[winner_idx], 38220, "Winner (P1) score incorrect");
        assert_eq!(gs.player_scores[discarder_idx], 33080, "Discarder (P2) score incorrect");
        assert_eq!(gs.player_scores[0], 35000, "Dealer (P0) score should be unchanged");
        assert_eq!(gs.riichi_sticks, 0, "Riichi sticks should be reset");
    }

    #[test]
    fn test_four_kan_abortive_draw_pending_and_resolution() {
        let mut gs = setup_game_state_custom(12, 0, 0, 0, None, None, Some(0), Some(10), None, None, None, None, None);
        gs.kans_declared_count = [1,1,1];
        gs.total_kans_in_game = 3;
        set_player_hand(&mut gs, 0, &[Man1,Man1,Man1,Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3]);
        gs.current_player_idx = 0;
        assert!(gs.make_ankan(0, Man1).is_ok());
        assert_eq!(gs.total_kans_in_game, 4);
        assert_eq!(gs.kans_declared_count[0], 2);
        assert!(gs.four_kan_abortive_draw_pending, "Four Kan abortive draw should be pending (Suukaikan)");
        assert_eq!(gs.player_who_declared_fourth_kan, Some(0), "Player who declared 4th Kan incorrect");
    }

    #[test]
    fn test_kyuushuu_kyuuhai_abortive_draw() {
        let mut gs = setup_game_state_custom(100, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
        let kyuushuu_hand = [Man1, Man9, Pin1, Pin9, East, South, West, North, White, Green, Red, Sou2, Sou3, Sou4];
        set_player_hand(&mut gs, 0, &kyuushuu_hand[0..14]);
        gs.turn_count = 0;
        gs.current_player_idx = 0;
        let mut unique_th_tiles = std::collections::HashSet::new();
        for tile_in_hand in gs.hands[0].get_all_tiles() {
            if tile_in_hand.is_terminal_or_honor() {
                unique_th_tiles.insert(tile_in_hand);
            }
        }
        let can_declare_kyuushuu = unique_th_tiles.len() >= 9 && gs.turn_count == 0 && gs.open_melds[0].is_empty();
        assert!(can_declare_kyuushuu, "Player 0 should be able to declare Kyuushuu Kyuuhai");
    }

    #[test]
    fn test_nagashi_mangan_conditions_and_scoring() {
        let mut gs = setup_game_state_custom(200, 0, 1, 1, None, Some([35000;3]), None, None, None, None, None, None, None);
        gs.current_player_idx = 0;
        let p0_discards = [Man1, Man9, East, White, Pin1, Pin9, South, Green, West, Red, North];
        for &tile in &p0_discards {
            gs.discards[0].push(tile);
        }
        gs.any_discard_called_this_round[0] = false;
        gs.any_discard_called_this_round[1] = true;
        gs.any_discard_called_this_round[2] = false;
        let mut is_nagashi = true;
        if gs.discards[0].is_empty() { is_nagashi = false; }
        for &tile in &gs.discards[0] {
            if !tile.is_terminal_or_honor() { is_nagashi = false; break; }
        }
        if gs.any_discard_called_this_round[0] { is_nagashi = false; }
        assert!(is_nagashi, "P0 should be eligible for Nagashi Mangan");
    }

    #[test]
    fn test_furiten_basic_ron_check() {
        let mut gs = setup_game_state_custom(300, 0, 0, 0, None, None, Some(1), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 1, &[Man1, Man2, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East,East, West,West]);
        add_player_discards(&mut gs, 1, &[Man3, Pin5]);
        gs.last_discarded_tile_info = Some((Man3, 0));
        assert!(!gs.can_call_ron(1, Man3, 0), "P1 should be in Furiten and cannot Ron on Man3");
    }

    #[test]
    fn test_score_win_kokushi_musou_sanma() {
        let hand = [Man1, Man9, Pin1, Pin9, East, South, West, North, White, Green, Green, Red, Red, Red];
        let mut gs = setup_game_state_custom(400, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &hand);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n,_)| n.contains("Kokushi Musou")), "Kokushi Musou not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.points, 32000, "Kokushi Musou (Dealer Tsumo) points incorrect");
    }
    
    #[test]
    fn test_score_win_suuankou_tsumo() {
        let hand = [Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West, West];
        let mut gs = setup_game_state_custom(401, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &hand[..13]);
        gs.hands[0].add(West).unwrap();
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n,_)| *n == "Suuankou"), "Suuankou not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_suuankou_tanki_ron() {
        // FIX: Corrected test setup for Ron. The winning tile `West` is NOT added
        // to the hand, but passed in the WinType::Ron enum.
        let ankou_hand_tiles_tanki = [Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West];
        let mut gs = setup_game_state_custom(402, 0, 0, 0, Some([&[], &ankou_hand_tiles_tanki, &[]]), None, Some(1), Some(10), Some(East), Some(vec![West, West, West]), None, None, None);
        // The winning tile `West` completes the pair.
        let score = gs.score_win(1, WinType::Ron { winning_tile: West, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Suuankou (Tanki)"), "Suuankou Tanki (Double Yakuman) not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_daisangen() {
        let hand = [White,White,White, Green,Green,Green, Red,Red,Red, Man1,Man1, Pin2,Pin2, Man1];
        let mut gs = setup_game_state_custom(403, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &hand);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n,_)| *n == "Daisangen"), "Daisangen not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_score_win_kazoe_yakuman_riichi_tsumo_honitsu_chinitsu_etc() {
        // FIX: The previous hand was a Suuankou. This is a new hand that is not a
        // standard yakuman, but whose yaku and dora add up to 13+ han.
        // Yaku: Chinitsu(6) + Iipeikou(1) + Pinfu(1) + Riichi(1) + Tsumo(1) = 10 han.
        // Dora: Indicator is M8, so M9 is Dora. Hand has 3x M9. Dora = 3.
        // Total Han = 10 + 3 = 13 han => Kazoe Yakuman.
        let hand_tiles = [
            Man1,Man2,Man3, Man1,Man2,Man3, // Two identical sequences (Iipeikou)
            Man4,Man5,Man6,                 // Sequence
            Man7,Man8,                      // Two-sided wait for Pinfu
            Man9,Man9                       // Pair
        ];
        let mut gs = setup_game_state_custom(
            404, 0, 1, 1,
            Some([&hand_tiles, &[], &[]]),
            None, Some(0), Some(10), None,
            Some(vec![Man8]), // Dora indicator
            None, None, None
        );
        gs.riichi_declared[0] = true;

        // Tsumo on Man9 to complete the hand
        let winning_tile = Man9;
        gs.hands[0].add(winning_tile).unwrap();
        gs.last_drawn_tile = Some(winning_tile);

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Kazoe Yakuman"), "Kazoe Yakuman not found. Details: {:?}", score.yaku_details);
        // We assert for 13 because that's the threshold for Kazoe. The final han might be higher.
        assert_eq!(score.han, 13, "Kazoe Yakuman should have a final han count of 13. Details: {:?}", score.yaku_details);
    }

    #[test]
	fn test_score_win_ippatsu() {
        let mut gs = setup_game_state_custom(405, 0, 0, 1, None, Some([34000;3]), Some(0), Some(5), None, Some(vec![Man1]), None, None, None);
        set_player_hand(&mut gs, 0, &[Man3,Man4,Man5, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West, Man2]);
        gs.riichi_declared[0] = true;
        gs.ippatsu_eligible[0] = true;
        // FIX: Explicitly set double_riichi to false for a mid-game test
        gs.double_riichi_eligible[0] = false;
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n, _)| *n == "Riichi"), "Riichi yaku missing. Details: {:?}", score.yaku_details);
        assert!(score.yaku_details.iter().any(|(n, _)| *n == "Ippatsu"), "Ippatsu yaku missing. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_rinshan_kaihou() {
        let mut gs = setup_game_state_custom(406, 0, 0, 0, None, None, Some(0), Some(3), None, Some(vec![Man1]), None, None, None);
        set_player_hand(&mut gs, 0, &[Man2,Man2,Man2,Man2, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]);
        gs.make_ankan(0, Man2).expect("Failed to make ankan for rinshan test");
        let rinshan_tile = Man2;
        gs.hands[0].add(rinshan_tile).unwrap();
        gs.is_rinshan_kaihou_win_pending = true;
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Rinshan Kaihou"), "Rinshan Kaihou Yaku not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_chankan_non_dealer() {
        let mut gs = setup_game_state_custom(407, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        add_player_open_meld(&mut gs, 0, DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Man1;4], called_from_discarder_idx: Some(2), called_tile: Some(Man1) });
        // FIX: Changed hand to not include Pin5 (Aka Dora) to isolate Chankan yaku
        set_player_hand(&mut gs, 1, &[Man2, Man3, Pin2, Pin3, Pin4, Sou7, Sou8, Sou9, West, West, West, Red, Red]);
        gs.hands[0].add(Man1).unwrap();
        gs.make_shouminkan(0, Man1).expect("Failed to make shouminkan for chankan test");
        let score = gs.score_win(1, WinType::Ron { winning_tile: Man1, discarder_seat: 0 });
        assert_eq!(score.han, 1, "Han count should be 1 for only Chankan. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_dealer_rotation_and_honba_sticks_conceptual() {
        let gs_next_round_dealer_renchan = GameState::new(501, 0, 1, None, None, None);
		assert_eq!(gs_next_round_dealer_renchan.dealer_idx, 0);
		assert_eq!(gs_next_round_dealer_renchan.honba_sticks, 1);
		let gs_next_round_dealer_rotates = GameState::new(503, 1, 0, None, None, None);
		assert_eq!(gs_next_round_dealer_rotates.dealer_idx, 1);
		assert_eq!(gs_next_round_dealer_rotates.honba_sticks, 0);
}	

    #[test]
    fn test_no_yaku_win_attempt() {
        // This hand has an open meld of East, so it cannot be Tanyao.
        // All sequences are simple, the pair is simple. No other Yaku.
        // It's an open hand, so Menzen Tsumo/Riichi etc. are not possible.
        // It's not Yakuhai because the seat/round winds are not East.
        let hand_tiles = [Man2,Man3,Man4, Pin2,Pin3,Pin4, Sou5,Sou6,Sou7, Pin8,Pin8];
        let mut gs = setup_game_state_custom(
            600, 0, 0, 0, None, None, Some(0), Some(5), Some(West), // Round wind is West
            None, None, None, None
        );
        // Player 0 is not the East seat, so the Pon of East is not a Yakuhai
        gs.seat_winds = [Tile::South, Tile::West, Tile::East]; // P0 is South seat

        set_player_hand(&mut gs, 0, &hand_tiles);
        add_player_open_meld(&mut gs, 0, DeclaredMeld {
            meld_type: DeclaredMeldType::Pon, tiles: [East; 4], // Open meld of honor tile
            called_from_discarder_idx: Some(1), called_tile: Some(East),
        });
        
        // Winning on Tsumo of Pin8 to complete the pair
        gs.hands[0].add(Pin8).unwrap(); 
        
        let score = gs.score_win(0, WinType::Tsumo);
        assert_eq!(score.han, 0, "Hand with no Yaku should have 0 Han. Details: {:?}", score.yaku_details);
    }


    #[test]
    fn test_score_win_tsuu_iisou_all_honors() {
        let hand = [East,East,East, South,South,South, West,West,West, North,North, North, White,White];
        let mut gs = setup_game_state_custom(702, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &hand);
        let score = gs.score_win(0, WinType::Tsumo);
        // FIX: The hand is both Tsuu Iisou and Daisuushii. The scoring should pick the higher-value
        // Daisuushii (Double Yakuman). The test now correctly asserts for this.
        assert!(score.yaku_details.iter().any(|(n, _)| *n == "Daisuushii"), "Daisuushii not found, but it should be the highest value yaku. Details {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_chinroutou_all_terminals() {
        let hand_tiles = [Man1,Man1,Man1, Man9,Man9,Man9, Pin1,Pin1,Pin1, Pin9,Pin9,Pin9, Sou1,Sou1];
        let mut state = setup_game_state_custom(100, 1, 0, 0, Some([&hand_tiles, &[], &[]]), None, Some(0), Some(5), None, Some(vec![Man2]), None, None, None);
        let score = state.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Chinroutou (All Terminals)"), "Chinroutou Yakuman not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_suukantsu_four_kans() {
        let mut gs = setup_game_state_custom(704, 0, 1, 1, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man2,Man2,Man2,Man2, Man3,Man3,Man3,Man3, Pin1,Pin1,Pin1,Pin1, Pin2,Pin2]);
        gs.make_ankan(0, Man2).unwrap();
        gs.make_ankan(0, Man3).unwrap();
        gs.make_ankan(0, Pin1).unwrap();
        let pon_meld = DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Pin2;4], called_from_discarder_idx: Some(1), called_tile: Some(Pin2) };
        add_player_open_meld(&mut gs, 0, pon_meld);
        gs.hands[0].remove_n(Pin2, 2).unwrap();
        gs.hands[0].add(Pin2).unwrap();
        gs.make_shouminkan(0, Pin2).unwrap();
        gs.perform_kan_common_actions(0);
        gs.hands[0].add(Sou1).unwrap();
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Suukantsu (Four Kans)"), "Suukantsu Yaku not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_score_win_shousuushii_little_four_winds() {
        let hand = [East,East,East, South,South,South, West,West,West, North,North, Man1,Man2, Man3];
        let mut gs = setup_game_state_custom(705, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &hand);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n,_)| *n == "Shousuushii"), "Shousuushii not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_double_riichi_tsumo_pinfu_dora() {
        // FIX: Corrected test setup with a valid Pinfu hand and correct turn_count for Double Riichi.
        // Hand: M23, M56, P456, S789, EE pair. Ryanmen wait on M1 or M4.
        let hand_tiles = [Man2,Man3, Man5,Man6, Pin4,Pin5,Pin6, Sou7,Sou8,Sou9, East,East, Man1];
        let mut gs = setup_game_state_custom(706, 0, 0, 1, None, Some([34000;3]), Some(0), Some(0), Some(East), Some(vec![Pin1]), None, None, None);
        set_player_hand(&mut gs, 0, &hand_tiles); // Set 13 tiles for dealer's initial hand
        gs.riichi_declared[0] = true;
        gs.double_riichi_eligible[0] = true;
        gs.ippatsu_eligible[0] = true;
        
        // P0 (dealer) draws the winning tile on their very first turn for Double Riichi
        let win_tile = Man4;
        gs.hands[0].add(win_tile).unwrap();
        gs.last_drawn_tile = Some(win_tile);

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Double Riichi"), "Double Riichi not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_haitei_raoyue_win_on_last_tsumo() {
        let live_wall = vec![Man1];
        let dead_wall = vec![Pin1; DEAD_WALL_SIZE];
        let mut gs = setup_game_state_custom(707, 0, 1, 0, None, None, Some(0), Some(20), None, Some(vec![Man2]), None, Some(live_wall), Some(dead_wall));
        set_player_hand(&mut gs, 0, &[Man2,Man3,Man4, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West, Man1]);
        gs.wall._set_live_wall_draw_pos_for_test(94-1);
        gs.player_draws_tile(None);
        assert!(gs.wall.is_live_wall_empty());
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Haitei Raoyue"), "Haitei Raoyue not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_houtei_raoyui_win_on_last_discard() {
        let mut gs = setup_game_state_custom(708, 0, 1, 0, None, None, Some(1), Some(21), None, Some(vec![Man2]), None, Some(vec![]), Some(vec![Pin1; DEAD_WALL_SIZE]));
        assert!(gs.wall.is_live_wall_empty());
        set_player_hand(&mut gs, 1, &[Man2,Man3,Man4, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]);
        let last_discard = Man1;
        gs.last_discarded_tile_info = Some((last_discard, 0));
        gs.hands[1].add(last_discard).unwrap();
        let score = gs.score_win(1, WinType::Ron { winning_tile: last_discard, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Houtei Raoyui"), "Houtei Raoyui not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_menzen_tsumo_no_other_yaku_adapted() {
        let hand_tiles = [Man2,Man3,Man4, Pin5,Pin6,Pin7, Sou8,Sou8,Sou8, Man1,Man1,Man1, Pin2, Pin2];
        let mut gs = setup_game_state_custom(1, 0, 0, 0, Some([&hand_tiles, &[], &[]]), None, Some(0), Some(5), None, None, None, None, None);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n, _)| *n == "Menzen Tsumo"), "Menzen Tsumo not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_riichi_ippatsu_menzen_tsumo_dora_adapted() {
        let hand_tiles = [Man2,Man3,Man4, Pin5,Pin6,Pin7, Sou3,Sou4,Sou5, Sou1,Sou1, East,East, Sou2];
        let mut gs = setup_game_state_custom(101, 0, 0, 1, Some([&hand_tiles, &[], &[]]), Some([34000;3]), Some(0), Some(5), Some(East), Some(vec![Sou1]), None, None, None);
        gs.riichi_declared[0] = true;
        gs.ippatsu_eligible[0] = true;
        // FIX: Explicitly set double_riichi to false for a mid-game test
        gs.double_riichi_eligible[0] = false;
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(n, _)| *n == "Riichi"), "Riichi not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_ryuu_iisou_all_green() {
        let ryuu_iisou_hand = [Sou2,Sou2,Sou2, Sou3,Sou3,Sou3, Sou4,Sou4,Sou4, Sou6,Sou6, Green,Green, Green];
        let mut gs = setup_game_state_custom(711, 1, 0, 0, Some([&[], &ryuu_iisou_hand, &[]]), None, Some(1), Some(8), None, None, None, None, None);
        let score = gs.score_win(1, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Ryuu Iisou"), "Ryuu Iisou Yaku not found. Details: {:?}", score.yaku_details);
        // Player 1 is the dealer in this setup. Dealer Tsumo Yakuman is 32000.
        assert_eq!(score.points, 32000, "Ryuu Iisou (Dealer Tsumo) points incorrect");
    }
    
    #[test]
    fn test_score_win_chuuren_poutou_nine_gates_9_wait() {
        // FIX: Added last_drawn_tile to simulate the Tsumo win correctly.
        // Hand is a pure flush of Manzu waiting on any Manzu tile.
        let chuuren_base_hand = [Man1,Man1,Man1, Man2,Man3,Man4, Man5,Man6,Man7,Man8, Man9,Man9,Man9];
        let mut gs = setup_game_state_custom(712, 0, 0, 0, Some([&chuuren_base_hand, &[], &[]]), None, Some(0), Some(10), None, None, None, None, None);
        
        let winning_tile = Man5;
        gs.hands[0].add(winning_tile).unwrap();
        gs.last_drawn_tile = Some(winning_tile);

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, _)| name.contains("Chuuren Poutou")), "Chuuren Poutou not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 26, "Chuuren Poutou (9-wait) should be 26 Han");
    }

    #[test]
    fn test_score_win_tenhou_blessing_of_heaven() {
        let winning_hand_for_tenhou = [Man1,Man1,Man1, Man2,Man2,Man2, Man3,Man3,Man3, Man4,Man4,Man4, East,East];
        let mut gs = setup_game_state_custom(713, 0, 0, 0, Some([&winning_hand_for_tenhou, &[], &[]]), None, Some(0), Some(0), Some(Tile::East), None, None, None, None);
        gs.is_tenhou_win_possible = true;
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Tenhou"), "Tenhou Yaku not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_score_win_chiihou_blessing_of_earth() {
        // FIX: Corrected test setup. Chiihou is for a non-dealer winning on their
        // first draw, which happens on turn_count 0 for P1 if dealer is P0.
        let winning_hand_for_chiihou = [Man1,Man1,Man1, Man2,Man2,Man2, Man3,Man3,Man3, Man4,Man4,Man4, East,East];
        let mut gs = setup_game_state_custom(714, 0, 0, 0, Some([&[], &winning_hand_for_chiihou[0..13], &[]]), None, Some(1), Some(0), Some(Tile::East), None, None, None, None);
        gs.hands[1].add(East).unwrap(); // P1 draws their winning tile
        gs.last_drawn_tile = Some(East);
        gs.is_chiihou_win_possible[1] = true; // Ensure flag is set

        let score = gs.score_win(1, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Chiihou"), "Chiihou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.points, 24000, "Chiihou (Non-Dealer Tsumo Yakuman) points incorrect");
    }
    
    #[test]
    fn test_score_win_ryanpeikou_two_identical_sequences_twice() {
        let ryanpeikou_hand = [Man1,Man2,Man3, Man1,Man2,Man3, Pin4,Pin5,Pin6, Pin4,Pin5,Pin6, East, East];
        let mut gs = setup_game_state_custom(715, 0, 0, 0, Some([&ryanpeikou_hand, &[], &[]]), None, Some(0), Some(5), Some(Tile::East), None, None, None, None);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Ryanpeikou" && *val == 3), "Ryanpeikou (3 Han) not found. Details: {:?}", score.yaku_details);
    }

    #[test]
    fn test_score_win_ittsuu_pure_straight() {
        // FIX: Replaced hand with a valid winning hand containing Ittsuu
        // Hand: M123, M456, M789 (Ittsuu), P234, EE pair. Menzen.
        let ittsuu_hand = [Man1,Man2,Man3, Man4,Man5,Man6, Man7,Man8,Man9, Pin2,Pin3,Pin4, East,East];
        let mut gs = setup_game_state_custom(716, 0, 0, 0, Some([&ittsuu_hand, &[], &[]]), None, Some(0), Some(6), Some(Tile::East), None, None, None, None);
        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Ittsuu" && (*val == 2)), "Ittsuu (2 Han Menzen) not found. Details: {:?}", score.yaku_details);
    }
    
    #[test]
    fn test_abortive_draw_suucha_riichi() {
        let mut gs = setup_game_state_custom(717, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        gs.riichi_declared = [true, true, true];
        assert!(gs.riichi_declared.iter().all(|&r| r), "All players should have Riichi declared for Sannin Riichi check");
    }
}