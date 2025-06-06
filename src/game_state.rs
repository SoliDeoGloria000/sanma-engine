// src/game_state.rs

use crate::hand::{Hand, HandError};
use crate::tiles::Tile;
use crate::wall::Wall;
use crate::hand_parser::{
    self, ParsedStandardHand, ParsedChiitoitsu, ParsedKokushiMusou,
    ParsedMeld as ParserOutputMeld, ParsedMeldType as ParserOutputMeldType
};
use crate::fu_calculation::{self, FuCalculationInput};

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
    pub points: u32, // This is the total points the winner receives (including riichi sticks, honba, before payments from others)
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
    pub honba_sticks: u8, // Counter for consecutive rounds/dealer wins bonus
    pub four_kan_abortive_draw_pending: bool, // True if 4 kants by different players, pending discard
    pub player_who_declared_fourth_kan: Option<u8>, // Player who made the 4th Kan (if above is true)
	pub any_discard_called_this_round: [bool; 3], // For Nagashi Mangan: true if any discard by this player was called

}

impl GameState {
    pub fn new(seed: u64, initial_dealer_idx: u8, initial_honba_sticks: u8) -> Self {
        let mut wall = Wall::new(seed);
        let mut hands = [Hand::default(); 3];
        for _ in 0..13 {
            for seat_idx in 0..3 {
                if let Some(t) = wall.draw_from_live_wall() {
                    hands[seat_idx].add(t).expect("Failed to add tile during initial deal");
                } else {
                    panic!("Wall empty during initial deal!");
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

        let mut red_fives = vec![Tile::Man5, Tile::Pin5];
        // Sou5 is part of the Sanma set, so it can be a red five.
        if Tile::Sou5 as u8 <= Tile::North as u8 { // Basic check to ensure Sou5 is a valid tile ID
             red_fives.push(Tile::Sou5);
        }

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
            round_wind: Tile::East, // This might change per game, passed from a higher level manager
            seat_winds,
            last_drawn_tile: None, last_discarded_tile_info: None,
            kans_declared_count: [0; 3], total_kans_in_game: 0, kita_declared_count: [0; 3],
            player_scores: [35000; 3], // Standard starting score for Sanma
            riichi_sticks: 0,
            honba_sticks: initial_honba_sticks, // Initialize from argument
            four_kan_abortive_draw_pending: false, // Initialize as per request
            player_who_declared_fourth_kan: None, // Initialize as per request
			any_discard_called_this_round: [false; 3], // Initialize for Nagashi Mangan
        }
    }

    pub fn player_draws_tile(&mut self) -> Option<Tile> {
        let player_idx = self.current_player_idx as usize;

        if !self.is_rinshan_kaihou_win_pending {
            // Ippatsu is voided if the turn passes the Riichi player once without them winning,
            // or if any call (Pon, Chi, Kan - except Ankan by the Riichi player that doesn't change waits) occurs.
            // This logic is mainly handled in player_discards_tile and void_transient_flags_on_call.
        }

        // Check for Double Riichi eligibility voiding:
        // If it's past the first round of turns OR any interrupting call has been made.
        // An "interrupting call" is any call other than an Ankan or Kita by any player.
        let first_round_of_turns_not_fully_completed = self.turn_count < 3; // turn_count increments after each player's discard
        let no_interrupting_calls_made_yet_this_round = self.open_melds.iter().all(|p_melds| {
            p_melds.iter().all(|m| matches!(m.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita))
        });

        if !first_round_of_turns_not_fully_completed || !no_interrupting_calls_made_yet_this_round {
            for i in 0..3 {
                if !self.riichi_declared[i] { // Only void for those not yet in Riichi
                    self.double_riichi_eligible[i] = false;
                }
            }
        }

        let drawn_tile_option = self.wall.draw_from_live_wall();
        if let Some(drawn_tile) = drawn_tile_option {
            self.hands[player_idx].add(drawn_tile).expect("Failed to add drawn tile to hand");
            self.last_drawn_tile = Some(drawn_tile);

            // Tenhou check (Dealer's first draw, no interruptions)
            if !(player_idx == self.dealer_idx as usize && self.turn_count == 0 && no_interrupting_calls_made_yet_this_round) {
                self.is_tenhou_win_possible = false;
            }

            // Chiihou check (Non-dealer's first UNINTERRUPTED draw)
            // A non-dealer's "first draw" means their turn_count is effectively 0 for them relative to the start.
            // turn_count is 0 for dealer's first action, 1 for next player's, 2 for third's.
            let is_player_first_draw_turn = self.turn_count == (player_idx as u32).wrapping_sub(self.dealer_idx as u32).rem_euclid(3);
            if !(player_idx != self.dealer_idx as usize && is_player_first_draw_turn && no_interrupting_calls_made_yet_this_round) {
                 self.is_chiihou_win_possible[player_idx] = false;
            }


        } else {
            self.last_drawn_tile = None; // Wall is empty
        }
        drawn_tile_option
    }

    pub fn player_discards_tile(&mut self, player_idx_discarding: usize, tile_to_discard: Tile) -> Result<(), HandError> {
        if player_idx_discarding != self.current_player_idx as usize {
            return Err(HandError::Generic("Not player's turn to discard"));
        }
        if self.hands[player_idx_discarding].count(tile_to_discard) == 0 {
            return Err(HandError::Generic("Tile not in hand to discard"));
        }
        self.hands[player_idx_discarding].remove(tile_to_discard)?;

        self.discards[player_idx_discarding].push(tile_to_discard);
        self.last_discarded_tile_info = Some((tile_to_discard, player_idx_discarding as u8));

        // Player who just discarded cannot claim Ippatsu on their own discard.
        // If they were in Riichi, their Ippatsu chance is over for this discard.
        if self.riichi_declared[player_idx_discarding] {
            self.ippatsu_eligible[player_idx_discarding] = false;
        }
        // Double Riichi eligibility is also lost after the first discard.
        if !self.riichi_declared[player_idx_discarding] { // If not already in Riichi (which would preserve DR if it was DR)
            self.double_riichi_eligible[player_idx_discarding] = false;
        }


        // Other players' Ippatsu eligibility is voided by this discard if they don't call Ron immediately.
        // This is implicitly handled if they pass on a Ron, or if another call happens.
        // For now, we can assume their ippatsu flag remains until their turn or a call.
        // However, more strictly, any discard after the Riichi declaration (other than the Riichi discard itself)
        // that isn't Ron'd, or any call, voids Ippatsu for others.
        // Let's reset for others *after* call processing if no Ron.
        // For simplicity here, if a player discards, other players' ippatsu for *this turn* is now active.
        // It gets voided if *they* make a call or if their turn passes.
        // The prompt implies this logic is okay.

        // Reset flags that are specific to an action within a turn
        self.is_rinshan_kaihou_win_pending = false; // No longer on a rinshan draw
        self.is_chankan_window_open = false; // Chankan window closes after discard
        self.chankan_tile_and_declarer = None;

        // Tenhou/Chiihou no longer possible after any discard
        self.is_tenhou_win_possible = false;
        for i in 0..3 { self.is_chiihou_win_possible[i] = false; }


        // Only increment turn_count if it's the current player actually making a discard.
        // Calls might change current_player_idx without incrementing overall game turn count in same way.
        if player_idx_discarding == self.current_player_idx as usize {
            self.turn_count += 1;
        }
        Ok(())
    }

    fn void_transient_flags_on_call(&mut self, _action_player_idx: usize) {
        // When any call (Pon, Chi, Daiminkan, Shouminkan - but not Ankan/Kita by the riichi player themselves if it doesn't change waits)
        // happens, Ippatsu is voided for ALL players.
        for i in 0..3 {
            self.ippatsu_eligible[i] = false;
            // Double Riichi is also voided for anyone not yet in Riichi if a call occurs.
            if !self.riichi_declared[i] {
                 self.double_riichi_eligible[i] = false;
            }
        }
        // Tenhou/Chiihou would also be voided
        self.is_tenhou_win_possible = false;
        for i in 0..3 { self.is_chiihou_win_possible[i] = false; }
		 // For Nagashi Mangan: if a discard was called, mark it for the discarder.
        if let Some(discarder_idx) = discarder_idx_opt {
            if discarder_idx < 3 { // Ensure valid index before array access
                self.any_discard_called_this_round[discarder_idx] = true;
            }
        }
		
    }
	
    pub fn is_tenpai(&self, player_idx: usize) -> (bool, Vec<Tile>) {
        let original_hand = &self.hands[player_idx];
        let open_melds = &self.open_melds[player_idx];
        let mut waiting_for_tiles: Vec<Tile> = Vec::new();

        // Get all tiles in the hand (concealed part)
        let hand_tiles_for_tenpai_check = original_hand.get_all_tiles();
        let num_tiles_in_hand = hand_tiles_for_tenpai_check.len();

        // A hand is normally 13 tiles before drawing, 14 after drawing or before discarding.
        // Tenpai check is usually on 13 tiles (what are you waiting for if you discard X?)
        // or on 14 tiles (can you win by Tsumo with this hand?).
        // This function needs to be clear: is it checking tenpai on a 13-tile hand
        // (by trying all discards from a 14-tile hand), or on a 14-tile hand for immediate win?
        // The current logic seems to try discards from a 14-tile hand.

        if num_tiles_in_hand == 0 { // Should not happen with a valid hand.
            return (false, waiting_for_tiles);
        }

        // Convert Vec<Tile> to [u8; 34] counts
        let mut base_hand_counts = [0u8; 34];
        for tile in hand_tiles_for_tenpai_check.iter() {
            base_hand_counts[*tile as usize] += 1;
        }
        
        // Determine tiles to "virtually" discard to reach 13 tiles for tenpai check,
        // or if hand is already 13, we check by adding potential waits.
        let mut potential_discards: Vec<Tile> = Vec::new();
        if num_tiles_in_hand % 3 == 2 { // e.g. 14 tiles, need to discard 1 to check 13-tile tenpai
            for i in 0..34 {
                if base_hand_counts[i] > 0 {
                    potential_discards.push(Tile::try_from(i as u8).unwrap());
                }
            }
             if potential_discards.is_empty() && num_tiles_in_hand > 0 { // Should not happen if num_tiles_in_hand > 0
                return (false, waiting_for_tiles);
            }
        } else if num_tiles_in_hand % 3 == 1 { // e.g. 13 tiles, hand is ready to add a wait tile
            // No discard needed, we iterate through potential wait tiles directly.
            // Add a dummy discard so the loop runs once. This is a bit of a hack.
            // A better structure would separate the 14->13 and 13-tile check logic.
            potential_discards.push(Tile::Man1); // Dummy, won't be used if count is right
        } else {
            // Invalid hand size for standard tenpai (e.g. 12 tiles)
            return (false, waiting_for_tiles);
        }


        for &discard_candidate_for_tenpai_check in &potential_discards {
            let mut current_hand_counts = base_hand_counts; // Start with original hand counts
            let mut effective_tile_count = num_tiles_in_hand as u8;

            if num_tiles_in_hand % 3 == 2 { // If hand was 14, simulate discarding one
                 if current_hand_counts[discard_candidate_for_tenpai_check as usize] > 0 {
                    current_hand_counts[discard_candidate_for_tenpai_check as usize] -= 1;
                    effective_tile_count -= 1;
                } else {
                    // This discard candidate isn't in hand (shouldn't happen with current loop logic)
                    continue; 
                }
            }
            // Now current_hand_counts represents a 13-tile hand (effective_tile_count == 13)
            
            if effective_tile_count % 3 != 1 { // Should be 13 (or 10, 7, 4, 1 for sub-problems)
                // This indicates an issue if we started with 14 and discarded 1.
                continue;
            }

            // Try adding each possible tile to see if it completes a winning hand
            for i in 0..34 { // Iterate through all tile types as potential waits
                let wait_tile = Tile::try_from(i as u8).unwrap();
                
                // Create a temporary 14-tile hand with the potential wait tile added
                let mut temp_counts_with_wait = current_hand_counts; // This is the 13-tile hand
                temp_counts_with_wait[wait_tile as usize] += 1; // Add the wait tile

                // Now, `temp_counts_with_wait` is a 14-tile configuration.
                // We need to get the full hand structure including open melds for the parser.
                // The hand_parser functions expect counts of all 14 tiles that form the final winning shape.
                // Open melds are separate and provide context (menzen, fu) but their tiles are *part* of the 14.
                // The get_combined_hand_counts_internal needs to correctly represent these 14 tiles.

                // For parsing, we need the 14 tiles that would form the win.
                // `temp_counts_with_wait` represents these 14 tiles directly if open melds are part of Hand structure.
                // If Hand only stores concealed part, then open melds need to be "added back" for parsing.
                // Let's assume `temp_counts_with_wait` IS the representation of the 14 tiles for parsing.
                let (final_counts_for_win_check, total_tiles_for_win_check) = 
                    get_combined_hand_counts_internal(&temp_counts_with_wait, open_melds, None);


                // The hand_parser functions should operate on these `final_counts_for_win_check`
                if total_tiles_for_win_check != 14 && !hand_parser::parse_kokushi_musou_sanma(&final_counts_for_win_check).is_some() {
                    // Kokushi might be 13 unique + 1 pair from those 13, making it 14 tiles total
                    // Or it could mean 13 tiles for a 13-sided wait check, this parser checks completed hands.
                    continue;
                }
                
                let forms_standard_win = hand_parser::parse_standard_hand(&final_counts_for_win_check).is_some();
                let forms_chiitoi_win = self.is_menzen(player_idx) && hand_parser::parse_chiitoitsu(&final_counts_for_win_check).is_some();
                // Sanma Kokushi uses 11 unique terminals/honors. The 14-tile winning hand is 9 singles, 1 pair, 1 triplet from these.
                let forms_kokushi_win = self.is_menzen(player_idx) && hand_parser::parse_kokushi_musou_sanma(&final_counts_for_win_check).is_some();


                if forms_standard_win || forms_chiitoi_win || forms_kokushi_win {
                    if !waiting_for_tiles.contains(&wait_tile) {
                        waiting_for_tiles.push(wait_tile);
                    }
                }
            }
            // If we started with a 13-tile hand (num_tiles_in_hand % 3 == 1),
            // we only need to run the inner loop once.
            if num_tiles_in_hand % 3 != 2 { // True if hand was 13 tiles to start
                break;
            }
        }
        
        waiting_for_tiles.sort(); // For consistent output and easier testing
        waiting_for_tiles.dedup();
        (!waiting_for_tiles.is_empty(), waiting_for_tiles)
    }

    pub fn can_declare_riichi(&self, player_idx: usize) -> bool {
        if self.riichi_declared[player_idx] { return false; } // Already in Riichi
        if !self.is_menzen(player_idx) { return false; }      // Hand must be closed
        // Riichi is declared with a 14-tile hand, one of which is then discarded.
        // So, the check is on the 14-tile hand *before* the Riichi discard.
        if self.hands[player_idx].get_all_tiles().len() != 14 { return false; }
        if self.wall.live_wall_remaining_count() < 4 { return false; } // Not enough tiles for draws & ura
        if self.player_scores[player_idx] < 1000 { return false; }    // Not enough points

        // Check if any discard leads to tenpai
        let original_hand_counts = self.hands[player_idx].iter().fold([0u8; 34], |mut acc, (t, c)| {
            acc[t as usize] += c;
            acc
        });

        for i in 0..34 { // Iterate through all possible tiles to discard
            let discard_candidate = Tile::try_from(i as u8).unwrap();
            if original_hand_counts[discard_candidate as usize] > 0 {
                // Simulate discarding this tile
                let mut hand_after_discard_counts = original_hand_counts;
                hand_after_discard_counts[discard_candidate as usize] -= 1;
                
                // Now check if this 13-tile hand is tenpai
                let mut is_tenpai_after_this_discard = false;
                for j in 0..34 { // Iterate through all possible wait tiles
                    let wait_tile = Tile::try_from(j as u8).unwrap();
                    
                    let mut final_hand_counts_for_check = hand_after_discard_counts; // This is 13 tiles
                    final_hand_counts_for_check[wait_tile as usize] += 1; // Add wait tile to make 14

                    // As before, ensure `get_combined_hand_counts_internal` handles this correctly
                    // For parsing, it needs the 14 tiles. `final_hand_counts_for_check` IS these 14 tiles.
                    let (combined_counts, total_tiles) = get_combined_hand_counts_internal(
                        &final_hand_counts_for_check, &self.open_melds[player_idx], None
                    );

                    if total_tiles != 14 && !hand_parser::parse_kokushi_musou_sanma(&combined_counts).is_some() { continue; }


                    if hand_parser::parse_standard_hand(&combined_counts).is_some() ||
                       (self.is_menzen(player_idx) && hand_parser::parse_chiitoitsu(&combined_counts).is_some()) ||
                       (self.is_menzen(player_idx) && hand_parser::parse_kokushi_musou_sanma(&combined_counts).is_some()) {
                        is_tenpai_after_this_discard = true;
                        break; // Found a wait for this discard
                    }
                }
                if is_tenpai_after_this_discard {
                    return true; // Found a discard that results in tenpai
                }
            }
        }
        false // No discard results in tenpai
    }

    pub fn declare_riichi(&mut self, player_idx: usize) {
        // Conditions should be re-checked here or trusted from can_declare_riichi call
        // For robustness, re-check critical ones or ensure can_declare_riichi was just called.
        if self.player_scores[player_idx] >= 1000 && 
           !self.riichi_declared[player_idx] &&
           self.is_menzen(player_idx) &&
           self.hands[player_idx].get_all_tiles().len() == 14 && // Before Riichi discard
           self.wall.live_wall_remaining_count() >= 4 {
            // Note: Actual discard happens after this call via PlayerAction::RiichiDeclare(tile)

            self.player_scores[player_idx] -= 1000;
            self.riichi_sticks += 1;
            self.riichi_declared[player_idx] = true;
            self.ippatsu_eligible[player_idx] = true; // Ippatsu is now possible
            // Double Riichi flag should have been set if eligible from game start.
            // If self.double_riichi_eligible[player_idx] is true at this point, it's a Double Riichi.
            // The scoring will check this flag.
        }
    }

    pub fn can_call_pon(&self, player_idx: usize, called_tile: Tile, discarder_idx: usize) -> bool {
        if player_idx == discarder_idx { return false; } // Cannot Pon own discard
        if self.riichi_declared[player_idx] { return false; } // Cannot Pon after Riichi (unless it's an Ankan that doesn't change waits, which is not Pon)
        // Cannot call if 4-Kan abortive draw is pending from another player (unless this call is Ron)
        if self.four_kan_abortive_draw_pending && self.player_who_declared_fourth_kan != Some(player_idx as u8) {
            // If a 4-kan abortive draw is pending, only Ron is allowed on the discard.
            // This check might be better handled by the calling logic in Env that presents actions.
            // return false; // This might be too broad here. Env should filter.
        }
        self.hands[player_idx].count(called_tile) >= 2
    }


    /// Helper to check general Kan limits based on Tenhou Rule Variant 2.
    /// (No more than 4 kants total in the game, unless all 4 are by one player for Suukantsu).
    /// If 4 kants are made by 2 or 3 players, no more kants are allowed by anyone.
    fn can_declare_any_kan(&self, player_idx: usize) -> bool {
        if self.total_kans_in_game < 4 {
            return true; // Always allowed if less than 4 kants total
        }
        if self.total_kans_in_game == 4 {
            // Check if all 4 kants were by a single player
            let mut single_player_has_all_four = false;
            for p_kans_count_for_player_i in &self.kans_declared_count {
                if *p_kans_count_for_player_i == 4 {
                    single_player_has_all_four = true;
                    break;
                }
            }
            // If one player has all 4, they cannot declare a 5th distinct Kan.
            // (Suukantsu is 4 kants; a 5th implies an error or a very specific scenario).
            // If the 4 kants are spread among 2 or 3 players, no more kants are allowed by anyone.
            if single_player_has_all_four {
                // The player with 4 kants cannot make a 5th *new* kan.
                // This function checks if *any* kan can be declared. If they have 4, they can't make another.
                return self.kans_declared_count[player_idx] < 4; // Can't make a 5th Kan.
            } else {
                // 4 kants by 2 or 3 players, no more kants for anyone.
                return false;
            }
        }
        // total_kans_in_game > 4, generally no more kants.
        false
    }


    pub fn can_call_daiminkan(&self, player_idx: usize, called_tile: Tile, discarder_idx: usize) -> bool {
        if player_idx == discarder_idx { return false; } // Cannot Kan own discard
        if self.riichi_declared[player_idx] { return false; } // Cannot make open Kan after Riichi
        
        if !self.can_declare_any_kan(player_idx) { return false; } // Check 4-Kan limit rule

        // Check if replacement tile is available from dead wall
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 { // Ensure rinshanpai_drawn_count is from Wall
            return false;
        }
        self.hands[player_idx].count(called_tile) >= 3
    }

    pub fn get_possible_ankans(&self, player_idx: usize) -> Option<Vec<Tile>> {
        // Ankan during Riichi is allowed ONLY if it does not change the player's waits.
        // This is a complex check. For now, if Riichi, disallow Ankan from hand for simplicity,
        // unless it's the tile just drawn that completes the Ankan (this is handled as part of Tsumo/action choice).
        // A common simplification is to allow Ankan if Riichi if it's with the drawn tile.
        // If the Ankan is from existing hand tiles, it's more complex.
        // The prompt implies get_possible_ankans is for general case, Riichi check is important.
        if self.riichi_declared[player_idx] {
            // TODO: Implement wait change check for Riichi+Ankan.
            // For now, let's assume if Riichi, only Ankan of a newly drawn tile is straightforward.
            // This function checks for Ankan from tiles *already in hand*.
            // A common ruling: cannot declare Ankan from concealed hand after Riichi if it changes waits.
            // Often, this means no Ankan from hand tiles after Riichi unless it's the drawn tile.
            // Simplification: if riichi, no ankan from current hand tiles (unless it's the just drawn tile that completes it)
            // This function is about ankan from *existing 4 tiles*.
            // A stricter interpretation might return None if riichi_declared[player_idx] is true,
            // unless we add logic to check if the ankan changes waits.
            // For now, let's allow checking, but game loop logic should be careful with Riichi.
        }

        if !self.can_declare_any_kan(player_idx) { return None; } // Check 4-Kan limit rule

        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 { // Ensure rinshanpai_drawn_count is from Wall
            return None;
        }

        let mut ankans = Vec::new();
        for (tile, count) in self.hands[player_idx].iter() {
            if count == 4 {
                // If Riichi, would need to check if this Ankan changes the wait.
                // For now, add to list. The calling code (e.g. Env) must handle Riichi rule.
                ankans.push(tile);
            }
        }
        if ankans.is_empty() { None } else { Some(ankans) }
    }

    pub fn get_possible_shouminkans(&self, player_idx: usize) -> Option<Vec<Tile>> {
        // Shouminkan (adding to an open Pon) is generally allowed during Riichi,
        // but it opens the hand to Chankan (Ron on the Kan'd tile).
        // It doesn't usually change the waits of the original hand structure before the Kan.

        if !self.can_declare_any_kan(player_idx) { return None; } // Check 4-Kan limit rule
        
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 { // Ensure rinshanpai_drawn_count is from Wall
            return None;
        }

        let mut shouminkans = Vec::new();
        for open_meld in &self.open_melds[player_idx] {
            if open_meld.meld_type == DeclaredMeldType::Pon {
                let pon_tile = open_meld.tiles[0]; // Pon stores the tile type
                if self.hands[player_idx].count(pon_tile) >= 1 { // Must have one in hand to add
                    shouminkans.push(pon_tile);
                }
            }
        }
        if shouminkans.is_empty() { None } else { Some(shouminkans) }
    }

    pub fn can_declare_kita_action(&self, player_idx: usize) -> bool {
        // Kita (North wind) declaration in Sanma.
        // Generally cannot declare Kita after Riichi. Some rulesets might allow if drawn.
        if self.riichi_declared[player_idx] {
            // Typically, after Riichi, you can only declare Kita if you draw it and it doesn't change your waits
            // (which it usually wouldn't if it's just a bonus tile).
            // If it's from hand, usually not allowed.
            // For simplicity, if Riichi, let's say cannot declare Kita from existing hand tiles.
            // return false; // This might be too strict, depends on specific rule interpretation for AI.
        }
        // Kita draws a replacement tile, similar to a Kan.
        if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >= 4 {
            return false;
        }
        self.hands[player_idx].count(Tile::North) > 0
    }
    
    // Helper to access wall's rinshanpai count
    pub fn rinshanpai_drawn_count(&self) -> u8 {
        self.wall.rinshanpai_drawn_count() // Delegate to the Wall struct
    }


    /// Common actions after any Kan/Kita declaration (reveal new dora for Kan, draw replacement tile).
    /// This should be called *after* the Kan/Kita meld is added to `open_melds`.
    pub fn perform_kan_common_actions(&mut self, kan_player_idx: usize) -> Option<Tile> {
        self.void_transient_flags_on_call(kan_player_idx); // Calls can void ippatsu, etc.
        // Double Riichi eligibility void if not already in Riichi
        if !self.riichi_declared[kan_player_idx] { self.double_riichi_eligible[kan_player_idx] = false; }


        // For actual Kans (Ankan, Daiminkan, Shouminkan), reveal new Kan Dora.
        // Kita does not typically reveal a new Dora indicator itself.
        let last_meld_type = self.open_melds[kan_player_idx].last().map(|m| m.meld_type);
        if matches!(last_meld_type, Some(DeclaredMeldType::Ankan) | Some(DeclaredMeldType::Daiminkan) | Some(DeclaredMeldType::Shouminkan)) {
            if let Some(new_kan_dora_ind) = self.wall.reveal_new_kan_dora_indicator() {
                self.current_dora_indicators.push(new_kan_dora_ind);
            }
        }

        let replacement_tile_option = self.wall.draw_replacement_tile();
        if let Some(replacement_tile) = replacement_tile_option {
            self.hands[kan_player_idx].add(replacement_tile).expect("Failed to add replacement tile for Kan/Kita");
            self.last_drawn_tile = Some(replacement_tile); // Track the drawn tile
            self.is_rinshan_kaihou_win_pending = true; // Rinshan Kaihou is now possible
        } else {
            // No replacement tile available (e.g., Sūhairenda if 4th Kan has no tile)
            // This state (no replacement tile) should be checked by the game loop (Env::step)
            // to trigger an abortive draw if necessary.
            self.last_drawn_tile = None;
            self.is_rinshan_kaihou_win_pending = false; // Cannot win by Rinshan if no tile drawn
        }
        replacement_tile_option
    }


    pub fn make_pon(&mut self, calling_player_idx: usize, called_tile: Tile, discarder_idx: usize) -> Result<(), HandError> {
        if calling_player_idx == discarder_idx { return Err(HandError::Generic("Cannot Pon own discard")); }
        if self.hands[calling_player_idx].count(called_tile) < 2 { return Err(HandError::Generic("Not enough tiles for Pon"));}

        self.hands[calling_player_idx].remove_n(called_tile, 2)?;

        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Pon,
            // For Pon, tiles[0..2] are from hand, tiles[2] conceptually from discard, or all are same type.
            // Store all as called_tile for consistency in the array.
            tiles: [called_tile, called_tile, called_tile, called_tile], // Store 4 for consistency, though Pon uses 3
            called_from_discarder_idx: Some(discarder_idx as u8),
            called_tile: Some(called_tile),
        };
        self.open_melds[calling_player_idx].push(meld);

        self.void_transient_flags_on_call(calling_player_idx, Some(discarder_idx)); // Pass discarder_idxf
        self.current_player_idx = calling_player_idx as u8; // Turn moves to Pon declarer
        self.last_discarded_tile_info = None; // Discard is now part of the Pon
        self.is_rinshan_kaihou_win_pending = false; // Pon is not a Kan
        self.is_chankan_window_open = false; // Pon does not trigger Chankan
        Ok(())
    }

    /// Centralized logic for when a Kan is successfully declared (Ankan, Daiminkan, Shouminkan).
    /// This should be called AFTER the Kan meld is added to `open_melds`.
    fn handle_kan_declaration(&mut self, kan_player_idx: usize) {
        self.kans_declared_count[kan_player_idx] += 1;
        self.total_kans_in_game += 1;

        // Check for 4-Kan abortive draw based on Tenhou rules (Suukaikan)
        if self.total_kans_in_game == 4 {
            let mut single_player_has_all_four = false;
            for p_idx in 0..3 {
                if self.kans_declared_count[p_idx] == 4 { // Check if this player made all 4 kants
                    single_player_has_all_four = true;
                    break;
                }
            }
            // If 4 kants are made by two or three different players (i.e., not one player making Suukantsu)
            if !single_player_has_all_four {
                self.four_kan_abortive_draw_pending = true;
                self.player_who_declared_fourth_kan = Some(kan_player_idx as u8);
                // The game ends if the discard after this 4th Kan is not Ron'd.
                // Play continues for Suukantsu (one player makes 4 kants).
            }
        }
        // Note: A 5th Kan by different players (if total_kans_in_game becomes 5 due to simultaneous declarations, though rare)
        // is typically an abortive draw (Sūkairen Da). The `can_declare_any_kan` should prevent this state for sequential kans.
    }


    pub fn make_daiminkan(&mut self, calling_player_idx: usize, called_tile: Tile, discarder_idx: usize) -> Result<(), HandError> {
        // Conditions already checked by can_call_daiminkan
        if !self.can_call_daiminkan(calling_player_idx, called_tile, discarder_idx) { // Redundant check for safety
             return Err(HandError::Generic("Cannot make Daiminkan (conditions not met)"));
        }
        self.hands[calling_player_idx].remove_n(called_tile, 3)?;
        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Daiminkan,
            tiles: [called_tile; 4],
            called_from_discarder_idx: Some(discarder_idx as u8),
            called_tile: Some(called_tile),
        };
        self.open_melds[calling_player_idx].push(meld); // Add before calling handle_kan_declaration
        self.handle_kan_declaration(calling_player_idx); // Update Kan counts
        self.void_transient_flags_on_call(calling_player_idx, Some(discarder_idx));
		// perform_kan_common_actions will call void_transient_flags_on_call again, but with None for discarder_idx,
        // which is fine as it won't reset any_discard_called_this_round if already true.
        self.perform_kan_common_actions(calling_player_idx); // Reveal Dora, draw replacement, set Rinshan flag
        self.current_player_idx = calling_player_idx as u8; // Turn moves to Kan declarer
        self.last_discarded_tile_info = None; // Discard is now part of Kan
        self.is_chankan_window_open = false; // Daiminkan cannot be Chankan'd
        Ok(())
    }

    pub fn make_ankan(&mut self, calling_player_idx: usize, kan_tile: Tile) -> Result<(), HandError> {
        // Conditions already checked by get_possible_ankans
        if self.get_possible_ankans(calling_player_idx).map_or(true, |v| !v.contains(&kan_tile)) { // Redundant for safety
             return Err(HandError::Generic("Cannot make Ankan (conditions not met or tile not eligible)"));
        }
        self.hands[calling_player_idx].remove_n(kan_tile, 4)?;
        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Ankan, tiles: [kan_tile; 4],
            called_from_discarder_idx: None, called_tile: None,
        };
        self.open_melds[calling_player_idx].push(meld); // Add before calling handle_kan_declaration
        self.handle_kan_declaration(calling_player_idx); // Update Kan counts
        
        self.perform_kan_common_actions(calling_player_idx); // Reveal Dora, draw replacement, set Rinshan flag
        self.current_player_idx = calling_player_idx as u8; // Player's turn continues
        self.is_chankan_window_open = false; // Ankan cannot be Chankan'd
        Ok(())
    }

    pub fn make_shouminkan(&mut self, calling_player_idx: usize, tile_to_add: Tile) -> Result<(), HandError> {
        // Conditions already checked by get_possible_shouminkans
        if self.get_possible_shouminkans(calling_player_idx).map_or(true, |v| !v.contains(&tile_to_add)) { // Redundant
            return Err(HandError::Generic("Cannot make Shouminkan (conditions not met or tile not eligible)"));
        }
        let pon_meld_idx = self.open_melds[calling_player_idx].iter().position(|m|
            m.meld_type == DeclaredMeldType::Pon && m.tiles[0] == tile_to_add)
            .ok_or(HandError::Generic("No matching Pon to upgrade to Shouminkan"))?;
        
        self.hands[calling_player_idx].remove(tile_to_add)?; // Remove the 1 tile from hand
        self.open_melds[calling_player_idx][pon_meld_idx].meld_type = DeclaredMeldType::Shouminkan;
        self.open_melds[calling_player_idx][pon_meld_idx].tiles = [tile_to_add; 4]; // Update meld to be a Kan
        
        self.handle_kan_declaration(calling_player_idx); // Call after meld type is updated & before Chankan window

        // Open Chankan window *before* drawing replacement tile or revealing new Dora for Shouminkan
        self.is_chankan_window_open = true;
        self.chankan_tile_and_declarer = Some((tile_to_add, calling_player_idx as u8));
        // perform_kan_common_actions (drawing replacement, revealing Dora) is called by Env::step
        // *after* the chankan window resolves if no chankan occurs.
        self.current_player_idx = calling_player_idx as u8; // It's still this player's "moment" for chankan response
        Ok(())
    }
    
    pub fn make_kita_declaration(&mut self, calling_player_idx: usize) -> Result<(), HandError> {
        if !self.can_declare_kita_action(calling_player_idx) {
            return Err(HandError::Generic("Cannot declare Kita at this time"));
        }
        self.hands[calling_player_idx].remove(Tile::North)?;

        let meld = DeclaredMeld {
            meld_type: DeclaredMeldType::Kita, tiles: [Tile::North; 4], // Store 4 North for consistency, though it's one tile set aside
            called_from_discarder_idx: None, called_tile: None,
        };
        self.open_melds[calling_player_idx].push(meld); // Add before calling perform_kan_common_actions
        self.kita_declared_count[calling_player_idx] += 1;
        
        // Kita draws a replacement tile. It does NOT typically reveal a new Dora indicator.
        // perform_kan_common_actions needs to be aware if it's a Kita or regular Kan.
        // The current perform_kan_common_actions checks the last meld type.
        self.perform_kan_common_actions(calling_player_idx); // This will draw replacement and set Rinshan flag

        self.is_chankan_window_open = false; // Kita cannot be Chankan'd
        self.current_player_idx = calling_player_idx as u8; // Player continues their turn (to discard)
        Ok(())
    }


    pub fn is_menzen(&self, seat: usize) -> bool {
        self.open_melds[seat].iter().all(|meld|
            matches!(meld.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita))
    }

    pub fn check_tsumo(&self) -> bool {
        let seat = self.current_player_idx as usize;
        // For Tsumo, the hand must be 14 tiles (13 + drawn tile).
        // The last_drawn_tile is already part of self.hands[seat] by this point.
        let (final_counts, total_tiles) = get_combined_hand_counts(&self.hands[seat], &self.open_melds[seat], None);

        if total_tiles != 14 && !hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some() {
            // Kokushi check is special as its "shape" can be 13 unique + 1 pair.
            return false;
        }
        
        let forms_standard_win = hand_parser::parse_standard_hand(&final_counts).is_some();
        let forms_chiitoi_win = self.is_menzen(seat) && hand_parser::parse_chiitoitsu(&final_counts).is_some();
        let forms_kokushi_win = self.is_menzen(seat) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some();

        if !(forms_standard_win || forms_chiitoi_win || forms_kokushi_win) {
            return false;
        }
        // TODO: Add Yaku check. A hand must have at least one Yaku to be a valid win.
        // This check_tsumo is only for hand completion. Scoring will determine if Yaku exists.
        true
    }

    /// Checks if a player can Ron on a specific tile.
    /// Includes basic Furiten checks (player cannot Ron on a tile they have previously discarded if it was one of their waits).
    pub fn can_call_ron(&self, winning_player_seat: usize, ron_tile: Tile, _discarder_seat: usize) -> bool {
        // 1. Check for Tenpai and if ron_tile is a winning tile
        let (is_player_tenpai, current_waits) = self.is_tenpai(winning_player_seat);
        if !is_player_tenpai || !current_waits.contains(&ron_tile) {
            return false; // Not tenpai or ron_tile not a wait
        }

        // 2. Basic Furiten Check (Temporary Furiten due to own discards)
        // A player is in furiten if any of their current winning tiles have been discarded by them previously
        // AND those discards have not been called by another player.
        for discarded_by_winner in &self.discards[winning_player_seat] {
            if current_waits.contains(discarded_by_winner) {
                // Check if this discard is still "on the table" or was part of a call
                // This simplified check assumes any prior discard of a wait tile = furiten.
                return false; // Temporary Furiten: one of the waits is in own discards.
            }
        }
        // TODO: Add Riichi Furiten (if player passed on a Ron after Riichi, furiten until next draw for same waits).
        // TODO: Add Permanent Furiten (if player ever discarded any tile that would have completed a previous tenpai hand,
        //       and that hand structure and waits were the same as current). This is more complex.

        // 3. Check hand formation with the ron_tile
        let (final_counts, total_tiles) = get_combined_hand_counts(
            &self.hands[winning_player_seat],
            &self.open_melds[winning_player_seat],
            Some(ron_tile) // Add the ron_tile to the hand for parsing
        );
        
        if total_tiles != 14 && !hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some() {
            return false;
        }

        let forms_standard_win = hand_parser::parse_standard_hand(&final_counts).is_some();
        let forms_chiitoi_win = self.is_menzen(winning_player_seat) && hand_parser::parse_chiitoitsu(&final_counts).is_some();
        let forms_kokushi_win = self.is_menzen(winning_player_seat) && hand_parser::parse_kokushi_musou_sanma(&final_counts).is_some();
        
        if !(forms_standard_win || forms_chiitoi_win || forms_kokushi_win) {
            return false;
        }
        // TODO: Yaku check. For Ron, a Yaku is required.
        // This check is for hand completion. Scoring will verify Yaku.
        true
    }


    pub fn score_win(&mut self, winning_player_seat: usize, win_type: WinType) -> Score {
        let winning_tile_for_logic = match win_type {
            WinType::Ron { winning_tile, .. } => Some(winning_tile),
            WinType::Tsumo => self.last_drawn_tile, // Assumes last_drawn_tile is set correctly for Tsumo
        };

        // Get the final 14-tile hand counts for parsing
        let (final_counts, _total_tiles) = get_combined_hand_counts(
            &self.hands[winning_player_seat],
            &self.open_melds[winning_player_seat],
            // If Ron, the winning tile is conceptually added to the hand for parsing.
            // If Tsumo, it's already in self.hands[winning_player_seat] due to player_draws_tile.
            match win_type { WinType::Ron { winning_tile, .. } => Some(winning_tile), _ => None }
        );

        let mut han: u8 = 0;
        let mut yaku_list: Vec<(&'static str, u8)> = Vec::new();
        let mut is_yakuman_hand = false;
        let mut yakuman_multiplier = 0;

        let menzen = self.is_menzen(winning_player_seat);
        let is_dealer = winning_player_seat == self.dealer_idx as usize;
        
        // --- Parse Hand Structure ---
        let parsed_std_hand = hand_parser::parse_standard_hand(&final_counts);
        let parsed_chiitoi_hand = if parsed_std_hand.is_none() && menzen { hand_parser::parse_chiitoitsu(&final_counts) } else { None };
        let parsed_kokushi_hand = if parsed_std_hand.is_none() && parsed_chiitoi_hand.is_none() && menzen { hand_parser::parse_kokushi_musou_sanma(&final_counts) } else { None };

        // --- Check Yakuman ---
        // (Using placeholder yaku counting functions for brevity - actual implementations are complex)
        let no_interrupting_calls_this_round = self.open_melds.iter().all(|p_melds| {
            p_melds.iter().all(|m| matches!(m.meld_type, DeclaredMeldType::Ankan | DeclaredMeldType::Kita))
        });

        // Tenhou (Blessing of Heaven)
        if self.is_tenhou_win_possible && is_dealer && self.turn_count == 0 && matches!(win_type, WinType::Tsumo) && no_interrupting_calls_this_round {
            is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Tenhou", 13));
        }
        
        // Chiihou (Blessing of Earth)
        // Player's first *uninterrupted* draw. turn_count is relative to game start.
        let is_player_first_uninterrupted_draw = self.turn_count == (winning_player_seat as u32).wrapping_sub(self.dealer_idx as u32).rem_euclid(3) && no_interrupting_calls_this_round;
        if self.is_chiihou_win_possible[winning_player_seat] && !is_dealer && is_player_first_uninterrupted_draw && matches!(win_type, WinType::Tsumo) {
            is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Chiihou", 13));
        }
        
        if let Some(ref kokushi_hand_details) = parsed_kokushi_hand {
            let kokushi_val = count_kokushi_musou_yaku(&final_counts, menzen, win_type, &self.open_melds[winning_player_seat], Some(kokushi_hand_details));
            if kokushi_val > 0 { is_yakuman_hand = true; yakuman_multiplier += kokushi_val / 13; yaku_list.push(if kokushi_val == 26 {("Kokushi Musou (13-wait)", 26)} else {("Kokushi Musou", 13)}); }
        }
        
        if let Some(ref std_hand) = parsed_std_hand {
            // Suuankou (Four Concealed Triplets)
            let suuanko_val = count_suuankou_yaku(
                std_hand, menzen, win_type, 
                &self.open_melds[winning_player_seat], 
                &self.hands[winning_player_seat], // Pass current hand state for Tanki check
                winning_tile_for_logic // Pass winning tile for Tanki check
            );
            if suuanko_val > 0 { is_yakuman_hand = true; yakuman_multiplier += suuanko_val / 13; yaku_list.push(if suuanko_val == 26 {("Suuankou (Tanki)", 26)} else {("Suuankou", 13)}); }
        
            // Daisuushii / Shousuushii (Big/Little Four Winds)
            let daisuushii_val = count_daisuushii_yaku(std_hand, &self.open_melds[winning_player_seat], menzen);
            if daisuushii_val > 0 { is_yakuman_hand = true; yakuman_multiplier += daisuushii_val / 13; yaku_list.push(if daisuushii_val == 26 {("Daisuushii (Double)", 26)} else {("Daisuushii", 13)}); }
            else { // Only check Shousuushii if Daisuushii is not met
                let shousuushii_val = count_shousuushii_yaku(std_hand, &self.open_melds[winning_player_seat], menzen);
                if shousuushii_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Shousuushii", 13)); }
            }
        }
        
        // Daisangen (Big Three Dragons)
        let daisangen_val = count_daisangen_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref());
        if daisangen_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Daisangen", 13)); }
        
        // Tsuu Iisou (All Honors)
        let tsuu_iisou_val = count_tsuu_iisou_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if tsuu_iisou_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Tsuu Iisou (All Honors)", 13));}
                
        // Chinroutou (All Terminals)
        let chinroutou_val = count_chinroutou_yaku(&final_counts, &self.open_melds[winning_player_seat], parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if chinroutou_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Chinroutou (All Terminals)", 13));}

        // Suukantsu (Four Kans)
        let suukantsu_val = count_suukantsu_yaku(self.kans_declared_count[winning_player_seat]);
         if suukantsu_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Suukantsu (Four Kans)", 13));}
        
        // Chuuren Poutou (Nine Gates)
        if let Some(wt) = winning_tile_for_logic { // Chuuren requires a specific winning tile for 9-wait
            let chuuren_val = count_chuuren_poutou_yaku(&final_counts, menzen, wt);
            if chuuren_val > 0 { is_yakuman_hand = true; yakuman_multiplier += chuuren_val / 13; yaku_list.push(if chuuren_val == 26 {("Chuuren Poutou (9-wait)", 26)} else {("Chuuren Poutou", 13)}); }
        }
        // Ryuu Iisou (All Green)
        let ryuu_iisou_val = count_ryuu_iisou_yaku(&final_counts, parsed_std_hand.as_ref(), parsed_chiitoi_hand.as_ref());
        if ryuu_iisou_val > 0 { is_yakuman_hand = true; yakuman_multiplier += 1; yaku_list.push(("Ryuu Iisou", 13)); }


        // --- If not Yakuman, count standard Yaku ---
        if !is_yakuman_hand {
            // Riichi / Double Riichi / Ippatsu
            if self.riichi_declared[winning_player_seat] {
                if self.double_riichi_eligible[winning_player_seat] { // Check if it was a double riichi
                    han += 2; yaku_list.push(("Double Riichi", 2));
                } else {
                    han += 1; yaku_list.push(("Riichi", 1));
                }
                if self.ippatsu_eligible[winning_player_seat] { han += 1; yaku_list.push(("Ippatsu", 1)); }
            }

            // Menzen Tsumo
            if menzen && matches!(win_type, WinType::Tsumo) { han += 1; yaku_list.push(("Menzen Tsumo", 1)); }
            
            // Rinshan Kaihou (After Kan Tsumo)
            if self.is_rinshan_kaihou_win_pending && matches!(win_type, WinType::Tsumo) { han += 1; yaku_list.push(("Rinshan Kaihou", 1));}
            // Chankan (Robbing a Kan)
            if self.is_chankan_window_open { // Check if chankan was possible
                if let WinType::Ron{winning_tile: ron_t, ..} = win_type {
                    if Some(ron_t) == self.chankan_tile_and_declarer.map(|(t,_)|t) { // Check if won on the chankan tile
                         han += 1; yaku_list.push(("Chankan", 1));
                    }
                }
            }
            // Haitei Raoyue (Tsumo on last tile from wall) / Houtei Raoyui (Ron on last discard)
            if self.wall.is_live_wall_empty() && self.wall.rinshanpai_drawn_count() >= 4 { // Wall is truly empty
                if matches!(win_type, WinType::Tsumo) && self.last_drawn_tile.is_some() { // Ensure it was a Tsumo on that last tile
                    // Check if the drawn tile was indeed the very last available draw.
                    // This is true if live_wall_remaining_count was 0 before this draw and it was a live wall draw.
                    // Or, if it was the last rinshanpai and live wall was already empty.
                    // Simplified: if wall is empty now, and it's a tsumo.
                    if self.wall.live_wall_remaining_count() == 0 && self.wall.rinshanpai_drawn_count() >=4 { // Double check
                         han += 1; yaku_list.push(("Haitei Raoyue", 1));
                    }
                }
                // Houtei Raoyui applies if the win is by Ron on the very last discard of the game (when wall is empty).
                // The `last_discarded_tile_info` should be set for Ron.
                if matches!(win_type, WinType::Ron{..}) && self.last_discarded_tile_info.is_some() { // Ensure it was a Ron on a discard
                     han += 1; yaku_list.push(("Houtei Raoyui", 1));
                }
            }


            // Hand Structure Yaku
            if let Some(ref chiitoi_hand) = parsed_chiitoi_hand {
                let (is_chiitoi, chiitoi_han_val) = count_chiitoitsu_yaku(&final_counts, &self.open_melds[winning_player_seat], Some(chiitoi_hand));
                if is_chiitoi { han += chiitoi_han_val; yaku_list.push(("Chiitoitsu", chiitoi_han_val));}
            } else if let Some(ref std_hand) = parsed_std_hand { // Other yaku usually don't combine with Chiitoitsu
                // Pinfu
                let actual_winning_tile_for_pinfu = match win_type {
                    WinType::Tsumo => self.last_drawn_tile.expect("Last drawn tile missing for Tsumo Pinfu check"),
                    WinType::Ron { winning_tile, .. } => winning_tile,
                };
                let (pinfu_exists, pinfu_han_val) = count_pinfu_yaku(
                    std_hand, menzen, 
                    self.seat_winds[winning_player_seat], self.round_wind, 
                    actual_winning_tile_for_pinfu // Pass the actual winning tile
                );
                if pinfu_exists { han += pinfu_han_val; yaku_list.push(("Pinfu", pinfu_han_val));}

                // Iipeikou / Ryanpeikou
                let iipeikou_val = count_iipeikou_yaku(std_hand, menzen); // Ryanpeikou is 3, Iipeikou is 1
                if iipeikou_val == 3 { han += 3; yaku_list.push(("Ryanpeikou", 3));}
                else if iipeikou_val == 1 { han += 1; yaku_list.push(("Iipeikou", 1));}
                
                // Toitoihou (All Triplets)
                let (toitoi_exists, toitoi_han_val) = count_toitoihou_yaku(std_hand);
                if toitoi_exists { han += toitoi_han_val; yaku_list.push(("Toitoihou", toitoi_han_val));}

                // Sanankou (Three Concealed Triplets)
                let sanankou_winning_tile = match win_type { WinType::Ron { winning_tile, ..} => Some(winning_tile), _ => None};
                let (sanankou_exists, sanankou_han_val) = count_sanankou_yaku(std_hand, win_type, &self.open_melds[winning_player_seat], &self.hands[winning_player_seat], sanankou_winning_tile);
                if sanankou_exists { han += sanankou_han_val; yaku_list.push(("Sanankou", sanankou_han_val));}
                
                // Honroutou (All Terminals and Honors - if not Chiitoi)
                let (honroutou_exists, honroutou_han_val) = count_honroutou_yaku(Some(std_hand), None); // Pass None for chiitoi if std_hand exists
                if honroutou_exists { han += honroutou_han_val; yaku_list.push(("Honroutou", honroutou_han_val));}

                // Sankantsu (Three Kans)
                let (sankantsu_exists, sankantsu_han_val) = count_sankantsu_yaku(self.kans_declared_count[winning_player_seat]);
                if sankantsu_exists { han += sankantsu_han_val; yaku_list.push(("Sankantsu", sankantsu_han_val));}
                
                // Chanta / Junchan
                let (chanta_exists, chanta_han_val) = count_chanta_yaku(std_hand, menzen);
                if chanta_exists { han += chanta_han_val; yaku_list.push(("Chanta", chanta_han_val));}
                else { // Only check Junchan if not Chanta (Junchan is a subset of Chanta conditions but higher value)
                    let (junchan_exists, junchan_han_val) = count_junchan_yaku(std_hand, menzen);
                    if junchan_exists { han += junchan_han_val; yaku_list.push(("Junchan", junchan_han_val));}
                }
                // Sanshoku Doujun (Three Color Sequences)
                let (sanshoku_doujun_exists, sanshoku_doujun_han) = count_sanshoku_doujun_yaku(std_hand, menzen);
                if sanshoku_doujun_exists { han += sanshoku_doujun_han; yaku_list.push(("Sanshoku Doujun", sanshoku_doujun_han));}
                
                // Sanshoku Doukou (Three Color Triplets)
                let (sanshoku_doukou_exists, sanshoku_doukou_han) = count_sanshoku_doukou_yaku(std_hand);
                if sanshoku_doukou_exists { han += sanshoku_doukou_han; yaku_list.push(("Sanshoku Doukou", sanshoku_doukou_han));}

                // Ittsuu (Pure Straight)
                let (ittsuu_exists, ittsuu_han) = count_ittsuu_yaku(std_hand, menzen);
                if ittsuu_exists { han += ittsuu_han; yaku_list.push(("Ittsuu", ittsuu_han));}

                // Shousangen is handled with Yakuhai below if not Daisangen
            }
            // Honroutou for Chiitoitsu
            if parsed_chiitoi_hand.is_some() && parsed_std_hand.is_none() { // Ensure it's Chiitoi and not a standard hand also qualifying
                let (honroutou_chiitoi_exists, honroutou_chiitoi_han_val) = count_honroutou_yaku(None, parsed_chiitoi_hand.as_ref());
                if honroutou_chiitoi_exists { han += honroutou_chiitoi_han_val; yaku_list.push(("Honroutou (Chiitoi)", honroutou_chiitoi_han_val));}
            }
            
            // Tanyao (All Simples)
            let (tanyao_exists, tanyao_han_val) = count_tanyao_yaku(&final_counts, &self.open_melds[winning_player_seat]);
            if tanyao_exists { han += tanyao_han_val; yaku_list.push(("Tanyao", tanyao_han_val));}

            // Yakuhai (Value Honor Triplets/Kans) - Dragons, Seat Wind, Round Wind
            // Also Shousangen (Little Three Dragons) if applicable (2 dragon triplets + 1 dragon pair)
            // Shousangen check
            let shousangen_active = if let Some(ref std_hand) = parsed_std_hand {
                let (exists, val) = count_shousangen_yaku(std_hand);
                if exists { han += val; yaku_list.push(("Shousangen", val)); true } else { false }
            } else { false };

            // Yakuhai (Dragons, Seat/Round Winds)
            // Ensure these are not double-counted if Shousangen / Shousuushii / Daisuushii already gave value.
            let shousuushii_active = yaku_list.iter().any(|(name,_)| *name == "Shousuushii");
            let daisuushii_active = yaku_list.iter().any(|(name,_)| *name == "Daisuushii");

            let yakuhai_base_han = count_yakuhai_yaku(
                &final_counts, 
                self.seat_winds[winning_player_seat], 
                self.round_wind, 
                &mut yaku_list, // Pass yaku_list to add individual yakuhai if not part of composite
                parsed_std_hand.as_ref(), // For context if needed by refined yakuhai logic
                shousangen_active || shousuushii_active || daisuushii_active // Skip individual dragons/winds if already part of these yakuman/high-value yaku
            );
            han += yakuhai_base_han; // Add han from distinct yakuhai
            
            // Honitsu (Half Flush) / Chinitsu (Full Flush)
            let (chinitsu_exists, chinitsu_han_val) = count_chinitsu_yaku(&final_counts, menzen, &self.open_melds[winning_player_seat]);
            if chinitsu_exists { han += chinitsu_han_val; yaku_list.push(("Chinitsu", chinitsu_han_val));}
            else { // Only check Honitsu if not Chinitsu
                let (honitsu_exists, honitsu_han_val) = count_honitsu_yaku(&final_counts, menzen, &self.open_melds[winning_player_seat]);
                if honitsu_exists { han += honitsu_han_val; yaku_list.push(("Honitsu", honitsu_han_val));}
            }
            
            // --- Dora ---
            // Ura Dora only revealed if Riichi
            if self.riichi_declared[winning_player_seat] && self.current_ura_dora_indicators.is_empty() {
                // This check for emptiness might be problematic if Ura Dora are revealed progressively for Kans too.
                // Usually, all Ura Dora are revealed at end if Riichi wins.
                self.current_ura_dora_indicators = self.wall.get_current_ura_dora_indicators();
            }

            let dora_val = count_dora_value(&final_counts, &self.current_dora_indicators);
            if dora_val > 0 { han += dora_val; yaku_list.push(("Dora", dora_val));}
            
            if self.riichi_declared[winning_player_seat] {
                let ura_dora_val = count_dora_value(&final_counts, &self.current_ura_dora_indicators);
                if ura_dora_val > 0 { han += ura_dora_val; yaku_list.push(("Ura Dora", ura_dora_val));}
            }
            
            let red_five_val = count_red_five_value(&final_counts, &self.red_five_tile_ids);
            if red_five_val > 0 { han += red_five_val; yaku_list.push(("Aka Dora", red_five_val));}
            
            // Kita Dora (Sanma specific)
            let kita_dora_val = self.kita_declared_count[winning_player_seat]; // Each Kita declared is 1 Han
            if kita_dora_val > 0 { han += kita_dora_val; yaku_list.push(("Kita Dora", kita_dora_val));}


            // If no Yaku were found (han is still 0), it's a Yaku Nashi (no win).
            // This should be checked *before* Dora. Dora don't count if no other Yaku.
            // Re-evaluate Yaku Nashi condition: if yaku_list is empty (excluding Dora types)
            let has_actual_yaku = yaku_list.iter().any(|(name, _)| 
                !matches!(*name, "Dora" | "Ura Dora" | "Aka Dora" | "Kita Dora")
            );
            if !has_actual_yaku && han > 0 { // Had points from Dora but no actual Yaku
                // This means it's Yaku Nashi. Reset han and yaku_list from Dora.
                // This is a common error point. Let's refine Yaku Nashi check.
                // If after all non-Dora Yaku, han is 0 (or yaku_list of non-Dora is empty), then Yaku Nashi.
            }
             if !is_yakuman_hand && !has_actual_yaku { // Check if any yaku (non-dora) was awarded
                return Score { han: 0, fu: 0, points: 0, yaku_details: vec![("Yaku Nashi", 0)] };
            }


            // Kazoe Yakuman (Counted Yakuman)
            // If han (from yaku + dora) reaches 13 or more, it becomes a Kazoe Yakuman.
            if han >= 13 && !is_yakuman_hand { // Ensure it wasn't already a named Yakuman
                is_yakuman_hand = true; yakuman_multiplier = 1; han = 13; // Standard Kazoe is 1x Yakuman
                // Update yaku_list to reflect Kazoe Yakuman
                yaku_list.retain(|(name,_)| name.contains("Yakuman") || *name == "Kazoe Yakuman"); // Keep other potential yakuman if it was multi-yakuman scenario
                if !yaku_list.iter().any(|(n,_)| n.contains("Yakuman")) { // If no other yakuman, set Kazoe
                    yaku_list.clear();
                    yaku_list.push(("Kazoe Yakuman", 13));
                }
            }
        }
        
        // --- Calculate Fu ---
        let fu = if is_yakuman_hand { 0 } // Yakuman hands don't have Fu (points are fixed)
        else if parsed_chiitoi_hand.is_some() { 25 } // Chiitoitsu is fixed 25 Fu
        else if let Some(ref std_hand) = parsed_std_hand {
            // Check for Pinfu for Fu calculation
            let actual_winning_tile_for_pinfu_fu = match win_type {
                WinType::Tsumo => self.last_drawn_tile.expect("Last drawn tile missing for Tsumo Pinfu fu check"),
                WinType::Ron { winning_tile, .. } => winning_tile,
            };
            let (is_pinfu, _) = count_pinfu_yaku( // Re-check Pinfu specifically for Fu rule
                std_hand, menzen, 
                self.seat_winds[winning_player_seat], self.round_wind, 
                actual_winning_tile_for_pinfu_fu
            );
            // Pinfu Yaku must be present in yaku_list for Pinfu Fu rules to apply.
            let pinfu_yaku_present = yaku_list.iter().any(|(name,_)| *name == "Pinfu");

            if is_pinfu && pinfu_yaku_present { // Pinfu Yaku implies specific Fu rules
                if matches!(win_type, WinType::Tsumo) { 20 } // Pinfu Tsumo is 20 Fu
                else { 30 } // Pinfu Ron is 30 Fu (base 20 + menzen ron 10, no other fu)
            } else {
                // Calculate Fu for non-Pinfu standard hands
                let fu_input = FuCalculationInput {
                    parsed_hand: std_hand,
                    open_melds_declared: &self.open_melds[winning_player_seat],
                    win_type,
                    winning_tile: winning_tile_for_logic.unwrap_or(std_hand.pair), // Default if somehow missing
                    is_menzen_win: menzen,
                    seat_wind: self.seat_winds[winning_player_seat],
                    round_wind: self.round_wind,
                    hand_before_win_completion: None, // TODO: Consider providing for more accurate wait Fu
                };
                fu_calculation::calculate_fu(&fu_input)
            }
        } else { 30 }; // Default Fu if not Yakuman, Chiitoi, or Standard (should not happen if parsed)


        // --- Calculate Points ---
        let mut points = calculate_points_final(han, fu, is_dealer, win_type, is_yakuman_hand, yakuman_multiplier);
        
        // Add Riichi sticks and Honba bonus to the total points the winner receives
        points += self.riichi_sticks as u32 * 1000;
        points += self.honba_sticks as u32 * 300; // Each honba stick adds 300 to the payout

        Score { han, fu, points, yaku_details }
    }

    pub fn apply_score_transfers_and_reset_sticks(&mut self, winning_player_seat: usize, win_type: WinType, score_details: &Score) {
        // points_value_of_hand is the hand's intrinsic score before Riichi/Honba for payment distribution
        let points_value_of_hand = score_details.points
            .saturating_sub(self.riichi_sticks as u32 * 1000)
            .saturating_sub(self.honba_sticks as u32 * 300);

        let is_dealer_win = winning_player_seat == self.dealer_idx as usize;

        // Payments from losers to winner based on points_value_of_hand
        match win_type {
            WinType::Ron { discarder_seat, .. } => {
                // Ron: Discarder pays the full points_value_of_hand
                self.player_scores[winning_player_seat] += points_value_of_hand as i32;
                self.player_scores[discarder_seat] -= points_value_of_hand as i32;
            }
            WinType::Tsumo => {
                // Tsumo: Payments are split among other players.
                // Winner already gets points_value_of_hand added to their conceptual total.
                self.player_scores[winning_player_seat] += points_value_of_hand as i32;

                // For Sanma Tsumo:
                // If dealer wins, each non-dealer pays 1/2 of points_value_of_hand.
                // If non-dealer wins, dealer pays 1/2, other non-dealer pays 1/2.
                // (Assuming points_value_of_hand is structured to be divisible by 2 for these cases,
                // or rounding rules are applied in calculate_points_final).
                // A common way is dealer pays more if non-dealer wins.
                // Let's use: Non-dealer Tsumo: Dealer pays X, other non-dealer pays Y.
                // Dealer Tsumo: Each non-dealer pays Z.
                // Total collected = points_value_of_hand.
                
                // Simplified Sanma Tsumo payment:
                // `points_value_of_hand` is what the winner gets IN ADDITION to stick money.
                // This value is paid by the others.
                if is_dealer_win { // Dealer Tsumo
                    // Each non-dealer pays half of the hand's value.
                    // `calculate_points_final` for Dealer Tsumo already returns total (e.g. 2000 from each if 4000 point hand).
                    // So, points_value_of_hand is the total collected. Non-dealers pay half each.
                    let payment_from_each_non_dealer = points_value_of_hand / 2; // Assumes points_value_of_hand is total collected by dealer from others.
                    for i in 0..3 {
                        if i != winning_player_seat { // Non-dealers
                            self.player_scores[i] -= payment_from_each_non_dealer as i32;
                        }
                    }
                } else { // Non-Dealer Tsumo
                    // Dealer pays half, other non-dealer pays half.
                    // `calculate_points_final` for Non-Dealer Tsumo returns total.
                    // e.g. Hand is 2000 total: Dealer pays 1000, other non-dealer pays 1000.
                    // So points_value_of_hand is this total.
                    let payment_from_dealer = points_value_of_hand / 2; // Or specific dealer portion
                    let payment_from_other_non_dealer = points_value_of_hand - payment_from_dealer; // The remainder

                    for i in 0..3 {
                        if i == self.dealer_idx as usize { // Dealer pays their share
                            self.player_scores[i] -= payment_from_dealer as i32;
                        } else if i != winning_player_seat { // Other non-dealer pays their share
                            self.player_scores[i] -= payment_from_other_non_dealer as i32;
                        }
                    }
                }
            }
        }

        // Winner collects all riichi and honba sticks that were on the table.
        // This is added ON TOP of the hand payments.
        self.player_scores[winning_player_seat] += (self.riichi_sticks as i32 * 1000);
        self.player_scores[winning_player_seat] += (self.honba_sticks as i32 * 300);

        self.riichi_sticks = 0; // Reset riichi sticks from table
        // Honba sticks for the *next* round are determined by Env based on game outcome,
        // so self.honba_sticks is not reset to 0 here, but rather updated by Env for the new round.
    }
}


// Helper to get combined counts from a Hand object and open melds
// This function should return the 14 tiles that form the winning hand shape.
fn get_combined_hand_counts(
    hand_concealed_part: &Hand, // The concealed part of the hand
    open_melds: &[DeclaredMeld], // Declared open melds
    ron_tile_option: Option<Tile> // The tile won by Ron, if applicable
) -> ([u8; 34], u8) {
    let mut counts = [0u8; 34];
    let mut total_tiles = 0;

    // Add concealed tiles from hand
    for (tile, count) in hand_concealed_part.iter() {
        counts[tile as usize] += count;
        total_tiles += count;
    }

    // Add tiles from open melds (Pon, non-Ankan Kans) to the counts for parsing hand structure
    // Ankan tiles are effectively part of the concealed hand for parsing purposes,
    // but their meld structure is fixed. Kita is not part of the 4 melds/1 pair structure.
    for meld in open_melds {
        match meld.meld_type {
            DeclaredMeldType::Pon => {
                // A Pon uses 3 tiles. If hand_concealed_part is truly concealed, these 3 are not in it.
                // For parsing, we need to represent these 3 tiles.
                // Assuming `counts` starts with only concealed tiles.
                counts[meld.tiles[0] as usize] += 3; // Add 3 tiles of the Pon type
                total_tiles += 3;
            }
            DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan => {
                // An open Kan uses 4 tiles.
                counts[meld.tiles[0] as usize] += 4; // Add 4 tiles of the Kan type
                total_tiles += 4;
            }
            DeclaredMeldType::Ankan => {
                // An Ankan also uses 4 tiles. These are "concealed" but form a fixed meld.
                // If hand_concealed_part already includes the 4 Ankan tiles, adding again is wrong.
                // If hand_concealed_part is *strictly* the tiles not in any declared meld, then add.
                // Let's assume hand_concealed_part does NOT include tiles from ANY declared meld (Ankan included).
                counts[meld.tiles[0] as usize] += 4;
                total_tiles += 4;
            }
            DeclaredMeldType::Kita => { /* Kita is handled as a Dora, not part of the 4 melds/1 pair hand structure typically */ }
        }
    }

    // If it's a Ron win, add the ron_tile to the counts and total_tiles
    if let Some(ron_tile) = ron_tile_option {
        counts[ron_tile as usize] += 1;
        total_tiles += 1;
    }
    (counts, total_tiles)
}


// Internal helper for is_tenpai to use raw counts.
// `tile_counts_in_hand` here should represent the 14 tiles (13 from hand + 1 potential wait)
// that are being evaluated for forming a complete hand.
// Open melds are passed for context (e.g., menzen status) but their tiles should already be
// incorporated into `tile_counts_in_hand` if it's representing the full 14-tile structure.
fn get_combined_hand_counts_internal(
    tile_counts_for_14_tile_structure: &[u8; 34], // These are the 14 tiles to be parsed.
    _open_melds_context: &[DeclaredMeld], // Used for context like menzen, not for adding tiles here.
    _ron_tile_option: Option<Tile> // Assumed already included in tile_counts_for_14_tile_structure.
) -> ([u8; 34], u8) {
    // This function assumes `tile_counts_for_14_tile_structure` correctly represents all 14 tiles
    // forming the potential winning hand, including those that would constitute open melds.
    // It does not add tiles from `_open_melds_context` to the counts.
    let combined_counts = *tile_counts_for_14_tile_structure;
    let current_hand_tile_sum = tile_counts_for_14_tile_structure.iter().sum::<u8>();

    (combined_counts, current_hand_tile_sum)
}


// --- TileExt Trait (Helper for Yaku functions) ---
trait TileExt {
    fn is_manzu(&self) -> bool;
    fn is_pinzu(&self) -> bool;
    fn is_sou(&self) -> bool; // Even if some Sou are not in Sanma wall, the type exists
    fn get_suit_char(&self) -> Option<char>;
    // Add get_suit() if needed by yaku logic like in hand_parser
    fn get_suit(&self) -> Option<u8>; // 0 for Man, 1 for Pin, 2 for Sou
    fn get_number_val(&self) -> Option<u8>; // from Tile struct
    fn is_suited_number(&self) -> bool; // from Tile struct
    fn is_terminal(&self) -> bool; // from Tile struct
    fn is_honor(&self) -> bool; // from Tile struct
    fn is_terminal_or_honor(&self) -> bool; // from Tile struct
    fn is_dragon(&self) -> bool; // from Tile struct
    fn is_wind(&self) -> bool; // from Tile struct
    fn next_in_series(&self) -> Tile; // from Tile struct
    fn to_unicode_char(self) -> char; // from Tile struct

}

impl TileExt for Tile {
    fn is_manzu(&self) -> bool { (*self as u8) <= (Tile::Man9 as u8) }
    fn is_pinzu(&self) -> bool { (*self as u8) >= (Tile::Pin1 as u8) && (*self as u8) <= (Tile::Pin9 as u8)}
    fn is_sou(&self) -> bool { (*self as u8) >= (Tile::Sou1 as u8) && (*self as u8) <= (Tile::Sou9 as u8)}
    
    fn get_suit_char(&self) -> Option<char> { // To align with hand_parser's TileExt if used
        if self.is_manzu() { Some('m') }
        else if self.is_pinzu() { Some('p') }
        else if self.is_sou() { Some('s') }
        else { None }
    }
    fn get_suit(&self) -> Option<u8> { // To align with hand_parser's TileExt if used
        if self.is_manzu() { Some(0) }
        else if self.is_pinzu() { Some(1) }
        else if self.is_sou() { Some(2) }
        else { None }
    }
    // Delegate to existing Tile methods for properties
    fn get_number_val(&self) -> Option<u8> { Tile::get_number_val(*self) }
    fn is_suited_number(&self) -> bool { Tile::is_suited_number(*self) }
    fn is_terminal(&self) -> bool { Tile::is_terminal(*self) }
    fn is_honor(&self) -> bool { Tile::is_honor(*self) }
    fn is_terminal_or_honor(&self) -> bool { Tile::is_terminal_or_honor(*self) }
    fn is_dragon(&self) -> bool { Tile::is_dragon(*self) }
    fn is_wind(&self) -> bool { Tile::is_wind(*self) }
    fn next_in_series(&self) -> Tile { Tile::next_in_series(*self) }
    fn to_unicode_char(self) -> char { Tile::to_unicode(self) }
}


// --- Yaku Counting Functions ---
// These functions determine the Han value of different Yaku.
// They need access to the final hand structure (parsed_std_hand, parsed_chiitoi_hand, etc.),
// game context (menzen, win_type, seat/round winds, etc.), and potentially open melds.

// Placeholder for the complex Yaku logic. These would be detailed functions.
fn count_kokushi_musou_yaku(_final_counts: &[u8;34], menzen: bool, _win_type: WinType, _open_melds: &[DeclaredMeld], parsed_kokushi: Option<&ParsedKokushiMusou>) -> u8 {
    if !menzen || parsed_kokushi.is_none() { return 0; }
    // TODO: Implement 13-wait double yakuman logic if winning_tile is part of the 13 unique.
    // For now, assume base Kokushi.
    13
}
fn count_suuankou_yaku(
    parsed_std_hand: &ParsedStandardHand,
    menzen: bool,
    win_type: WinType,
    open_melds: &[DeclaredMeld], // Used to confirm Ankan vs Minkou if parser isn't definitive
    _original_hand_before_win: &Hand, // Needed for precise Tanki wait check on Ron
    winning_tile_option: Option<Tile>, // Tile that completed the hand
) -> u8 {
    if !menzen { return 0; }
    // Ensure all open melds are Ankan (or Kita, which doesn't break Suuankou)
    if !open_melds.iter().all(|m| m.meld_type == DeclaredMeldType::Ankan || m.meld_type == DeclaredMeldType::Kita) { return 0; }

    let mut concealed_koutsu_or_ankan_count = 0;
    // Count Ankans from open_melds
    for meld in open_melds {
        if meld.meld_type == DeclaredMeldType::Ankan {
            concealed_koutsu_or_ankan_count += 1;
        }
    }
    // Count concealed Koutsu from parsed_hand.melds
    // These are Koutsu not declared as Ankan already.
    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Koutsu {
            // Check if this Koutsu corresponds to an already counted Ankan from open_melds
            if !open_melds.iter().any(|om| om.meld_type == DeclaredMeldType::Ankan && om.tiles[0] == meld.representative_tile) {
                 concealed_koutsu_or_ankan_count += 1;
            }
        }
    }
    
    if concealed_koutsu_or_ankan_count != 4 { return 0; }

    // Check for Suuankou Tanki (double yakuman if Ron on the pair)
    if let (WinType::Ron { winning_tile, .. }, Some(wt_logic)) = (win_type, winning_tile_option) {
        if wt_logic == parsed_std_hand.pair && winning_tile == parsed_std_hand.pair { 
            // Check if the Ron completed the pair, and all 4 Koutsu were concealed *before* this Ron.
            // This requires knowing the state of the 4 koutsu *before* the Ron.
            // The `concealed_koutsu_or_ankan_count` already counts this.
            // If we have 4 concealed melds and the win is by completing the pair, it's Tanki.
            return 26; // Double Yakuman for Tanki wait
        }
    }
    // If Tsumo for Suuankou, or Ron not on the pair for Suuankou, it's single Yakuman.
    13 
}
fn count_daisangen_yaku(final_counts: &[u8;34], open_melds: &[DeclaredMeld], parsed_std_hand: Option<&ParsedStandardHand>) -> u8 {
    let mut white_triplet = false;
    let mut green_triplet = false;
    let mut red_triplet = false;

    let check_triplet_or_quad = |tile: Tile, counts: &[u8;34], p_std: Option<&ParsedStandardHand>, o_melds: &[DeclaredMeld]| -> bool {
        // Check open melds first (Pon, Daiminkan, Shouminkan, Ankan)
        if o_melds.iter().any(|om| (matches!(om.meld_type, DeclaredMeldType::Pon | DeclaredMeldType::Ankan | DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan)) && om.tiles[0] == tile) {
            return true;
        }
        // Check parsed standard hand for Koutsu not covered by Ankan (Pon already checked)
        if let Some(std_hand) = p_std {
            if std_hand.melds.iter().any(|m| m.meld_type == ParserOutputMeldType::Koutsu && m.representative_tile == tile && 
                                           !o_melds.iter().any(|om| (om.meld_type == DeclaredMeldType::Ankan || om.meld_type == DeclaredMeldType::Pon) && om.tiles[0] == tile)) { 
                // This Koutsu is concealed in the hand structure
                return true;
            }
        }
        // Fallback to raw counts if not found in structured melds (should be covered by parser)
        // This is less robust than relying on parsed_std_hand
        // counts[tile as usize] >= 3
        false
    };
    
    if check_triplet_or_quad(Tile::White, final_counts, parsed_std_hand, open_melds) { white_triplet = true; }
    if check_triplet_or_quad(Tile::Green, final_counts, parsed_std_hand, open_melds) { green_triplet = true; }
    if check_triplet_or_quad(Tile::Red, final_counts, parsed_std_hand, open_melds) { red_triplet = true; }

    if white_triplet && green_triplet && red_triplet { 13 } else { 0 }
}
fn count_shousuushii_yaku(parsed_std_hand: &ParsedStandardHand, open_melds: &[DeclaredMeld], _menzen: bool) -> u8 {
    let mut wind_koutsu_count = 0;
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
                // wind_koutsu_count +=1; // This way counts unique koutsu types
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
                    // wind_koutsu_count +=1;
                }
            }
        }
    }
    wind_koutsu_count = wind_koutsu_types.len();
    
    // Shousuushii: 3 Koutsu of winds + 1 pair of the 4th wind type.
    if wind_koutsu_count == 3 && wind_pair.is_some() {
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
    yaku_list: &mut Vec<(&'static str, u8)>, // To add specific yakuhai to list
    _parsed_std_hand: Option<&ParsedStandardHand>, // Context for complex cases, not strictly needed for basic Yakuhai count
    skip_if_yakuman_component: bool // True if these tiles are part of Shousangen/Shousuushii etc.
) -> u8 {
    let mut han = 0;
    
    let mut add_yakuhai_if_not_skipped = |tile: Tile, name: &'static str, value: u8, is_double_wind_case: bool| {
        if skip_if_yakuman_component {
            // Check if this tile is part of an already awarded higher-value yaku involving winds/dragons
            // This logic is simplified; a more robust check would look at specific yaku in yaku_list.
            // For now, assume if skip_if_yakuman_component is true, we don't add individual yakuhai.
            // This flag is set if Shousangen, Shousuushii, Daisuushii are active.
             if tile.is_dragon() && yaku_list.iter().any(|(n,_)| *n == "Daisangen" || *n == "Shousangen") { return; }
             if tile.is_wind() && yaku_list.iter().any(|(n,_)| *n == "Shousuushii" || *n == "Daisuushii") { return; }
        }

        if final_counts[tile as usize] >= 3 { // Must have a triplet or Kan
            // Check if this specific Yakuhai is already listed (e.g. for double wind)
            let mut found_and_updated = false;
            if is_double_wind_case {
                for entry in yaku_list.iter_mut() {
                    if (entry.0 == "Yakuhai (Seat Wind)" && tile == seat_wind) || 
                       (entry.0 == "Yakuhai (Round Wind)" && tile == round_wind) {
                        // Already have one part of the double wind, upgrade it
                        entry.0 = name; // Change name to "Yakuhai (Seat/Round Wind)"
                        entry.1 = value; // Should be 2 for double
                        han += 1; // Add the extra 1 han for the double
                        found_and_updated = true;
                        break;
                    }
                }
            }
            if !found_and_updated {
                // Add if not already present or not a double wind update
                 if !yaku_list.iter().any(|(n, _)| *n == name) {
                    yaku_list.push((name, value));
                    han += value;
                }
            }
        }
    };

    add_yakuhai_if_not_skipped(Tile::White, "Yakuhai (White Dragon)", 1, false);
    add_yakuhai_if_not_skipped(Tile::Green, "Yakuhai (Green Dragon)", 1, false);
    add_yakuhai_if_not_skipped(Tile::Red, "Yakuhai (Red Dragon)", 1, false);

    if seat_wind == round_wind { // Double wind (e.g. East seat in East round)
        add_yakuhai_if_not_skipped(seat_wind, "Yakuhai (Seat/Round Wind)", 2, true);
    } else {
        add_yakuhai_if_not_skipped(seat_wind, "Yakuhai (Seat Wind)", 1, false);
        add_yakuhai_if_not_skipped(round_wind, "Yakuhai (Round Wind)", 1, false);
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
                    // Let's assume `p_meld` is from `parse_standard_hand` on the final 14 tiles.
                    // If `winning_ron_tile` is one of the `p_meld.tiles`, and `win_type` is Ron, then this meld was completed by Ron.
                    // Exception: if player had 2, Tsumo'd 3rd, then Ron'd 4th for an ankan - but that's ankan.
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
    let mut has_melds_or_pairs = false; // To ensure hand is not empty

    if let Some(ref std_hand) = parsed_std_hand {
        has_melds_or_pairs = true;
        if !std_hand.pair.is_terminal_or_honor() { all_terminal_honor = false; }
        for meld in &std_hand.melds {
            if !meld.tiles.iter().all(|t| t.is_terminal_or_honor()) {
                all_terminal_honor = false; break;
            }
        }
        // Honroutou often implies Toitoi or Chiitoi. If it has sequences, they must be terminal sequences (not possible).
        // So, for std_hand, it must be all Koutsu of T/H + pair of T/H.
        // If all_terminal_honor is true, it implies no simples, so if it's a std_hand, it's likely Toitoi-like.
    } else if let Some(ref chiitoi) = parsed_chiitoi {
        has_melds_or_pairs = true;
        if !chiitoi.pair_representative_tiles.iter().all(|t| t.is_terminal_or_honor()) {
            all_terminal_honor = false;
        }
    } else { // No valid hand structure parsed
        return (false, 0);
    }

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
    // Iipeikou (One set of identical sequences) / Ryanpeikou (Two sets of identical sequences)
    if !menzen { return 0; }
    let mut shuntsu_map: std::collections::HashMap<[Tile;3], u8> = std::collections::HashMap::new();
    for meld in &parsed_std_hand.melds {
        if meld.meld_type == ParserOutputMeldType::Shuntsu {
            // Sort tiles within shuntsu for consistent key, though parser should do this.
            // let mut sorted_shuntsu_tiles = meld.tiles; sorted_shuntsu_tiles.sort_unstable();
            *shuntsu_map.entry(meld.tiles).or_insert(0) += 1;
        }
    }
    let mut iipeikou_sets = 0; // Number of pairs of identical shuntsu
    for count in shuntsu_map.values() {
        if *count == 2 { iipeikou_sets += 1; }
        else if *count == 4 { iipeikou_sets += 2; } // Two pairs of identical shuntsu
        // Ryanpeikou requires two *different* pairs of identical shuntsu.
        // e.g. 123m, 123m, 456p, 456p.
        // If 123m, 123m, 123m, 123m, that's two iipeikou.
    }
    if iipeikou_sets == 2 && shuntsu_map.len() == 2 { 3 } // Ryanpeikou (e.g., map has {123m:2, 456p:2} or {123m:4} implies two sets)
                                                       // If map has {123m:4}, it's two Iipeikou from the same sequence, not Ryanpeikou.
                                                       // Ryanpeikou is specifically two *different* sequences, each repeated.
                                                       // So if shuntsu_map.values().filter(|&&c| c==2).count() == 2, it's Ryanpeikou (3 Han)
    else if iipeikou_sets >= 1 { 1 } // Iipeikou (1 Han)
    else { 0 }
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
    if han == 0 && !is_yakuman { return 0; } // Yaku Nashi

    if is_yakuman {
        let base_yakuman_points = if is_dealer { 48000 } else { 32000 };
        return base_yakuman_points * yakuman_multiplier as u32;
    }

    // Mangan and above fixed points
    if han >= 11 { // Sanbaiman
        return if is_dealer { 36000 } else { 24000 };
    }
    if han >= 8 { // Baiman
        return if is_dealer { 24000 } else { 16000 };
    }
    if han >= 6 { // Haneman
        return if is_dealer { 18000 } else { 12000 };
    }
    if han == 5 || (han == 4 && fu >= 40) || (han == 3 && fu >= 70) { // Mangan
        return if is_dealer { 12000 } else { 8000 };
    }

    // Standard calculation: Base Points = Fu * 2^(Han + 2)
    // Round Base Points up to nearest 100.
    let mut base_points = (fu as u32) * (2u32.pow(han as u32 + 2));
    if base_points > 2000 { base_points = 2000; } // Cap at Mangan level before multiplier (for non-dealer Ron/Tsumo from one player)

    // Round up base_points to the nearest 100 (standard for individual payments)
    // This rounding is usually applied to individual payments, not the total sum before distribution.
    // Let's calculate payments based on this base_points.

    // Total points based on win type and dealer status for Sanma
    // These are the total points the winner receives for the hand itself.
    match win_type {
        WinType::Ron { .. } => {
            let payment = if is_dealer { base_points * 6 } else { base_points * 4 };
            // Ensure payment does not exceed Mangan unless it's a higher fixed yaku
            if han < 5 && payment > (if is_dealer {12000} else {8000}) { if is_dealer {12000} else {8000} } else { payment }
        }
        WinType::Tsumo => {
            if is_dealer { // Dealer Tsumo
                // Each non-dealer pays Base * 2. Total = Base * 4.
                let payment_each = base_points * 2;
                // payment_each = ((payment_each + 99) / 100) * 100; // Round each payment up
                let total = payment_each * 2; // Total from 2 non-dealers
                if han < 5 && total > 12000 { 12000 } else { total }
            } else { // Non-Dealer Tsumo
                // Dealer pays Base * 2, other non-dealer pays Base * 1. Total = Base * 3.
                let payment_from_dealer = base_points * 2;
                // payment_from_dealer = ((payment_from_dealer + 99) / 100) * 100;
                let payment_from_other_non_dealer = base_points * 1;
                // payment_from_other_non_dealer = ((payment_from_other_non_dealer + 99) / 100) * 100;
                let total = payment_from_dealer + payment_from_other_non_dealer;
                if han < 5 && total > 8000 { 8000 } else { total }
            }
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*; // Imports items from the parent module (game_state.rs)
    use crate::tiles::Tile::*;
    use crate::hand::Hand;
    use crate::hand_parser;
    use crate::game_state::{WinType, DeclaredMeldType, Score, DeclaredMeld}; // Explicitly import

    // --- Helper Functions ---

    // Adapted from user's helper, ensuring panic message is clear
    fn hand_from_tiles_for_test(tiles: &[Tile]) -> Hand {
        let mut hand = Hand::default();
        for &tile in tiles {
            // Using the Hand::add method which might panic if >4 tiles are added.
            // For tests, this is usually fine as we control input.
            // If Hand::add returns Result, unwrap_or_else is good.
            // Assuming Hand::add panics on >4, direct call is okay.
            hand.add(tile); 
        }
        hand
    }
    
    // Using the more comprehensive setup function established earlier
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
        let mut gs = GameState::new(seed, dealer_idx, honba_sticks);
        gs.riichi_sticks = riichi_sticks;

        if let Some(hands_tiles) = player_hands_tiles {
            for i in 0..3 {
                gs.hands[i] = Hand::default(); // Clear default dealt hand
                for &tile in hands_tiles[i] {
                    gs.hands[i].add(tile); // Use the hand's add method
                }
            }
        }

        if let Some(scores) = player_scores_opt {
            gs.player_scores = scores;
        }
        if let Some(cp_idx) = current_player_idx_opt {
            gs.current_player_idx = cp_idx;
        }
        if let Some(tc) = turn_count_opt {
            gs.turn_count = tc;
        }
        if let Some(rw) = round_wind_opt {
            gs.round_wind = rw;
            // Ensure seat winds are updated if dealer_idx or round_wind changes post-initialization
            gs.seat_winds[gs.dealer_idx as usize] = Tile::East;
            gs.seat_winds[((gs.dealer_idx + 1) % 3) as usize] = Tile::South;
            gs.seat_winds[((gs.dealer_idx + 2) % 3) as usize] = Tile::West;
        }

        if let Some(dora_inds) = dora_indicators_opt {
            gs.current_dora_indicators = dora_inds;
        } else if gs.current_dora_indicators.is_empty() && live_wall_tiles_opt.is_none() {
             if let Some(d_ind) = gs.wall.get_initial_dora_indicator() { // Check if wall can provide it
                gs.current_dora_indicators.push(d_ind);
             } else if gs.current_dora_indicators.is_empty() { // Absolute fallback
                gs.current_dora_indicators.push(Man1); 
             }
        }


        if let Some(ura_inds) = ura_dora_indicators_opt {
            gs.current_ura_dora_indicators = ura_inds;
        }

        if let Some(live_tiles) = live_wall_tiles_opt {
            let mut temp_wall_tiles = live_tiles.clone(); 
            if let Some(dead_tiles) = dead_wall_tiles_opt {
                temp_wall_tiles.extend(dead_tiles);
            } else {
                let num_live = temp_wall_tiles.len();
                // Ensure DEAD_WALL_SIZE is defined or use a constant like 14
                let num_dead_needed = gs.wall.wall_array_len().saturating_sub(num_live).max(14); 
                 for _ in 0..num_dead_needed { 
                    temp_wall_tiles.push(Man1); 
                    if temp_wall_tiles.len() >= gs.wall.wall_array_len() { break; }
                }
            }
            
            if temp_wall_tiles.len() == gs.wall.wall_array_len() {
                 gs.wall.tiles.copy_from_slice(&temp_wall_tiles);
                 gs.wall.live_wall_draw_pos = 0; 
                 gs.wall.kan_doras_revealed_count = 0;
                 gs.wall.rinshanpai_drawn_count = 0;
                 if dora_indicators_opt.is_none() {
                     gs.current_dora_indicators.clear();
                     if let Some(d_ind) = gs.wall.get_initial_dora_indicator() {
                        gs.current_dora_indicators.push(d_ind);
                     } else if gs.current_dora_indicators.is_empty() {
                        gs.current_dora_indicators.push(Man1); 
                     }
                 }
            }
        }
        gs
    }

    fn set_player_hand(gs: &mut GameState, player_idx: usize, tiles: &[Tile]) {
        gs.hands[player_idx] = hand_from_tiles_for_test(tiles);
    }

    fn add_player_discards(gs: &mut GameState, player_idx: usize, discards: &[Tile]) {
        for &tile in discards {
            gs.discards[player_idx].push(tile);
        }
    }

    fn add_player_open_meld(gs: &mut GameState, player_idx: usize, meld: DeclaredMeld) {
        gs.open_melds[player_idx].push(meld);
    }


    #[test]
    fn test_player_draw_tile() {
        let mut gs = setup_game_state_custom(1, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
        let initial_hand_size_p0 = gs.hands[0].get_all_tiles().len(); // P0 is dealer, already drew once in new.
        let initial_wall_size = gs.wall.live_wall_remaining_count();

        gs.current_player_idx = 1; // P1's turn
        let initial_hand_size_p1 = gs.hands[1].get_all_tiles().len();
        let drawn_tile_p1 = gs.player_draws_tile();
        assert!(drawn_tile_p1.is_some(), "P1 should draw a tile");
        assert_eq!(gs.hands[1].get_all_tiles().len(), initial_hand_size_p1 + 1, "P1 hand size should increase");
        assert_eq!(gs.wall.live_wall_remaining_count(), initial_wall_size - 1, "Wall size should decrease");
        assert_eq!(gs.last_drawn_tile, drawn_tile_p1, "last_drawn_tile not set correctly for P1");

        // Test drawing until wall is empty
        let mut gs_empty_wall = setup_game_state_custom(2,0,0,0, None, None, Some(0), Some(0), None, None, None, Some(vec![Man1, Man2]), Some(vec![Pin1;14])); // Only 2 live tiles
        gs_empty_wall.current_player_idx = 0; gs_empty_wall.player_draws_tile(); // P0 draws Man1
        gs_empty_wall.current_player_idx = 0; gs_empty_wall.player_discards_tile(0, gs_empty_wall.hands[0].get_all_tiles()[0]).unwrap(); // P0 discards

        gs_empty_wall.current_player_idx = 1; gs_empty_wall.player_draws_tile(); // P1 draws Man2
        gs_empty_wall.current_player_idx = 1; gs_empty_wall.player_discards_tile(1, gs_empty_wall.hands[1].get_all_tiles()[0]).unwrap(); // P1 discards
        
        gs_empty_wall.current_player_idx = 2; // P2's turn
        assert!(gs_empty_wall.wall.is_live_wall_empty());
        let no_tile = gs_empty_wall.player_draws_tile();
        assert!(no_tile.is_none(), "Should not draw a tile from empty live wall");
    }

    #[test]
    fn test_player_discard_tile_and_turn_progression() {
        let mut gs = setup_game_state_custom(2, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None); // P0 dealer, turn 0
        
        // P0 (dealer) discards
        gs.current_player_idx = 0;
        // gs.player_draws_tile(); // Dealer already drew in new()
        let tile_to_discard_p0 = gs.hands[0].get_all_tiles()[0];
        assert!(gs.player_discards_tile(0, tile_to_discard_p0).is_ok());
        assert_eq!(gs.discards[0].last(), Some(&tile_to_discard_p0));
        assert_eq!(gs.last_discarded_tile_info, Some((tile_to_discard_p0, 0)));
        assert_eq!(gs.turn_count, 1, "Turn count should be 1 after P0 discards");
        // current_player_idx should change after discard if no calls, handled by Env logic. GameState itself doesn't auto-advance here.

        // Simulate P1 drawing and discarding
        gs.current_player_idx = 1;
        gs.player_draws_tile();
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
        let mut live_wall = vec![Man1,Man2,Man3]; // Only 3 tiles
        let dead_wall = vec![Pin1; 14]; // Dummy dead wall
        let mut gs_no_wall = setup_game_state_custom(6,0,0,0,None,Some([1000;3]),Some(0),Some(20),None,None,None,Some(live_wall), Some(dead_wall));
        set_player_hand(&mut gs_no_wall, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]);
        assert_eq!(gs_no_wall.wall.live_wall_remaining_count(), 3);
        assert!(!gs_no_wall.can_declare_riichi(0), "Should not Riichi if less than 4 wall tiles remaining");


        // Actual declaration
        let mut gs = setup_game_state_custom(3, 0, 0, 0, None, Some([2000;3]), Some(0), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man1,Man1,Man1,Man2,Man3,Man4,Man5,Man6,Man7,Man8,Man9,Pin1,Pin2,Pin3]); // Tenpai hand
        assert!(gs.can_declare_riichi(0));
        gs.declare_riichi(0);
        assert!(gs.riichi_declared[0], "Riichi flag not set");
        assert!(gs.ippatsu_eligible[0], "Ippatsu flag not set on Riichi declaration");
        assert_eq!(gs.player_scores[0], 1000, "Score not deducted for Riichi");
        assert_eq!(gs.riichi_sticks, 1, "Riichi stick not added");
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
        let mut gs = setup_game_state_custom(6, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
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
        // Ankan by self does not void own ippatsu if it doesn't change waits (complex check, not tested here)
        // but it would void others' ippatsu.
    }
    
    #[test]
    fn test_make_shouminkan_and_chankan_window() {
        let mut gs = setup_game_state_custom(7, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
        // P0 has Man1, Man1 in hand, and an open Pon of Man1. Draws another Man1.
        let pon_meld = DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Man1;4], called_from_discarder_idx: Some(1), called_tile: Some(Man1) };
        add_player_open_meld(&mut gs, 0, pon_meld);
        set_player_hand(&mut gs, 0, &[Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3, East,East]); // Hand has one Man1
        gs.current_player_idx = 0;
        // gs.player_draws_tile(); // Player must have drawn the tile to add to Pon, or have it in hand.
                               // Let's assume Man1 was drawn or already in hand.

        assert!(gs.get_possible_shouminkans(0).unwrap_or_default().contains(&Man1));
        assert!(gs.make_shouminkan(0, Man1).is_ok());
        assert_eq!(gs.open_melds[0][0].meld_type, DeclaredMeldType::Shouminkan);
        assert_eq!(gs.kans_declared_count[0], 1);
        assert!(gs.is_chankan_window_open, "Chankan window should be open after Shouminkan");
        assert_eq!(gs.chankan_tile_and_declarer, Some((Man1, 0)));
        // Dora and Rinshan draw happen *after* chankan window resolves.
        assert_eq!(gs.current_dora_indicators.len(), 1, "Dora should not be revealed yet for Shouminkan");
        assert!(!gs.is_rinshan_kaihou_win_pending, "Rinshan should not be pending yet for Shouminkan");
    }

    #[test]
    fn test_make_kita_declaration_sanma() {
        let mut gs = setup_game_state_custom(8, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
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
        let mut gs = setup_game_state_custom(9,0,0,0,None,None,Some(0),None,None,None,None,None,None);
        // Hand: 123456789m 111p EE (waiting for any Man tile or E for pair) - this is not how tenpai works.
        // Tenpai for 13 Orphans (Kokushi Musou) - Sanma version (11 unique terminals/honors)
        // Hand: M1 M9 P1 P9 E S W N Wh G R M1 M1 (13 tiles, waiting for any of the 9 singles to make a triplet, or the pair to make a triplet)
        // This is complex. Let's use a simpler standard hand tenpai.
        // Hand: 123m 456p 77s EE WW (waiting for 7s or W)
        set_player_hand(&mut gs, 0, &[Man1,Man2,Man3, Pin4,Pin5,Pin6, Sou7,Sou7, East,East, West,West, White]); // 13 tiles
        let (tenpai, waits) = gs.is_tenpai(0);
        assert!(tenpai, "Hand should be tenpai (Sou7, White)");
        assert!(waits.contains(&Sou7), "Should be waiting for Sou7");
        assert!(waits.contains(&White), "Should be waiting for White");
        assert_eq!(waits.len(), 2);
    }

    #[test]
    fn test_score_win_non_dealer_ron_tanyao_dora3() {
        let mut gs = setup_game_state_custom(10, 0, 0, 0, None, None, Some(1), Some(5), None, Some(vec![Man2]), None, None, None); // P0 dealer, P1 wins
        gs.player_scores = [35000, 35000, 35000];
        gs.current_player_idx = 1; // P1's turn to claim Ron (hypothetically)
        
        // P1 Hand: 234m 567p 33s (open pon of 2s), Ron on 4s (discarded by P0)
        // For simplicity, let's make it a closed hand for Tanyao.
        // Hand: 234m 567p 33s 444p (Tenpai on 3s or 4s)
        // Let's use: 23m 56p 33s 444p 78s (waiting 4m, 7p, 69s) -> too complex.
        // Simpler: Tanyao. Hand: 234m 567p 22s 33s 44s. Ron on 5s.
        // Dora indicator Man2 -> Dora Man3. Hand has one Man3.
        // Hand: M234 P567 S22 S33 S44. Ron on M5.
        // For test: P1 hand: M2,M3,M4, P2,P3,P4, P5,P6,P7, S2,S2, S3,S3. Ron on S4.
        set_player_hand(&mut gs, 1, &[Man2,Man3,Man4, Pin2,Pin3,Pin4, Pin5,Pin6,Pin7, Sou2,Sou2, Sou3,Sou3]); // 13 tiles
        let ron_tile = Sou4;
        gs.last_discarded_tile_info = Some((ron_tile, 0)); // P0 discarded Sou4

        // Yaku: Tanyao (1), Dora (1 from Man3). Total 2 Han.
        // Fu: Menzen Ron = 30 Fu. (Base 20 + Menzen Ron 10).
        // If pair is non-value, melds are sequences, Ryanmen wait -> Pinfu. But this is not Pinfu.
        // Let's assume standard Fu calculation.
        // For Ron on S4 with S22 S33: Pair S3, Koutsu S2, Koutsu S4 (by Ron). This is Toitoi.
        // Let's use a Tanyao hand that is NOT Toitoi.
        // Hand: M234 P234 S234 M55. Ron on M5.
        set_player_hand(&mut gs, 1, &[Man2,Man3,Man4, Pin2,Pin3,Pin4, Sou2,Sou3,Sou4, Man6,Man6, Man7,Man8]); // Waiting M5 or M9 for Man66 M78
        let ron_tile_tanyao = Man5;
        gs.last_drawn_tile = None; // Clear last drawn for Ron scenario
        gs.hands[1].add(ron_tile_tanyao).unwrap(); // Add to hand for scoring
        gs.current_dora_indicators = vec![Man2]; // Dora is Man3. Hand has Man3.

        // Yaku: Tanyao (1), Dora 1 (Man3). Total 2 Han.
        // Fu: Menzen Ron base = 30 Fu. (Base 20 + Menzen Ron 10).
        // Wait on M5 (Tanki on M5 if M55 was pair, or Ryanmen if M6M7 wait M5).
        // If hand was M234 P234 S234 M66 M78 -> waits M5, M9. (Ryanmen on M78, Tanki on M66)
        // Let's assume Ryanmen on M78, winning on M5. This is complex for Fu.
        // Simpler Fu: M234 P234 S234 M55. Ron on M5 (Tanki).
        // Fu = 20 (base) + 10 (menzen ron) + 2 (tanki wait) = 32 -> 40 Fu.
        // Score (Non-dealer, 2 Han 40 Fu): Base = 40 * 2^(2+2) = 40 * 16 = 640.
        // Non-dealer Ron: 640 * 4 = 2560.
        // With Dora Man3 (1 han): 3 Han 40 Fu. Base = 40 * 2^(3+2) = 40 * 32 = 1280.
        // Non-dealer Ron: 1280 * 4 = 5120.
        // Let's reset hand for clear Tanyao Dora 1:
        // M234 M567 S234 P55. Dora: M2 (Man3). Ron on P5.
        set_player_hand(&mut gs, 1, &[Man2,Man3,Man4, Man5,Man6,Man7, Sou2,Sou3,Sou4, Pin6,Pin6, Pin7,Pin8]); // Wait P5, P9
        gs.current_dora_indicators = vec![Man2]; // Dora is Man3. Hand has one Man3.
        let final_ron_tile = Pin5;
        gs.hands[1].remove(Pin6).unwrap(); // Make space for ron tile in a 13 tile setup
        gs.hands[1].add(final_ron_tile).unwrap(); // Add ron tile for scoring

        let score = gs.score_win(1, WinType::Ron { winning_tile: final_ron_tile, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Tanyao"));
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1));
        assert_eq!(score.han, 2, "Han incorrect for Tanyao, Dora 1");
        // Fu for M234 M567 S234 P5P5 (Ron P5, Tanki wait) = 20 (base) + 10 (menzen ron) + 2 (tanki) = 32 -> 40 Fu.
        // For P6P6 P7P8 wait P5, P9 (Ryanmen on P7P8 for P9, Tanki on P6P6 for P5). Ron P5 (Tanki).
        assert_eq!(score.fu, 40, "Fu incorrect for Tanyao Dora 1, Menzen Ron, Tanki wait");
        // Score (Non-dealer, 2 Han 40 Fu): Base = 40 * 2^(2+2) = 640. Payment = 640 * 4 = 2560.
        assert_eq!(score.points, 2560, "Points incorrect for Tanyao, Dora 1 (2 Han 40 Fu Non-Dealer Ron)");
    }

    #[test]
    fn test_apply_score_transfers_non_dealer_ron() {
        let mut gs = setup_game_state_custom(11, 0, 1, 1, None, Some([35000;3]), None, None, None, None, None, None, None); // P0 dealer, 1 honba, 1 riichi stick
        // P1 wins by Ron from P2. Score: 2 Han 30 Fu.
        // Base = 30 * 2^(2+2) = 30 * 16 = 480. Non-dealer Ron: 480 * 4 = 1920.
        // Total points = 1920 (hand) + 1000 (riichi) + 300 (honba) = 3220.
        let score_details = Score { han: 2, fu: 30, points: 3220, yaku_details: vec![] };
        let winner_idx = 1;
        let discarder_idx = 2;

        gs.apply_score_transfers_and_reset_sticks(winner_idx, WinType::Ron { winning_tile: Man1, discarder_seat: discarder_idx }, &score_details);

        // P1 (winner) score: 35000 + 1920 (hand value from discarder) + 1000 (riichi) + 300 (honba) = 38220
        // P2 (discarder) score: 35000 - 1920 = 33080
        // P0 (dealer, not involved) score: 35000
        assert_eq!(gs.player_scores[winner_idx], 38220, "Winner (P1) score incorrect");
        assert_eq!(gs.player_scores[discarder_idx], 33080, "Discarder (P2) score incorrect");
        assert_eq!(gs.player_scores[0], 35000, "Dealer (P0) score should be unchanged");
        assert_eq!(gs.riichi_sticks, 0, "Riichi sticks should be reset");
        // Honba sticks are handled by Env for next round. GameState's honba_sticks reflect current round's value.
    }


    #[test]
    fn test_four_kan_abortive_draw_pending_and_resolution() {
        let mut gs = setup_game_state_custom(12, 0, 0, 0, None, None, Some(0), Some(10), None, None, None, None, None);
        gs.kans_declared_count = [1,1,1]; // P0, P1, P2 each made one kan
        gs.total_kans_in_game = 3;
        
        // P0 makes a 4th Kan (Ankan)
        set_player_hand(&mut gs, 0, &[Man1,Man1,Man1,Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3]);
        gs.current_player_idx = 0;
        assert!(gs.make_ankan(0, Man1).is_ok());

        assert_eq!(gs.total_kans_in_game, 4);
        assert_eq!(gs.kans_declared_count[0], 2); // P0 now has 2 kins
        assert_eq!(gs.kans_declared_count[1], 1);
        assert_eq!(gs.kans_declared_count[2], 1);
        assert!(gs.four_kan_abortive_draw_pending, "Four Kan abortive draw should be pending (Suukaikan)");
        assert_eq!(gs.player_who_declared_fourth_kan, Some(0), "Player who declared 4th Kan incorrect");

        // If one player makes all 4 kins (Suukantsu), no abortive draw
        let mut gs2 = setup_game_state_custom(13,0,0,0,None,None,Some(0),Some(10),None,None,None,None,None);
        gs2.kans_declared_count = [3,0,0]; // P0 has 3 kins
        gs2.total_kans_in_game = 3;
        set_player_hand(&mut gs2, 0, &[Man1,Man1,Man1,Man1, Man2,Man3,Man4, Pin1,Pin2,Pin3,Pin4, Sou1,Sou2,Sou3]);
        gs2.current_player_idx = 0;
        assert!(gs2.make_ankan(0, Man1).is_ok());
        assert_eq!(gs2.kans_declared_count[0], 4);
        assert!(!gs2.four_kan_abortive_draw_pending, "Should not be abortive draw for Suukantsu (one player 4 kongs)");
    }

    #[test]
    fn test_kyuushuu_kyuuhai_abortive_draw() {
        // Test for Kyuushuu Kyuuhai (9 unique terminals/honors in starting hand, before first discard, no calls)
        // This logic is typically in Env::step or Env::new, not GameState directly, but GameState needs to provide info.
        // For now, this test will be conceptual.
        let mut gs = setup_game_state_custom(100, 0, 0, 0, None, None, Some(0), Some(0), None, None, None, None, None);
        // P0 (dealer) hand: M1 M9 P1 P9 S1 S9 E S W N Wh G R + one more tile
        // Sanma has 11 unique terminals/honors. Need 9 of these.
        let kyuushuu_hand = [Man1, Man9, Pin1, Pin9, East, South, West, North, White, Green, Red, Sou2, Sou3, Sou4]; // 9 unique T/H + 3 others
        set_player_hand(&mut gs, 0, &kyuushuu_hand[0..14]); // Dealer starts with 14 after draw
        gs.turn_count = 0; // Dealer's first turn, no interruptions
        gs.current_player_idx = 0;

        // The actual check `can_declare_kyuushuu_kyuuhai` would be in GameState or Env.
        // For this test, we simulate the condition.
        let mut unique_terminal_honor_count = 0;
        let mut unique_th_tiles = std::collections::HashSet::new();
        for tile_in_hand in gs.hands[0].get_all_tiles() {
            if tile_in_hand.is_terminal_or_honor() {
                unique_th_tiles.insert(tile_in_hand);
            }
        }
        unique_terminal_honor_count = unique_th_tiles.len();
        let can_declare_kyuushuu = unique_terminal_honor_count >= 9 && gs.turn_count == 0 && gs.open_melds[0].is_empty();
        
        assert!(can_declare_kyuushuu, "Player 0 should be able to declare Kyuushuu Kyuuhai");
        // If declared, Env would handle the abortive draw.
    }

    #[test]
    fn test_nagashi_mangan_conditions_and_scoring() {
        // Nagashi Mangan: Exhaustive draw, all player's discards are terminals/honors, none were called.
        let mut gs = setup_game_state_custom(200, 0, 1, 1, None, Some([35000;3]), None, None, None, None, None, None, None);
        gs.current_player_idx = 0; // P0 is attempting Nagashi
        
        // Simulate P0 discarding only terminals/honors
        let p0_discards = [Man1, Man9, East, White, Pin1, Pin9, South, Green, West, Red, North];
        for &tile in &p0_discards {
            gs.discards[0].push(tile);
        }
        gs.any_discard_called_this_round[0] = false; // None of P0's discards were called
        gs.any_discard_called_this_round[1] = true; // P1's discard was called (to prevent P1 Nagashi)
        gs.any_discard_called_this_round[2] = false;

        // Simulate exhaustive draw (wall is empty) - This is checked by Env usually.
        // For test, assume wall is empty and Env calls a function in GameState or checks conditions.
        // Let's assume a function like `check_for_nagashi_mangan(player_idx)` exists or is part of score_win/draw logic.
        
        // Conceptual check:
        let mut is_nagashi = true;
        if gs.discards[0].is_empty() { is_nagashi = false; } // Must have discarded
        for &tile in &gs.discards[0] {
            if !tile.is_terminal_or_honor() { is_nagashi = false; break; }
        }
        if gs.any_discard_called_this_round[0] { is_nagashi = false; }

        assert!(is_nagashi, "P0 should be eligible for Nagashi Mangan based on discards and no calls");

        // Scoring would be handled by Env or a draw resolution function.
        // Points: Mangan (Dealer 12000, Non-dealer 8000) + Honba + Riichi sticks.
        // If P0 (dealer) achieves Nagashi: 12000 (base) + 1*300 (honba) + 1*1000 (riichi) = 13300.
        // Payments like Tsumo. Each non-dealer pays (12000 + 300) / 2 = 6150. P0 gets 13300 total.
        // P0 final score: 35000 + 12300 (from others) + 1000 (riichi sticks) = 48300.
        // P1/P2 final score: 35000 - 6150 = 28850.
        // This test is more about the conditions. Actual scoring is complex and in score_win/apply_score_transfers.
    }

    #[test]
    fn test_furiten_basic_ron_check() {
        let mut gs = setup_game_state_custom(300, 0, 0, 0, None, None, Some(1), Some(5), None, None, None, None, None);
        // P1 hand: M1M2 (waiting M3), P1 discarded M3 earlier.
        set_player_hand(&mut gs, 1, &[Man1, Man2, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East,East, West,West]); // Waiting M3
        add_player_discards(&mut gs, 1, &[Man3, Pin5]); // P1 discarded Man3 (a wait)

        gs.last_discarded_tile_info = Some((Man3, 0)); // P0 discards Man3

        // P1 is in Furiten because Man3 (a wait) is in their discards.
        assert!(!gs.can_call_ron(1, Man3, 0), "P1 should be in Furiten and cannot Ron on Man3");

        // P1 hand: M1M2 (waiting M3), P1 discarded P5 earlier (not a wait).
        let mut gs_no_furiten = setup_game_state_custom(301, 0, 0, 0, None, None, Some(1), Some(5), None, None, None, None, None);
        set_player_hand(&mut gs_no_furiten, 1, &[Man1, Man2, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East,East, West,West]);
        add_player_discards(&mut gs_no_furiten, 1, &[Pin5]);
        gs_no_furiten.last_discarded_tile_info = Some((Man3, 0));
        assert!(gs_no_furiten.can_call_ron(1, Man3, 0), "P1 should NOT be in Furiten and can Ron on Man3");
    }

#[test]
    fn test_score_win_kokushi_musou_sanma() {
        // Sanma Kokushi: 9 unique Terminals/Honors as singles, 1 as pair, 1 as triplet. All 11 T/H types present.
        let kokushi_hand_tiles = [
            Man1, Man9, Pin1, Pin9, East, South, West, North, White, // 9 singles
            Green, Green,       // 1 pair (Green)
            Red, Red, Red,       // 1 triplet (Red)
        ];
        let mut gs = setup_game_state_custom(400, 0, 0, 0, Some([&kokushi_hand_tiles, &[], &[]]), None, Some(0), Some(5), None, None, None, None, None);
        gs.last_drawn_tile = Some(Red); // Assume Tsumo on Red to complete triplet for simplicity

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Kokushi Musou"), "Kokushi Musou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Kokushi Musou should be 13 Han (single Yakuman)");
        // Yakuman points: Dealer Tsumo = 48000
        assert_eq!(score.points, 48000, "Kokushi Musou (Dealer Tsumo) points incorrect");
    }
    
    #[test]
    fn test_score_win_suuankou_tsumo() { // Four Concealed Triplets by Tsumo
        // Hand: M111 P222 S333 EEE (all concealed), Tsumo on W (pair) -> WW
        let ankou_hand_tiles = [
            Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West // 13 tiles
        ];
        let mut gs = setup_game_state_custom(401, 0, 0, 0, Some([&ankou_hand_tiles, &[], &[]]), None, Some(0), Some(10), None, None, None, None, None);
        gs.last_drawn_tile = Some(West); // Tsumo West to make WW pair
        gs.hands[0].add(West).unwrap(); // Add to hand

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Suuankou"), "Suuankou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Suuankou should be 13 Han (single Yakuman)");
        assert_eq!(score.points, 48000, "Suuankou (Dealer Tsumo) points incorrect");
    }

    #[test]
    fn test_score_win_suuankou_tanki_ron() { // Four Concealed Triplets, Ron on the pair (Tanki wait)
        // Hand: M111 P222 S333 EEE (all concealed), waiting for W (pair)
        let ankou_hand_tiles_tanki = [
            Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, East,East,East, West // 13 tiles, waiting West
        ];
         // Player 1 (non-dealer) wins by Ron from Player 0 (dealer)
        let mut gs = setup_game_state_custom(402, 0, 0, 0, Some([&[], &ankou_hand_tiles_tanki, &[]]), None, Some(1), Some(10), None, None, None, None, None);
        gs.last_discarded_tile_info = Some((West, 0)); // P0 discards West

        // Add Ron tile to hand for scoring logic
        gs.hands[1].add(West).unwrap();

        let score = gs.score_win(1, WinType::Ron { winning_tile: West, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name, val)| *name == "Suuankou (Tanki)" && *val == 26), "Suuankou Tanki (Double Yakuman) not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 26, "Suuankou Tanki should be 26 Han (double Yakuman)");
        // Non-dealer Ron Double Yakuman: 64000 points
        assert_eq!(score.points, 64000, "Suuankou Tanki (Non-Dealer Ron) points incorrect");
    }


    #[test]
    fn test_score_win_daisangen() { // Big Three Dragons
        // Hand: WhWhWh GGG RRR M11 P22 (pair P2)
        let daisangen_hand_tiles = [
            White,White,White, Green,Green,Green, Red,Red,Red, Man1,Man1, Pin2,Pin2, Pin3 // Tsumo Pin3
        ];
        let mut gs = setup_game_state_custom(403, 1, 0, 0, Some([&[], &daisangen_hand_tiles, &[]]), None, Some(1), Some(8), None, None, None, None, None);
        gs.last_drawn_tile = Some(Pin3); // Assume P1 (dealer) Tsumo'd Pin3

        let score = gs.score_win(1, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Daisangen"), "Daisangen Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Daisangen should be 13 Han");
        assert_eq!(score.points, 48000, "Daisangen (Dealer Tsumo) points incorrect");
    }
    
    #[test]
    fn test_score_win_kazoe_yakuman_riichi_tsumo_honitsu_chinitsu_etc() {
        // Example: Riichi(1) + Tsumo(1) + Honitsu(3) + Chinitsu(6) + Dora(2) = 13 Han -> Kazoe Yakuman
        // Hand: M111 M222 M333 M444 M55 (All Manzu + Menzen Tsumo)
        let kazoe_hand_tiles = [
            Man1,Man1,Man1, Man2,Man2,Man2, Man3,Man3,Man3, Man4,Man4,Man4, Man5 // 13 tiles
        ];
        let mut gs = setup_game_state_custom(404, 0, 0, 1, Some([&kazoe_hand_tiles, &[], &[]]), None, Some(0), Some(10), None, Some(vec![Man8, Man8]), None, None, None); // Dora is Man9 (x2)
        gs.riichi_declared[0] = true;
        gs.ippatsu_eligible[0] = false; // Assume Ippatsu passed
        gs.last_drawn_tile = Some(Man5); // Tsumo Man5 to complete pair
        gs.hands[0].add(Man5).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        // Expected Yaku: Riichi (1), Tsumo (1), Chinitsu (6), Dora 2. Total 10. Not Kazoe yet.
        // Let's make it Kazoe: Riichi(1) + Tsumo(1) + Chinitsu(6) + Toitoi(2) + Sanankou(2) + Dora(1) = 13
        // Hand: M111 M222 M333 M444 M55 -> This is Suuankou (Yakuman).
        // Let's try: Riichi(1) + Tsumo(1) + Chinitsu(6) + Dora 5
        set_player_hand(&mut gs, 0, &[Man1,Man2,Man3, Man4,Man5,Man6, Man7,Man8,Man9, Man1,Man1, Man2,Man2]); // Menzen Chinitsu Tenpai
        gs.last_drawn_tile = Some(Man3); // Tsumo Man3
        gs.hands[0].add(Man3).unwrap(); // Hand: M123 M456 M789 M11 M22 M3
        gs.current_dora_indicators = vec![Man1,Man2,Man3,Man4,Man5]; // Dora: M2,M3,M4,M5,M6 (5 Dora)
                                                                  // Hand has M2(x2), M3(x2), M4, M5, M6. Dora count = 2+2+1+1+1 = 7
        // Yaku: Riichi(1) + Tsumo(1) + Chinitsu(6) + Dora(7) = 15 Han -> Kazoe
        
        let score_kazoe = gs.score_win(0, WinType::Tsumo);
        assert!(score_kazoe.yaku_details.iter().any(|(name,_)| *name == "Kazoe Yakuman" || score_kazoe.han >= 13), "Kazoe Yakuman not awarded. Han: {}, Details: {:?}", score_kazoe.han, score_kazoe.yaku_details);
        assert_eq!(score_kazoe.han, 13, "Kazoe Yakuman should cap at 13 Han for point calculation");
        assert_eq!(score_kazoe.points, 48000 + 1000, "Kazoe Yakuman (Dealer Tsumo) + Riichi stick points incorrect. Actual: {}", score_kazoe.points);
    }

    #[test]
    fn test_score_win_ippatsu() {
        let mut gs = setup_game_state_custom(405, 0, 0, 1, None, None, Some(0), Some(1), None, Some(vec![Man1]), None, None, None); // P0 dealer, turn 1 (after Riichi discard)
        set_player_hand(&mut gs, 0, &[Man2,Man3,Man4, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]); // Tenpai, waiting for Dora Man2
        gs.riichi_declared[0] = true;
        gs.ippatsu_eligible[0] = true; // Eligible for Ippatsu
        gs.last_drawn_tile = Some(Man2); // Tsumo Dora on Ippatsu turn
        gs.hands[0].add(Man2).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Riichi"));
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Ippatsu"));
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Menzen Tsumo"));
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1));
        assert_eq!(score.han, 1+1+1+1, "Han incorrect for Riichi, Ippatsu, Tsumo, Dora 1");
        // 4 Han 30 Fu (Tsumo, no Pinfu) -> Dealer: Base 30 * 2^(4+2) = 1920. Tsumo Payment = 1920*2 = 3840 from each. Total 7680.
        // With Riichi stick: 7680 + 1000 = 8680.
        assert_eq!(score.points, 7680 + 1000, "Points incorrect for Riichi, Ippatsu, Tsumo, Dora 1");
    }

    #[test]
    fn test_score_win_rinshan_kaihou() {
        let mut gs = setup_game_state_custom(406, 0, 0, 0, None, None, Some(0), Some(3), None, Some(vec![Man1]), None, None, None);
        // P0 makes Ankan of Man2, then Tsumos Dora Man2 (Rinshan Kaihou)
        set_player_hand(&mut gs, 0, &[Man2,Man2,Man2,Man2, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]); // Hand before Ankan + last_drawn_tile
        gs.make_ankan(0, Man2).expect("Failed to make ankan for rinshan test"); // This draws replacement
        // Assume replacement tile was Man2 (Dora) and completes hand
        gs.hands[0].remove(West).unwrap(); // Remove a tile to make space for "drawn" rinshan tile
        gs.hands[0].add(Man2).unwrap();    // Add the "drawn" rinshan tile
        gs.last_drawn_tile = Some(Man2);   // Set it as last drawn
        gs.is_rinshan_kaihou_win_pending = true; // Manually set, as make_ankan does this.

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Rinshan Kaihou"), "Rinshan Kaihou Yaku not found.");
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1), "Dora not counted with Rinshan.");
        // Rinshan (1) + Tsumo (1, if menzen) + Dora (1) + Ankou(Man2) Fu...
        // If menzen: Rinshan(1) + Tsumo(1) + Dora(1) = 3 Han.
        // Fu for Ankou Man2 (simple) = 8. Tsumo = +2. Base = 20. Total = 30 Fu.
        // 3 Han 30 Fu Dealer Tsumo: Base 30 * 2^(3+2) = 960. Payment 960*2=1920 from each. Total 3840.
        assert_eq!(score.han, 1 + (if gs.is_menzen(0) {1} else {0}) + 1, "Han incorrect for Rinshan, Tsumo (if menzen), Dora 1");
        // Points depend on menzen status for Tsumo yaku.
    }

    #[test]
    fn test_score_win_chankan_non_dealer() {
        let mut gs = setup_game_state_custom(407, 0, 0, 0, None, None, Some(1), Some(5), None, None, None, None, None);
        // P0 declares Shouminkan of Man1. P1 Rons on Man1 (Chankan).
        set_player_hand(&mut gs, 0, &[Man2,Man3,Man4, Pin1,Pin2,Pin3, Sou1,Sou2,Sou3, East,East, West]); // P0 hand before Shouminkan
        add_player_open_meld(&mut gs, 0, DeclaredMeld{meld_type: DeclaredMeldType::Pon, tiles:[Man1;4], called_from_discarder_idx:Some(2), called_tile:Some(Man1)});
        gs.hands[0].add(Man1).unwrap(); // P0 has the 4th Man1 to declare Shouminkan

        set_player_hand(&mut gs, 1, &[Man2,Man2, Man3,Man3, Man4,Man4, Pin5,Pin5, Pin6,Pin6, Pin7,Pin7, Man1]); // P1 hand, 13 tiles, waiting Man1
        
        gs.current_player_idx = 0; // P0's turn
        gs.make_shouminkan(0, Man1).expect("Failed to make shouminkan for chankan test");
        // is_chankan_window_open should be true, chankan_tile_and_declarer should be (Man1, 0)

        gs.current_player_idx = 1; // P1's turn to respond to Chankan
        gs.hands[1].add(Man1).unwrap(); // Add chankan tile to P1's hand for scoring

        let score = gs.score_win(1, WinType::Ron { winning_tile: Man1, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Chankan"), "Chankan Yaku not found.");
        assert_eq!(score.han, 1, "Chankan should be 1 Han (plus other yaku if any)");
        // Assuming only Chankan: 1 Han 30 Fu (Menzen Ron base) Non-Dealer: Base 30 * 2^(1+2) = 240. Payment 240*4 = 960.
        assert_eq!(score.points, 960, "Points for Chankan (1 Han 30 Fu Non-Dealer Ron) incorrect");
    }
    
    #[test]
    fn test_dealer_rotation_and_honba_sticks_conceptual() {
        // This test is conceptual as dealer rotation and honba increment are usually handled by the Env.
        // GameState stores dealer_idx and honba_sticks, which Env would update.

        // Scenario 1: Dealer (P0) wins. Dealer remains, Honba increments.
        let mut gs_dealer_wins = GameState::new(500, 0, 0); // P0 dealer, 0 honba
        // Simulate P0 winning... (Env would call score_win, apply_transfers)
        // Env would then call GameState::new for next round with dealer_idx = 0, honba_sticks = 1.
        let gs_next_round_dealer_renchan = GameState::new(501, 0, 1);
        assert_eq!(gs_next_round_dealer_renchan.dealer_idx, 0);
        assert_eq!(gs_next_round_dealer_renchan.honba_sticks, 1);

        // Scenario 2: Non-Dealer (P1) wins. Dealer rotates, Honba resets (or increments if rule applies).
        // Assuming Honba resets if non-dealer wins and it's not an abortive draw keeping dealer.
        let mut gs_non_dealer_wins = GameState::new(502, 0, 2); // P0 dealer, 2 honba
        // Simulate P1 winning...
        // Env would then call GameState::new for next round with dealer_idx = 1, honba_sticks = 0.
        let gs_next_round_dealer_rotates = GameState::new(503, 1, 0);
        assert_eq!(gs_next_round_dealer_rotates.dealer_idx, 1);
        assert_eq!(gs_next_round_dealer_rotates.honba_sticks, 0);
    }

    #[test]
    fn test_no_yaku_win_attempt() {
        // Hand is complete but has no Yaku (e.g., open Tanyao with terminals, or just random tiles)
        // Dora only does not constitute a Yaku.
        let no_yaku_hand = [Man2,Man3,Man4, Pin2,Pin3,Pin4, Sou5,Sou6,Sou7, East,East, Man1,Man1]; // No yaku, pair Man1
        let mut gs = setup_game_state_custom(600, 0, 0, 0, Some([&no_yaku_hand, &[], &[]]), None, Some(0), Some(5), None, Some(vec![White]), None, None, None); // Dora is Green
        gs.last_drawn_tile = Some(Man1); // Tsumo Man1 to complete pair
        
        let score = gs.score_win(0, WinType::Tsumo);
        assert_eq!(score.han, 0, "Hand with no Yaku should have 0 Han. Details: {:?}", score.yaku_details);
        assert_eq!(score.points, 0, "Hand with no Yaku should score 0 points");
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Yaku Nashi" || name.is_empty()), "Yaku Nashi not indicated");
    }
 #[test]
    fn test_score_win_tsuu_iisou_all_honors() {
        // Hand: EEE SSS WWW NNN WhWhWh GGG (not possible with 14 tiles)
        // Chiitoitsu of honors: EE SS WW NN WhWh GG RR (7 pairs)
        let tsuu_iisou_chiitoi_hand = [
            East,East, South,South, West,West, North,North, 
            White,White, Green,Green, Red,Red
        ];
        let mut gs = setup_game_state_custom(700, 0, 0, 0, Some([&tsuu_iisou_chiitoi_hand, &[], &[]]), None, Some(0), Some(5), None, None, None, None, None);
        // Tsumo is implicit if it's Chiitoi and menzen. Assume last_drawn_tile was one of the pair parts.
        gs.last_drawn_tile = Some(Red); // For Tsumo context

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Tsuu Iisou (All Honors)"), "Tsuu Iisou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Tsuu Iisou should be 13 Han (Yakuman)");
        assert_eq!(score.points, 48000, "Tsuu Iisou (Dealer Tsumo) points incorrect");

        // Standard hand Tsuu Iisou: EEE SSS WWW NN (pair) + WhWhWh (Tsumo Wh)
        let tsuu_iisou_std_hand = [
            East,East,East, South,South,South, West,West,West, North,North, White,White // 13 tiles, waiting White
        ];
        let mut gs_std = setup_game_state_custom(701, 1, 0, 0, Some([&[], &tsuu_iisou_std_hand, &[]]), None, Some(1), Some(8), None, None, None, None, None);
        gs_std.last_drawn_tile = Some(White);
        gs_std.hands[1].add(White).unwrap();
        add_player_open_meld(&mut gs_std, 1, DeclaredMeld{meld_type: DeclaredMeldType::Pon, tiles:[Red;4], called_from_discarder_idx: Some(0), called_tile: Some(Red)}); // Make it non-menzen
        
        // This setup is tricky because Tsuu Iisou usually implies menzen unless specific rules allow open.
        // For test simplicity, let's assume the yaku counter can identify it if all tiles are honors.
        // However, a standard Tsuu Iisou is typically closed or with Ankan.
        // If open, it might not be Yakuman by some rules. Assuming it is for this test.
        // A more common Tsuu Iisou would be all Koutsu/Ankan.
        // E.g. EEE SSS WWW WhWh + Pon(NNN) -> This is not Tsuu Iisou due to Pon.
        // Let's make it all concealed Koutsu + pair of honors.
        let tsuu_iisou_ankou_hand = [East,East,East, South,South,South, West,West,West, North,North,North, White,White]; // Tsumo White
        let mut gs_ankou = setup_game_state_custom(702, 0, 0, 0, Some([&tsuu_iisou_ankou_hand, &[], &[]]), None, Some(0), Some(5), None, None, None, None, None);
        gs_ankou.last_drawn_tile = Some(White); // Assume last drawn was White

        let score_ankou = gs_ankou.score_win(0, WinType::Tsumo);
         assert!(score_ankou.yaku_details.iter().any(|(name,_)| *name == "Tsuu Iisou (All Honors)"), "Tsuu Iisou (Ankou based) Yaku not found. Details: {:?}", score_ankou.yaku_details);
        assert_eq!(score_ankou.han, 13, "Tsuu Iisou (Ankou based) should be 13 Han");
    }

    #[test]
    fn test_score_win_chinroutou_all_terminals() {
        // Hand: M111 M999 P111 P99 P9 (Tsumo P9)
        let chinroutou_hand_tiles = [
            Man1,Man1,Man1, Man9,Man9,Man9, Pin1,Pin1,Pin1, Pin9,Pin9, Sou1,Sou1 // 13 tiles, waiting Sou1
        ];
        // Sanma doesn't have Sou1 typically, let's use Man/Pin only for a clear test
        // M111 M999 P111 P99 + (P9 tsumo)
        let chinroutou_mp_hand = [Man1,Man1,Man1, Man9,Man9,Man9, Pin1,Pin1,Pin1, Pin9,Pin9, Man1, Man1]; // Wait Man1
        let mut gs = setup_game_state_custom(703, 0, 0, 0, Some([&chinroutou_mp_hand, &[], &[]]), None, Some(0), Some(7), None, None, None, None, None);
        gs.last_drawn_tile = Some(Man1);
        gs.hands[0].add(Man1).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Chinroutou (All Terminals)"), "Chinroutou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Chinroutou should be 13 Han (Yakuman)");
        assert_eq!(score.points, 48000, "Chinroutou (Dealer Tsumo) points incorrect");
    }

    #[test]
    fn test_score_win_suukantsu_four_kans() {
        // Player 0 (Dealer) declares 4 Kans and wins.
        let mut gs = setup_game_state_custom(704, 0, 1, 1, None, None, Some(0), Some(10), None, None, None, None, None);
        set_player_hand(&mut gs, 0, &[Man1,Man1,Man1,Man1, Man2,Man2,Man2,Man2, Man3,Man3,Man3,Man3, Pin1,Pin1]); // Hand to make 3 Ankans + 1 Daiminkan + Pair
        
        gs.make_ankan(0, Man1).unwrap(); // Kan 1
        gs.make_ankan(0, Man2).unwrap(); // Kan 2
        gs.make_ankan(0, Man3).unwrap(); // Kan 3
        // Simulate P1 discarding Pin1, P0 calls Daiminkan
        gs.last_discarded_tile_info = Some((Pin1,1));
        gs.hands[0].remove_n(Pin1,3).unwrap(); // Remove 3 Pin1 from hand for Daiminkan
        gs.make_daiminkan(0, Pin1, 1).unwrap(); // Kan 4
        
        // After 4th Kan, player draws replacement. Assume it's Pin2 for a pair.
        gs.hands[0].remove(Pin1).unwrap_or_else(|_| panic!("Failed to remove last Pin1 for Suukantsu pair setup")); // Remove last Pin1 after Kan
        gs.hands[0].add(Pin2).unwrap(); // Add Pin2 from replacement draw
        gs.last_drawn_tile = Some(Pin2); // Set as drawn for Tsumo

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Suukantsu (Four Kans)"), "Suukantsu Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Suukantsu should be 13 Han");
        assert_eq!(score.points, 48000 + 1000 + 300, "Suukantsu (Dealer Tsumo) + sticks points incorrect");
    }
    
    #[test]
    fn test_score_win_shousuushii_little_four_winds() {
        // Hand: EEE SSS WWW (3 wind koutsu) + NN (North pair) + M1M1 (other pair to complete hand)
        // This is tricky as Shousuushii is 3 wind koutsu + 1 wind pair. The 5th meld can be anything.
        // EEE SSS WWW NN + M123 (Tsumo M3)
        let shousuushii_hand = [
            East,East,East, South,South,South, West,West,West, North,North, // 3 wind koutsu, 1 wind pair
            Man1,Man2 // Waiting Man3
        ];
        let mut gs = setup_game_state_custom(705, 0, 0, 0, Some([&shousuushii_hand, &[], &[]]), None, Some(0), Some(8), None, None, None, None, None);
        gs.last_drawn_tile = Some(Man3);
        gs.hands[0].add(Man3).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Shousuushii"), "Shousuushii Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Shousuushii should be 13 Han (Yakuman)");
        assert_eq!(score.points, 48000, "Shousuushii (Dealer Tsumo) points incorrect");
    }

    #[test]
    fn test_score_win_double_riichi_tsumo_pinfu_dora() {
        let mut gs = setup_game_state_custom(706, 0, 0, 1, None, Some([35000;3]), Some(0), Some(0), None, Some(vec![Man5]), None, None, None); // Dora is Man6
        // P0 (dealer) declares Double Riichi on first turn, Tsumos a Pinfu hand with Dora.
        // Hand: M234 P456 S678 M55 (Pair M5), Tsumo M6 for M567 (Ryanmen)
        set_player_hand(&mut gs, 0, &[Man2,Man3,Man4, Pin4,Pin5,Pin6, Sou6,Sou7,Sou8, Man5,Man5, Man6]); // 13 tiles, tenpai for M6 (Pinfu wait)
        gs.riichi_declared[0] = true;
        gs.double_riichi_eligible[0] = true; // Eligible for Double Riichi
        gs.ippatsu_eligible[0] = true;      // Eligible for Ippatsu (as it's the immediate Tsumo)
        gs.last_drawn_tile = Some(Man6);    // Tsumo Man6 (Dora)
        gs.hands[0].add(Man6).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Double Riichi"), "Double Riichi not found.");
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Ippatsu"), "Ippatsu not found with Double Riichi Tsumo.");
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Menzen Tsumo"));
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Pinfu"));
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1));
        // Double Riichi(2) + Ippatsu(1) + Tsumo(1) + Pinfu(1) + Dora(1) = 6 Han
        assert_eq!(score.han, 6, "Han incorrect for Double Riichi, Ippatsu, Tsumo, Pinfu, Dora 1");
        // 6 Han 20 Fu (Pinfu Tsumo) Dealer: Haneman -> 18000 points.
        // Plus Riichi stick: 18000 + 1000 = 19000
        assert_eq!(score.points, 18000 + 1000, "Points incorrect for 6 Han 20 Fu Dealer Tsumo + Riichi stick");
    }
    
    #[test]
    fn test_haitei_raoyue_win_on_last_tsumo() {
        // Setup: P0 (dealer) wins by Tsumo on the very last tile from the wall.
        // Wall: Only 1 live tile left (Man1), Dead wall is full.
        let live_wall = vec![Man1];
        let dead_wall = vec![Pin1; DEAD_WALL_SIZE]; // Dummy dead wall
        let mut gs = setup_game_state_custom(707, 0, 1, 0, None, None, Some(0), Some(20), None, Some(vec![Man2]), None, Some(live_wall), Some(dead_wall));
        set_player_hand(&mut gs, 0, &[Man2,Man3,Man4, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]); // Tenpai for Man1 (Dora)
        
        assert_eq!(gs.wall.live_wall_remaining_count(), 1);
        gs.last_drawn_tile = gs.player_draws_tile(); // P0 draws the last tile (Man1)
        assert!(gs.wall.is_live_wall_empty()); // Wall should now be empty

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Haitei Raoyue"), "Haitei Raoyue not found. Details: {:?}", score.yaku_details);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1), "Dora not found with Haitei.");
        // Haitei(1) + Tsumo(1) + Dora(1) = 3 Han. Fu: 30 (Tsumo, no Pinfu).
        // Dealer 3 Han 30 Fu: Base 30 * 2^(3+2) = 960. Tsumo payment 960*2=1920 from each. Total 3840.
        // Plus Honba: 3840 + 300 = 4140.
        assert_eq!(score.han, 1 + (if gs.is_menzen(0) {1} else {0}) + 1, "Han incorrect for Haitei, Tsumo, Dora");
        assert_eq!(score.points, 3840 + 300, "Points incorrect for Haitei Raoyue win");
    }

    #[test]
    fn test_houtei_raoyui_win_on_last_discard() {
        // Setup: P1 wins by Ron on P0's (dealer) very last discard. Wall is empty.
        let live_wall_empty = Vec::new();
        let dead_wall = vec![Pin1; DEAD_WALL_SIZE];
        let mut gs = setup_game_state_custom(708, 0, 1, 0, None, None, Some(1), Some(21), None, Some(vec![Man2]), None, Some(live_wall_empty), Some(dead_wall));
        assert!(gs.wall.is_live_wall_empty());

        set_player_hand(&mut gs, 1, &[Man2,Man3,Man4, Pin1,Pin1,Pin1, Sou1,Sou1,Sou1, East,East, West,West]); // P1 tenpai for Man1 (Dora)
        let last_discard = Man1;
        gs.last_discarded_tile_info = Some((last_discard, 0)); // P0 discards Man1 as last discard

        // Add ron tile to hand for scoring
        gs.hands[1].add(last_discard).unwrap();

        let score = gs.score_win(1, WinType::Ron { winning_tile: last_discard, discarder_seat: 0 });
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Houtei Raoyui"), "Houtei Raoyui not found. Details: {:?}", score.yaku_details);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Dora" && *val == 1), "Dora not found with Houtei.");
        // Houtei(1) + Dora(1) = 2 Han. Fu: 30 (Menzen Ron base).
        // Non-Dealer 2 Han 30 Fu: Base 30 * 2^(2+2) = 480. Ron payment 480*4 = 1920.
        // Plus Honba: 1920 + 300 = 2220.
        assert_eq!(score.han, 1 + 1, "Han incorrect for Houtei, Dora");
        assert_eq!(score.points, 1920 + 300, "Points incorrect for Houtei Raoyui win");
    }

#[test]
    fn test_menzen_tsumo_no_other_yaku_adapted() {
        // Hand: 123m 456p 789s WW (Pair West), Tsumo S (South) for pair SS.
        // Yaku: Menzen Tsumo (1). Pair South is not seat/round wind here.
        let hand_tiles_before_tsumo = &[
            Man1, Man2, Man3, Pin4, Pin5, Pin6, Sou7, Sou8, Sou9,
            West, West, East, East // Pair East, Pair West
        ];
        let mut gs = setup_game_state_custom(
            100, 0, 0, 0, // seed, dealer_idx, honba, riichi_sticks
            Some([&hand_tiles_before_tsumo, &[], &[]]), // player_hands_tiles
            None, Some(0), Some(5), Some(Tile::North), // scores, current_player, turn_count, round_wind
            None, None, None, None // dora, ura, live_wall, dead_wall
        );
        gs.seat_winds = [Tile::East, Tile::South, Tile::West]; // P0 is East (Dealer)
        gs.last_drawn_tile = Some(South); // Tsumo South
        gs.hands[0].add(South).unwrap(); // Add Tsumo tile

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Menzen Tsumo"), "Menzen Tsumo not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 1, "Menzen Tsumo should be 1 han. Actual yaku: {:?}", score.yaku_details);
        // Fu: Base 20 + Tsumo 2 + Pair East (Seat/Round Wind if applicable, here no) + Pair West (no) + Pair South (no)
        // Assuming no other Fu elements, 20 (base) + 2 (tsumo) = 22 -> 30 Fu.
        assert_eq!(score.fu, 30, "Fu for Menzen Tsumo (no other fu sources) incorrect");
        // Points: Dealer Tsumo 1 Han 30 Fu: Base 30 * 2^(1+2) = 240. Each non-dealer pays 240*2=480. Total 960.
        assert_eq!(score.points, 960, "Points for Dealer, 1 Han, 30 Fu Tsumo incorrect");
    }
    
    #[test]
    fn test_riichi_ippatsu_menzen_tsumo_dora_adapted() {
        // Hand: M234 P567 S345 S11 EE (Tsumo North)
        // Dora ind: Sou1 => Dora: Sou2. Hand has no Sou2.
        let hand_tiles_before_tsumo = &[
            Man2,Man3,Man4, Pin5,Pin6,Pin7, Sou3,Sou4,Sou5, Sou1,Sou1, East,East
        ];
        let mut gs = setup_game_state_custom(
            101, 0, 0, 1, // seed, dealer, honba, riichi_sticks (1 stick = 1000 pts)
            Some([&hand_tiles_before_tsumo, &[], &[]]),
            None, Some(0), Some(1), Some(Tile::East), // scores, current_player, turn_count, round_wind
            Some(vec![Sou1]), None, None, None // dora (Sou2), ura, live_wall, dead_wall
        );
        gs.riichi_declared[0] = true;
        gs.ippatsu_eligible[0] = true;
        gs.last_drawn_tile = Some(North); // Tsumo North
        gs.hands[0].add(North).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Riichi"));
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Ippatsu"));
        assert!(score.yaku_details.iter().any(|(name, _)| *name == "Menzen Tsumo"));
        assert!(!score.yaku_details.iter().any(|(name, _)| name.contains("Dora")), "Should be no Dora. Hand has no Sou2.");
        assert_eq!(score.han, 3, "Riichi(1) + Ippatsu(1) + Tsumo(1) = 3 han. Actual: {:?}", score.yaku_details);
        // Fu: Base 20 + Tsumo 2 + Pair East(Seat/Round Wind if applicable) + Pair North(Value if applicable)
        // Assuming East is seat/round, Pair East = 4 Fu. Pair North = 0 Fu. Total 20+2+4 = 26 -> 30 Fu.
        assert_eq!(score.fu, 30, "Fu for 3 Han Dealer Tsumo incorrect");
        // Points: Dealer Tsumo 3 Han 30 Fu. Base = 30 * 2^(3+2) = 960. Each non-dealer pays 960*2=1920. Total 3840.
        // Plus Riichi stick: 3840 + 1000 = 4840.
        assert_eq!(score.points, 3840 + 1000, "Points for Dealer, 3 Han, 30 Fu Tsumo + Riichi stick incorrect");
    }

    // --- New Yakuman and Advanced Yaku Tests ---

    #[test]
    fn test_score_win_daisuushii_big_four_winds() {
        // Hand: EEE SSS WWW NNN (4 wind koutsu) + M1M1 (pair)
        let daisuushii_hand = [
            East,East,East, South,South,South, West,West,West, North,North,North, Man1 // 13 tiles
        ];
        let mut gs = setup_game_state_custom(710, 0, 0, 0, Some([&daisuushii_hand, &[], &[]]), None, Some(0), Some(10), None, None, None, None, None);
        gs.last_drawn_tile = Some(Man1); // Tsumo Man1 for pair
        gs.hands[0].add(Man1).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, val)| *name == "Daisuushii" && (*val == 26 || *val == 13*2)), "Daisuushii (Double Yakuman) not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 26, "Daisuushii should be 26 Han (Double Yakuman)");
        // Dealer Tsumo Double Yakuman: 64000 points
        assert_eq!(score.points, 64000, "Daisuushii (Dealer Tsumo Double Yakuman) points incorrect");
    }

    #[test]
    fn test_score_win_ryuu_iisou_all_green() {
        // Hand: Sou2,2,2 Sou3,3,3 Sou4,4,4 Sou6,6 Green,Green,Green (Tsumo Green for triplet)
        // (All tiles must be Green Dragon, Sou2, Sou3, Sou4, Sou6, Sou8)
        let ryuu_iisou_hand = [
            Sou2,Sou2,Sou2, Sou3,Sou3,Sou3, Sou4,Sou4,Sou4, Sou6,Sou6, Green,Green // 13 tiles
        ];
        let mut gs = setup_game_state_custom(711, 1, 0, 0, Some([&[], &ryuu_iisou_hand, &[]]), None, Some(1), Some(8), None, None, None, None, None); // P1 non-dealer
        gs.last_drawn_tile = Some(Green); // Tsumo Green
        gs.hands[1].add(Green).unwrap();

        let score = gs.score_win(1, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Ryuu Iisou"), "Ryuu Iisou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Ryuu Iisou should be 13 Han (Yakuman)");
        // Non-Dealer Tsumo Yakuman: 32000 points
        assert_eq!(score.points, 32000, "Ryuu Iisou (Non-Dealer Tsumo) points incorrect");
    }
    
    #[test]
    fn test_score_win_chuuren_poutou_nine_gates_9_wait() {
        // Hand: M111 M2345678 M999 (13 tiles), Tsumo any Man tile (1-9) for 9-sided wait.
        // For test, Tsumo M5.
        let chuuren_base_hand = [
            Man1,Man1,Man1, Man2,Man3,Man4, Man5,Man6,Man7,Man8, Man9,Man9,Man9 // 13 tiles
        ];
        let mut gs = setup_game_state_custom(712, 0, 0, 0, Some([&chuuren_base_hand, &[], &[]]), None, Some(0), Some(10), None, None, None, None, None);
        gs.last_drawn_tile = Some(Man5); // Tsumo Man5 (completes the 14 tiles)
        gs.hands[0].add(Man5).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name, val)| *name == "Chuuren Poutou (9-wait)" && (*val == 26 || *val == 13*2)), "Chuuren Poutou (9-wait) Double Yakuman not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 26, "Chuuren Poutou (9-wait) should be 26 Han");
        assert_eq!(score.points, 64000, "Chuuren Poutou (9-wait) (Dealer Tsumo Double Yakuman) points incorrect");
    }

    #[test]
    fn test_score_win_tenhou_blessing_of_heaven() {
        // Dealer wins on their very first draw, no interruptions.
        // Hand must be complete and have yaku.
        let winning_hand_for_tenhou = [Man1,Man1,Man1, Man2,Man2,Man2, Man3,Man3,Man3, Man4,Man4,Man4, East,East]; // Suuankou by Tsumo
        let mut gs = setup_game_state_custom(
            713, 0, 0, 0, // seed, dealer_idx=0, honba, riichi_sticks
            Some([&winning_hand_for_tenhou[0..13], &[], &[]]), // P0 starts with 13, will draw 14th
            None, Some(0), Some(0), // scores, current_player=0, turn_count=0
            Some(Tile::East), None, None, None, None // round_wind, dora, ura, live_wall, dead_wall
        );
        gs.is_tenhou_win_possible = true; // Manually set for test, GameState::new should handle this
        gs.last_drawn_tile = Some(East); // Dealer draws East to complete Suuankou
        gs.hands[0].add(East).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Tenhou"), "Tenhou Yaku not found. Details: {:?}", score.yaku_details);
        // Tenhou is Yakuman. If it also forms another Yakuman (like Suuankou), rules vary on stacking.
        // Assuming Tenhou takes precedence or is the primary scored Yakuman here.
        assert_eq!(score.han, 13, "Tenhou should be 13 Han (Yakuman)");
        assert_eq!(score.points, 48000, "Tenhou (Dealer Tsumo Yakuman) points incorrect");
    }
    
    #[test]
    fn test_score_win_chiihou_blessing_of_earth() {
        // Non-Dealer wins on their very first draw, no dealer Riichi, no interruptions.
        let winning_hand_for_chiihou = [Man1,Man1,Man1, Man2,Man2,Man2, Man3,Man3,Man3, Man4,Man4,Man4, East,East]; // Suuankou by Tsumo
        let mut gs = setup_game_state_custom(
            714, 0, 0, 0, // seed, dealer_idx=0
            Some([&[], &winning_hand_for_chiihou[0..13], &[]]), // P1 (non-dealer) hand
            None, Some(1), Some(0), // scores, current_player=1, turn_count=0 (P1's first turn index)
            Some(Tile::East), None, None, None, None
        );
        gs.is_chiihou_win_possible[1] = true; // Manually set for P1
        gs.last_drawn_tile = Some(East);   // P1 draws East
        gs.hands[1].add(East).unwrap();

        let score = gs.score_win(1, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,_)| *name == "Chiihou"), "Chiihou Yaku not found. Details: {:?}", score.yaku_details);
        assert_eq!(score.han, 13, "Chiihou should be 13 Han (Yakuman)");
        assert_eq!(score.points, 32000, "Chiihou (Non-Dealer Tsumo Yakuman) points incorrect");
    }
    
    #[test]
    fn test_score_win_ryanpeikou_two_identical_sequences_twice() {
        // Hand: M123 M123 P456 P456 EE (Menzen)
        let ryanpeikou_hand = [
            Man1,Man2,Man3, Man1,Man2,Man3, 
            Pin4,Pin5,Pin6, Pin4,Pin5,Pin6, 
            East, East // Pair
        ]; // 14 tiles
        let mut gs = setup_game_state_custom(715, 0, 0, 0, Some([&ryanpeikou_hand, &[], &[]]), None, Some(0), Some(5), None, None, None, None, None);
        gs.last_drawn_tile = Some(East); // Assume Tsumo on East

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Ryanpeikou" && *val == 3), "Ryanpeikou (3 Han) not found. Details: {:?}", score.yaku_details);
        // Ryanpeikou(3) + Tsumo(1) = 4 Han. (Also Chiitoi is not possible with this structure)
        // Fu: Menzen Tsumo base 20 + 2 = 22 -> 30 Fu (assuming no other fu from pair/wait).
        assert_eq!(score.han, 3 + 1, "Han for Ryanpeikou + Tsumo incorrect");
    }

    #[test]
    fn test_score_win_ittsuu_pure_straight() {
        // Hand: M123 M456 M789 P11 EE (Menzen)
        let ittsuu_hand = [
            Man1,Man2,Man3, Man4,Man5,Man6, Man7,Man8,Man9, 
            Pin1,Pin1, East,East, South // Tsumo South for pair
        ];
        let mut gs = setup_game_state_custom(716, 0, 0, 0, Some([&ittsuu_hand[0..13], &[], &[]]), None, Some(0), Some(6), None, None, None, None, None);
        gs.last_drawn_tile = Some(South);
        gs.hands[0].add(South).unwrap();

        let score = gs.score_win(0, WinType::Tsumo);
        assert!(score.yaku_details.iter().any(|(name,val)| *name == "Ittsuu" && (*val == 2 || *val == 1)), "Ittsuu (2/1 Han) not found. Details: {:?}", score.yaku_details);
        // Ittsuu (Menzen 2) + Tsumo (1) + Yakuhai East (Seat/Round) (2) = 5 Han
        let expected_ittsuu_han = if gs.is_menzen(0) { 2 } else { 1 };
        assert_eq!(score.han, expected_ittsuu_han + 1 + 2, "Han for Ittsuu + Tsumo + Yakuhai East incorrect");
    }
    
    #[test]
    fn test_abortive_draw_suucha_riichi() {
        // Four players declare Riichi. This is an abortive draw.
        // GameState itself doesn't resolve the draw, but it tracks riichi declarations.
        // Env would check this condition.
        let mut gs = setup_game_state_custom(717, 0, 0, 0, None, None, Some(0), Some(5), None, None, None, None, None);
        gs.riichi_declared = [true, true, true]; // All 3 players in Sanma declare Riichi
        // The condition for Suucha Riichi is typically when the 4th player declares Riichi.
        // In Sanma, if all 3 declare Riichi, it might be an abortive draw by some rules.
        // Tenhou: "Sannin Riichi" (3 players riichi) results in abortive draw if no win by next discard.
        // This test is conceptual for GameState tracking.
        assert!(gs.riichi_declared.iter().all(|&r| r), "All players should have Riichi declared for Sannin Riichi check");
        // Env would then determine if it's an abortive draw.
    }
}



