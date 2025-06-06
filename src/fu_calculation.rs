// src/fu_calculation.rs

use crate::tiles::Tile;
use crate::hand_parser::{ParsedStandardHand, ParsedMeldType as HandParserMeldType};
use crate::game_state::{WinType, DeclaredMeld, DeclaredMeldType}; // For context from GameState

/// Contains all necessary information about the winning hand and game context for Fu calculation.
/// This struct should be prepared by `GameState::score_win` before calling `calculate_fu`.
#[derive(Debug)]
pub struct FuCalculationInput<'a> {
    /// The parsed structure of the winning hand (4 melds and 1 pair).
    /// These melds are derived from the final 14 tiles.
    pub parsed_hand: &'a ParsedStandardHand,
    
    /// Information about any open melds the player had. This is crucial for
    /// determining if a Koutsu in `parsed_hand` was actually a Minkou (Pon)
    /// or part of an open Kan (Minkan).
    pub open_melds_declared: &'a [DeclaredMeld],
    
    /// How the hand was won (Tsumo or Ron).
    pub win_type: WinType,
    
    /// The specific tile that completed the hand. Essential for wait pattern Fu.
    pub winning_tile: Tile,
    
    /// True if the hand was Menzen (closed) at the point of winning.
    /// For Ron, this means no open melds (except Ankan/Kita) before calling Ron.
    /// For Tsumo, this means the hand is generally Menzen.
    pub is_menzen_win: bool,
    
    pub seat_wind: Tile,
    pub round_wind: Tile,

    /// The player's hand object *before* the winning tile was added.
    /// This is OPTIONAL but VERY HELPFUL for unambiguously determining the wait pattern.
    /// If None, wait pattern detection will be an estimation based on the final 14 tiles.
    pub hand_before_win_completion: Option<&'a crate::hand::Hand>, // Using crate::hand::Hand path
}

/// Calculates Fu for a standard winning hand (4 melds, 1 pair).
/// IMPORTANT:
/// - This function should NOT be called for Yakuman hands (no Fu).
/// - This function should NOT be called for Chiitoitsu (fixed 25 Fu).
/// - If GameState determines the hand is Pinfu, it should use Pinfu's fixed Fu (20 Tsumo, 30 Ron)
///   and IGNORE the result of this function, as Pinfu's definition implies no certain Fu elements.
///
/// The calculated Fu is the sum of components, then rounded up to the nearest 10.
pub fn calculate_fu(input: &FuCalculationInput) -> u8 {
    let mut fu: u32 = 20; // 1. Base Fu (fūtei)

    // --- 2. Fu from Winning Method ---
    // This section assumes the hand is NOT Pinfu. Pinfu's Fu is handled by GameState.
    match input.win_type {
        WinType::Tsumo => {
            // Standard Tsumo (non-Pinfu) adds 2 fu.
            // Pinfu-Tsumo results in a total of 20 fu (no +2 for tsumo).
            // This function calculates as if not Pinfu. If it IS Pinfu, GameState overrides.
            fu += 2; 
        }
        WinType::Ron { .. } => {
            if input.is_menzen_win {
                fu += 10; // Menzen Ron (closed hand win by discard)
            }
        }
    }

    // --- 3. Fu from Melds (Koutsu/Kantsu) and Pair ---
    // We need to analyze each of the 4 melds and the pair from `input.parsed_hand`.
    // For each Koutsu in `parsed_hand.melds`, we must determine if it was:
    //   - Ankou (concealed triplet)
    //   - Minkou (open triplet, from a Pon)
    //   - Ankan (concealed quad) - should be in `open_melds_declared`
    //   - Minkan (open quad, from Daiminkan or Shouminkan) - should be in `open_melds_declared`

    // Create a helper structure to hold properties of the 4 final melds + pair
    struct FinalMeldInfo {
        tile: Tile, // Representative tile
        is_koutsu: bool, // True if Koutsu/Kantsu, false if Shuntsu
        is_kantsu: bool,
        is_open: bool, // True if Minkou or Minkan (not Ankou/Ankan)
        is_terminal_or_honor: bool,
    }

    let mut final_meld_infos: Vec<FinalMeldInfo> = Vec::with_capacity(4);

    // Process Kantsu first from `open_melds_declared` as they are explicit.
    let mut kantsu_tiles_processed: Vec<Tile> = Vec::new(); // To avoid double counting with parsed_hand

    for open_meld in input.open_melds_declared {
        let meld_tile = open_meld.tiles[0]; // Representative tile
        let is_toh = meld_tile.is_terminal_or_honor();
        match open_meld.meld_type {
            DeclaredMeldType::Ankan => {
                fu += if is_toh { 32 } else { 16 };
                kantsu_tiles_processed.push(meld_tile);
            }
            DeclaredMeldType::Daiminkan | DeclaredMeldType::Shouminkan => {
                fu += if is_toh { 16 } else { 8 };
                kantsu_tiles_processed.push(meld_tile);
            }
            _ => {} // Pon/Chi/Kita are handled differently or don't add fu here directly
        }
    }
    
    // Process melds from `parsed_hand` (these are from the 14-tile structure)
    for p_meld in &input.parsed_hand.melds {
        let rep_tile = p_meld.representative_tile;
        // Skip if this meld corresponds to an already processed Kan
        if kantsu_tiles_processed.contains(&rep_tile) && p_meld.meld_type == HandParserMeldType::Koutsu {
            continue; 
        }

        if p_meld.meld_type == HandParserMeldType::Koutsu {
            let is_toh = rep_tile.is_terminal_or_honor();
            // Was this Koutsu an open Pon?
            let is_minkou = input.open_melds_declared.iter().any(|om| {
                om.meld_type == DeclaredMeldType::Pon && om.tiles[0] == rep_tile
            });

            if is_minkou { // Minkou (Open Triplet from Pon)
                fu += if is_toh { 4 } else { 2 };
            } else { // Ankou (Concealed Triplet from hand)
                fu += if is_toh { 8 } else { 4 };
            }
        }
        // Shuntsu (sequences) do not add Fu.
    }

    // --- 4. Fu from Pair (jantō) ---
    let pair_tile = input.parsed_hand.pair;
    let is_pair_dragon = pair_tile.is_dragon();
    let is_pair_seat_wind = pair_tile == input.seat_wind;
    let is_pair_round_wind = pair_tile == input.round_wind;

    if is_pair_dragon { fu += 2; }
    // Add Fu for seat/round winds. Handle double wind pair carefully.
    if is_pair_seat_wind && is_pair_round_wind { // Pair is both seat and round wind
        fu += 4; // Common ruling: +4 fu for "ダブ東 (dabuton)" or "ダブ南 (dabunan)" pair
    } else {
        if is_pair_seat_wind && !is_pair_dragon { fu += 2; } // Seat wind (if not already counted as dragon)
        if is_pair_round_wind && !is_pair_dragon && !is_pair_seat_wind { fu += 2; } // Round wind (if not already dragon or seat)
    }


    // --- 5. Fu from Wait (machi) ---
    // This is complex and ideally uses the 13-tile tenpai state.
    // We attempt to infer from the final 14-tile `parsed_hand` and `winning_tile`.
    let winning_t = input.winning_tile;
    let mut wait_fu_added = false;

    // Check if winning tile completed the pair (Tanki wait)
    if winning_t == input.parsed_hand.pair {
        // Check if this Tanki wait is not part of a sequence completion for Pinfu
        // (Pinfu requires ryanmen, so Tanki is not Pinfu wait)
        fu += 2; // Tanki (pair wait)
        wait_fu_added = true;
    }

    // Check if winning tile completed one of the 4 melds
    if !wait_fu_added {
        for p_meld in &input.parsed_hand.melds {
            if p_meld.tiles.contains(&winning_t) { // Winning tile is part of this meld
                if p_meld.meld_type == HandParserMeldType::Shuntsu {
                    let t0 = p_meld.tiles[0];
                    let t1 = p_meld.tiles[1];
                    let t2 = p_meld.tiles[2];
                    
                    // Kanchan (closed interval wait, e.g., 4-6 waiting for 5)
                    if winning_t == t1 { // Winning tile is the middle of the sequence
                        fu += 2; wait_fu_added = true; break;
                    }
                    // Penchan (edge wait, e.g., 1-2 waiting for 3, or 8-9 waiting for 7)
                    if (winning_t == t2 && t0.get_number_val() == Some(1)) || // 1-2-W, won on W=3
                       (winning_t == t0 && t2.get_number_val() == Some(9)) {  // W-8-9, won on W=7
                        fu += 2; wait_fu_added = true; break;
                    }
                    // If it's a Shuntsu and not Kanchan or Penchan, it's Ryanmen (0 fu for wait)
                    // or a more complex wait that resolved into this Shuntsu.
                    // For simplicity, if it's a sequence completion and not clearly Kanchan/Penchan, assume 0 wait fu.
                    if !wait_fu_added { break; } // Assume Ryanmen or other 0-fu wait for this meld
                }
                // If winning_tile completes a Koutsu, it's often a Shanpon wait (0 fu for wait itself, Koutsu gives meld fu)
                // or was a Tanki on one of the pair that became a Koutsu (Tanki fu already handled if pair was the wait).
                // So, no additional wait fu if it completes a Koutsu here.
                break; // Found the meld the winning tile belongs to.
            }
        }
    }
    
    // --- 6. Rounding Up ---
    // Pinfu Tsumo (20 fu) and Chiitoitsu (25 fu) are exceptions and handled by GameState.
    // All other hands are rounded up to the nearest 10.
    if fu < 20 { fu = 20; } // Should not happen if base is 20, but as a safeguard.

    if fu % 10 != 0 {
        fu = ((fu + 9) / 10) * 10;
    }
    
    // Ensure minimums like 30 fu for non-Pinfu closed Ron.
    // If menzen ron and calculated fu < 30, it becomes 30. (Base 20 + MenzenRon 10 = 30)
    // If open ron and calculated fu < 20 (after rounding), it stays (e.g. 20).
    // This rounding logic should generally handle it.

    fu as u8
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*;
    use crate::hand_parser; // To call parse_standard_hand

    // Helper to create a ParsedStandardHand for testing FuInput
    fn create_test_parsed_hand(pair_tile: Tile, melds_data: Vec<(HandParserMeldType, [Tile;3])>) -> ParsedStandardHand {
        let melds = melds_data.into_iter().map(|(mt, tiles_arr)| {
            HandParserMeld {
                meld_type: mt,
                tiles: tiles_arr,
                is_concealed: true, // Assume concealed for parser output unless specified
                representative_tile: tiles_arr[0], // Simplification
            }
        }).collect();
        ParsedStandardHand { pair: pair_tile, melds }
    }
    
    #[test]
    fn test_base_fu_menzen_ron() {
        // Simplest hand: 123m 123p 123s 123m EE (Ron on E), all concealed, no other fu elements
        let parsed_melds_data = vec![
            (HandParserMeldType::Shuntsu, [Man1, Man2, Man3]),
            (HandParserMeldType::Shuntsu, [Pin1, Pin2, Pin3]),
            (HandParserMeldType::Shuntsu, [Sou1, Sou2, Sou3]), // Not in Sanma, but for generic test
            (HandParserMeldType::Shuntsu, [Man4, Man5, Man6]),
        ];
        let parsed_hand = create_test_parsed_hand(East, parsed_melds_data.clone());
        let input = FuCalculationInput {
            parsed_hand: &parsed_hand,
            open_melds_declared: &[],
            win_type: WinType::Ron { winning_tile: East, discarder_seat: 1 },
            winning_tile: East,
            is_menzen_win: true,
            seat_wind: East, round_wind: East,
            hand_before_win_completion: None,
        };
        // Base 20 + Menzen Ron 10 = 30 Fu. Pair is Yakuhai (Seat+Round) = +4. Total 34 -> 40 Fu.
        // Wait is Tanki on East = +2. Total 36 -> 40 Fu.
        // Let's make pair non-yakuhai for simpler base test: Pair Man7
        let parsed_hand_simple = create_test_parsed_hand(Man7, parsed_melds_data.clone());
         let input_simple = FuCalculationInput {
            parsed_hand: &parsed_hand_simple, open_melds_declared: &[],
            win_type: WinType::Ron { winning_tile: Man7, discarder_seat: 1 }, winning_tile: Man7,
            is_menzen_win: true, seat_wind: South, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + Menzen Ron 10 = 30. Pair Man7 (non-yakuhai) = 0. Wait Tanki on Man7 = +2. Total 32 -> 40 Fu.
        // The test above had Sou1,2,3 which are not in Sanma. Let's use Pin/Man only for a clean test.
        let parsed_melds_sanma = vec![
            (HandParserMeldType::Shuntsu, [Man1, Man2, Man3]),
            (HandParserMeldType::Shuntsu, [Pin1, Pin2, Pin3]),
            (HandParserMeldType::Shuntsu, [Man4, Man5, Man6]),
            (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]),
        ];
        let parsed_hand_sanma_simple = create_test_parsed_hand(Man7, parsed_melds_sanma);
        let input_sanma_simple = FuCalculationInput {
            parsed_hand: &parsed_hand_sanma_simple, open_melds_declared: &[],
            win_type: WinType::Ron { winning_tile: Man7, discarder_seat: 1 }, winning_tile: Man7,
            is_menzen_win: true, seat_wind: South, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + Menzen Ron 10 = 30. Pair Man7 (non-yakuhai) = 0. Wait Tanki on Man7 = +2. Total 32 -> 40 Fu.
        assert_eq!(calculate_fu(&input_sanma_simple), 40, "Menzen Ron, simple pair, Tanki wait");
    }

    #[test]
    fn test_tsumo_no_pinfu() {
        let parsed_melds_data = vec![
            (HandParserMeldType::Koutsu, [Man1, Man1, Man1]), // Ankou simples = 4 fu
            (HandParserMeldType::Shuntsu, [Pin1, Pin2, Pin3]),
            (HandParserMeldType::Shuntsu, [Man4, Man5, Man6]),
            (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]),
        ];
        let parsed_hand = create_test_parsed_hand(Man7, parsed_melds_data); // Pair Man7 (non-yakuhai)
        let input = FuCalculationInput {
            parsed_hand: &parsed_hand,
            open_melds_declared: &[],
            win_type: WinType::Tsumo,
            winning_tile: Man7, // Assume Tsumo on pair completion
            is_menzen_win: true, // Tsumo is menzen for this test
            seat_wind: South, round_wind: East,
            hand_before_win_completion: None,
        };
        // Base 20 + Tsumo 2 = 22. Ankou Man1 = +4. Pair Man7 = 0. Wait Tanki Man7 = +2.
        // Total = 22 + 4 + 2 = 28 -> 30 Fu.
        assert_eq!(calculate_fu(&input), 30, "Tsumo, 1 Ankou (simples), simple pair, Tanki wait");
    }

    #[test]
    fn test_yakuhai_pair_fu() {
        let parsed_melds_data = vec![ /* all sequences */
            (HandParserMeldType::Shuntsu, [Man1, Man2, Man3]), (HandParserMeldType::Shuntsu, [Pin1, Pin2, Pin3]),
            (HandParserMeldType::Shuntsu, [Man4, Man5, Man6]), (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]),
        ];
        // Pair is East, Seat is East, Round is East (DabuTon)
        let parsed_hand_dabuton = create_test_parsed_hand(East, parsed_melds_data.clone());
        let input_dabuton = FuCalculationInput {
            parsed_hand: &parsed_hand_dabuton, open_melds_declared: &[],
            win_type: WinType::Tsumo, winning_tile: East, is_menzen_win: true,
            seat_wind: East, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + Tsumo 2 = 22. DabuTon Pair East = +4. Wait Tanki East = +2. Total = 22+4+2 = 28 -> 30 Fu.
        assert_eq!(calculate_fu(&input_dabuton), 30, "DabuTon pair fu check");

        // Pair is White Dragon
        let parsed_hand_dragon = create_test_parsed_hand(White, parsed_melds_data.clone());
        let input_dragon = FuCalculationInput {
            parsed_hand: &parsed_hand_dragon, open_melds_declared: &[],
            win_type: WinType::Tsumo, winning_tile: White, is_menzen_win: true,
            seat_wind: South, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + Tsumo 2 = 22. Dragon Pair White = +2. Wait Tanki White = +2. Total = 22+2+2 = 26 -> 30 Fu.
        assert_eq!(calculate_fu(&input_dragon), 30, "Dragon pair fu check");
    }

    #[test]
    fn test_meld_fu_ankou_minkou() {
        // Hand: Ankou Man1 (terminal), Minkou Pin2 (simple, from Pon), Seq, Seq, Pair Sou7
        let ankou_meld = HandParserMeld { meld_type: HandParserMeldType::Koutsu, tiles: [Man1,Man1,Man1], is_concealed: true, representative_tile: Man1};
        let seq1_meld = HandParserMeld { meld_type: HandParserMeldType::Shuntsu, tiles: [Man4,Man5,Man6], is_concealed: true, representative_tile: Man4};
        let seq2_meld = HandParserMeld { meld_type: HandParserMeldType::Shuntsu, tiles: [Pin4,Pin5,Pin6], is_concealed: true, representative_tile: Pin4};
        // The Minkou (Pon) of Pin2 would be one of the 4 melds in ParsedStandardHand.
        // Let's say hand_parser identified it as a Koutsu [Pin2,Pin2,Pin2].
        let minkou_as_parsed_koutsu = HandParserMeld { meld_type: HandParserMeldType::Koutsu, tiles: [Pin2,Pin2,Pin2], is_concealed: true, representative_tile: Pin2};

        let parsed_hand = ParsedStandardHand {
            pair: Sou7, // Not in Sanma, use Man7
            // melds: vec![ankou_meld, minkou_as_parsed_koutsu, seq1_meld, seq2_meld],
            melds: vec![ankou_meld, minkou_as_parsed_koutsu, seq1_meld, HandParserMeld { meld_type: HandParserMeldType::Shuntsu, tiles: [Man7,Man8,Man9], is_concealed: true, representative_tile: Man7}],
        };
         let open_melds = vec![
            DeclaredMeld { meld_type: DeclaredMeldType::Pon, tiles: [Pin2,Pin2,Pin2,Pin2], called_from_discarder_idx: Some(1), called_tile: Some(Pin2)}
        ];
        let input = FuCalculationInput {
            parsed_hand: &parsed_hand,
            open_melds_declared: &open_melds,
            win_type: WinType::Ron { winning_tile: Man7, discarder_seat: 1 }, // Ron on pair
            winning_tile: Man7,
            is_menzen_win: false, // Has an open Pon
            seat_wind: East, round_wind: East,
            hand_before_win_completion: None,
        };
        // Base 20 (no menzen ron).
        // Ankou Man1 (terminal) = +8.
        // Minkou Pin2 (simple) = +2. (This requires matching logic to work correctly)
        // Pair Man7 (non-yakuhai) = 0.
        // Wait Tanki Man7 = +2.
        // Total = 20 + 8 + 2 + 2 = 32 -> 40 Fu.
        // The current fu calculation for melds needs refinement to correctly identify Minkou from ParsedHand + OpenMelds.
        // For this test, let's assume the logic correctly gives Minkou its fu.
        // If the test `calculate_fu` directly uses the `is_open_meld` logic based on `open_melds_declared`, it might work.
        // The current `calculate_fu` has a loop for `parsed_hand.melds` and then for `open_melds_declared` for Kantsu.
        // This test will stress the Koutsu part of `parsed_hand.melds`.
        // The `is_minkou` check inside `calculate_fu` should correctly identify the Pon of Pin2.

        // Let's trace `calculate_fu` for this input:
        // fu = 20 (base)
        // WinType::Ron, is_menzen_win = false -> no +10.
        // Loop `parsed_hand.melds`:
        //   1. ankou_meld (Man1,Man1,Man1 Koutsu): rep_tile=Man1. kantsu_tiles_processed is empty.
        //      is_toh=true. is_minkou=false (no Pon of Man1 in open_melds).
        //      It's Ankou: fu += 8. (fu = 20 + 8 = 28)
        //   2. minkou_as_parsed_koutsu (Pin2,Pin2,Pin2 Koutsu): rep_tile=Pin2.
        //      is_toh=false. is_minkou=true (Pon of Pin2 is in open_melds).
        //      It's Minkou: fu += 2. (fu = 28 + 2 = 30)
        //   3. seq1_meld (Shuntsu) -> no fu.
        //   4. seq_man789 (Shuntsu) -> no fu.
        // Loop `open_melds_declared` for Kantsu: only one Pon, no Kantsu -> no fu added here.
        // Pair Fu: Man7 (non-yakuhai) -> 0 fu.
        // Wait Fu: winning_tile = Man7 (pair completion) -> Tanki wait = +2. (fu = 30 + 2 = 32)
        // Rounding: 32 -> 40 fu.
        assert_eq!(calculate_fu(&input), 40, "Ankou (T/H) + Minkou (Simple) fu");
    }

    #[test]
    fn test_kan_fu() {
        // Hand: Ankan Man1 (terminal), Minkan Pin2 (simple), Seq, Pair Sou7
        // Let parsed_hand reflect the structure after kants are conceptually "absorbed"
        let seq1 = (HandParserMeldType::Shuntsu, [Man4, Man5, Man6]);
        let seq2 = (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]);
        // The Kantsu don't appear as Koutsu in parsed_hand if they are already open_melds.
        // So parsed_hand would only have 2 sequences and the pair if the other 2 melds are Kantsu.
        // This means FuCalculationInput needs to be robust to parsed_hand.melds.len() < 4 if Kants are present.
        // Or, `parse_standard_hand` must still find 4 "meld slots", where Kants fill some.
        // For now, assume parsed_hand always has 4 melds + 1 pair structure.
        // If Ankan of Man1 is made, it's an open_meld. hand_parser would see remaining tiles.
        // This highlights that FuCalculationInput needs careful construction by GameState.

        // Let's test Fu parts directly.
        // Assume a hand where these are the only Fu components besides base/win.
        let dummy_parsed_hand = create_test_parsed_hand(Man7, vec![
            seq1,
            seq2,
            (HandParserMeldType::Shuntsu, [Man7, Man8, Man9]),
            (HandParserMeldType::Shuntsu, [Pin7, Pin8, Pin9]),
        ]);
        let open_melds_with_kans = vec![
            DeclaredMeld { meld_type: DeclaredMeldType::Ankan, tiles: [Man1,Man1,Man1,Man1], called_from_discarder_idx: None, called_tile: None},
            DeclaredMeld { meld_type: DeclaredMeldType::Daiminkan, tiles: [Pin2,Pin2,Pin2,Pin2], called_from_discarder_idx: Some(1), called_tile: Some(Pin2)},
        ];
         let input = FuCalculationInput {
            parsed_hand: &dummy_parsed_hand, // This is not fully accurate for this test, but fu calc uses open_melds_declared for kans
            open_melds_declared: &open_melds_with_kans,
            win_type: WinType::Tsumo, winning_tile: Man7, is_menzen_win: false, // Open Minkan
            seat_wind: East, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + Tsumo 2 = 22.
        // Ankan Man1 (T/H) = +32.
        // Minkan Pin2 (Simple) = +8.
        // Pair Man7 (non-yakuhai) = 0.
        // Wait Tanki Man7 = +2.
        // Total = 22 + 32 + 8 + 2 = 64 -> 70 Fu.
        // The current fu calculation iterates `parsed_hand.melds` and then `open_melds_declared` for Kantsu.
        // This can lead to double counting if a Kan is also represented as a Koutsu in `parsed_hand`.
        // The `kantsu_tiles_processed` logic aims to prevent this.

        // Trace:
        // fu = 20 (base) + 2 (tsumo) = 22.
        // Loop parsed_hand.melds: all are Shuntsu -> no fu.
        // Loop open_melds_declared:
        //   Ankan Man1: is_toh=true. fu += 32. (fu = 22+32=54). kantsu_tiles_processed.push(Man1).
        //   Daiminkan Pin2: is_toh=false. fu += 8. (fu = 54+8=62). kantsu_tiles_processed.push(Pin2).
        // Pair Fu: Man7 (non-yakuhai) -> 0.
        // Wait Fu: Tanki on Man7 -> +2. (fu = 62+2=64).
        // Rounding: 64 -> 70.
        assert_eq!(calculate_fu(&input), 70, "Ankan (T/H) + Minkan (Simple) fu");
    }

     #[test]
    fn test_wait_fu_penchan_kanchan() {
        // Hand: 12m waiting on 3m (Penchan)
        let penchan_melds = vec![
            (HandParserMeldType::Shuntsu, [Man1, Man2, Man3]), // Completed by winning on Man3
            (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]),
            (HandParserMeldType::Shuntsu, [Man7, Man8, Man9]),
            (HandParserMeldType::Koutsu, [East, East, East]),
        ];
        let parsed_penchan = create_test_parsed_hand(South, penchan_melds);
        let input_penchan = FuCalculationInput {
            parsed_hand: &parsed_penchan, open_melds_declared: &[],
            win_type: WinType::Ron{winning_tile: Man3, discarder_seat: 1}, winning_tile: Man3,
            is_menzen_win: true, seat_wind: South, round_wind: East, hand_before_win_completion: None,
        };
        // Base 20 + MenzenRon 10 = 30. Koutsu East (seat) = +2. Pair South (seat) = +2. Penchan Man3 = +2.
        // Total = 30 + Ankou East (seat+round) +8. Pair South (seat) +2. Penchan +2.
        // Ankou East (seat+round) = +8. Pair South (seat) = +2.
        // Total = 30 (base+ron) + 8 (ankou East) + 2 (pair South) + 2 (penchan) = 42 -> 50 Fu.
        assert_eq!(calculate_fu(&input_penchan), 50, "Penchan wait fu");

        // Hand: 13m waiting on 2m (Kanchan)
        let kanchan_melds = vec![
            (HandParserMeldType::Shuntsu, [Man1, Man2, Man3]), // Completed by winning on Man2
            (HandParserMeldType::Shuntsu, [Pin4, Pin5, Pin6]),
            (HandParserMeldType::Shuntsu, [Man7, Man8, Man9]),
            (HandParserMeldType::Koutsu, [East, East, East]),
        ];
        let parsed_kanchan = create_test_parsed_hand(South, kanchan_melds);
        let input_kanchan = FuCalculationInput {
            parsed_hand: &parsed_kanchan, open_melds_declared: &[],
            win_type: WinType::Ron{winning_tile: Man2, discarder_seat: 1}, winning_tile: Man2,
            is_menzen_win: true, seat_wind: South, round_wind: East, hand_before_win_completion: None,
        };
        // Same Fu components as above, just different wait type. Total 42 -> 50 Fu.
        assert_eq!(calculate_fu(&input_kanchan), 50, "Kanchan wait fu");
    }
}