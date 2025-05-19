// src/hand_parser.rs

use crate::tiles::{Tile, TileExt}; // Use the actual Tile enum and TileExt trait from your project

/// Type of a parsed meld from a standard hand (sequence or triplet).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub enum ParsedMeldType {
    Shuntsu, // Sequence (e.g., 1-2-3 of a suit)
    Koutsu,  // Triplet (e.g., 7-7-7)
             // Kantsu (Quads) are typically handled by GameState as open/closed melds
             // before this parser is called on the remaining 14 tiles. If a Kantsu
             // is part of the 14 tiles, this parser might see it as a Koutsu + 1 extra tile.
}

/// Represents a single parsed meld within a standard hand.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct ParsedMeld {
    pub meld_type: ParsedMeldType,
    /// The tiles forming the meld. For Shuntsu, sorted low to high. For Koutsu, all same.
    pub tiles: [Tile; 3],
    /// Indicates if this meld was formed from the input `tile_counts`.
    /// `GameState` will use its `open_melds` to determine true concealed/open status for Yaku.
    pub is_concealed: bool,
    /// A representative tile for the meld (e.g., the lowest tile in a Shuntsu, or the tile itself for Koutsu).
    pub representative_tile: Tile,
}

/// Represents a fully parsed standard hand (4 melds and 1 pair).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedStandardHand {
    pub pair: Tile, 
    pub melds: Vec<ParsedMeld>, // Should contain 4 melds, sorted by representative_tile.
}

/// Represents a parsed Chiitoitsu (Seven Pairs) hand.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedChiitoitsu {
    /// Contains one tile from each of the seven unique pairs. Sorted for consistency.
    pub pair_representative_tiles: [Tile; 7],
}

/// Represents a parsed Kokushi Musou (Thirteen Orphans) hand for Sanma.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParsedKokushiMusou {
    /// The 11 unique Sanma terminal and honor tiles that form the hand. Sorted.
    pub unique_tiles: [Tile; 11],
    /// The tile that forms the conceptual "pair" in the "1 of each + pair" pattern.
    /// For the 14-tile structure (9 singles, 1 pair, 1 triplet), this would be the tile forming the pair.
    pub main_pair_tile: Tile,
    /// The tile that forms the triplet in the 14-tile structure.
    pub triplet_tile: Tile,
    // Note: For 11-sided wait (double yaku) determination, GameState would need to analyze
    // the hand state *before* the winning tile. This parser identifies the final structure.
}

/// Parses a set of 14 tiles (represented by counts) into a standard hand structure
/// (one pair and four melds).
/// Returns `Some(ParsedStandardHand)` or `None`.
pub fn parse_standard_hand(initial_tile_counts: &[u8; 34]) -> Option<ParsedStandardHand> {
    if initial_tile_counts.iter().sum::<u8>() != 14 {
        return None; 
    }

    let mut mutable_counts = *initial_tile_counts;

    for pair_idx in 0..34 { 
        if mutable_counts[pair_idx] >= 2 {
            let pair_tile = Tile::try_from(pair_idx as u8)
                .expect("Invalid tile index in pair search for parse_standard_hand");

            mutable_counts[pair_idx] -= 2; 

            if let Some(mut found_melds) = find_melds_recursive(&mut mutable_counts, 4) {
                found_melds.sort_unstable(); 
                return Some(ParsedStandardHand {
                    pair: pair_tile,
                    melds: found_melds,
                });
            }
            mutable_counts[pair_idx] += 2; // Backtrack
        }
    }
    None 
}

/// Recursive helper to find melds for `parse_standard_hand`.
fn find_melds_recursive(current_counts: &mut [u8; 34], melds_to_find: u8) -> Option<Vec<ParsedMeld>> {
    if melds_to_find == 0 {
        return if current_counts.iter().all(|&c| c == 0) { Some(Vec::new()) } else { None };
    }

    let start_idx = match current_counts.iter().position(|&count| count > 0) {
        Some(idx) => idx,
        None => return None, // No tiles left, but still need to find melds
    };

    // 1. Try to form a Koutsu (Triplet)
    if current_counts[start_idx] >= 3 {
        let meld_tile = Tile::try_from(start_idx as u8).expect("Invalid tile index for Koutsu");
        current_counts[start_idx] -= 3;

        if let Some(mut subsequent_melds) = find_melds_recursive(current_counts, melds_to_find - 1) {
            subsequent_melds.push(ParsedMeld {
                meld_type: ParsedMeldType::Koutsu,
                tiles: [meld_tile, meld_tile, meld_tile],
                is_concealed: true, 
                representative_tile: meld_tile,
            });
            return Some(subsequent_melds);
        }
        current_counts[start_idx] += 3; // Backtrack
    }

    // 2. Try to form a Shuntsu (Sequence)
    let t1_tile = Tile::try_from(start_idx as u8).expect("Invalid tile index for Shuntsu t1");
    if t1_tile.is_suited_number() { 
        if let Some(num_val) = t1_tile.get_number_val() {
            if num_val <= 7 { // Sequence can start with 1 through 7
                let t1_idx = start_idx;
                let t2_idx = start_idx + 1;
                let t3_idx = start_idx + 2;

                // Ensure t2_idx and t3_idx are valid tile indices
                if t3_idx < 34 { // Max tile ID is 33 (North)
                    if let (Some(t2_tile), Some(t3_tile)) = (Tile::try_from(t2_idx as u8).ok(), Tile::try_from(t3_idx as u8).ok()) {
                        // Check if t2 and t3 are in the same suit as t1
                        if t1_tile.get_suit() == t2_tile.get_suit() && t1_tile.get_suit() == t3_tile.get_suit() {
                            if current_counts[t1_idx] > 0 && current_counts[t2_idx] > 0 && current_counts[t3_idx] > 0 {
                                current_counts[t1_idx] -= 1;
                                current_counts[t2_idx] -= 1;
                                current_counts[t3_idx] -= 1;

                                if let Some(mut subsequent_melds) = find_melds_recursive(current_counts, melds_to_find - 1) {
                                    subsequent_melds.push(ParsedMeld {
                                        meld_type: ParsedMeldType::Shuntsu,
                                        tiles: [t1_tile, t2_tile, t3_tile],
                                        is_concealed: true,
                                        representative_tile: t1_tile, 
                                    });
                                    return Some(subsequent_melds);
                                }
                                current_counts[t1_idx] += 1;
                                current_counts[t2_idx] += 1;
                                current_counts[t3_idx] += 1; // Backtrack
                            }
                        }
                    }
                }
            }
        }
    }
    None 
}

/// Parses a set of 14 tiles for Chiitoitsu (Seven Pairs).
/// Returns `Some(ParsedChiitoitsu)` or `None`.
/// GameState should verify the hand is closed (no open melds other than Kita).
pub fn parse_chiitoitsu(tile_counts: &[u8; 34]) -> Option<ParsedChiitoitsu> {
    if tile_counts.iter().sum::<u8>() != 14 {
        return None;
    }

    let mut pairs_found = 0;
    let mut pair_rep_tiles_vec = Vec::with_capacity(7);

    for i in 0..34 {
        match tile_counts[i] {
            2 => {
                pairs_found += 1;
                pair_rep_tiles_vec.push(Tile::try_from(i as u8).expect("Invalid tile index for Chiitoitsu pair"));
            }
            0 => {} // No tiles of this type, continue
            _ => { 
                // Any count other than 0 or 2 (e.g., 1, 3, 4) invalidates standard Chiitoitsu.
                // Standard Riichi rules do not allow a quad (4 identical tiles) to be split into two pairs for Chiitoitsu.
                return None;
            }
        }
    }

    if pairs_found == 7 {
        // Ensure Vec has exactly 7 elements before trying to convert to array
        if pair_rep_tiles_vec.len() == 7 {
            let mut pair_tiles_arr = [Tile::Man1; 7]; // Placeholder, will be overwritten
            pair_rep_tiles_vec.sort_unstable(); // Sort for consistent output
            pair_tiles_arr.copy_from_slice(&pair_rep_tiles_vec);
            Some(ParsedChiitoitsu { pair_representative_tiles: pair_tiles_arr })
        } else {
             None // Should be unreachable if pairs_found is 7
        }
    } else {
        None
    }
}

/// Parses a 14-tile hand for Sanma Kokushi Musou (Thirteen Orphans variant for 3-player).
/// A Sanma Kokushi winning hand (14 tiles) must consist of:
/// - 9 of the 11 unique Sanma terminal/honor tiles as singles.
/// - 1 of the 11 unique Sanma terminal/honor tiles as a pair.
/// - 1 of the 11 unique Sanma terminal/honor tiles as a triplet.
/// All 11 Sanma T/H types must be represented. No other tile types allowed.
/// Returns `Some(ParsedKokushiMusou)` or `None`.
pub fn parse_kokushi_musou_sanma(tile_counts: &[u8; 34]) -> Option<ParsedKokushiMusou> {
    if tile_counts.iter().sum::<u8>() != 14 {
        return None;
    }

    let sanma_orphan_types: [Tile; 11] = [
        Tile::Man1, Tile::Man9, Tile::Pin1, Tile::Pin9,
        Tile::East, Tile::South, Tile::West, Tile::North,
        Tile::White, Tile::Green, Tile::Red,
    ];

    let mut counts_of_orphan_types = [0u8; 11];
    let mut present_orphan_types_vec = Vec::with_capacity(11);

    // Check if only orphan types are present and count them
    for i in 0..34 {
        if tile_counts[i] > 0 {
            let current_tile = Tile::try_from(i as u8).expect("Invalid tile index in Kokushi check");
            if let Some(orphan_idx) = sanma_orphan_types.iter().position(|&t| t == current_tile) {
                counts_of_orphan_types[orphan_idx] = tile_counts[i];
            } else {
                return None; // Contains a non-orphan tile
            }
        }
    }

    let mut single_count = 0;
    let mut pair_count = 0;
    let mut triplet_count = 0;
    let mut main_pair_tile_candidate: Option<Tile> = None;
    let mut triplet_tile_candidate: Option<Tile> = None;

    for (idx, &count) in counts_of_orphan_types.iter().enumerate() {
        if count > 0 {
            present_orphan_types_vec.push(sanma_orphan_types[idx]);
        }
        match count {
            1 => single_count += 1,
            2 => {
                pair_count += 1;
                main_pair_tile_candidate = Some(sanma_orphan_types[idx]);
            }
            3 => {
                triplet_count += 1;
                triplet_tile_candidate = Some(sanma_orphan_types[idx]);
            }
            _ if count > 3 => return None, // More than a triplet of an orphan type
            0 => {} // This orphan type is not present, which is only okay if we haven't found all 11 yet.
        }
    }

    // All 11 unique orphan types must be represented in the hand.
    if present_orphan_types_vec.len() != 11 {
        return None;
    }
    
    // Check for the specific Sanma Kokushi 14-tile structure: 9 singles, 1 pair, 1 triplet.
    if single_count == 9 && pair_count == 1 && triplet_count == 1 {
        if let (Some(p_tile), Some(t_tile)) = (main_pair_tile_candidate, triplet_tile_candidate) {
             // Ensure the pair and triplet tiles are different for this specific structure.
            if p_tile == t_tile { return None; }

            let mut unique_tiles_arr = [Tile::Man1; 11]; // Placeholder
            present_orphan_types_vec.sort_unstable();
            unique_tiles_arr.copy_from_slice(&present_orphan_types_vec);

            return Some(ParsedKokushiMusou {
                unique_tiles: unique_tiles_arr,
                main_pair_tile: p_tile,
                triplet_tile: t_tile,
            });
        }
    }
    
    None
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*; 

    fn get_counts(tiles: &[Tile]) -> [u8; 34] {
        let mut counts = [0u8; 34];
        for tile in tiles {
            counts[*tile as usize] += 1;
        }
        counts
    }

    #[test]
    fn test_std_parse_simple_koutsu_hand() {
        // Man1x3, Pin2x3, Sou3x3, Man4x3, Eastx2 (Pair East)
        // Note: Sou tiles are not in Sanma wall, but parser is generic.
        let hand_tiles = [Man1,Man1,Man1, Pin2,Pin2,Pin2, Sou3,Sou3,Sou3, Man4,Man4,Man4, East,East];
        let counts = get_counts(&hand_tiles);
        let parsed = parse_standard_hand(&counts);
        assert!(parsed.is_some());
        if let Some(p) = parsed { assert_eq!(p.pair, East); assert_eq!(p.melds.len(), 4); }
    }

    #[test]
    fn test_std_parse_simple_shuntsu_hand() {
        let hand_tiles = [Man1,Man2,Man3, Pin4,Pin5,Pin6, Sou7,Sou8,Sou9, Pin1,Pin2,Pin3, East,East];
        let counts = get_counts(&hand_tiles);
        let parsed = parse_standard_hand(&counts);
        assert!(parsed.is_some(), "Hand parsing failed for shuntsu hand. Counts: {:?}", counts);
        if let Some(p) = parsed { assert_eq!(p.pair, East); assert_eq!(p.melds.len(), 4); }
    }
    
    // --- Tests for parse_chiitoitsu ---
    #[test]
    fn test_parse_valid_chiitoitsu() {
        let hand_tiles = [Man1,Man1, Man9,Man9, Pin2,Pin2, Pin7,Pin7, East,East, South,South, White,White];
        let counts = get_counts(&hand_tiles);
        let parsed = parse_chiitoitsu(&counts);
        assert!(parsed.is_some());
        if let Some(p_chiitoi) = parsed {
            assert_eq!(p_chiitoi.pair_representative_tiles.len(), 7);
            let mut unique_check = p_chiitoi.pair_representative_tiles.to_vec();
            unique_check.sort_unstable(); // Already sorted by parser
            let original_len = unique_check.len();
            unique_check.dedup();
            assert_eq!(unique_check.len(), original_len, "Chiitoitsu pairs should be distinct types");
        }
    }

    #[test]
    fn test_parse_invalid_chiitoitsu_quad_standard_rules() {
        let hand_tiles = [Man1,Man1,Man1,Man1, Man9,Man9, Pin2,Pin2, Pin7,Pin7, East,East, South,South];
        let counts = get_counts(&hand_tiles);
        assert!(parse_chiitoitsu(&counts).is_none(), "Chiitoitsu should not allow a quad as two pairs in standard rules");
    }

    // --- Tests for parse_kokushi_musou_sanma (14-tile structure: 9 singles, 1 pair, 1 triplet from 11 Sanma T/H) ---
    #[test]
    fn test_parse_valid_sanma_kokushi_14_tiles() {
        // 9 singles: M9,P1,P9,S,W,N,Wh,G,R
        // 1 pair: East,East
        // 1 triplet: Man1,Man1,Man1
        // All 11 Sanma T/H types are represented.
        let hand_tiles = [
            Man9, Pin1, Pin9, South, West, North, White, Green, Red, // 9 singles
            East, East,             // 1 pair
            Man1, Man1, Man1,       // 1 triplet
        ];
        assert_eq!(hand_tiles.len(), 14, "Test hand should have 14 tiles for Kokushi parser");
        let counts = get_counts(&hand_tiles);
        let parsed = parse_kokushi_musou_sanma(&counts);
        assert!(parsed.is_some(), "Failed to parse valid Sanma Kokushi. Counts: {:?}", counts);
        if let Some(p_kokushi) = parsed {
            assert_eq!(p_kokushi.unique_tiles.len(), 11);
            assert_eq!(p_kokushi.main_pair_tile, East);
            assert_eq!(p_kokushi.triplet_tile, Man1);
        }
    }

    #[test]
    fn test_parse_invalid_sanma_kokushi_wrong_tile_type() {
        // Includes Man2 (not a Sanma orphan type)
        let hand_tiles = [
            Man2, Pin1, Pin9, South, West, North, White, Green, Red, 
            East, East,             
            Man1, Man1, Man1,       
        ];
        let counts = get_counts(&hand_tiles);
        assert!(parse_kokushi_musou_sanma(&counts).is_none());
    }

    #[test]
    fn test_parse_invalid_sanma_kokushi_not_all_11_types_present() {
        // Missing Red, has extra Man1 instead of the triplet structure
        let hand_tiles = [
            Man9, Pin1, Pin9, South, West, North, White, Green, /* Red missing */
            East, East,             
            Man1, Man1, Man1, Man1, // Man1 quad instead of triplet, and Red missing
        ];
        let counts = get_counts(&hand_tiles);
        assert!(parse_kokushi_musou_sanma(&counts).is_none());
    }

    #[test]
    fn test_parse_invalid_sanma_kokushi_wrong_counts_structure() {
        // All 11 types present, but not 9 singles, 1 pair, 1 triplet.
        // e.g., 8 singles, 3 pairs
        let hand_tiles = [
            Man9, Pin1, South, West, North, White, Green, Red, // 8 singles
            East, East,         // Pair 1
            Man1, Man1,         // Pair 2
            Pin9, Pin9,         // Pair 3
        ];
        let counts = get_counts(&hand_tiles);
        assert!(parse_kokushi_musou_sanma(&counts).is_none());
    }
     #[test]
    fn test_parse_another_valid_sanma_kokushi() {
        // Singles: M1,M9,P1,P9,E,S,W,N,Wh
        // Pair: Green, Green
        // Triplet: Red, Red, Red
        let hand_tiles = [
            Man1, Man9, Pin1, Pin9, East, South, West, North, White, // 9 singles
            Green, Green,       // Pair
            Red, Red, Red,       // Triplet
        ];
        assert_eq!(hand_tiles.len(), 14);
        let counts = get_counts(&hand_tiles);
        let parsed = parse_kokushi_musou_sanma(&counts);
        assert!(parsed.is_some(), "Failed for valid Kokushi with G pair, R triplet. Counts: {:?}", counts);
        if let Some(p_kokushi) = parsed {
            assert_eq!(p_kokushi.main_pair_tile, Green);
            assert_eq!(p_kokushi.triplet_tile, Red);
            // Check unique tiles contains all 11
             let expected_uniques = [Man1, Man9, Pin1, Pin9, East, South, West, North, White, Green, Red];
             let mut sorted_parsed_uniques = p_kokushi.unique_tiles.to_vec();
             sorted_parsed_uniques.sort_unstable();
             let mut sorted_expected_uniques = expected_uniques.to_vec();
             sorted_expected_uniques.sort_unstable();
             assert_eq!(sorted_parsed_uniques, sorted_expected_uniques, "Unique tiles mismatch");
        }
    }
}