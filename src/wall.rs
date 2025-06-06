// src/wall.rs
use rand::{rngs::StdRng, SeedableRng, seq::SliceRandom};
use crate::tiles::Tile;

// Standard Sanma includes Man 1&9, Pin 1-9, Sou 1-9, and Honors.
// Man (2 suits * 4 copies = 8 tiles for 1&9)
// Pin (9 suits * 4 copies = 36 tiles)
// Sou (9 suits * 4 copies = 36 tiles)
// Honors (East, South, West, North, White, Green, Red = 7 types * 4 copies = 28 tiles)
// Total tiles = 8 + 36 + 36 + 28 = 108 tiles.
const TOTAL_TILES_GENERATED: usize = 108;
const DEAD_WALL_SIZE: usize = 14;

// The `tiles` array in the Wall struct will hold all generated tiles.
const WALL_ARRAY_LEN: usize = TOTAL_TILES_GENERATED; // Size of the `self.tiles` array

// The live wall is the portion of WALL_ARRAY_LEN available for drawing after dead wall is set aside.
const LIVE_WALL_SIZE: usize = WALL_ARRAY_LEN - DEAD_WALL_SIZE; // 108 - 14 = 94

// Dead wall components are indexed from the start of the dead wall section of the `tiles` array.
// The dead wall occupies indices from LIVE_WALL_SIZE to WALL_ARRAY_LEN - 1.
// e.g., self.tiles[LIVE_WALL_SIZE] is the first tile of the dead wall section.

// Dora indicator is the first tile of the dead wall section (as per current indexing scheme).
// For Tenhou, the Dora is typically revealed from a specific position in the physical dead wall.
// This implementation uses a contiguous block at the end of the shuffled `tiles` array for the dead wall.
const DORA_INDICATOR_IDX: usize = LIVE_WALL_SIZE; // e.g., Index 94
const URA_DORA_FOR_MAIN_DORA_IDX: usize = DORA_INDICATOR_IDX + 1; // Index 95

const KAN_DORA_INDICATORS_START_IDX: usize = URA_DORA_FOR_MAIN_DORA_IDX + 1; // Index 96
const MAX_KAN_DORA_SETS: usize = 4; // Each set is (Kan Dora Ind, Ura Kan Dora Ind) = 2 tiles. Total 8 tiles.

// Rinshanpai are after all Dora/Ura indicators in the dead wall block.
// Total indicator tiles used so far in dead wall: 1 (Dora) + 1 (Ura) + MAX_KAN_DORA_SETS * 2 (KanDora+Ura) = 2 + 8 = 10 tiles.
// Indices used: DORA_INDICATOR_IDX to DORA_INDICATOR_IDX + 9.
const RINSHANPAI_START_IDX: usize = DORA_INDICATOR_IDX + 10; // e.g., Index 94 + 10 = 104
const NUM_RINSHANPAI: usize = 4; // Tiles at indices 104, 105, 106, 107.

// Check total dead wall size: 1 (Dora) + 1 (Ura) + (4 Kan Sets * 2 tiles/set) + 4 (Rinshanpai) = 1 + 1 + 8 + 4 = 14 tiles. Correct.

pub struct Wall {
    tiles: [Tile; WALL_ARRAY_LEN], // Now holds all 108 tiles
    live_wall_draw_pos: usize,
    kan_doras_revealed_count: u8,
    rinshanpai_drawn_count: u8,
}

impl Wall {
    pub fn new(seed: u64) -> Self {
        let mut deck: Vec<Tile> = Vec::with_capacity(TOTAL_TILES_GENERATED);
        for id in 0u8..=33 { // Iterate through all defined Tile IDs
            if let Ok(t) = Tile::try_from(id) {
                // Sanma rules: Keep Man 1 & 9, all Pin 1-9, all Sou 1-9, all Honors.
                // Remove Man 2 through Man 8.
                if matches!(t, Tile::Man2 | Tile::Man3 | Tile::Man4 | Tile::Man5 | Tile::Man6 | Tile::Man7 | Tile::Man8) {
                    continue; // Skip Man 2-8
                }
                // All other tiles (Man1, Man9, all Pins, all Sous, all Honors) are included.
                for _ in 0..4 {
                    deck.push(t);
                }
            }
        }
        assert_eq!(deck.len(), TOTAL_TILES_GENERATED,
            "Initial Sanma deck should be {} tiles. Actual: {}",
            TOTAL_TILES_GENERATED, deck.len()
        );

        let mut rng = StdRng::seed_from_u64(seed);
        deck.shuffle(&mut rng);

        // No longer truncating the deck to 70. We use all 108 tiles for the wall array.
        // deck.truncate(TOTAL_WALL_TILES); // This line is removed.

        let mut tiles_arr = [Tile::Man1; WALL_ARRAY_LEN]; // Array for 108 tiles
        tiles_arr.copy_from_slice(&deck); // deck is 108 tiles long

        Self {
            tiles: tiles_arr,
            live_wall_draw_pos: 0,
            kan_doras_revealed_count: 0,
            rinshanpai_drawn_count: 0,
        }
    }

    pub fn draw_from_live_wall(&mut self) -> Option<Tile> {
        if self.live_wall_draw_pos < LIVE_WALL_SIZE { // LIVE_WALL_SIZE is now 94
            let tile = self.tiles[self.live_wall_draw_pos];
            self.live_wall_draw_pos += 1;
            Some(tile)
        } else {
            None
        }
    }

    pub fn get_initial_dora_indicator(&self) -> Option<Tile> {
        // DORA_INDICATOR_IDX is now LIVE_WALL_SIZE (e.g., 94)
        if WALL_ARRAY_LEN > DORA_INDICATOR_IDX {
            Some(self.tiles[DORA_INDICATOR_IDX])
        } else {
            None // Should not happen if constants are correct
        }
    }

    pub fn get_current_ura_dora_indicators(&self) -> Vec<Tile> {
        let mut uras = Vec::new();
        // URA_DORA_FOR_MAIN_DORA_IDX is LIVE_WALL_SIZE + 1 (e.g., 95)
        if WALL_ARRAY_LEN > URA_DORA_FOR_MAIN_DORA_IDX {
            uras.push(self.tiles[URA_DORA_FOR_MAIN_DORA_IDX]);
        }
        for i in 0..self.kan_doras_revealed_count {
            // KAN_DORA_INDICATORS_START_IDX is LIVE_WALL_SIZE + 2 (e.g., 96)
            // Ura for Kan Dora i is at KAN_DORA_INDICATORS_START_IDX + (i * 2) + 1
            let ura_idx = KAN_DORA_INDICATORS_START_IDX + (i as usize * 2) + 1;
            if WALL_ARRAY_LEN > ura_idx {
                uras.push(self.tiles[ura_idx]);
            }
        }
        uras
    }

    pub fn reveal_new_kan_dora_indicator(&mut self) -> Option<Tile> {
        if self.kan_doras_revealed_count < MAX_KAN_DORA_SETS as u8 {
            // Kan Dora i indicator is at KAN_DORA_INDICATORS_START_IDX + (i * 2)
            let kan_dora_idx = KAN_DORA_INDICATORS_START_IDX + (self.kan_doras_revealed_count as usize * 2);
            if WALL_ARRAY_LEN > kan_dora_idx {
                let indicator = self.tiles[kan_dora_idx];
                self.kan_doras_revealed_count += 1;
                Some(indicator)
            } else { None }
        } else { None }
    }

    pub fn draw_replacement_tile(&mut self) -> Option<Tile> {
        if self.rinshanpai_drawn_count < NUM_RINSHANPAI as u8 {
            // RINSHANPAI_START_IDX is LIVE_WALL_SIZE + 10 (e.g., 104)
            let rinshan_idx = RINSHANPAI_START_IDX + self.rinshanpai_drawn_count as usize;
             if WALL_ARRAY_LEN > rinshan_idx {
                let tile = self.tiles[rinshan_idx];
                self.rinshanpai_drawn_count += 1;
                Some(tile)
            } else { None }
        } else { None }
    }

    pub fn live_wall_remaining_count(&self) -> usize {
        if self.live_wall_draw_pos <= LIVE_WALL_SIZE { // LIVE_WALL_SIZE is 94
            LIVE_WALL_SIZE - self.live_wall_draw_pos
        } else { 0 }
    }

    pub fn is_live_wall_empty(&self) -> bool {
        self.live_wall_draw_pos >= LIVE_WALL_SIZE
    }

    // Renamed for clarity, as it returns the total number of tiles in the wall array (108)
    pub fn wall_array_len(&self) -> usize {
        self.tiles.len()
    }

    // Added for debugging or external verification if needed
    pub fn rinshanpai_drawn_count(&self) -> u8 {
        self.rinshanpai_drawn_count
    }
}