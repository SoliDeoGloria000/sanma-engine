// src/wall.rs
use rand::{rngs::StdRng, SeedableRng, seq::SliceRandom};
use crate::tiles::Tile;

const TOTAL_TILES_GENERATED: usize = 72; // 18 types * 4 copies
const TOTAL_WALL_TILES: usize = 70;      // As per project guideline for Sanma wall size
const DEAD_WALL_SIZE: usize = 14;
const LIVE_WALL_SIZE: usize = TOTAL_WALL_TILES - DEAD_WALL_SIZE; // 70 - 14 = 56

// Absolute indices in the `tiles` array for dead wall components
const DORA_INDICATOR_IDX: usize = LIVE_WALL_SIZE; // Index 56
const URA_DORA_FOR_MAIN_DORA_IDX: usize = DORA_INDICATOR_IDX + 1; // Index 57

// Kan Dora indicators and their Ura Dora are interleaved
// Kan Dora 1: tiles[58], Ura for KD1: tiles[59]
// Kan Dora 2: tiles[60], Ura for KD2: tiles[61]
// ...
const KAN_DORA_INDICATORS_START_IDX: usize = URA_DORA_FOR_MAIN_DORA_IDX + 1; // Index 58
const MAX_KAN_DORA_SETS: usize = 4; // Max 4 Kans can reveal new Dora

// Rinshanpai are at the end of the dead wall
// After Dora (1), Ura (1), KanDora+Ura (4*2=8) = 10 tiles used for indicators
const RINSHANPAI_START_IDX: usize = DORA_INDICATOR_IDX + 1 + 1 + (MAX_KAN_DORA_SETS * 2); // 56 + 10 = 66
const NUM_RINSHANPAI: usize = 4;

pub struct Wall {
    tiles: [Tile; TOTAL_WALL_TILES],
    live_wall_draw_pos: usize,          // Tracks next tile to draw from live wall (0 to LIVE_WALL_SIZE-1)
    kan_doras_revealed_count: u8,     // Number of Kan Doras revealed (0 to MAX_KAN_DORA_SETS)
    rinshanpai_drawn_count: u8,       // Number of Rinshanpai drawn (0 to NUM_RINSHANPAI)
}

impl Wall {
    pub fn new(seed: u64) -> Self {
        // Build initial deck of 72 tiles (18 types * 4 copies)
        // (Manzu 1,9; Pinzu 1-9; Honor tiles E,S,W,N,Wd,Gd,Rd)
        let mut deck: Vec<Tile> = Vec::with_capacity(TOTAL_TILES_GENERATED);
        for id in 0u8..=33 { // Tile IDs 0-33
            let t = Tile::try_from(id).unwrap();
            // Skip 2-8 Manzu as per Sanma rules (Man1 and Man9 are kept)
            if matches!(t, Tile::Man2 | Tile::Man3 | Tile::Man4 | Tile::Man5 | Tile::Man6 | Tile::Man7 | Tile::Man8) {
                continue;
            }
            // Skip all Souzu (Bamboo) tiles as per this Sanma variant's rules
            if matches!(t, Tile::Sou1 | Tile::Sou2 | Tile::Sou3 | Tile::Sou4 | Tile::Sou5 | Tile::Sou6 | Tile::Sou7 | Tile::Sou8 | Tile::Sou9) {
                continue;
            }
            // Add 4 copies of the remaining 18 types of tiles
            for _ in 0..4 {
                deck.push(t);
            }
        }
        assert_eq!(deck.len(), TOTAL_TILES_GENERATED, "Initial Sanma deck before truncation should be 72 tiles");

        // Shuffle deterministically
        let mut rng = StdRng::seed_from_u64(seed);
        deck.shuffle(&mut rng);

        // Truncate to the specified wall size (70 tiles for this Sanma variant)
        deck.truncate(TOTAL_WALL_TILES);
        assert_eq!(deck.len(), TOTAL_WALL_TILES, "Sanma wall must be 70 tiles after truncation");

        let mut tiles_arr = [Tile::Man1; TOTAL_WALL_TILES]; // Placeholder, will be overwritten
        tiles_arr.copy_from_slice(&deck);

        Self {
            tiles: tiles_arr,
            live_wall_draw_pos: 0,
            kan_doras_revealed_count: 0,
            rinshanpai_drawn_count: 0,
        }
    }

    /// Draws a tile from the live wall.
    pub fn draw_from_live_wall(&mut self) -> Option<Tile> {
        if self.live_wall_draw_pos < LIVE_WALL_SIZE {
            let tile = self.tiles[self.live_wall_draw_pos];
            self.live_wall_draw_pos += 1;
            Some(tile)
        } else {
            None // Live wall is exhausted
        }
    }

    /// Reveals the initial Dora indicator tile from the dead wall.
    /// This is typically called once at the start of a hand.
    pub fn get_initial_dora_indicator(&self) -> Option<Tile> {
        if TOTAL_WALL_TILES > DORA_INDICATOR_IDX {
            Some(self.tiles[DORA_INDICATOR_IDX])
        } else {
            None // Should not happen in a correctly sized wall
        }
    }

    /// Gets all currently revealed Ura-Dora indicators.
    /// This includes the Ura-Dora for the initial Dora, and for any revealed Kan-Doras.
    pub fn get_current_ura_dora_indicators(&self) -> Vec<Tile> {
        let mut uras = Vec::new();
        // Ura for initial Dora
        if TOTAL_WALL_TILES > URA_DORA_FOR_MAIN_DORA_IDX {
            uras.push(self.tiles[URA_DORA_FOR_MAIN_DORA_IDX]);
        }

        // Uras for revealed Kan Doras
        for i in 0..self.kan_doras_revealed_count {
            // Each Kan Dora indicator is at KAN_DORA_INDICATORS_START_IDX + (i * 2)
            // Its Ura Dora is at KAN_DORA_INDICATORS_START_IDX + (i * 2) + 1
            let ura_idx = KAN_DORA_INDICATORS_START_IDX + (i as usize * 2) + 1;
            if TOTAL_WALL_TILES > ura_idx {
                uras.push(self.tiles[ura_idx]);
            }
        }
        uras
    }

    /// Reveals a new Kan Dora indicator from the dead wall.
    /// Returns the revealed Kan Dora indicator tile.
    /// GameState should store this revealed indicator.
    pub fn reveal_new_kan_dora_indicator(&mut self) -> Option<Tile> {
        if self.kan_doras_revealed_count < MAX_KAN_DORA_SETS as u8 {
            let kan_dora_idx = KAN_DORA_INDICATORS_START_IDX + (self.kan_doras_revealed_count as usize * 2);
            if TOTAL_WALL_TILES > kan_dora_idx {
                let indicator = self.tiles[kan_dora_idx];
                self.kan_doras_revealed_count += 1;
                Some(indicator)
            } else {
                None // Not enough tiles in dead wall (should not happen)
            }
        } else {
            None // Max Kan Doras already revealed
        }
    }

    /// Draws a replacement tile (Rinshanpai) from the dead wall after a Kan.
    pub fn draw_replacement_tile(&mut self) -> Option<Tile> {
        if self.rinshanpai_drawn_count < NUM_RINSHANPAI as u8 {
            let rinshan_idx = RINSHANPAI_START_IDX + self.rinshanpai_drawn_count as usize;
            if TOTAL_WALL_TILES > rinshan_idx {
                let tile = self.tiles[rinshan_idx];
                self.rinshanpai_drawn_count += 1;
                Some(tile)
            } else {
                None // Not enough tiles for rinshanpai (should not happen)
            }
        } else {
            None // All Rinshanpai drawn
        }
    }

    /// Returns the number of tiles remaining in the live wall that can be drawn.
    pub fn live_wall_remaining_count(&self) -> usize {
        if self.live_wall_draw_pos <= LIVE_WALL_SIZE {
            LIVE_WALL_SIZE - self.live_wall_draw_pos
        } else {
            0
        }
    }
    
    /// Checks if the live wall is empty.
    pub fn is_live_wall_empty(&self) -> bool {
        self.live_wall_draw_pos >= LIVE_WALL_SIZE
    }

    /// Returns the total number of tiles the wall was initialized with.
    pub fn total_len(&self) -> usize {
        self.tiles.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile; // Ensure Tile enum is in scope for tests

    #[test]
    fn wall_initialization_correct_size() {
        let wall = Wall::new(42);
        assert_eq!(wall.total_len(), TOTAL_WALL_TILES);
        assert_eq!(wall.live_wall_remaining_count(), LIVE_WALL_SIZE);
        assert_eq!(wall.kan_doras_revealed_count, 0);
        assert_eq!(wall.rinshanpai_drawn_count, 0);
    }

    #[test]
    fn determinism() {
        let a = Wall::new(42);
        let b = Wall::new(42);
        assert_eq!(a.tiles.as_slice(), b.tiles.as_slice(), "Walls with same seed should be identical");
    }

    #[test]
    fn draw_from_live_wall_exhausts_correctly() {
        let mut wall = Wall::new(1);
        for _ in 0..LIVE_WALL_SIZE {
            assert!(wall.draw_from_live_wall().is_some(), "Should be able to draw from live wall");
        }
        assert!(wall.draw_from_live_wall().is_none(), "Live wall should be empty");
        assert_eq!(wall.live_wall_remaining_count(), 0);
    }

    #[test]
    fn initial_dora_indicator() {
        let wall = Wall::new(5);
        let indicator = wall.get_initial_dora_indicator();
        assert!(indicator.is_some(), "Should get an initial Dora indicator");
        // Check it's the expected tile based on structure
        assert_eq!(indicator.unwrap(), wall.tiles[DORA_INDICATOR_IDX]);
    }

    #[test]
    fn kan_dora_and_ura_dora_logic() {
        let mut wall = Wall::new(10);

        // Initial state
        let initial_uras = wall.get_current_ura_dora_indicators();
        assert_eq!(initial_uras.len(), 1, "Initially one Ura Dora for the main Dora");
        assert_eq!(initial_uras[0], wall.tiles[URA_DORA_FOR_MAIN_DORA_IDX]);

        // Reveal 1st Kan Dora
        let kd1 = wall.reveal_new_kan_dora_indicator().unwrap();
        assert_eq!(kd1, wall.tiles[KAN_DORA_INDICATORS_START_IDX]);
        assert_eq!(wall.kan_doras_revealed_count, 1);
        let uras_after_kd1 = wall.get_current_ura_dora_indicators();
        assert_eq!(uras_after_kd1.len(), 2, "Should have Ura for main Dora and 1st Kan Dora");
        assert_eq!(uras_after_kd1[1], wall.tiles[KAN_DORA_INDICATORS_START_IDX + 1]);

        // Reveal all Kan Doras
        for i in 1..MAX_KAN_DORA_SETS {
            let kd = wall.reveal_new_kan_dora_indicator().unwrap();
            let expected_kd_idx = KAN_DORA_INDICATORS_START_IDX + (i * 2);
            assert_eq!(kd, wall.tiles[expected_kd_idx]);
        }
        assert_eq!(wall.kan_doras_revealed_count, MAX_KAN_DORA_SETS as u8);
        assert!(wall.reveal_new_kan_dora_indicator().is_none(), "Should not reveal more than max Kan Doras");
        
        let final_uras = wall.get_current_ura_dora_indicators();
        assert_eq!(final_uras.len(), 1 + MAX_KAN_DORA_SETS, "Should have Ura for main Dora and all Kan Doras");
    }

    #[test]
    fn rinshanpai_draw_logic() {
        let mut wall = Wall::new(15);
        for i in 0..NUM_RINSHANPAI {
            let rp = wall.draw_replacement_tile().unwrap();
            assert_eq!(rp, wall.tiles[RINSHANPAI_START_IDX + i]);
            assert_eq!(wall.rinshanpai_drawn_count, (i + 1) as u8);
        }
        assert!(wall.draw_replacement_tile().is_none(), "Should not draw more than available Rinshanpai");
        assert_eq!(wall.rinshanpai_drawn_count, NUM_RINSHANPAI as u8);
    }

    #[test]
    fn wall_emptying_interactions() {
        let mut wall = Wall::new(20);
        // Draw almost all live wall tiles
        for _ in 0..(LIVE_WALL_SIZE - 1) {
            wall.draw_from_live_wall();
        }
        assert_eq!(wall.live_wall_remaining_count(), 1);

        // Dead wall operations should still work
        assert!(wall.get_initial_dora_indicator().is_some());
        assert!(wall.reveal_new_kan_dora_indicator().is_some());
        assert!(wall.draw_replacement_tile().is_some());

        // Draw last live wall tile
        wall.draw_from_live_wall();
        assert_eq!(wall.live_wall_remaining_count(), 0);
        assert!(wall.is_live_wall_empty());
    }
}