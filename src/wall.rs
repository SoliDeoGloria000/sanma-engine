// src/wall.rs
use rand::{rngs::StdRng, seq::SliceRandom, SeedableRng};
use crate::tiles::Tile;

// These constants are for a 108-tile Sanma game (Manzu 2-8 removed).
pub const TOTAL_TILES_GENERATED: usize = 108;
pub const DEAD_WALL_SIZE: usize = 14;
pub const LIVE_WALL_SIZE: usize = TOTAL_TILES_GENERATED - DEAD_WALL_SIZE;

const WALL_ARRAY_LEN: usize = TOTAL_TILES_GENERATED;

const DORA_INDICATOR_IDX: usize = LIVE_WALL_SIZE;
const URA_DORA_FOR_MAIN_DORA_IDX: usize = DORA_INDICATOR_IDX + 1;

const KAN_DORA_INDICATORS_START_IDX: usize = URA_DORA_FOR_MAIN_DORA_IDX + 1;
const MAX_KAN_DORA_SETS: usize = 4;

const RINSHANPAI_START_IDX: usize = DORA_INDICATOR_IDX + 10;
const NUM_RINSHANPAI: usize = 4;

pub struct Wall {
    tiles: [Tile; WALL_ARRAY_LEN],
    live_wall_draw_pos: usize,
    kan_doras_revealed_count: u8,
    rinshanpai_drawn_count: u8,
}

impl Wall {
    pub fn new(seed: u64) -> Self {
        let mut deck: Vec<Tile> = Vec::with_capacity(TOTAL_TILES_GENERATED);
        for id in 0u8..=33 {
            if let Ok(t) = Tile::try_from(id) {
                // This block correctly removes Manzu 2-8 for a 108-tile Sanma deck.
                if matches!(t, Tile::Man2 | Tile::Man3 | Tile::Man4 | Tile::Man5 | Tile::Man6 | Tile::Man7 | Tile::Man8) {
                    continue;
                }
                for _ in 0..4 {
                    deck.push(t);
                }
            }
        }
        // This assertion will now pass because the code and the constant both use 108.
        assert_eq!(deck.len(), TOTAL_TILES_GENERATED, "Initial Sanma deck size mismatch");

        let mut rng = StdRng::seed_from_u64(seed);
        deck.shuffle(&mut rng);

        let mut tiles_arr = [Tile::Man1; WALL_ARRAY_LEN];
        tiles_arr.copy_from_slice(&deck);

        Self {
            tiles: tiles_arr,
            live_wall_draw_pos: 0,
            kan_doras_revealed_count: 0,
            rinshanpai_drawn_count: 0,
        }
    }

    pub fn draw_from_live_wall(&mut self) -> Option<Tile> {
        if self.live_wall_draw_pos < LIVE_WALL_SIZE {
            let tile = self.tiles[self.live_wall_draw_pos];
            self.live_wall_draw_pos += 1;
            Some(tile)
        } else {
            None
        }
    }

    pub fn get_initial_dora_indicator(&self) -> Option<Tile> {
        if WALL_ARRAY_LEN > DORA_INDICATOR_IDX {
            Some(self.tiles[DORA_INDICATOR_IDX])
        } else { None }
    }

    pub fn get_current_ura_dora_indicators(&self) -> Vec<Tile> {
        let mut uras = Vec::new();
        if WALL_ARRAY_LEN > URA_DORA_FOR_MAIN_DORA_IDX {
            uras.push(self.tiles[URA_DORA_FOR_MAIN_DORA_IDX]);
        }
        for i in 0..self.kan_doras_revealed_count {
            let ura_idx = KAN_DORA_INDICATORS_START_IDX + (i as usize * 2) + 1;
            if WALL_ARRAY_LEN > ura_idx {
                uras.push(self.tiles[ura_idx]);
            }
        }
        uras
    }

    pub fn reveal_new_kan_dora_indicator(&mut self) -> Option<Tile> {
        if self.kan_doras_revealed_count < MAX_KAN_DORA_SETS as u8 {
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
            let rinshan_idx = RINSHANPAI_START_IDX + self.rinshanpai_drawn_count as usize;
             if WALL_ARRAY_LEN > rinshan_idx {
                let tile = self.tiles[rinshan_idx];
                self.rinshanpai_drawn_count += 1;
                Some(tile)
            } else { None }
        } else { None }
    }
	
	pub fn from_predetermined(tiles_vec: Vec<Tile>) -> Self {
        let mut tiles_arr = [Tile::Sou9; TOTAL_TILES_GENERATED]; // Default to a dummy tile
        
        // Copy the tiles from the log. If the log is from an incomplete game,
        // the rest of the wall will be dummy tiles.
        let len = tiles_vec.len().min(TOTAL_TILES_GENERATED);
        tiles_arr[..len].copy_from_slice(&tiles_vec[..len]);

        Self {
            tiles: tiles_arr,
            live_wall_draw_pos: 0,
            kan_doras_revealed_count: 0,
            rinshanpai_drawn_count: 0,
        }
    }


    pub fn live_wall_remaining_count(&self) -> usize {
        if self.live_wall_draw_pos <= LIVE_WALL_SIZE {
            LIVE_WALL_SIZE - self.live_wall_draw_pos
        } else { 0 }
    }

    pub fn is_live_wall_empty(&self) -> bool {
        self.live_wall_draw_pos >= LIVE_WALL_SIZE
    }
    
    pub fn rinshanpai_drawn_count(&self) -> u8 {
        self.rinshanpai_drawn_count
    }

    pub fn remove_tile_for_testing(&mut self, tile_to_remove: Tile) {
        if let Some(pos) = self.tiles[self.live_wall_draw_pos..LIVE_WALL_SIZE]
            .iter()
            .position(|&t| t == tile_to_remove)
        {
            let absolute_pos = self.live_wall_draw_pos + pos;
            self.tiles.swap(self.live_wall_draw_pos, absolute_pos);
            self.live_wall_draw_pos += 1;
            return;
        }

        if let Some(dead_pos) = self.tiles[LIVE_WALL_SIZE..]
            .iter()
            .position(|&t| t == tile_to_remove)
        {
            let last_live_tile_index = LIVE_WALL_SIZE - 1;
            if self.live_wall_draw_pos > last_live_tile_index {
                println!("ERROR: Wall exhausted, cannot swap from dead wall. Log is invalid.");
                return;
            }
            let absolute_dead_pos = LIVE_WALL_SIZE + dead_pos;
            self.tiles.swap(last_live_tile_index, absolute_dead_pos);

            if let Some(pos) = self.tiles[self.live_wall_draw_pos..LIVE_WALL_SIZE]
                .iter()
                .position(|&t| t == tile_to_remove)
            {
                let absolute_pos = self.live_wall_draw_pos + pos;
                self.tiles.swap(self.live_wall_draw_pos, absolute_pos);
                self.live_wall_draw_pos += 1;
                return;
            }
        }
        
        println!("WARNING: Could not find any more copies of tile {:?} in the entire wall. The log is likely invalid.", tile_to_remove);
    }
	
	#[cfg(test)]
	pub(crate) fn _reset_for_test(&mut self, live_tiles: Vec<Tile>, dead_tiles: Vec<Tile>) {
        let mut temp_wall_tiles = live_tiles;
        temp_wall_tiles.extend(dead_tiles);

        if temp_wall_tiles.len() == self.tiles.len() {
            self.tiles.copy_from_slice(&temp_wall_tiles);
            self.live_wall_draw_pos = 0;
            self.kan_doras_revealed_count = 0;
            self.rinshanpai_drawn_count = 0;
        } else {
            panic!(
                "Test setup error: Provided tiles ({}) do not match wall size ({}).",
                temp_wall_tiles.len(),
                self.tiles.len()
            );
        }
    }

    #[cfg(test)]
    pub fn _set_live_wall_draw_pos_for_test(&mut self, new_pos: usize) {
        self.live_wall_draw_pos = new_pos;
    }
}