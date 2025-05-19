// src/lib.rs
use pyo3::prelude::*;
use numpy::{IntoPyArray, PyArray, PyArray1, Ix1, Ix3, PyReadonlyArray1}; // For Ix3
use pyo3::types::{PyDict, PyList};

mod tiles;
mod hand;
mod wall; // Ensure this uses the version with dead wall logic
mod hand_parser; 
mod game_state; // Ensure this uses the version compatible with the new wall
mod fu_calculation; // If Fu calculation is separated

pub use tiles::{Tile, TileExt};
pub use hand::{Hand, HandError};
pub use wall::Wall;
pub use hand_parser; 
pub use game_state::{GameState, WinType, DeclaredMeldType, Score}; // Expose Score for reward
// pub use fu_calculation; // If used directly by GameState or here

const OBS_SHAPE: (usize, usize, usize) = (149, 5, 5); // As per guideline (placeholder content)
const MAX_LEGAL_ACTIONS: usize = 100; // Placeholder for max number of distinct actions

/// Defines the types of actions a player can take.
/// This will be mapped from an integer action passed from Python.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlayerAction {
    Discard(Tile),
    Riichi(Tile),       // Tile is the discard accompanying Riichi
    Ankan(Tile),
    Shouminkan(Tile),   // Tile is the one being added to an existing Pon
    Daiminkan(Tile),    // Tile is the one being called for Kan
    Pon(Tile),          // Tile is the one being called for Pon
    Kita,
    TsumoAgari,
    RonAgari,
    Pass, // Pass on an opportunity to call (Ron, Pon, Kan)
}

#[pyclass]
struct Env {
    state: GameState,
    // To manage game flow where multiple decisions might be needed before control returns to Python
    // For example, after a Kan, the same player needs to discard.
    // Or after a discard, other players get a chance to call.
    // This simple Env might not fully model that without a more complex state machine.
    // For now, we assume `step` resolves until the next player's primary action (draw/discard).
    requires_discard_after_kan: bool, // True if current player just kanned and needs to discard
    // TODO: Add state for pending calls from other players after a discard.
}

#[pymethods]
impl Env {
    #[new]
    fn new(seed: Option<u64>, initial_dealer_idx: Option<u8>) -> Self {
        let actual_seed = seed.unwrap_or_else(|| {
            // Basic time-based seed if None, for some randomness.
            // For reproducible RL, Python side should manage and pass explicit seeds.
            std::time::SystemTime::now().duration_since(std.time::UNIX_EPOCH).unwrap().as_secs()
        });
        let dealer = initial_dealer_idx.unwrap_or(0);
        if dealer >= 3 {
            panic!("Initial dealer index must be 0, 1, or 2.");
        }
        Self { 
            state: GameState::new(actual_seed, dealer),
            requires_discard_after_kan: false,
        }
    }

    /// Resets the environment to a new game.
    /// `seed`: Optional seed for game initialization.
    /// `initial_dealer_idx`: Optional dealer for the new game.
    fn reset<'py>(
        &mut self, 
        py: Python<'py>, 
        seed: Option<u64>, 
        initial_dealer_idx: Option<u8>
    ) -> PyResult<(Py<PyArray<u8, Ix3>>, Py<PyArray1<bool>>)> {
        let actual_seed = seed.unwrap_or_else(|| {
            std::time::SystemTime::now().duration_since(std.time::UNIX_EPOCH).unwrap().as_secs()
        });
        let dealer = initial_dealer_idx.unwrap_or(0);
        if dealer >= 3 {
            return Err(PyValueError::new_err("Initial dealer index must be 0, 1, or 2."));
        }
        self.state = GameState::new(actual_seed, dealer);
        self.requires_discard_after_kan = false;
        
        // Initial draw for the dealer before first action
        if self.state.turn_count == 0 { // Only if it's the very start
             self.state.player_draws_tile(); // Dealer draws their first tile
        }

        Ok(self._get_obs_and_legal_actions(py))
    }

    /// Takes a step in the environment.
    /// `action_idx`: An integer representing the chosen action from the legal action space.
    ///               The mapping of this index to `PlayerAction` needs to be defined.
    /// `tile_data_idx`: Optional u8, for actions involving a specific tile (e.g., discard Tile::Man1).
    ///                  The interpretation depends on `action_idx`.
    fn step<'py>(
        &mut self,
        py: Python<'py>,
        action_id: u8, // Represents an index from the agent's policy output
        // tile_data: Option<u8>, // Optional tile for discard/kan etc.
                               // This should be part of action_id decoding.
    ) -> PyResult<(Py<PyArray<u8, Ix3>>, f32, bool, Py<PyDict>)> {
        
        let current_player_idx = self.state.current_player_idx as usize;
        let mut reward = 0.0;
        let mut done = false;
        let info = PyDict::new(py); // Empty info dict for now

        // --- 1. Decode Action ---
        // This is a placeholder. A full system would map action_id to PlayerAction + Tile.
        // For now, let's assume a simplified mapping or that GameState methods are called directly
        // based on a more complex action structure passed from Python (not just u8).
        // Given the guideline `action: u8` in original lib.rs, this is tricky.
        // Let's simulate a discard action for now if action_id is a tile ID.
        // A proper action space is needed.

        // Placeholder: Convert action_id to a PlayerAction.
        // This requires a defined mapping from action indices to specific game moves.
        // E.g., 0-33: Discard Tile ID 0-33. 34: Riichi. 35: TsumoAgari etc.
        // This is a CRITICAL part for the RL agent to interact.
        // For this refinement, we'll assume action_id directly implies a type of action
        // and potentially uses a hardcoded tile for simplicity if needed.

        let player_action: PlayerAction; // To be determined from action_id

        // Example: If action_id < 34, it's a discard of Tile::try_from(action_id).
        if action_id < 34 { // Assuming 0-33 are discard actions for Tile ID 0-33
            if let Ok(tile_to_discard) = Tile::try_from(action_id) {
                player_action = PlayerAction::Discard(tile_to_discard);
            } else {
                return Err(PyValueError::new_err(format!("Invalid tile ID for discard action: {}", action_id)));
            }
        } else if action_id == 34 { // Example: Riichi (needs a discard)
            // Riichi needs a discard. The agent must pick a combined Riichi+Discard action.
            // For now, assume Riichi implies discarding the last drawn tile (simplification).
            // A real agent would choose which tile to discard with Riichi.
            let tile_to_discard = self.state.last_drawn_tile.or_else(|| self.state.hands[current_player_idx].get_all_tiles().first().cloned())
                .ok_or_else(|| PyValueError::new_err("No tile to discard for Riichi"))?;
            player_action = PlayerAction::Riichi(tile_to_discard);
        } else if action_id == 35 { // Example: TsumoAgari
            player_action = PlayerAction::TsumoAgari;
        }
        // ... Add more mappings for other actions like Ankan, Kita, Pass, Ron, Pon etc.
        else {
            // Default to Pass or an error if action_id is unmapped
            // player_action = PlayerAction::Pass; // Or error
            return Err(PyValueError::new_err(format!("Unmapped action_id: {}", action_id)));
        }


        // --- 2. Execute Player Action ---
        match player_action {
            PlayerAction::Discard(tile) => {
                if self.state.hands[current_player_idx].count(tile) == 0 {
                     return Err(PyValueError::new_err(format!("Attempted to discard tile {:?} not in hand.", tile)));
                }
                match self.state.player_discards_tile(current_player_idx, tile) {
                    Ok(_) => {
                        // Check for Ron from other players
                        let discarder_seat = current_player_idx; // Player who just discarded
                        let discarded_tile_value = tile; // The tile that was discarded

                        for i in 0..3 {
                            if i == discarder_seat { continue; }
                            // The player whose turn it would be next to check Ron is (discarder_seat + 1 + i_offset) % 3
                            // Simpler: check all other players.
                            let potential_ron_player_idx = i;
                            if self.state.check_ron_for_player(discarded_tile_value, potential_ron_player_idx, discarder_seat) {
                                // TODO: Handle Ron call priority / multiple Rons (Dabururon)
                                // For now, first detected Ron wins.
                                let score = self.state.score_win(potential_ron_player_idx, WinType::Ron { winning_tile: discarded_tile_value, discarder_seat });
                                reward = score.points as f32;
                                done = true;
                                // GameState's current_player_idx might need to be set to the winner for obs, or obs reflects post-win state.
                                // For now, obs will be for the next player after the original discarder if no win.
                                let (obs_py, legal_actions_py) = self._get_obs_and_legal_actions(py);
                                return Ok((obs_py, reward, done, info));
                            }
                        }
                        // If no Ron, the turn has already advanced in player_discards_tile.
                        // The new current player needs to draw.
                        self.state.player_draws_tile(); 
                        self.requires_discard_after_kan = false;
                    }
                    Err(e) => return Err(PyValueError::new_err(format!("Discard error: {}", e))),
                }
            }
            PlayerAction::Riichi(tile_to_discard) => {
                // TODO: Implement self.state.declare_riichi(current_player_idx). For now, just set flag.
                if self.state.can_declare_riichi(current_player_idx) { // Add this method to GameState
                    self.state.riichi_declared[current_player_idx] = true;
                    // Riichi fee deduction would happen here.
                    // Then, proceed with discard logic (same as PlayerAction::Discard)
                    match self.state.player_discards_tile(current_player_idx, tile_to_discard) {
                        Ok(_) => { /* Check Ron, then next player draws, as in Discard */ 
                            // Duplicating Ron check logic here for brevity, should be refactored
                            for i in 0..3 {
                                if i == current_player_idx { continue; }
                                if self.state.check_ron_for_player(tile_to_discard, i, current_player_idx) {
                                    let score = self.state.score_win(i, WinType::Ron { winning_tile: tile_to_discard, discarder_seat: current_player_idx });
                                    reward = score.points as f32; done = true;
                                    let (obs_py, legal_actions_py) = self._get_obs_and_legal_actions(py);
                                    return Ok((obs_py, reward, done, info));
                                }
                            }
                            self.state.player_draws_tile();
                            self.requires_discard_after_kan = false;
                        }
                        Err(e) => return Err(PyValueError::new_err(format!("Riichi discard error: {}", e))),
                    }
                } else {
                    return Err(PyValueError::new_err("Cannot declare Riichi at this time."));
                }
            }
            PlayerAction::TsumoAgari => {
                if self.state.check_tsumo() { // check_tsumo uses current_player_idx implicitly
                    let score = self.state.score_win(current_player_idx, WinType::Tsumo);
                    reward = score.points as f32;
                    done = true;
                } else {
                    return Err(PyValueError::new_err("Invalid Tsumo Agari claim."));
                }
            }
            PlayerAction::Ankan(tile) => {
                match self.state.make_ankan(current_player_idx, tile) {
                    Ok(_) => {
                        // After Ankan, player draws rinshanpai (handled in make_ankan)
                        // Check for Rinshan Kaihou / Tsumo
                        if self.state.check_tsumo() { // Rinshan is a type of Tsumo
                            let score = self.state.score_win(current_player_idx, WinType::Tsumo);
                            reward = score.points as f32;
                            done = true;
                        } else {
                            // Player needs to discard again.
                            self.requires_discard_after_kan = true; 
                            // current_player_idx remains the same.
                        }
                    }
                    Err(e) => return Err(PyValueError::new_err(format!("Ankan error: {}", e))),
                }
            }
            PlayerAction::Kita => {
                 match self.state.make_kita_declaration(current_player_idx) {
                    Ok(_) => {
                        // Player continues their turn, usually discards next.
                        // No change to current_player_idx from Kita itself.
                        self.requires_discard_after_kan = false; // Kita doesn't grant rinshan discard state
                    }
                    Err(e) => return Err(PyValueError::new_err(format!("Kita error: {}", e))),
                }
            }
            // TODO: Implement Shouminkan, Daiminkan, Pon, RonAgari, Pass
            // These actions often happen in response to an opponent's discard,
            // which complicates the simple `current_player_idx` turn flow.
            // A full game FSM (Finite State Machine) might be needed in `Env` or `GameState`.
            _ => {
                // For unhandled actions or Pass for now.
                // If it was a "Pass" on an opponent's discard, the next player in sequence should draw.
                // This part needs careful state management for whose turn it becomes.
                // Assuming for now, if no other action, next player (already set by previous discard) draws.
                if !done && !self.requires_discard_after_kan { // If game not ended and not waiting for discard after Kan
                    // This draw might be redundant if previous discard already set up next player and they drew.
                    // The GameState's player_draws_tile and player_discards_tile manage current_player_idx.
                    // If a player "passes" on a call, the game proceeds to the next player's draw.
                    // This is implicitly handled if the action was a discard and no one called.
                }
            }
        }

        // If game ended due to exhaustive draw (Ryuukyoku)
        if !done && self.state.wall.is_live_wall_empty() && !self.state.is_rinshan_kaihou_win_pending {
            // Handle Ryuukyoku (exhaustive draw) - e.g., check for Noten Bappu payments
            // For now, just end the game with 0 reward.
            done = true;
            // TODO: Implement Noten Bappu / scoring for different types of draws.
            info.set_item("draw_type", "exhaustive_draw")?;
        }
        
        // --- 3. Prepare and Return Observation, Reward, Done, Info ---
        let (obs_py, legal_actions_py) = self._get_obs_and_legal_actions(py);
        // The observation should be for the *new* current_player_idx.
        // The legal_actions should also be for them.
        
        // Store legal actions in the info dictionary as per typical gym envs
        info.set_item("legal_actions", legal_actions_py)?;

        Ok((obs_py, reward, done, info))
    }

    /// Helper: build observation tensor and legal action mask.
    /// This is a MAJOR placeholder. The actual observation tensor is complex.
    fn _get_obs_and_legal_actions<'py>(&self, py: Python<'py>) -> (Py<PyArray<u8, Ix3>>, Py<PyArray1<bool>>) {
        // Placeholder for the observation tensor (149, 5, 5)
        // For now, just a small part of it, e.g., current player's hand and some game state.
        // This needs to be carefully constructed according to the spec.
        let obs_array = PyArray::zeros(py, OBS_SHAPE, false);
        // Example: Fill a tiny part of obs_array
        // obs_array.get_mut([0,0,0]).unwrap() = self.state.current_player_idx;
        // ... fill with actual game state features ...

        // Placeholder for legal action mask
        // This needs to check all possible actions (discard each tile, Riichi, Kan, etc.)
        // based on the current player's hand and game state.
        let mut legal_actions_vec = vec![false; MAX_LEGAL_ACTIONS];
        // Example: Mark discard of first tile in hand as legal (if hand not empty)
        // if let Some(tile_to_discard) = self.state.hands[self.state.current_player_idx as usize].get_all_tiles().first() {
        //    legal_actions_vec[tile_to_discard.to_id() as usize] = true; // Assuming to_id() gives 0-33
        // }
        // Mark TsumoAgari as legal if check_tsumo is true
        // if self.state.check_tsumo() { legal_actions_vec[ACTION_ID_TSUMO_AGARI] = true; }
        // ... and so on for all actions. This is very complex.
        // For now, let's say first few actions are "legal" for testing.
        if MAX_LEGAL_ACTIONS > 0 { legal_actions_vec[0] = true; }
        if MAX_LEGAL_ACTIONS > 1 { legal_actions_vec[1] = true; }


        let legal_actions_pyarray = PyArray1::from_vec(py, legal_actions_vec);

        (obs_array.to_owned(), legal_actions_pyarray.to_owned())
    }

    /// Get current player index (for Python side to know who should act).
    fn current_player_idx(&self) -> usize {
        self.state.current_player_idx as usize
    }
    
    // Expose some GameState fields for debugging or Python-side logic if needed
    fn get_hand_tiles_pystr(&self, player_idx: usize) -> PyResult<String> {
        if player_idx >= 3 { return Err(PyIndexError::new_err("Player index out of range")); }
        let hand_str = self.state.hands[player_idx].get_all_tiles().iter()
            .map(|t| t.to_unicode().to_string())
            .collect::<Vec<String>>()
            .join(" ");
        Ok(hand_str)
    }

    fn get_dora_indicators_pystr(&self) -> String {
        self.state.dora_indicators.iter().map(|t| t.to_unicode().to_string()).collect::<Vec<String>>().join(" ")
    }
}

// Helper trait/impl for Tile to get its u8 ID for action mapping (if not already present)
// Assuming Tile can be cast to u8 directly for its ID.
// Trait TileExt in tiles.rs should provide to_id() or similar if needed.

// Add GameState methods that might be missing for Env::step (like can_declare_riichi)
// This should be in game_state.rs
/*
impl GameState {
    pub fn can_declare_riichi(&self, player_idx: usize) -> bool {
        // Conditions: Menzen, Tenpai, >= 1000 points (not checked here), wall has enough tiles.
        self.is_menzen(player_idx) && 
        self.is_tenpai(player_idx) && // is_tenpai would be a new complex check
        self.wall.live_wall_remaining_count() >= 4 // Standard Riichi rule
    }
    // fn is_tenpai(&self, player_idx: usize) -> bool { ... } // Complex: check if 1 tile away from win
    // fn declare_riichi(&mut self, player_idx: usize) -> Result<(), &'static str> { ... }
}
*/


#[pymodule]
fn sanma_engine(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Env>()?;
    // Could also expose Tile enum or other structs if Python needs to construct them,
    // but usually action IDs are simpler for the interface.
    Ok(())
}