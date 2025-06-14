// src/lib.rs
use pyo3::prelude::*;
use pyo3::Bound;
use numpy::{PyArray, PyArray1, Ix3, ndarray::s, PyArrayMethods};
use pyo3::types::PyDict;
use pyo3::exceptions::{PyValueError, PyIndexError};
use std::collections::VecDeque;

mod tiles;
mod hand;
mod wall;
mod hand_parser;
mod game_state;
mod fu_calculation;

// Re-export items that GameState might need from its own module or sibling modules
pub use tiles::Tile;
pub use hand::{Hand};
pub use wall::Wall;
pub use game_state::{GameState, WinType, DeclaredMeldType, Score, DeclaredMeld};

// --- Action Space Definition ---
const NUM_TILE_TYPES: u8 = 34;

const ACTION_ID_DISCARD_START: u8 = 0;
const ACTION_ID_DISCARD_END: u8 = ACTION_ID_DISCARD_START + NUM_TILE_TYPES - 1;

const ACTION_ID_RIICHI_DISCARD_START: u8 = ACTION_ID_DISCARD_END + 1;
const ACTION_ID_RIICHI_DISCARD_END: u8 = ACTION_ID_RIICHI_DISCARD_START + NUM_TILE_TYPES - 1;

const ACTION_ID_ANKAN_START: u8 = ACTION_ID_RIICHI_DISCARD_END + 1;
const ACTION_ID_ANKAN_END: u8 = ACTION_ID_ANKAN_START + NUM_TILE_TYPES - 1;

const ACTION_ID_SHOUMINKAN_START: u8 = ACTION_ID_ANKAN_END + 1;
const ACTION_ID_SHOUMINKAN_END: u8 = ACTION_ID_SHOUMINKAN_START + NUM_TILE_TYPES - 1;

const ACTION_ID_KITA: u8 = ACTION_ID_SHOUMINKAN_END + 1;
const ACTION_ID_TSUMO_AGARI: u8 = ACTION_ID_KITA + 1;
const ACTION_ID_RON_AGARI: u8 = ACTION_ID_TSUMO_AGARI + 1;
const ACTION_ID_PON: u8 = ACTION_ID_RON_AGARI + 1;
const ACTION_ID_DAIMINKAN: u8 = ACTION_ID_PON + 1;
const ACTION_ID_PASS: u8 = ACTION_ID_DAIMINKAN + 1;

const ACTION_SPACE_SIZE: usize = (ACTION_ID_PASS + 1) as usize;
const OBS_SHAPE: (usize, usize, usize) = (149, 5, 5);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PlayerAction {
    Discard(Tile),
    RiichiDeclare(Tile),
    Ankan(Tile),
    Shouminkan(Tile),
    Kita,
    TsumoAgari,
    RonAgari,
    Pon(Tile),
    Daiminkan(Tile),
    Pass,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GamePhase {
    PlayerTurnDraw,
    PlayerTurnAction,
    WaitingForCalls,
    ProcessingShouminkanChankan,
    RoundOver,
}

#[pyclass]
struct Env {
    state: GameState,
    current_phase: GamePhase,
    pending_call_options: Vec<(usize, Vec<PlayerAction>)>,
    player_who_made_shouminkan: Option<usize>,
    draw_queues: [VecDeque<Tile>; 3],
}

#[pymethods]
impl Env {
    #[new]
    fn new() -> Self {
        Self {
            state: GameState::new(0, 0, 0, None, None, None),
            current_phase: GamePhase::RoundOver,
            pending_call_options: Vec::new(),
            player_who_made_shouminkan: None,
            draw_queues: Default::default(),
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn reset<'py>(
        &mut self,
        py: Python<'py>,
        seed: Option<u64>,
        initial_dealer_idx: Option<u8>,
        initial_honba_sticks: Option<u8>,
        initial_hands: Option<Vec<Vec<u8>>>,
        initial_draws: Option<Vec<Vec<u8>>>,
        initial_scores: Option<[i32; 3]>,
        override_wall_u8: Option<Vec<u8>>,
    ) -> PyResult<(Py<PyArray<u8, Ix3>>, Py<PyArray1<bool>>)> {
        let actual_seed = seed.unwrap_or_else(|| {
            std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).unwrap().as_secs()
        });
        let dealer = initial_dealer_idx.unwrap_or(self.state.dealer_idx);
        let honba = initial_honba_sticks.unwrap_or(self.state.honba_sticks);

        let hands_as_tiles = initial_hands.map(|h_vecs| {
            let mut arr: [Vec<Tile>; 3] = Default::default();
            for (i, h) in h_vecs.iter().enumerate() {
                arr[i] = h.iter().map(|&tid| Tile::try_from(tid).unwrap()).collect();
            }
            arr
        });
		
		self.draw_queues = if let Some(draws_u8) = initial_draws {
            let mut queues: [VecDeque<Tile>; 3] = Default::default();
            for i in 0..3 {
                if i < draws_u8.len() {
                    queues[i] = draws_u8[i].iter().map(|&tid| Tile::try_from(tid).unwrap()).collect();
                }
            }
            queues
        } else {
            Default::default()
        };
        
        let override_wall_tiles = override_wall_u8.map(|wall_u8| {
            wall_u8.iter().map(|&tid| Tile::try_from(tid).unwrap()).collect()
        });

        self.state = GameState::new(actual_seed, dealer, honba, hands_as_tiles, initial_scores, override_wall_tiles);
        
        if self.state.turn_count == 0 && self.state.current_player_idx == self.state.dealer_idx {
            self._env_draw_tile_for_player(self.state.current_player_idx as usize);
        }

        self.current_phase = GamePhase::PlayerTurnAction;
        self.pending_call_options.clear();
        self.player_who_made_shouminkan = None;

        Ok(self.get_obs_and_legal_actions(py))
    }

    fn step<'py>(
        &mut self,
        py: Python<'py>,
        action_id: u8,
    ) -> PyResult<(Py<PyArray<u8, Ix3>>, f32, bool, Py<PyDict>)> {

        let mut reward = 0.0;
        let mut done = false;
        let info = PyDict::new_bound(py);

        if self.current_phase == GamePhase::RoundOver {
            info.set_item("status", "RoundOver, please reset")?;
            let (obs_py, legal_actions_py) = self.get_obs_and_legal_actions(py);
            info.set_item("legal_actions_mask", legal_actions_py)?;
            return Ok((obs_py, reward, true, info.unbind()));
        }

        let decoded_action_result = self._decode_action(action_id);
        let player_action = match decoded_action_result {
            Ok(action) => action,
            Err(e) => return Err(PyValueError::new_err(format!("Invalid action_id: {}. Error: {}", action_id, e))),
        };

        match self.current_phase {
            GamePhase::PlayerTurnAction => {
                let current_player = self.state.current_player_idx as usize;
                match player_action {
                    PlayerAction::Discard(tile) => {
                        if self.state.player_discards_tile(current_player, tile).is_ok() {
                            self._transition_to_waiting_for_calls(tile, current_player);
                        } else { return Err(PyValueError::new_err("Invalid discard action")); }
                    }
                    PlayerAction::RiichiDeclare(tile_to_discard_with_riichi) => {
                        if self.state.can_declare_riichi(current_player) {
                            self.state.declare_riichi(current_player);
                            if self.state.player_discards_tile(current_player, tile_to_discard_with_riichi).is_ok() {
                                self._transition_to_waiting_for_calls(tile_to_discard_with_riichi, current_player);
                            } else { return Err(PyValueError::new_err("Invalid Riichi discard")); }
                        } else { return Err(PyValueError::new_err("Cannot declare Riichi")); }
                    }
                    PlayerAction::Ankan(tile) => {
                        if self.state.make_ankan(current_player, tile).is_ok() {
                            if self.state.check_tsumo() {
                                let score = self.state.score_win(current_player, WinType::Tsumo);
                                if score.han > 0 {
                                    self.state.apply_score_transfers_and_reset_sticks(current_player, WinType::Tsumo, &score);
                                    reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                    self._populate_win_info(&info, current_player, &score, WinType::Tsumo)?;
                                }
                            } else {
                                self.current_phase = GamePhase::PlayerTurnAction;
                            }
                        } else { return Err(PyValueError::new_err("Invalid Ankan")); }
                    }
                    PlayerAction::Shouminkan(tile) => {
                        if self.state.make_shouminkan(current_player, tile).is_ok() {
                            self.current_phase = GamePhase::ProcessingShouminkanChankan;
                            self.player_who_made_shouminkan = Some(current_player);
                            self.pending_call_options = self._get_chankan_options(tile, current_player);
                            if self.pending_call_options.is_empty() {
                                self.state.perform_kan_common_actions(current_player);
                                if self.state.check_tsumo() {
                                     let score = self.state.score_win(current_player, WinType::Tsumo);
                                     if score.han > 0 {
                                        self.state.apply_score_transfers_and_reset_sticks(current_player, WinType::Tsumo, &score);
                                        reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                        self._populate_win_info(&info, current_player, &score, WinType::Tsumo)?;
                                     }
                                } else {
                                    self.current_phase = GamePhase::PlayerTurnAction;
                                }
                                self.state.is_chankan_window_open = false;
                                self.state.chankan_tile_and_declarer = None;
                                self.player_who_made_shouminkan = None;
                            } else {
                                self.state.current_player_idx = self.pending_call_options[0].0 as u8;
                            }
                        } else { return Err(PyValueError::new_err("Invalid Shouminkan")); }
                    }
                    PlayerAction::Kita => {
                        if self.state.make_kita_declaration(current_player).is_ok() {
                            if self.state.is_rinshan_kaihou_win_pending && self.state.check_tsumo() {
                                 let score = self.state.score_win(current_player, WinType::Tsumo);
                                 if score.han > 0 {
                                    self.state.apply_score_transfers_and_reset_sticks(current_player, WinType::Tsumo, &score);
                                    reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                    self._populate_win_info(&info, current_player, &score, WinType::Tsumo)?;
                                 }
                            } else {
                                self.current_phase = GamePhase::PlayerTurnAction;
                            }
                        } else { return Err(PyValueError::new_err("Invalid Kita declaration")); }
                    }
                    PlayerAction::TsumoAgari => {
                        if self.state.check_tsumo() {
                            let score = self.state.score_win(current_player, WinType::Tsumo);
                            if score.han > 0 {
                                self.state.apply_score_transfers_and_reset_sticks(current_player, WinType::Tsumo, &score);
                                reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                self._populate_win_info(&info, current_player, &score, WinType::Tsumo)?;
                            } else {
                                return Err(PyValueError::new_err("Tsumo Agari claimed but no Yaku found"));
                            }
                        } else { return Err(PyValueError::new_err("Invalid Tsumo Agari claim (hand not complete)")); }
                    }
                    _ => return Err(PyValueError::new_err(format!("Invalid action {:?} for phase PlayerTurnAction", player_action))),
                }
            }
            GamePhase::WaitingForCalls => {
                let (discarded_tile, discarder_idx_u8) = self.state.last_discarded_tile_info.expect("Missing last discard info in WaitingForCalls");
                let discarder_idx = discarder_idx_u8 as usize;
                let acting_player_idx = self.state.current_player_idx as usize;

                match player_action {
                    PlayerAction::RonAgari => {
                        if self.state.can_call_ron(acting_player_idx, discarded_tile, discarder_idx) {
                            let score = self.state.score_win(acting_player_idx, WinType::Ron { winning_tile: discarded_tile, discarder_seat: discarder_idx });
                            if score.han > 0 {
                                self.state.apply_score_transfers_and_reset_sticks(acting_player_idx, WinType::Ron { winning_tile: discarded_tile, discarder_seat: discarder_idx }, &score);
                                reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                self._populate_win_info(&info, acting_player_idx, &score, WinType::Ron { winning_tile: discarded_tile, discarder_seat: discarder_idx })?;
                            } else { return Err(PyValueError::new_err("RonAgari claimed but no Yaku found")); }
                        } else { return Err(PyValueError::new_err("Invalid RonAgari claim (conditions not met)")); }
                    }
                    PlayerAction::Pon(tile_to_pon) => {
                        if tile_to_pon == discarded_tile && self.state.can_call_pon(acting_player_idx, tile_to_pon, discarder_idx) {
                           if self.state.make_pon(acting_player_idx, tile_to_pon, discarder_idx).is_ok() {
                                self.state.current_player_idx = acting_player_idx as u8;
                                self.current_phase = GamePhase::PlayerTurnAction;
                                self.pending_call_options.clear();
                            } else { return Err(PyValueError::new_err("Failed to make Pon")); }
                        } else { return Err(PyValueError::new_err("Invalid Pon action")); }
                    }
                    PlayerAction::Daiminkan(tile_to_kan) => {
                        if tile_to_kan == discarded_tile && self.state.can_call_daiminkan(acting_player_idx, tile_to_kan, discarder_idx) {
                           if self.state.make_daiminkan(acting_player_idx, tile_to_kan, discarder_idx).is_ok() {
                                self.state.current_player_idx = acting_player_idx as u8;
                                 if self.state.check_tsumo() {
                                    let score = self.state.score_win(acting_player_idx, WinType::Tsumo);
                                     if score.han > 0 {
                                        self.state.apply_score_transfers_and_reset_sticks(acting_player_idx, WinType::Tsumo, &score);
                                        reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                        self._populate_win_info(&info, acting_player_idx, &score, WinType::Tsumo)?;
                                     }
                                } else {
                                    self.current_phase = GamePhase::PlayerTurnAction;
                                }
                                self.pending_call_options.clear();
                            } else { return Err(PyValueError::new_err("Failed to make Daiminkan")); }
                        } else { return Err(PyValueError::new_err("Invalid Daiminkan action")); }
                    }
                    PlayerAction::Pass => {
                        self.pending_call_options.retain(|(p_idx, _)| *p_idx != acting_player_idx);
                        if self.pending_call_options.is_empty() {
                            self.state.current_player_idx = (discarder_idx_u8 + 1) % 3;
                            self._env_draw_tile_for_player(self.state.current_player_idx as usize);
                            if self.state.last_drawn_tile.is_none() {
                                done = true;
                            }
                            self.current_phase = GamePhase::PlayerTurnAction;
                        } else {
                            self.state.current_player_idx = self.pending_call_options[0].0 as u8;
                        }
                    }
                    _ => return Err(PyValueError::new_err(format!("Invalid action {:?} for phase WaitingForCalls", player_action))),
                }
            }
            GamePhase::ProcessingShouminkanChankan => {
                let (chankan_tile, shouminkan_declarer_idx_u8) = self.state.chankan_tile_and_declarer.expect("Missing Chankan info");
                let shouminkan_declarer_idx = shouminkan_declarer_idx_u8 as usize;
                let acting_player_idx = self.state.current_player_idx as usize;

                match player_action {
                    PlayerAction::RonAgari => {
                        if acting_player_idx != shouminkan_declarer_idx && self.state.can_call_ron(acting_player_idx, chankan_tile, shouminkan_declarer_idx) {
                            let score = self.state.score_win(acting_player_idx, WinType::Ron { winning_tile: chankan_tile, discarder_seat: shouminkan_declarer_idx });
                            if score.han > 0 {
                                self.state.apply_score_transfers_and_reset_sticks(acting_player_idx, WinType::Ron { winning_tile: chankan_tile, discarder_seat: shouminkan_declarer_idx }, &score);
                                reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                self._populate_win_info(&info, acting_player_idx, &score, WinType::Ron { winning_tile: chankan_tile, discarder_seat: shouminkan_declarer_idx })?;
                            } else { return Err(PyValueError::new_err("Chankan RonAgari claimed but no Yaku found")); }
                        } else { return Err(PyValueError::new_err("Invalid Chankan RonAgari claim")); }
                    }
                    PlayerAction::Pass => {
                        self.pending_call_options.retain(|(p_idx, _)| *p_idx != acting_player_idx);
                        if self.pending_call_options.is_empty() {
                            let kan_player = self.player_who_made_shouminkan.expect("Missing Shouminkan player info");
                            self.state.perform_kan_common_actions(kan_player);
                            self.state.current_player_idx = kan_player as u8;
                            if self.state.check_tsumo() {
                                 let score = self.state.score_win(kan_player, WinType::Tsumo);
                                 if score.han > 0 {
                                    self.state.apply_score_transfers_and_reset_sticks(kan_player, WinType::Tsumo, &score);
                                    reward = score.points as f32; done = true; self.current_phase = GamePhase::RoundOver;
                                    self._populate_win_info(&info, kan_player, &score, WinType::Tsumo)?;
                                 }
                            } else {
                                self.current_phase = GamePhase::PlayerTurnAction;
                            }
                            self.state.is_chankan_window_open = false;
                            self.state.chankan_tile_and_declarer = None;
                            self.player_who_made_shouminkan = None;
                        } else {
                             self.state.current_player_idx = self.pending_call_options[0].0 as u8;
                        }
                    }
                     _ => return Err(PyValueError::new_err(format!("Invalid action {:?} for phase ProcessingShouminkanChankan", player_action))),
                }
            }
            GamePhase::PlayerTurnDraw => {
                let current_player_idx = self.state.current_player_idx as usize;
                self._env_draw_tile_for_player(current_player_idx);
                if self.state.last_drawn_tile.is_none() {
                    done = true;
                }
                self.current_phase = GamePhase::PlayerTurnAction;
            }
            GamePhase::RoundOver => { /* Handled earlier */ }
        }

        if !done {
            if self.state.four_kan_abortive_draw_pending &&
               self.state.player_who_declared_fourth_kan == Some(self.state.current_player_idx) &&
               self.current_phase == GamePhase::WaitingForCalls && 
               self.pending_call_options.iter().all(|(_, actions)| !actions.iter().any(|a| matches!(a, PlayerAction::RonAgari)))
            {
                info.set_item("draw_type", "four_kan_abortive")?;
                done = true; self.current_phase = GamePhase::RoundOver;
            }

            let is_wall_effectively_empty = self.state.wall.is_live_wall_empty() &&
                                           (self.current_phase != GamePhase::PlayerTurnDraw &&
                                            !(self.state.is_rinshan_kaihou_win_pending && self.state.wall.rinshanpai_drawn_count() < 4) &&
                                            !(self.state.is_chankan_window_open));

            if is_wall_effectively_empty {
                let mut nagashi_mangan_achieved_by: Option<usize> = None;
                if !self.state.discards.iter().all(|d| d.is_empty()) {
                    for p_idx in 0..3 {
                        if self.state.discards[p_idx].is_empty() { continue; }
                        let all_discards_are_terminals_or_honors = self.state.discards[p_idx]
                            .iter()
                            .all(|&tile| tile.is_terminal_or_honor());

                        if all_discards_are_terminals_or_honors && !self.state.any_discard_called_this_round[p_idx] {
                            nagashi_mangan_achieved_by = Some(p_idx);
                            break;
                        }
                    }
                }

                if let Some(winner_idx) = nagashi_mangan_achieved_by {
                    info.set_item("draw_type", "nagashi_mangan")?;
                    info.set_item("nagashi_mangan_winner", winner_idx)?;
                    let is_dealer = winner_idx == self.state.dealer_idx as usize;
                    let mut mangan_value = if is_dealer { 12000 } else { 8000 };
                    mangan_value += self.state.honba_sticks as u32 * 300;
                    let collected_riichi_value = self.state.riichi_sticks as u32 * 1000;
                    self.state.player_scores[winner_idx] += collected_riichi_value as i32;
                    self.state.riichi_sticks = 0;
                    info.set_item("nagashi_mangan_total_payout", mangan_value)?;
                    info.set_item("collected_riichi_sticks_value", collected_riichi_value)?;
                    self.state.player_scores[winner_idx] += mangan_value as i32;
                    if is_dealer {
                        let payment_from_each_non_dealer = mangan_value / 2;
                        for i in 0..3 { if i != winner_idx { self.state.player_scores[i] -= payment_from_each_non_dealer as i32; } }
                    } else {
                        let payment_from_dealer = mangan_value / 2;
                        let payment_from_other_non_dealer = mangan_value - payment_from_dealer;
                        for i in 0..3 {
                            if i == self.state.dealer_idx as usize && i != winner_idx { self.state.player_scores[i] -= payment_from_dealer as i32; }
                            else if i != winner_idx && i != self.state.dealer_idx as usize { self.state.player_scores[i] -= payment_from_other_non_dealer as i32; }
                        }
                    }
                    for p_idx_info in 0..3 { info.set_item(format!("player_{}_score_after_nagashi", p_idx_info), self.state.player_scores[p_idx_info])?; }
                    reward = mangan_value as f32;
                } else {
                    info.set_item("draw_type", "exhaustive_draw_ryuukyoku")?;
                }
                done = true;
                self.current_phase = GamePhase::RoundOver;
            }
        }

        let (obs_py, legal_actions_py) = self.get_obs_and_legal_actions(py);
        info.set_item("legal_actions_mask", legal_actions_py)?;
        info.set_item("current_phase", format!("{:?}", self.current_phase))?;
        info.set_item("current_player_for_action", self.state.current_player_idx)?;
        if let Some((last_discard_tile, last_discarder)) = self.state.last_discarded_tile_info {
            info.set_item("last_discarded_tile_val", last_discard_tile as u8)?;
            info.set_item("last_discarder_idx", last_discarder)?;
        }
         if let Some(last_drawn_tile) = self.state.last_drawn_tile {
            info.set_item("last_drawn_tile_for_current_player_val", last_drawn_tile as u8)?;
        }
        info.set_item("riichi_sticks_on_table", self.state.riichi_sticks)?;
        info.set_item("honba_sticks_active", self.state.honba_sticks)?;
        for i in 0..3 {
            info.set_item(format!("player_{}_score", i), self.state.player_scores[i])?;
            info.set_item(format!("player_{}_riichi_declared", i), self.state.riichi_declared[i])?;
        }

        Ok((obs_py, reward, done, info.unbind()))
    }
    
    fn current_player_idx_py(&self) -> usize { self.state.current_player_idx as usize }

    fn get_hand_tiles_pystr(&self, player_idx: usize) -> PyResult<String> {
        if player_idx >= 3 { return Err(PyIndexError::new_err("Player index out of range")); }
        Ok(self.state.hands[player_idx].get_all_tiles().iter().map(|t| Tile::to_unicode(*t).to_string()).collect::<Vec<String>>().join(" "))
    }
	
	fn get_last_drawn_tile_for_current_player_val(&self) -> Option<u8> {
        if self.state.current_player_idx as usize == self.state.current_player_idx as usize {
             self.state.last_drawn_tile.map(|t| t as u8)
        } else {
            None
        }
    }
	
	fn get_hand_size(&self, player_idx: usize) -> PyResult<usize> {
        if player_idx >= 3 {
            return Err(PyIndexError::new_err("Player index out of range"));
        }
        Ok(self.state.hands[player_idx].get_all_tiles().len())
    }

    fn get_current_dora_indicators_pystr(&self) -> String {
        self.state.current_dora_indicators.iter().map(|t| Tile::to_unicode(*t).to_string()).collect::<Vec<String>>().join(" ")
    }
    
    fn get_current_ura_dora_indicators_pystr(&self) -> String {
		self.state.current_ura_dora_indicators.iter().map(|t| Tile::to_unicode(*t).to_string()).collect::<Vec<String>>().join(" ")
    }
		
	fn get_game_phase_pystr(&self) -> String { format!("{:?}", self.current_phase) }

    fn get_wall_live_remaining_py(&self) -> usize { self.state.wall.live_wall_remaining_count() }
    fn get_wall_dead_rinshan_drawn_py(&self) -> u8 { self.state.wall.rinshanpai_drawn_count() }

    fn get_player_open_melds_pystr(&self, player_idx: usize) -> PyResult<String> {
        if player_idx >= 3 { return Err(PyIndexError::new_err("Player index out of range")); }
        let mut meld_strs = Vec::new();
        for meld in &self.state.open_melds[player_idx] {
            let type_str = match meld.meld_type {
                DeclaredMeldType::Pon => "Pon",
                DeclaredMeldType::Ankan => "Ankan",
                DeclaredMeldType::Daiminkan => "Daiminkan",
                DeclaredMeldType::Shouminkan => "Shouminkan",
                DeclaredMeldType::Kita => "Kita",
            };
            let tiles_str = if meld.meld_type == DeclaredMeldType::Kita {
                Tile::North.to_unicode().to_string()
            } else {
                (0..(if meld.meld_type == DeclaredMeldType::Pon {3} else {4}))
                    .map(|_| Tile::to_unicode(meld.tiles[0]).to_string())
                    .collect::<Vec<String>>().join("")
            };
            meld_strs.push(format!("{}({})", type_str, tiles_str));
        }
        Ok(meld_strs.join(", "))
    }

    fn get_player_discards_pystr(&self, player_idx: usize) -> PyResult<String> {
        if player_idx >= 3 { return Err(PyIndexError::new_err("Player index out of range")); }
        Ok(self.state.discards[player_idx].iter().map(|t| Tile::to_unicode(*t).to_string()).collect::<Vec<String>>().join(" "))
    }

    pub fn get_obs_and_legal_actions<'py>(
        &self,
        py: Python<'py>,
    ) -> (Py<PyArray<u8, Ix3>>, Py<PyArray1<bool>>) {
        let obs_array_bound = PyArray::zeros_bound(py, OBS_SHAPE, false);

        unsafe {
            let mut obs = obs_array_bound.as_array_mut();
            let current_player_idx = self.state.current_player_idx as usize;
            let mut cursor: usize = 0;

            // Discards history (12 planes)
            for i in 0..3 {
                let player_to_observe = (current_player_idx + i) % 3;
                let discard_pile = &self.state.discards[player_to_observe];
                let history_depth = 4;
                let start = discard_pile.len().saturating_sub(history_depth);
                let last_discards = &discard_pile[start..];

                for j in 0..history_depth {
                    let mut plane = obs.slice_mut(s![cursor + j, .., ..]);
                    if let Some(tile) = last_discards.get(j) {
                        let tile_idx = *tile as usize;
                        if tile_idx < 34 {
                            let row = tile_idx / 7;
                            let col = tile_idx % 5;
                            plane[[row, col]] = 1;
                        }
                    }
                }
                cursor += history_depth;
            }
            
            // Player hand (4 planes for counts)
            let hand = &self.state.hands[current_player_idx];
            for (tile, count) in hand.iter() {
                if count > 0 && count <= 4 {
                    let tile_idx = tile as usize;
                    if tile_idx < 34 {
                        let mut plane = obs.slice_mut(s![cursor + (count - 1) as usize, .., ..]);
                        let row = tile_idx / 7;
                        let col = tile_idx % 5;
                        plane[[row, col]] = 1;
                    }
                }
            }
            cursor += 4;

            // Dora indicators (1 plane)
            {
                let mut plane = obs.slice_mut(s![cursor, .., ..]);
                for &indicator in &self.state.current_dora_indicators {
                    let tile_idx = indicator as usize;
                    if tile_idx < 34 {
                        let row = tile_idx / 7;
                        let col = tile_idx % 5;
                        plane[[row, col]] = 1;
                    }
                }
            }
            cursor += 1;
            
            // Kita count (1 plane)
            {
                let mut plane = obs.slice_mut(s![cursor, .., ..]);
                plane[[0,0]] = self.state.kita_declared_count[current_player_idx];
            }
            cursor += 1;

            // Round Wind (1 plane)
            {
                let mut plane = obs.slice_mut(s![cursor, .., ..]);
                plane[[0, 0]] = self.state.round_wind as u8;
            }
            cursor += 1;

            // Seat Winds (3 planes)
            for i in 0..3 {
                let player_to_observe = (current_player_idx + i) % 3;
                let mut plane = obs.slice_mut(s![cursor + i, .., ..]);
                plane[[0, 0]] = self.state.seat_winds[player_to_observe] as u8;
            }
            cursor += 3;

            // Scores & Sticks (1 plane)
            {
                let mut plane = obs.slice_mut(s![cursor, .., ..]);
                for i in 0..3 {
                    let player_to_observe = (current_player_idx + i) % 3;
                    plane[[0, i]] = (self.state.player_scores[player_to_observe] / 1000) as u8;
                }
                plane[[1, 0]] = self.state.riichi_sticks;
                plane[[1, 1]] = self.state.honba_sticks;
            }
            cursor += 1;

            // Riichi Declared Status (1 plane)
            {
                let mut plane = obs.slice_mut(s![cursor, .., ..]);
                for i in 0..3 {
                    let player_to_observe = (current_player_idx + i) % 3;
                    plane[[0, i]] = self.state.riichi_declared[player_to_observe] as u8;
                }
            }
            cursor += 1;

            // Open Melds (12 planes)
            for i in 0..3 {
                let player_to_observe = (current_player_idx + i) % 3;
                let open_melds = &self.state.open_melds[player_to_observe];

                for j in 0..4 {
                    let plane_idx = cursor + (i * 4) + j;
                    let mut plane = obs.slice_mut(s![plane_idx, .., ..]);
                    if let Some(meld) = open_melds.get(j) {
                        plane[[0, 0]] = meld.meld_type as u8;
                        plane[[0, 1]] = meld.tiles[0] as u8;
                        plane[[0, 2]] = meld.called_from_discarder_idx.unwrap_or(3);
                    }
                }
            }
            cursor += 12;
            let _ = cursor;
        }

        let legal_actions_vec = self._get_legal_actions_mask_vec();
        let legal_actions_pyarray = PyArray1::from_vec_bound(py, legal_actions_vec);
        (obs_array_bound.unbind(), legal_actions_pyarray.unbind())
    }
}

// --- Private helper functions ---
impl Env {
	
	fn _env_draw_tile_for_player(&mut self, player_idx: usize) {
        let forced_draw = self.draw_queues[player_idx].pop_front();
        self.state.player_draws_tile(forced_draw);
    }
	
    fn _populate_win_info(&self, info: &Bound<PyDict>, winner_idx: usize, score: &Score, win_type: WinType) -> PyResult<()> {
        info.set_item("winner", winner_idx)?;
        info.set_item("win_type", format!("{:?}", win_type))?;
        info.set_item("han", score.han)?;
        info.set_item("fu", score.fu)?;
        info.set_item("score_value_awarded", score.points)?;
        let yaku_strs: Vec<String> = score
            .yaku_details
            .iter()
            .map(|(name, val)| format!("{}: {} han", name, val))
            .collect();
        info.set_item("yaku_list", yaku_strs)?;
        Ok(())
    }

    fn _transition_to_waiting_for_calls(&mut self, discarded_tile: Tile, discarder_idx: usize) {
        self.pending_call_options = self._get_call_options_for_discard(discarded_tile, discarder_idx);
        if self.pending_call_options.is_empty() {
            self.state.current_player_idx = ((discarder_idx + 1) % 3) as u8;
            self._env_draw_tile_for_player(self.state.current_player_idx as usize);
            self.current_phase = GamePhase::PlayerTurnAction;
        } else {
            self.state.current_player_idx = self.pending_call_options[0].0 as u8;
            self.current_phase = GamePhase::WaitingForCalls;
        }
    }

    fn _decode_action(&self, action_id: u8) -> Result<PlayerAction, String> {
        match action_id {
            id if (ACTION_ID_DISCARD_START..=ACTION_ID_DISCARD_END).contains(&id) => {
                Tile::try_from(id - ACTION_ID_DISCARD_START)
                    .map(PlayerAction::Discard)
                    .map_err(|_| "Invalid tile ID for discard".to_string())
            }
            id if (ACTION_ID_RIICHI_DISCARD_START..=ACTION_ID_RIICHI_DISCARD_END).contains(&id) => {
                Tile::try_from(id - ACTION_ID_RIICHI_DISCARD_START)
                    .map(PlayerAction::RiichiDeclare)
                    .map_err(|_| "Invalid tile ID for Riichi discard".to_string())
            }
            id if (ACTION_ID_ANKAN_START..=ACTION_ID_ANKAN_END).contains(&id) => {
                Tile::try_from(id - ACTION_ID_ANKAN_START)
                    .map(PlayerAction::Ankan)
                    .map_err(|_| "Invalid tile ID for Ankan".to_string())
            }
            id if (ACTION_ID_SHOUMINKAN_START..=ACTION_ID_SHOUMINKAN_END).contains(&id) => {
                Tile::try_from(id - ACTION_ID_SHOUMINKAN_START)
                    .map(PlayerAction::Shouminkan)
                    .map_err(|_| "Invalid tile ID for Shouminkan".to_string())
            }
            ACTION_ID_KITA => Ok(PlayerAction::Kita),
            ACTION_ID_TSUMO_AGARI => Ok(PlayerAction::TsumoAgari),
            ACTION_ID_RON_AGARI => Ok(PlayerAction::RonAgari),
            ACTION_ID_PON => {
                let (tile, _) = self
                    .state
                    .last_discarded_tile_info
                    .ok_or_else(|| "Cannot decode Pon: No last discarded tile info".to_string())?;
                Ok(PlayerAction::Pon(tile))
            }
            ACTION_ID_DAIMINKAN => {
                let (tile, _) = self
                    .state
                    .last_discarded_tile_info
                    .ok_or_else(|| "Cannot decode Daiminkan: No last discarded tile info".to_string())?;
                Ok(PlayerAction::Daiminkan(tile))
            }
            ACTION_ID_PASS => Ok(PlayerAction::Pass),
            _ => Err(format!("Unmapped action_id: {}", action_id)),
        }
    }

    fn _player_action_to_id(&self, action: PlayerAction) -> Option<u8> {
        match action {
            PlayerAction::Discard(tile) => Some(ACTION_ID_DISCARD_START + tile as u8),
            PlayerAction::RiichiDeclare(tile) => Some(ACTION_ID_RIICHI_DISCARD_START + tile as u8),
            PlayerAction::Ankan(tile) => Some(ACTION_ID_ANKAN_START + tile as u8),
            PlayerAction::Shouminkan(tile) => Some(ACTION_ID_SHOUMINKAN_START + tile as u8),
            PlayerAction::Kita => Some(ACTION_ID_KITA),
            PlayerAction::TsumoAgari => Some(ACTION_ID_TSUMO_AGARI),
            PlayerAction::RonAgari => Some(ACTION_ID_RON_AGARI),
            PlayerAction::Pon(_) => Some(ACTION_ID_PON),
            PlayerAction::Daiminkan(_) => Some(ACTION_ID_DAIMINKAN),
            PlayerAction::Pass => Some(ACTION_ID_PASS),
        }
    }

    fn _get_legal_actions_mask_vec(&self) -> Vec<bool> {
        let mut legal_actions_vec = vec![false; ACTION_SPACE_SIZE];
        let player_idx = self.state.current_player_idx as usize;

        match self.current_phase {
            GamePhase::PlayerTurnAction => {
                for (tile_in_hand, _count) in self.state.hands[player_idx].iter() {
                    if let Some(id) = self._player_action_to_id(PlayerAction::Discard(tile_in_hand)) {
                        legal_actions_vec[id as usize] = true;
                    }
                }
                if self.state.can_declare_riichi(player_idx) {
                    for (tile_in_hand, _count) in self.state.hands[player_idx].iter() {
                        // A riichi declaration must be followed by a discard.
                        // We create a unique action ID for each potential Riichi-discard.
                        if let Some(id) = self._player_action_to_id(PlayerAction::RiichiDeclare(tile_in_hand)) {
                            legal_actions_vec[id as usize] = true;
                        }
                    }
                }
                if let Some(possible_ankans) = self.state.get_possible_ankans(player_idx) {
                    for tile in possible_ankans {
                        if let Some(id) = self._player_action_to_id(PlayerAction::Ankan(tile)) {
                            legal_actions_vec[id as usize] = true;
                        }
                    }
                }
                if let Some(possible_shouminkans) = self.state.get_possible_shouminkans(player_idx) {
                    for tile in possible_shouminkans {
                        if let Some(id) = self._player_action_to_id(PlayerAction::Shouminkan(tile)) {
                            legal_actions_vec[id as usize] = true;
                        }
                    }
                }
                if self.state.can_declare_kita_action(player_idx) {
                    if let Some(id) = self._player_action_to_id(PlayerAction::Kita) {
                        legal_actions_vec[id as usize] = true;
                    }
                }
                if self.state.check_tsumo() {
                    if let Some(id) = self._player_action_to_id(PlayerAction::TsumoAgari) {
                        legal_actions_vec[id as usize] = true;
                    }
                }
            }
            GamePhase::WaitingForCalls | GamePhase::ProcessingShouminkanChankan => {
                if let Some((_, actions)) = self.pending_call_options.iter().find(|(p_idx, _)| *p_idx == player_idx) {
                    for action in actions {
                        if let Some(id) = self._player_action_to_id(*action) {
                            legal_actions_vec[id as usize] = true;
                        }
                    }
                }
                // Always allow passing on a call
                legal_actions_vec[ACTION_ID_PASS as usize] = true;
            }
            GamePhase::PlayerTurnDraw | GamePhase::RoundOver => {
                // In these phases, the environment should transition automatically,
                // but we can provide PASS as a legal "no-op" from the agent's perspective.
                legal_actions_vec[ACTION_ID_PASS as usize] = true;
            }
        }

        // If for some reason no other action is legal (should be rare), make PASS legal.
        if legal_actions_vec.iter().all(|&x| !x) && self.current_phase != GamePhase::RoundOver {
            legal_actions_vec[ACTION_ID_PASS as usize] = true;
        }
        
        legal_actions_vec
    }

    fn _get_call_options_for_discard(
        &self,
        discarded_tile: Tile,
        discarder_idx: usize,
    ) -> Vec<(usize, Vec<PlayerAction>)> {
        let mut all_options: Vec<(usize, Vec<PlayerAction>)> = Vec::new();
        for p_idx_offset in 1..3 {
            let p_idx = (discarder_idx + p_idx_offset) % 3;
            let mut player_specific_options = Vec::new();
            if self.state.can_call_ron(p_idx, discarded_tile, discarder_idx) {
                player_specific_options.push(PlayerAction::RonAgari);
            }
            if self
                .state
                .can_call_daiminkan(p_idx, discarded_tile, discarder_idx)
            {
                player_specific_options.push(PlayerAction::Daiminkan(discarded_tile));
            }
            if self.state.can_call_pon(p_idx, discarded_tile, discarder_idx) {
                player_specific_options.push(PlayerAction::Pon(discarded_tile));
            }
            if !player_specific_options.is_empty() {
                all_options.push((p_idx, player_specific_options));
            }
        }
        all_options.sort_by(|(p_idx_a, actions_a), (p_idx_b, actions_b)| {
            let prio_a = actions_a.iter().any(|a| matches!(a, PlayerAction::RonAgari));
            let prio_b = actions_b.iter().any(|a| matches!(a, PlayerAction::RonAgari));
            if prio_a != prio_b {
                return prio_b.cmp(&prio_a);
            }
            let kan_a = actions_a.iter().any(|a| matches!(a, PlayerAction::Daiminkan(_)));
            let kan_b = actions_b.iter().any(|a| matches!(a, PlayerAction::Daiminkan(_)));
            if kan_a != kan_b {
                return kan_b.cmp(&kan_a);
            }
            let order_a = (p_idx_a + 3 - discarder_idx) % 3;
            let order_b = (p_idx_b + 3 - discarder_idx) % 3;
            order_a.cmp(&order_b)
        });
        all_options
    }

    fn _get_chankan_options(
        &self,
        chankan_tile: Tile,
        shouminkan_declarer_idx: usize,
    ) -> Vec<(usize, Vec<PlayerAction>)> {
        let mut chankan_options = Vec::new();
        for p_idx in 0..3 {
            if p_idx == shouminkan_declarer_idx {
                continue;
            }
            if self
                .state
                .can_call_ron(p_idx, chankan_tile, shouminkan_declarer_idx)
            {
                chankan_options.push((p_idx, vec![PlayerAction::RonAgari]));
            }
        }
        chankan_options.sort_by(|(p_idx_a, _), (p_idx_b, _)| {
            let order_a = (p_idx_a + 3 - shouminkan_declarer_idx) % 3;
            let order_b = (p_idx_b + 3 - shouminkan_declarer_idx) % 3;
            order_a.cmp(&order_b)
        });
        chankan_options
    }
}

#[pymodule]
fn sanma_engine(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<Env>()?;
    m.add_class::<Tile>()?;

    // Manually add each tile as a constant to the sanma_engine module
    m.add("Man1", Tile::Man1)?;
    m.add("Man2", Tile::Man2)?;
    m.add("Man3", Tile::Man3)?;
    m.add("Man4", Tile::Man4)?;
    m.add("Man5", Tile::Man5)?;
    m.add("Man6", Tile::Man6)?;
    m.add("Man7", Tile::Man7)?;
    m.add("Man8", Tile::Man8)?;
    m.add("Man9", Tile::Man9)?;

    m.add("Pin1", Tile::Pin1)?;
    m.add("Pin2", Tile::Pin2)?;
    m.add("Pin3", Tile::Pin3)?;
    m.add("Pin4", Tile::Pin4)?;
    m.add("Pin5", Tile::Pin5)?;
    m.add("Pin6", Tile::Pin6)?;
    m.add("Pin7", Tile::Pin7)?;
    m.add("Pin8", Tile::Pin8)?;
    m.add("Pin9", Tile::Pin9)?;

    m.add("Sou1", Tile::Sou1)?;
    m.add("Sou2", Tile::Sou2)?;
    m.add("Sou3", Tile::Sou3)?;
    m.add("Sou4", Tile::Sou4)?;
    m.add("Sou5", Tile::Sou5)?;
    m.add("Sou6", Tile::Sou6)?;
    m.add("Sou7", Tile::Sou7)?;
    m.add("Sou8", Tile::Sou8)?;
    m.add("Sou9", Tile::Sou9)?;

    m.add("East", Tile::East)?;
    m.add("South", Tile::South)?;
    m.add("West", Tile::West)?;
    m.add("North", Tile::North)?;

    m.add("White", Tile::White)?;
    m.add("Green", Tile::Green)?;
    m.add("Red", Tile::Red)?;

    Ok(())
}
