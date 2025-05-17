use pyo3::prelude::*;
use numpy::{IntoPyArray, PyArray1};
use pyo3::exceptions::{PyIndexError, PyValueError};

mod tiles;
mod hand;
mod wall;
mod game_state;

pub use tiles::Tile;
pub use hand::Hand;
pub use wall::Wall;
pub use game_state::{GameState, WinType};

#[pyclass]
struct Env {
    state: GameState,
}

#[pymethods]
impl Env {
    #[new]
    fn new(seed: u64) -> Self {
        Self { state: GameState::new(seed) }
    }

    fn reset<'py>(&mut self, py: Python<'py>) -> &'py PyArray1<u8> {
        *self = Self::new(0);
        self._obs(py)
    }

    fn step<'py>(
        &mut self,
        action: u8,
        py: Python<'py>,
    ) -> PyResult<(&'py PyArray1<u8>, f32, bool)> {
        // 1. Interpret action as a Tile
        let tile = Tile::try_from(action)
            .map_err(|_| PyValueError::new_err("Invalid tile action"))?;

        // 2. Discard
        if !self.state.discard_current(tile) {
            return Err(PyValueError::new_err("Invalid discard"));
        }

        // 3. Check for Ron
        for seat in 0..3 {
            if seat != self.state.turn as usize {
                if self.state.check_ron(tile, self.state.turn as usize) {
                    // pass the actual tile into Ron(), not a usize
                    let score = self.state.score_win(seat, WinType::Ron(tile));
                    let reward = score.points as f32;
                    return Ok((self._obs(py), reward, true));
                }
            }
        }

        // 4. Check for Tsumo
        if self.state.check_tsumo() {
            let score = self.state.score_win(self.state.turn as usize, WinType::Tsumo);
            let reward = score.points as f32;
            return Ok((self._obs(py), reward, true));
        }

        // 5. Advance turn and draw
        self.state.next_turn();
        self.state.draw_for_current();

        Ok((self._obs(py), 0.0, false))
    }

    /// Helper: build observation tensor; encode current turn in slot[0]
    fn _obs<'py>(&self, py: Python<'py>) -> &'py PyArray1<u8> {
        let vec = vec![self.state.turn];
        vec.into_pyarray(py)
    }

    /// Return the number of tiles in hand `seat` (0, 1, or 2).
    fn hand_size(&self, seat: usize) -> PyResult<usize> {
        if seat >= 3 {
            Err(PyIndexError::new_err("seat index out of range"))
        } else {
            let count = self
                .state
                .hands[seat]
                .iter()
                .map(|(_, c)| c as usize)
                .sum();
            Ok(count)
        }
    }
}

#[pymodule]
fn sanma_engine(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_class::<Env>()?;
    Ok(())
}
