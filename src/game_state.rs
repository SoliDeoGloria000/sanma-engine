use crate::{Hand, Tile, Wall};

/// Type of win: self-draw (Tsumo) or Ron (discard win, with discarder index)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WinType {
    Tsumo,
    Ron(usize),
}

pub struct GameState {
    pub wall: Wall,
    pub hands: [Hand; 3],
    pub turn: u8,          // 0..2
}

impl GameState {
    /// Initialize game: deal 13 tiles each
    pub fn new(seed: u64) -> Self {
        let mut wall = Wall::new(seed);
        let mut hands = [Hand::default(), Hand::default(), Hand::default()];

        for _ in 0..13 {
            for seat in 0..3 {
                if let Some(t) = wall.draw() {
                    hands[seat].add(t);
                }
            }
        }

        Self { wall, hands, turn: 0 }
    }

    /// Advance to next player
    pub fn next_turn(&mut self) {
        self.turn = (self.turn + 1) % 3;
    }

    /// Current player draws a tile
    pub fn draw_for_current(&mut self) -> Option<Tile> {
        self.wall.draw().map(|t| {
            self.hands[self.turn as usize].add(t);
            t
        })
    }

    /// Current player discards a tile from their hand
    pub fn discard_current(&mut self, tile: Tile) -> bool {
        self.hands[self.turn as usize].remove(tile)
    }

    /// Check if the current player's hand is a winning hand by tsumo
    pub fn check_tsumo(&self) -> bool {
        // TODO: implement Riichi hand pattern matching
        false
    }

    /// Check if a win occurs by ron on the last discard
    pub fn check_ron(&self, tile: Tile, discarder: usize) -> bool {
        // TODO: implement reaction to last discard
        false
    }

    /// Score a winning hand (points), given the winner and win type
    pub fn score_win(&self, seat: usize, win_type: WinType) -> u32 {
        // TODO: compute han, fu, and point calculation
        0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn initial_hand_size() {
        let gs = GameState::new(7);
        let count0: usize = gs.hands[0]
            .iter()
            .map(|(_, c)| c as usize)
            .sum();
        assert_eq!(count0, 13);
    }

    #[test]
    fn discard_works() {
        let mut gs = GameState::new(42);
        if let Some(tile) = gs.draw_for_current() {
            assert!(gs.hands[0].iter().any(|(t, _)| t == tile));
            let before = gs.hands[0].iter().count();
            assert!(gs.discard_current(tile));
            let after = gs.hands[0].iter().count();
            assert_eq!(after, before - 1);
        } else {
            panic!("Expected draw to succeed");
        }
    }

    #[test]
    fn check_tsumo_defaults_to_false() {
        let gs = GameState::new(0);
        assert!(!gs.check_tsumo());
    }

    #[test]
    fn check_ron_defaults_to_false() {
        let gs = GameState::new(0);
        assert!(!gs.check_ron(Tile::Man1, 0));
    }
}
