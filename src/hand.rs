use crate::tiles::Tile;
use std::vec::Vec; // Ensure Vec is imported

/// Bit-packed hand: 3 bits per tile kind (0–7 copies is enough)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Hand(u128);

impl Hand {
    pub fn add(&mut self, tile: Tile) {
        let idx = tile as u8;
        let shift = idx * 3;
        let mask = 0b111u128 << shift;
        let count = ((self.0 & mask) >> shift) + 1;
        // assert!(count <= 4, "too many copies of {:?} in hand", tile); // Standard Mahjong rule
        // For game setup or special cases, this assert might be too strict if a hand can temporarily hold >4
        // For general play, it's a good sanity check. Let's assume it's okay for now.
        if count > 4 {
            // Optionally handle this error more gracefully, e.g., by not adding or returning a Result
            // For now, we'll keep the assert but note that robust error handling might be needed.
             panic!("Cannot add more than 4 copies of {:?} to hand", tile);
        }
        self.0 = (self.0 & !mask) | (count << shift);
    }

    pub fn count(&self, tile: Tile) -> u8 {
        let shift = (tile as u8) * 3;
        ((self.0 >> shift) & 0b111) as u8
    }

    /// Removes a single instance of the specified tile.
    /// Returns true if the tile was present and removed, false otherwise.
    pub fn remove(&mut self, tile: Tile) -> bool {
        let shift = (tile as u8) * 3;
        let mask  = 0b111u128 << shift;
        let count = (self.0 & mask) >> shift;
        if count == 0 { return false; }
        self.0 = (self.0 & !mask) | ((count - 1) << shift);
        true
    }

    /// Removes `n` instances of the specified tile.
    /// Returns `Ok(())` on success, or `Err(&'static str)` if not enough tiles.
    pub fn remove_n(&mut self, tile: Tile, n: u8) -> Result<(), &'static str> {
        if self.count(tile) < n {
            return Err("Not enough tiles in hand to remove specified count");
        }
        for _ in 0..n {
            if !self.remove(tile) {
                // This should not happen if the initial count check passed
                return Err("Inconsistent state during remove_n");
            }
        }
        Ok(())
    }

    pub fn iter(&self) -> impl Iterator<Item = (Tile, u8)> + '_ {
        (0u8..=33).filter_map(move |i| {
            // Ensure Tile::North (33) is the max index
            if i > Tile::North as u8 { return None; }
            let t = Tile::try_from(i).ok()?; // Use ok? to convert Result to Option
            let c = self.count(t);
            (c > 0).then_some((t, c))
        })
    }

    /// Returns all tiles in the hand as a Vec<Tile>.
    /// Each tile instance is listed individually.
    pub fn get_all_tiles(&self) -> Vec<Tile> {
        let mut tiles_vec = Vec::new();
        for (tile, count) in self.iter() {
            for _ in 0..count {
                tiles_vec.push(tile);
            }
        }
        tiles_vec
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*; // Assuming Tile enum is in scope

    #[test]
    fn add_and_count() {
        let mut h = Hand::default();
        h.add(Man5);
        h.add(Man5);
        assert_eq!(h.count(Man5), 2);
        h.add(Green);
        assert_eq!(h.count(Green), 1);
    }

    #[test]
    #[should_panic]
    fn add_too_many_tiles() {
        let mut h = Hand::default();
        h.add(Man1); h.add(Man1); h.add(Man1); h.add(Man1);
        h.add(Man1); // Fifth Man1 should panic
    }

    #[test]
    fn remove_and_iter() {
        let mut h = Hand::default();
        h.add(Red);
        h.add(Red);
        assert_eq!(h.count(Red), 2);
        assert!(h.remove(Red));
        assert_eq!(h.count(Red), 1);
        assert!(h.remove(Red));
        assert!(!h.remove(Red));
        assert_eq!(h.iter().count(), 0);
    }

    #[test]
    fn test_remove_n() {
        let mut h = Hand::default();
        h.add(Man1); h.add(Man1); h.add(Man1);
        h.add(Pin2);

        assert!(h.remove_n(Man1, 2).is_ok());
        assert_eq!(h.count(Man1), 1);
        assert_eq!(h.count(Pin2), 1);

        assert!(h.remove_n(Man1, 1).is_ok());
        assert_eq!(h.count(Man1), 0);

        assert!(h.remove_n(Man1, 1).is_err()); // Not enough Man1
        assert!(h.remove_n(Pin2, 2).is_err()); // Not enough Pin2
    }

    #[test]
    fn test_get_all_tiles() {
        let mut h = Hand::default();
        h.add(Man1); h.add(Man1);
        h.add(Pin5);
        h.add(East);

        let mut tiles = h.get_all_tiles();
        tiles.sort_unstable(); // Sort for consistent comparison

        let mut expected = vec![Man1, Man1, Pin5, East];
        expected.sort_unstable();

        assert_eq!(tiles, expected);

        let h_empty = Hand::default();
        assert!(h_empty.get_all_tiles().is_empty());
    }
}
