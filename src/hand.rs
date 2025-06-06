use crate::tiles::Tile;
use std::vec::Vec; // Ensure Vec is imported
use std::fmt;

/// Error type for operations on a `Hand`.
#[derive(Debug, Clone)]
pub enum HandError {
    /// Generic error with a static message.
    Generic(&'static str),
}

impl fmt::Display for HandError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HandError::Generic(msg) => write!(f, "{}", msg),
        }
    }
}

impl std::error::Error for HandError {}

impl From<&'static str> for HandError {
    fn from(s: &'static str) -> Self {
        HandError::Generic(s)
    }
}

/// Bit-packed hand: 3 bits per tile kind (0–7 copies is enough)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Hand(u128);

impl Hand {
    pub fn add(&mut self, tile: Tile) -> Result<(), HandError> {
        let idx = tile as u8;
        let shift = idx * 3;
        let mask = 0b111u128 << shift;
        let count = ((self.0 & mask) >> shift) + 1;
        // assert!(count <= 4, "too many copies of {:?} in hand", tile); // Standard Mahjong rule
        // For game setup or special cases, this assert might be too strict if a hand can temporarily hold >4
        // For general play, it's a good sanity check. Let's assume it's okay for now.
        if count > 4 {
            return Err(HandError::Generic("Cannot add more than 4 copies of tile to hand"));
        }
        self.0 = (self.0 & !mask) | (count << shift);
        Ok(())
    }

    pub fn count(&self, tile: Tile) -> u8 {
        let shift = (tile as u8) * 3;
        ((self.0 >> shift) & 0b111) as u8
    }

    /// Removes a single instance of the specified tile.
    /// Returns true if the tile was present and removed, false otherwise.
    pub fn remove(&mut self, tile: Tile) -> Result<(), HandError> {
        let shift = (tile as u8) * 3;
        let mask  = 0b111u128 << shift;
        let count = (self.0 & mask) >> shift;
        if count == 0 {
            return Err(HandError::Generic("Tile not in hand"));
        }
        self.0 = (self.0 & !mask) | ((count - 1) << shift);
        Ok(())
    }

    /// Removes `n` instances of the specified tile.
    /// Returns `Ok(())` on success, or `Err(&'static str)` if not enough tiles.
    pub fn remove_n(&mut self, tile: Tile, n: u8) -> Result<(), HandError> {
        if self.count(tile) < n {
            return Err(HandError::Generic("Not enough tiles in hand to remove specified count"));
        }
        for _ in 0..n {
            self.remove(tile)?;
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
        h.add(Man5).unwrap();
        h.add(Man5).unwrap();
        assert_eq!(h.count(Man5), 2);
        h.add(Green).unwrap();
        assert_eq!(h.count(Green), 1);
    }

    #[test]
    fn add_too_many_tiles() {
        let mut h = Hand::default();
        assert!(h.add(Man1).is_ok());
        assert!(h.add(Man1).is_ok());
        assert!(h.add(Man1).is_ok());
        assert!(h.add(Man1).is_ok());
        assert!(h.add(Man1).is_err()); // Fifth Man1 should error
    }

    #[test]
    fn remove_and_iter() {
        let mut h = Hand::default();
        h.add(Red).unwrap();
        h.add(Red).unwrap();
        assert_eq!(h.count(Red), 2);
        assert!(h.remove(Red).is_ok());
        assert_eq!(h.count(Red), 1);
        assert!(h.remove(Red).is_ok());
        assert!(h.remove(Red).is_err());
        assert_eq!(h.iter().count(), 0);
    }

    #[test]
    fn test_remove_n() {
        let mut h = Hand::default();
        h.add(Man1).unwrap();
        h.add(Man1).unwrap();
        h.add(Man1).unwrap();
        h.add(Pin2).unwrap();

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
        h.add(Man1).unwrap();
        h.add(Man1).unwrap();
        h.add(Pin5).unwrap();
        h.add(East).unwrap();

        let mut tiles = h.get_all_tiles();
        tiles.sort_unstable(); // Sort for consistent comparison

        let mut expected = vec![Man1, Man1, Pin5, East];
        expected.sort_unstable();

        assert_eq!(tiles, expected);

        let h_empty = Hand::default();
        assert!(h_empty.get_all_tiles().is_empty());
    }
}
