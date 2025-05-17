use crate::tiles::Tile;

/// Bit-packed hand: 3 bits per tile kind (0–7 copies is enough)
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Hand(u128);

impl Hand {
    pub fn add(&mut self, tile: Tile) {
        let idx = tile as u8;
        let shift = idx * 3;
        let mask = 0b111u128 << shift;
        let count = ((self.0 & mask) >> shift) + 1;
        assert!(count <= 4, "too many copies of {:?}", tile);
        self.0 = (self.0 & !mask) | (count << shift);
    }

    pub fn count(&self, tile: Tile) -> u8 {
        let shift = (tile as u8) * 3;
        ((self.0 >> shift) & 0b111) as u8
    }

    pub fn remove(&mut self, tile: Tile) -> bool {
        let shift = (tile as u8) * 3;
        let mask  = 0b111u128 << shift;
        let count = (self.0 & mask) >> shift;
        if count == 0 { return false; }
        // decrement
        self.0 = (self.0 & !mask) | ((count - 1) << shift);
        true
    }

    pub fn iter(&self) -> impl Iterator<Item = (Tile, u8)> + '_ {
        (0u8..=33).filter_map(move |i| {
            let t = Tile::try_from(i).unwrap();
            let c = self.count(t);
            (c > 0).then_some((t, c))
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tiles::Tile::*;

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
    fn remove_and_iter() {
        let mut h = Hand::default();
        h.add(Tile::Red);
        h.add(Tile::Red);
        assert_eq!(h.count(Tile::Red), 2);
        assert!(h.remove(Tile::Red));
        assert_eq!(h.count(Tile::Red), 1);
        assert!(h.remove(Tile::Red));
        assert!(!h.remove(Tile::Red));      // already zero
        assert_eq!(h.iter().count(), 0);    // empty hand
    }
}
