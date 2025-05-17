// src/wall.rs
use rand::{rngs::StdRng, SeedableRng};
use rand::prelude::SliceRandom;
use crate::tiles::Tile;

pub struct Wall {
    tiles: [Tile; 70],   // Sanma: 70 tiles incl. dead wall
    pos: usize,          // next draw index
}

impl Wall {
    pub fn new(seed: u64) -> Self {
        // build full deck minus 2–8 Man and all Bamboo (Souzu) for Sanma
        let mut deck: Vec<Tile> = Vec::with_capacity(136);
        for id in 0u8..=33 {
            let t = Tile::try_from(id).unwrap();
            // skip 2–8 of Characters (Manzu)
            if matches!(t,
                Tile::Man2 | Tile::Man3 | Tile::Man4 |
                Tile::Man5 | Tile::Man6 | Tile::Man7 | Tile::Man8)
            {
                continue;
            }
            // skip all Bamboo (Souzu)
            if matches!(t,
                Tile::Sou1 | Tile::Sou2 | Tile::Sou3 |
                Tile::Sou4 | Tile::Sou5 | Tile::Sou6 |
                Tile::Sou7 | Tile::Sou8 | Tile::Sou9)
            {
                continue;
            }
            // keep 4 copies of each remaining tile
            for _ in 0..4 {
                deck.push(t);
            }
        }
        // at this point we should have 18 types ×4 = 72 tiles
        assert_eq!(deck.len(), 72, "Sanma deck before truncation must be 72 tiles");

        // shuffle deterministically
        let mut rng = StdRng::seed_from_u64(seed);
        deck.shuffle(&mut rng);

        // truncate to the 70-tile Wall (includes dead wall)
        deck.truncate(70);
        assert_eq!(deck.len(), 70, "Sanma deck must be 70 tiles");

        // copy into fixed-size array
        let mut tiles = [Tile::Man1; 70];
        tiles.copy_from_slice(&deck);
        Self { tiles, pos: 0 }
    }

    pub fn draw(&mut self) -> Option<Tile> {
        if self.pos >= self.tiles.len() {
            return None;
        }
        let t = self.tiles[self.pos];
        self.pos += 1;
        Some(t)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn determinism() {
        let a = Wall::new(42);
        let b = Wall::new(42);
        assert_eq!(&a.tiles[..], &b.tiles[..]);
    }

    #[test]
    fn draw_all() {
        let mut w = Wall::new(1);
        for _ in 0..70 {
            assert!(w.draw().is_some());
        }
        assert!(w.draw().is_none());
    }
}
