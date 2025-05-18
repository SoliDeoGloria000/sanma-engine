// src/tiles.rs
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)] // Added PartialOrd, Ord, Hash
pub enum Tile {
    Man1 = 0, Man2, Man3, Man4, Man5, Man6, Man7, Man8, Man9,
    Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Pin7, Pin8, Pin9,
    Sou1, Sou2, Sou3, Sou4, Sou5, Sou6, Sou7, Sou8, Sou9,
    East, South, West, // North is separate for Kita
    White, Green, Red,
    North,           // Kita (often treated as a separate honor/yakuhai in Sanma)
}

impl TryFrom<u8> for Tile {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        use Tile::*;
        Ok(match v {
            0 => Man1, 1 => Man2, 2 => Man3, 3 => Man4, 4 => Man5,
            5 => Man6, 6 => Man7, 7 => Man8, 8 => Man9,
            9 => Pin1, 10 => Pin2, 11 => Pin3, 12 => Pin4, 13 => Pin5,
            14 => Pin6, 15 => Pin7, 16 => Pin8, 17 => Pin9,
            18 => Sou1, 19 => Sou2, 20 => Sou3, 21 => Sou4, 22 => Sou5,
            23 => Sou6, 24 => Sou7, 25 => Sou8, 26 => Sou9,
            27 => East, 28 => South, 29 => West,
            30 => White, 31 => Green, 32 => Red,
            33 => North, // Kita
            _ => return Err(()),
        })
    }
}

impl Tile {
    /// Returns the common Unicode Mahjong tile symbol
    pub fn to_unicode(self) -> char {
        use Tile::*;
        match self {
            Man1 => '🀇', Man2 => '🀈', Man3 => '🀉', Man4 => '🀊', Man5 => '🀋',
            Man6 => '�', Man7 => '🀍', Man8 => '🀎', Man9 => '🀏',
            Pin1 => '🀙', Pin2 => '🀚', Pin3 => '🀛', Pin4 => '🀜', Pin5 => '🀝',
            Pin6 => '🀞', Pin7 => '🀟', Pin8 => '🀠', Pin9 => '🀡',
            Sou1 => '🀐', Sou2 => '🀑', Sou3 => '🀒', Sou4 => '🀓', Sou5 => '🀔',
            Sou6 => '🀕', Sou7 => '🀖', Sou8 => '🀗', Sou9 => '🀘',
            East => '🀀', South => '🀁', West => '🀂', 
            White => '🀆', Green => '🀅', Red => '🀄',
            North => '🀃', // Kita
        }
    }

    // Helper for dora calculation (example)
    pub fn next_in_series(self) -> Tile {
        match self {
            Tile::Man9 => Tile::Man1, Tile::Pin9 => Tile::Pin1, Tile::Sou9 => Tile::Sou1,
            Tile::East => Tile::South, Tile::South => Tile::West, Tile::West => Tile::North, Tile::North => Tile::East,
            Tile::White => Tile::Green, Tile::Green => Tile::Red, Tile::Red => Tile::White,
            // For numbered tiles 1-8
            tile if (tile as u8) < (Tile::East as u8) && (tile as u8 % 9 < 8) => {
                Tile::try_from(tile as u8 + 1).unwrap_or(tile) // Should always succeed for 1-8
            }
            _ => self // Should not happen for valid indicators, or return self if no next
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_to_unicode() {
        let tile = Tile::try_from(0).unwrap();
        assert_eq!(tile.to_unicode(), '🀇'); // Man1

        let north = Tile::try_from(33).unwrap();
        assert_eq!(north.to_unicode(), '🀃'); // North / Kita

        assert!(Tile::try_from(99).is_err());
    }

    #[test]
    fn test_next_in_series() {
        assert_eq!(Tile::Man1.next_in_series(), Tile::Man2);
        assert_eq!(Tile::Man8.next_in_series(), Tile::Man9);
        assert_eq!(Tile::Man9.next_in_series(), Tile::Man1);

        assert_eq!(Tile::Pin1.next_in_series(), Tile::Pin2);
        assert_eq!(Tile::Pin9.next_in_series(), Tile::Pin1);
        
        // Sou tiles are not in Sanma according to wall.rs, but test general logic
        // assert_eq!(Tile::Sou1.next_in_series(), Tile::Sou2);
        // assert_eq!(Tile::Sou9.next_in_series(), Tile::Sou1);

        assert_eq!(Tile::East.next_in_series(), Tile::South);
        assert_eq!(Tile::North.next_in_series(), Tile::East);

        assert_eq!(Tile::White.next_in_series(), Tile::Green);
        assert_eq!(Tile::Red.next_in_series(), Tile::White);
    }
}