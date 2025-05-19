// src/tiles.rs
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
    pub fn to_unicode(self) -> char {
        use Tile::*;
        match self {
            Man1 => '🀇', Man2 => '🀈', Man3 => '🀉', Man4 => '🀊', Man5 => '🀋',
            Man6 => '🀌', Man7 => '🀍', Man8 => '🀎', Man9 => '🀏', // Corrected Man6
            Pin1 => '🀙', Pin2 => '🀚', Pin3 => '🀛', Pin4 => '🀜', Pin5 => '🀝',
            Pin6 => '🀞', Pin7 => '🀟', Pin8 => '🀠', Pin9 => '🀡',
            Sou1 => '🀐', Sou2 => '🀑', Sou3 => '🀒', Sou4 => '🀓', Sou5 => '🀔',
            Sou6 => '🀕', Sou7 => '🀖', Sou8 => '🀗', Sou9 => '🀘',
            East => '🀀', South => '🀁', West => '🀂',
            White => '🀆', Green => '🀅', Red => '🀄',
            North => '🀃',
        }
    }

    pub fn next_in_series(self) -> Tile {
        match self {
            Tile::Man9 => Tile::Man1, Tile::Pin9 => Tile::Pin1, Tile::Sou9 => Tile::Sou1,
            Tile::East => Tile::South, Tile::South => Tile::West, Tile::West => Tile::North, Tile::North => Tile::East,
            Tile::White => Tile::Green, Tile::Green => Tile::Red, Tile::Red => Tile::White,
            tile if (tile as u8) < (Tile::East as u8) && (tile as u8 % 9 < 8) => {
                Tile::try_from(tile as u8 + 1).unwrap_or(tile)
            }
            _ => self
        }
    }

    // Added Helper Methods:
    pub fn get_number_val(self) -> Option<u8> {
        if self.is_suited_number() {
            Some(((self as u8) % 9) + 1)
        } else {
            None
        }
    }

    pub fn is_suited_number(self) -> bool {
        (self as u8) < (Tile::East as u8) // Man (0-8), Pin (9-17), Sou (18-26)
    }

    pub fn is_terminal(self) -> bool {
        if !self.is_suited_number() {
            return false;
        }
        let val = self.get_number_val().unwrap(); // Safe due to is_suited_number check
        val == 1 || val == 9
    }

    pub fn is_honor(self) -> bool {
        // East, South, West, White, Green, Red, North
        (self as u8) >= (Tile::East as u8) && (self as u8) <= (Tile::North as u8)
    }

    pub fn is_terminal_or_honor(self) -> bool {
        self.is_terminal() || self.is_honor()
    }

    pub fn is_dragon(self) -> bool {
        matches!(self, Tile::White | Tile::Green | Tile::Red)
    }

    pub fn is_wind(self) -> bool {
        matches!(self, Tile::East | Tile::South | Tile::West | Tile::North)
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
        assert_eq!(Tile::East.next_in_series(), Tile::South);
        assert_eq!(Tile::North.next_in_series(), Tile::East);
        assert_eq!(Tile::White.next_in_series(), Tile::Green);
        assert_eq!(Tile::Red.next_in_series(), Tile::White);
    }

    #[test]
    fn test_tile_properties() {
        assert!(Tile::Man1.is_suited_number());
        assert_eq!(Tile::Man1.get_number_val(), Some(1));
        assert!(Tile::Man1.is_terminal());
        assert!(!Tile::Man1.is_honor());
        assert!(Tile::Man5.is_suited_number());
        assert_eq!(Tile::Man5.get_number_val(), Some(5));
        assert!(!Tile::Man5.is_terminal());
        assert!(!Tile::Pin2.is_honor());
        assert!(Tile::East.is_honor());
        assert!(!Tile::East.is_suited_number());
        assert!(Tile::East.is_wind());
        assert!(!Tile::East.is_dragon());
        assert!(Tile::White.is_honor());
        assert!(Tile::White.is_dragon());
        assert!(!Tile::White.is_wind());
        assert!(Tile::North.is_honor());
        assert!(Tile::North.is_wind());
        assert!(Tile::Red.is_terminal_or_honor());
        assert!(Tile::Man9.is_terminal_or_honor());
        assert!(!Tile::Pin5.is_terminal_or_honor());
    }
}
