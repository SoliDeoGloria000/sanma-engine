// src/tiles.rs
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Tile {
    Man1 = 0, Man2, Man3, Man4, Man5, Man6, Man7, Man8, Man9,
    Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Pin7, Pin8, Pin9,
    Sou1, Sou2, Sou3, Sou4, Sou5, Sou6, Sou7, Sou8, Sou9,
    East, South, West,
    White, Green, Red,
    North,           // Kita
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
            33 => North,
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
            Man6 => '🀌', Man7 => '🀍', Man8 => '🀎', Man9 => '🀏',
            Pin1 => '🀙', Pin2 => '🀚', Pin3 => '🀛', Pin4 => '🀜', Pin5 => '🀝',
            Pin6 => '🀞', Pin7 => '🀟', Pin8 => '🀠', Pin9 => '🀡',
            Sou1 => '🀐', Sou2 => '🀑', Sou3 => '🀒', Sou4 => '🀓', Sou5 => '🀔',
            Sou6 => '🀕', Sou7 => '🀖', Sou8 => '🀗', Sou9 => '🀘',
            East => '🀀', South => '🀁', West => '🀂',
            White => '🀆', Green => '🀅', Red => '🀄',
            North => '🀃',
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn id_to_unicode() {
        // 0 → Man1 → 🀇
        let tile = Tile::try_from(0).unwrap();
        assert_eq!(tile.to_unicode(), '🀇');

        // 33 → North → 🀃
        let north = Tile::try_from(33).unwrap();
        assert_eq!(north.to_unicode(), '🀃');

        // invalid id
        assert!(Tile::try_from(99).is_err());
    }
}
