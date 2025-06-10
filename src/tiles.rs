// src/tiles.rs

// --- IMPORTS FIRST ---
// All 'use' statements should be at the top of the file.
use pyo3::prelude::*;
use pyo3::exceptions::PyValueError; // <-- Add this missing import

// --- ALL ATTRIBUTES GO DIRECTLY ABOVE THE ENUM ---
#[pyclass(frozen)]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Tile {
    Man1 = 0, Man2, Man3, Man4, Man5, Man6, Man7, Man8, Man9,
    Pin1, Pin2, Pin3, Pin4, Pin5, Pin6, Pin7, Pin8, Pin9,
    Sou1, Sou2, Sou3, Sou4, Sou5, Sou6, Sou7, Sou8, Sou9,
    East, South, West,
    White, Green, Red,
    North,
}

// This block exposes methods to Python.
#[pymethods]
impl Tile {
    #[getter]
    fn value(&self) -> u8 {
        *self as u8
    }

    #[staticmethod]
    pub fn to_unicode_py(tile_val: u8) -> PyResult<String> {
        match Tile::try_from(tile_val) {
            Ok(tile) => Ok(tile.to_unicode().to_string()),
            Err(_) => Err(PyValueError::new_err("Invalid tile value"))
        }
    }
}

// This block is for pure Rust logic.
impl Tile {
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

    pub fn get_number_val(self) -> Option<u8> {
        if self.is_suited_number() {
            Some(((self as u8) % 9) + 1)
        } else { None }
    }

    pub fn is_suited_number(self) -> bool { (self as u8) < (Tile::East as u8) }
    pub fn is_terminal(self) -> bool {
        if !self.is_suited_number() { return false; }
        let val = self.get_number_val().unwrap();
        val == 1 || val == 9
    }
    pub fn is_honor(self) -> bool { (self as u8) >= (Tile::East as u8) && (self as u8) <= (Tile::North as u8) }
    pub fn is_terminal_or_honor(self) -> bool { self.is_terminal() || self.is_honor() }
    pub fn is_dragon(self) -> bool { matches!(self, Tile::White | Tile::Green | Tile::Red) }
    pub fn is_wind(self) -> bool { matches!(self, Tile::East | Tile::South | Tile::West | Tile::North) }
}

impl TryFrom<u8> for Tile {
    type Error = ();
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        if v <= 33 { Ok(unsafe { std::mem::transmute(v) }) } else { Err(()) }
    }
}

// --- TRAIT DEFINITION AND IMPLEMENTATION ---
pub trait TileExt {
    fn is_manzu(&self) -> bool;
    fn is_pinzu(&self) -> bool;
    fn is_sou(&self) -> bool;
    fn get_suit_char(&self) -> Option<char>;
    fn get_suit(&self) -> Option<u8>;
    // vvv ADD THESE MISSING LINES vvv
    fn is_terminal(&self) -> bool;
    fn is_honor(&self) -> bool;
    fn is_terminal_or_honor(&self) -> bool;
    fn next_in_series(&self) -> Tile;
}

impl TileExt for Tile {
    fn is_manzu(&self) -> bool { (*self as u8) <= (Tile::Man9 as u8) }
    fn is_pinzu(&self) -> bool { (*self as u8) >= (Tile::Pin1 as u8) && (*self as u8) <= (Tile::Pin9 as u8) }
    fn is_sou(&self) -> bool { (*self as u8) >= (Tile::Sou1 as u8) && (*self as u8) <= (Tile::Sou9 as u8) }

    fn get_suit_char(&self) -> Option<char> {
        if self.is_manzu() { Some('m') }
        else if self.is_pinzu() { Some('p') }
        else if self.is_sou() { Some('s') }
        else { None }
    }

    fn get_suit(&self) -> Option<u8> {
        if self.is_manzu() { Some(0) }
        else if self.is_pinzu() { Some(1) }
        else if self.is_sou() { Some(2) }
        else { None }
    }
    
    // --- IMPLEMENTED THE MISSING METHODS ---
    fn is_terminal(&self) -> bool { Tile::is_terminal(*self) }
    fn is_honor(&self) -> bool { Tile::is_honor(*self) }
    fn is_terminal_or_honor(&self) -> bool { Tile::is_terminal_or_honor(*self) }
    fn next_in_series(&self) -> Tile { Tile::next_in_series(*self) }
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
