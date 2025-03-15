use crate::color::Color;
use core::fmt;
use enum_map::{Enum, EnumMap};
use std::{mem, str::FromStr};
use thiserror::Error;

#[rustfmt::skip]
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Enum)]
pub enum Square {
    A1 = 0, B1, C1, D1, E1, F1, G1, H1,
    A2, B2, C2, D2, E2, F2, G2, H2,
    A3, B3, C3, D3, E3, F3, G3, H3,
    A4, B4, C4, D4, E4, F4, G4, H4,
    A5, B5, C5, D5, E5, F5, G5, H5,
    A6, B6, C6, D6, E6, F6, G6, H6,
    A7, B7, C7, D7, E7, F7, G7, H7,
    A8, B8, C8, D8, E8, F8, G8, H8,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Enum, PartialOrd, Ord)]
pub enum File {
    A = 0,
    B,
    C,
    D,
    E,
    F,
    G,
    H,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Enum, PartialOrd, Ord)]
pub enum Rank {
    First = 0,
    Second,
    Third,
    Fourth,
    Fifth,
    Sixth,
    Seventh,
    Eighth,
}

#[derive(Error, Clone, Copy, Debug, PartialEq, Eq)]
#[error("invalid square notation, expected '[a-h][1-8]'.")]
pub struct SquareParseError;

#[derive(Error, Clone, Copy, Debug, PartialEq, Eq)]
#[error("invalid file notation, expected '[a-h]'.")]
pub struct FileParseError;

#[derive(Error, Clone, Copy, Debug, PartialEq, Eq)]
#[error("invalid rank notation, expected '[1-8]'.")]
pub struct RankParseError;

use Square::*;
impl Square {
    #[rustfmt::skip]
    pub const ALL: [Square; 64] = [
        A1, B1, C1, D1, E1, F1, G1, H1,
        A2, B2, C2, D2, E2, F2, G2, H2,
        A3, B3, C3, D3, E3, F3, G3, H3,
        A4, B4, C4, D4, E4, F4, G4, H4,
        A5, B5, C5, D5, E5, F5, G5, H5,
        A6, B6, C6, D6, E6, F6, G6, H6,
        A7, B7, C7, D7, E7, F7, G7, H7,
        A8, B8, C8, D8, E8, F8, G8, H8,
    ];
    pub const COUNT: usize = Self::ALL.len();

    #[inline]
    pub fn from_index(index: usize) -> Self {
        assert!(index < 64);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const unsafe fn from_index_unchecked(index: usize) -> Self {
        mem::transmute(index as u8)
    }

    #[inline]
    pub fn new(file: File, rank: Rank) -> Self {
        unsafe { Square::from_index_unchecked(file as usize + rank as usize * 8) }
    }

    #[inline]
    pub fn distance(self, other: Square) -> u32 {
        self.file()
            .distance(other.file())
            .max(self.rank().distance(other.rank()))
    }

    #[inline]
    pub fn file(self) -> File {
        unsafe { File::from_index_unchecked(self as usize % 8) }
    }

    #[inline]
    pub fn rank(self) -> Rank {
        unsafe { Rank::from_index_unchecked(self as usize / 8) }
    }

    #[inline]
    pub fn with_file(self, file: File) -> Self {
        Square::new(file, self.rank())
    }

    #[inline]
    pub fn with_rank(self, rank: Rank) -> Self {
        Square::new(self.file(), rank)
    }

    #[inline]
    pub fn offset_by(self, file_offset: i32, rank_offset: i32) -> Option<Square> {
        Some(Square::new(
            self.file().offset_by(file_offset)?,
            self.rank().offset_by(rank_offset)?,
        ))
    }

    #[inline]
    pub unsafe fn add_unchecked(self, offset: i32) -> Self {
        unsafe { Self::from_index_unchecked((self as i32 + offset as i32) as usize) }
    }
}

impl File {
    pub const ALL: [File; 8] = [
        File::A,
        File::B,
        File::C,
        File::D,
        File::E,
        File::F,
        File::G,
        File::H,
    ];
    pub const COUNT: usize = Self::ALL.len();

    #[inline]
    pub fn from_index(index: usize) -> Self {
        assert!(index < 8);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const unsafe fn from_index_unchecked(index: usize) -> Self {
        mem::transmute(index as u8)
    }

    #[inline]
    pub const fn distance(self, other: Self) -> u32 {
        u32::abs_diff(self as u32, other as u32)
    }

    #[inline]
    pub const fn offset_by(self, offset: i32) -> Option<Self> {
        let index = self as i32 + offset;
        if index < 0 || index >= 8 {
            return None;
        }
        Some(unsafe { File::from_index_unchecked(index as usize) })
    }
}

macro_rules! relative_ranks {
    { $($name: ident, $white: ident, $black: ident),* } => {
        $(#[inline]
        pub const fn $name(perspective: Color) -> Rank {
            match perspective {
                Color::White => Rank::$white,
                Color::Black => Rank::$black,
            }
        })*
    };
}

impl Rank {
    pub const ALL: [Rank; 8] = [
        Rank::First,
        Rank::Second,
        Rank::Third,
        Rank::Fourth,
        Rank::Fifth,
        Rank::Sixth,
        Rank::Seventh,
        Rank::Eighth,
    ];
    pub const COUNT: usize = Self::ALL.len();

    #[inline]
    pub fn from_index(index: usize) -> Self {
        assert!(index < 8);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const unsafe fn from_index_unchecked(index: usize) -> Self {
        mem::transmute(index as u8)
    }

    pub const fn back_rank(perspective: Color) -> Rank {
        Rank::first_for(perspective)
    }

    relative_ranks! {
        first_for, First, Eighth,
        second_for, Second, Seventh,
        third_for, Third, Sixth,
        fourth_for, Fourth, Fifth,
        fifth_for, Fifth, Fourth,
        sixth_for, Sixth, Third,
        seventh_for, Seventh, Second,
        eighth_for, Eighth, First
    }

    #[inline]
    pub const fn distance(self, other: Self) -> u32 {
        u32::abs_diff(self as u32, other as u32)
    }

    #[inline]
    pub const fn offset_by(self, offset: i32) -> Option<Self> {
        let index = self as i32 + offset;
        if index < 0 || index >= 8 {
            return None;
        }
        Some(unsafe { Rank::from_index_unchecked(index as usize) })
    }
}

impl TryFrom<char> for File {
    type Error = FileParseError;
    fn try_from(c: char) -> Result<Self, FileParseError> {
        match c {
            'a' => Ok(File::A),
            'b' => Ok(File::B),
            'c' => Ok(File::C),
            'd' => Ok(File::D),
            'e' => Ok(File::E),
            'f' => Ok(File::F),
            'g' => Ok(File::G),
            'h' => Ok(File::H),
            _ => Err(FileParseError),
        }
    }
}

impl TryFrom<char> for Rank {
    type Error = RankParseError;
    fn try_from(c: char) -> Result<Self, RankParseError> {
        match c {
            '1' => Ok(Rank::First),
            '2' => Ok(Rank::Second),
            '3' => Ok(Rank::Third),
            '4' => Ok(Rank::Fourth),
            '5' => Ok(Rank::Fifth),
            '6' => Ok(Rank::Sixth),
            '7' => Ok(Rank::Seventh),
            '8' => Ok(Rank::Eighth),
            _ => Err(RankParseError),
        }
    }
}

impl FromStr for File {
    type Err = FileParseError;
    fn from_str(s: &str) -> Result<Self, FileParseError> {
        match s {
            "a" => Ok(File::A),
            "b" => Ok(File::B),
            "c" => Ok(File::C),
            "d" => Ok(File::D),
            "e" => Ok(File::E),
            "f" => Ok(File::F),
            "g" => Ok(File::G),
            "h" => Ok(File::H),
            _ => Err(FileParseError),
        }
    }
}

impl FromStr for Rank {
    type Err = RankParseError;
    fn from_str(s: &str) -> Result<Self, RankParseError> {
        match s {
            "1" => Ok(Rank::First),
            "2" => Ok(Rank::Second),
            "3" => Ok(Rank::Third),
            "4" => Ok(Rank::Fourth),
            "5" => Ok(Rank::Fifth),
            "6" => Ok(Rank::Sixth),
            "7" => Ok(Rank::Seventh),
            "8" => Ok(Rank::Eighth),
            _ => Err(RankParseError),
        }
    }
}

impl FromStr for Square {
    type Err = SquareParseError;

    fn from_str(s: &str) -> Result<Self, SquareParseError> {
        if s.len() != 2 {
            return Err(SquareParseError);
        }

        let file: File = s[0..1].parse().map_err(|_| SquareParseError)?;
        let rank: Rank = s[1..2].parse().map_err(|_| SquareParseError)?;

        Ok(Square::new(file, rank))
    }
}

impl fmt::Display for File {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                File::A => 'a',
                File::B => 'b',
                File::C => 'c',
                File::D => 'd',
                File::E => 'e',
                File::F => 'f',
                File::G => 'g',
                File::H => 'h',
            }
        )
    }
}

impl fmt::Display for Rank {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Rank::First => '1',
                Rank::Second => '2',
                Rank::Third => '3',
                Rank::Fourth => '4',
                Rank::Fifth => '5',
                Rank::Sixth => '6',
                Rank::Seventh => '7',
                Rank::Eighth => '8',
            }
        )
    }
}

impl fmt::Display for Square {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

pub type BySquare<T> = EnumMap<Square, T>;

#[macro_export]
macro_rules! by_square {
    ($($square:ident => $value:expr),*$(,)?) => {
        enum_map! {
            $(Square::$square => $value),*
        }
    };
}

#[cfg(test)]
mod tests {
    use super::{Square, Square::*};

    #[test]
    fn square_str() {
        assert_eq!("e4".parse::<Square>(), Ok(E4));
        assert_eq!("d7".parse::<Square>(), Ok(D7));
        assert_eq!("b3".parse::<Square>(), Ok(B3));
        assert_eq!("f5".parse::<Square>(), Ok(F5));
        assert_eq!("a2".parse::<Square>(), Ok(A2));

        assert_eq!(format!("{}", F3), "f3");
        assert_eq!(format!("{}", H8), "h8");
        assert_eq!(format!("{}", D2), "d2");
        assert_eq!(format!("{}", A5), "a5");
        assert_eq!(format!("{}", C2), "c2");
    }
}
