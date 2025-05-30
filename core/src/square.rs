use crate::{color::Color, helpers::mapped_enum};
use core::{fmt, slice};
use std::{
    array, iter, mem,
    ops::{Index, IndexMut},
    str::FromStr,
};
use thiserror::Error;

mapped_enum! {
    #[rustfmt::skip]
    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Square {
        A1, B1, C1, D1, E1, F1, G1, H1,
        A2, B2, C2, D2, E2, F2, G2, H2,
        A3, B3, C3, D3, E3, F3, G3, H3,
        A4, B4, C4, D4, E4, F4, G4, H4,
        A5, B5, C5, D5, E5, F5, G5, H5,
        A6, B6, C6, D6, E6, F6, G6, H6,
        A7, B7, C7, D7, E7, F7, G7, H7,
        A8, B8, C8, D8, E8, F8, G8, H8,
    }
}

mapped_enum! {
    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum File {
        A,
        B,
        C,
        D,
        E,
        F,
        G,
        H,
    }
}

mapped_enum! {
    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
    pub enum Rank {
        First,
        Second,
        Third,
        Fourth,
        Fifth,
        Sixth,
        Seventh,
        Eighth,
    }
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

impl Square {
    #[inline]
    pub const fn from_index(index: usize) -> Self {
        assert!(index < 64);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const fn try_from_index(index: usize) -> Option<Self> {
        if index < 64 {
            Some(unsafe { Self::from_index_unchecked(index) })
        } else {
            None
        }
    }

    #[inline]
    pub const unsafe fn from_index_unchecked(index: usize) -> Self {
        mem::transmute(index as u8)
    }

    #[inline]
    pub fn from_ascii(s: &[u8]) -> Result<Self, SquareParseError> {
        if s.len() != 2 {
            return Err(SquareParseError);
        }

        let file: File = File::try_from(s[0] as char).map_err(|_| SquareParseError)?;
        let rank: Rank = Rank::try_from(s[1] as char).map_err(|_| SquareParseError)?;

        Ok(Square::new(file, rank))
    }

    #[inline]
    pub const fn new(file: File, rank: Rank) -> Self {
        unsafe { Square::from_index_unchecked(file as usize + rank as usize * 8) }
    }

    #[inline]
    pub fn distance(self, other: Square) -> u32 {
        self.file()
            .distance(other.file())
            .max(self.rank().distance(other.rank()))
    }

    #[inline]
    pub const fn file(self) -> File {
        unsafe { File::from_index_unchecked(self as usize % 8) }
    }

    #[inline]
    pub const fn rank(self) -> Rank {
        unsafe { Rank::from_index_unchecked(self as usize / 8) }
    }

    #[inline]
    pub const fn with_file(self, file: File) -> Self {
        Square::new(file, self.rank())
    }

    #[inline]
    pub const fn with_rank(self, rank: Rank) -> Self {
        Square::new(self.file(), rank)
    }

    #[inline]
    pub const fn flip_vertical(self) -> Self {
        unsafe { Self::from_index_unchecked(self as usize ^ 0b111000) }
    }

    #[inline]
    pub const fn flip_horizontal(self) -> Self {
        unsafe { Self::from_index_unchecked(self as usize ^ 0b000111) }
    }

    #[inline]
    pub const fn rotate_180(self) -> Self {
        unsafe { Self::from_index_unchecked(self as usize ^ 0b111111) }
    }

    #[inline]
    pub const fn offset_by(self, file_offset: i32, rank_offset: i32) -> Option<Square> {
        if let Some(file) = self.file().offset_by(file_offset) {
            if let Some(rank) = self.rank().offset_by(rank_offset) {
                Some(Square::new(file, rank))
            } else {
                None
            }
        } else {
            None
        }
    }

    #[inline]
    pub unsafe fn add_unchecked(self, offset: i32) -> Self {
        unsafe { Self::from_index_unchecked((self as i32 + offset) as usize) }
    }
}

impl File {
    #[inline]
    pub const fn from_index(index: usize) -> Self {
        assert!(index < 8);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const fn try_from_index(index: usize) -> Option<Self> {
        if index < 8 {
            Some(unsafe { Self::from_index_unchecked(index) })
        } else {
            None
        }
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
    pub const fn flip(self) -> Self {
        unsafe { Self::from_index_unchecked(7 - self as usize) }
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
    #[inline]
    pub const fn from_index(index: usize) -> Self {
        assert!(index < 8);
        unsafe { Self::from_index_unchecked(index) }
    }

    #[inline]
    pub const fn try_from_index(index: usize) -> Option<Self> {
        if index < 8 {
            Some(unsafe { Self::from_index_unchecked(index) })
        } else {
            None
        }
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
    pub const fn flip(self) -> Self {
        unsafe { Self::from_index_unchecked(7 - self as usize) }
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
        Square::from_ascii(s.as_bytes())
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

#[derive(Clone, Debug)]
pub struct IntoIter<T>(iter::Enumerate<array::IntoIter<T, { Square::COUNT }>>);

#[derive(Clone, Debug)]
pub struct Iter<'a, T>(iter::Enumerate<slice::Iter<'a, T>>);

#[derive(Debug)]
pub struct IterMut<'a, T>(iter::Enumerate<slice::IterMut<'a, T>>);

#[derive(Clone, Debug)]
pub struct IntoValues<T>(array::IntoIter<T, { Square::COUNT }>);

#[derive(Clone, Debug)]
pub struct Values<'a, T>(slice::Iter<'a, T>);

#[derive(Debug)]
pub struct ValuesMut<'a, T>(slice::IterMut<'a, T>);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BySquare<T>([T; Square::COUNT]);

impl<T> BySquare<T> {
    #[inline]
    pub const fn from_array(arr: [T; Square::COUNT]) -> Self {
        Self(arr)
    }

    #[inline]
    pub fn from_fn(mut f: impl FnMut(Square) -> T) -> Self {
        Self(array::from_fn(|sq| unsafe {
            f(Square::from_index_unchecked(sq))
        }))
    }

    #[inline]
    pub fn iter(&self) -> Iter<T> {
        Iter(self.0.iter().enumerate())
    }

    #[inline]
    pub fn iter_mut(&mut self) -> IterMut<T> {
        IterMut(self.0.iter_mut().enumerate())
    }

    #[inline]
    pub fn into_values(self) -> IntoValues<T> {
        IntoValues(self.0.into_iter())
    }

    #[inline]
    pub fn values(&self) -> Values<T> {
        Values(self.0.iter())
    }

    #[inline]
    pub fn values_mut(&mut self) -> ValuesMut<T> {
        ValuesMut(self.0.iter_mut())
    }
}

impl<T> IntoIterator for BySquare<T> {
    type Item = (Square, T);
    type IntoIter = IntoIter<T>;

    #[inline]
    fn into_iter(self) -> IntoIter<T> {
        IntoIter(self.0.into_iter().enumerate())
    }
}

impl<'a, T> IntoIterator for &'a BySquare<T> {
    type Item = (Square, &'a T);
    type IntoIter = Iter<'a, T>;

    fn into_iter(self) -> Iter<'a, T> {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut BySquare<T> {
    type Item = (Square, &'a mut T);
    type IntoIter = IterMut<'a, T>;

    fn into_iter(self) -> IterMut<'a, T> {
        self.iter_mut()
    }
}

impl<T> Iterator for IntoIter<T> {
    type Item = (Square, T);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(i, value)| (unsafe { Square::from_index_unchecked(i) }, value))
    }
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = (Square, &'a T);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(i, value)| (unsafe { Square::from_index_unchecked(i) }, value))
    }
}

impl<'a, T> Iterator for IterMut<'a, T> {
    type Item = (Square, &'a mut T);

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        self.0
            .next()
            .map(|(i, value)| (unsafe { Square::from_index_unchecked(i) }, value))
    }
}

impl<'a, T> Iterator for Values<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<&'a T> {
        self.0.next()
    }
}

impl<'a, T> Iterator for ValuesMut<'a, T> {
    type Item = &'a mut T;

    #[inline]
    fn next(&mut self) -> Option<&'a mut T> {
        self.0.next()
    }
}

impl<T> Default for BySquare<T>
where
    T: Default,
{
    #[inline]
    fn default() -> Self {
        Self::from_array(array::from_fn(|_| Default::default()))
    }
}

impl<T> From<[T; Square::COUNT]> for BySquare<T> {
    #[inline]
    fn from(arr: [T; Square::COUNT]) -> Self {
        Self::from_array(arr)
    }
}

impl<T> Index<Square> for BySquare<T> {
    type Output = T;

    #[inline]
    fn index(&self, square: Square) -> &T {
        &self.0[square as usize]
    }
}

impl<T> IndexMut<Square> for BySquare<T> {
    #[inline]
    fn index_mut(&mut self, square: Square) -> &mut T {
        &mut self.0[square as usize]
    }
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

    #[test]
    fn square_flip() {
        assert_eq!(E4.flip_vertical(), E5);
        assert_eq!(A1.flip_vertical(), A8);
        assert_eq!(B2.flip_vertical(), B7);
        assert_eq!(F6.flip_vertical(), F3);
        assert_eq!(A8.flip_horizontal(), H8);
        assert_eq!(E4.flip_horizontal(), D4);
        assert_eq!(F2.flip_horizontal(), C2);
        assert_eq!(B7.flip_horizontal(), G7);
    }
}
