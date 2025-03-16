use core::fmt;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use crate::square::{File, Rank, Square};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct SquareSet(u64);

impl SquareSet {
    pub const EMPTY: Self = Self::from_bits(0);
    pub const FULL: Self = Self::from_bits(!0);

    pub const CORNERS: Self = Self::from_square(Square::A1)
        .with(Square::H1)
        .with(Square::A8)
        .with(Square::H8);

    pub const EDGES: Self = Self::from_file(File::A)
        .union(Self::from_file(File::H))
        .union(Self::from_rank(Rank::First))
        .union(Self::from_rank(Rank::Eighth));

    pub const LIGHT_SQUARES: Self = Self::from_bits(0xaaaaaaaaaaaaaaaa);
    pub const DARK_SQUARES: Self = Self::from_bits(0x5555555555555555);

    #[inline]
    pub const fn from_bits(bits: u64) -> Self {
        Self(bits)
    }

    #[inline]
    pub const fn from_square(square: Square) -> Self {
        Self(1 << square as usize)
    }

    #[inline]
    pub const fn from_file(file: File) -> Self {
        Self(0x0101010101010101u64 << file as u32)
    }

    #[inline]
    pub const fn from_rank(rank: Rank) -> Self {
        Self(0xffu64 << (rank as u32 * 8))
    }

    #[inline]
    pub const fn files_before(file: File) -> Self {
        match file {
            File::A => Self(0),
            File::B => Self(0x0101010101010101),
            File::C => Self(0x0303030303030303),
            File::D => Self(0x0707070707070707),
            File::E => Self(0x0f0f0f0f0f0f0f0f),
            File::F => Self(0x1f1f1f1f1f1f1f1f),
            File::G => Self(0x3f3f3f3f3f3f3f3f),
            File::H => Self(0x7f7f7f7f7f7f7f7f),
        }
    }

    #[inline]
    pub const fn files_after(file: File) -> Self {
        match file {
            File::H => Self(0),
            File::G => Self(0x8080808080808080),
            File::F => Self(0xc0c0c0c0c0c0c0c0),
            File::E => Self(0xe0e0e0e0e0e0e0e0),
            File::D => Self(0xf0f0f0f0f0f0f0f0),
            File::C => Self(0xf8f8f8f8f8f8f8f8),
            File::B => Self(0xfcfcfcfcfcfcfcfc),
            File::A => Self(0xfefefefefefefefe),
        }
    }

    #[inline]
    pub const fn contains(self, square: Square) -> bool {
        self.0 & SquareSet::from_square(square).0 != 0
    }

    #[inline]
    pub const fn with(self, square: Square) -> Self {
        Self(self.0 | SquareSet::from_square(square).0)
    }

    #[inline]
    pub const fn without(self, square: Square) -> Self {
        Self(self.0 & !SquareSet::from_square(square).0)
    }

    #[inline]
    pub const fn insert(&mut self, square: Square) {
        self.0 |= SquareSet::from_square(square).0;
    }

    #[inline]
    pub const fn remove(&mut self, square: Square) {
        self.0 &= !SquareSet::from_square(square).0;
    }

    #[inline]
    pub const fn toggle(&mut self, square: Square) {
        self.0 ^= SquareSet::from_square(square).0;
    }

    #[inline]
    pub const fn union(self, other: SquareSet) -> SquareSet {
        SquareSet(self.0 | other.0)
    }

    #[inline]
    pub const fn intersection(self, other: SquareSet) -> SquareSet {
        SquareSet(self.0 & other.0)
    }

    #[inline]
    pub const fn difference(self, other: SquareSet) -> SquareSet {
        SquareSet(self.0 & !other.0)
    }

    #[inline]
    pub const fn symmetric_difference(self, other: SquareSet) -> SquareSet {
        SquareSet(self.0 ^ other.0)
    }

    #[inline]
    pub const fn complement(self) -> SquareSet {
        SquareSet(!self.0)
    }

    #[inline]
    pub const fn to_bits(self) -> u64 {
        self.0
    }

    #[inline]
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub const fn count(self) -> u32 {
        self.0.count_ones()
    }

    #[inline]
    pub const fn shift_right(self) -> SquareSet {
        SquareSet((self.0 & !SquareSet::from_file(File::H).0).wrapping_shl(1))
    }

    #[inline]
    pub const fn shift_left(self) -> SquareSet {
        SquareSet((self.0 & !SquareSet::from_file(File::A).0).wrapping_shr(1))
    }

    #[inline]
    pub const fn shift_up(self, offset: u32) -> SquareSet {
        SquareSet(self.0.wrapping_shl(8 * offset))
    }

    #[inline]
    pub const fn shift_down(self, offset: u32) -> SquareSet {
        SquareSet(self.0.wrapping_shr(8 * offset))
    }

    #[inline]
    pub const fn flip_vertical(self) -> SquareSet {
        SquareSet(self.0.swap_bytes())
    }
    
    #[inline]
    pub const fn flip_horizontal(mut self) -> SquareSet {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Horizontal
        const MASK1: u64 = 0x5555555555555555;
        const MASK2: u64 = 0x3333333333333333;
        const MASK4: u64 = 0x0f0f0f0f0f0f0f0f;

        self.0 = ((self.0 >> 1) & MASK1) | ((self.0 & MASK1) << 1);
        self.0 = ((self.0 >> 2) & MASK2) | ((self.0 & MASK2) << 2);
        self.0 = ((self.0 >> 4) & MASK4) | ((self.0 & MASK4) << 4);

        self
    }

    #[inline]
    pub const fn rotate_180(self) -> SquareSet {
        self.flip_horizontal().flip_vertical()
    }

    #[inline]
    pub const fn first(self) -> Option<Square> {
        if self.is_empty() {
            return None;
        }
        Some(unsafe { Square::from_index_unchecked(self.0.trailing_zeros() as usize) })
    }

    #[inline]
    pub const fn last(self) -> Option<Square> {
        if self.is_empty() {
            return None;
        }
        Some(unsafe { Square::from_index_unchecked(63 - self.0.leading_zeros() as usize) })
    }

    #[inline]
    pub fn as_square(self) -> Option<Square> {
        let first = self.first()?;
        if !(self ^ first.into()).is_empty() {
            return None;
        }
        Some(first)
    }

    #[inline]
    pub fn iter(self) -> Iter {
        Iter { remaining: self }
    }

    #[inline]
    pub fn subsets(self) -> Subsets {
        Subsets {
            is_done: false,
            target: self,
            subset: SquareSet::EMPTY,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Iter {
    remaining: SquareSet,
}

impl Iterator for Iter {
    type Item = Square;

    #[inline]
    fn next(&mut self) -> Option<Square> {
        if let Some(square) = self.remaining.first() {
            self.remaining ^= SquareSet::from_square(square);
            return Some(square);
        }
        None
    }
}

impl IntoIterator for SquareSet {
    type Item = Square;
    type IntoIter = Iter;

    #[inline]
    fn into_iter(self) -> Iter {
        self.iter()
    }
}

#[derive(Clone, Debug)]
pub struct Subsets {
    target: SquareSet,
    subset: SquareSet,
    is_done: bool,
}

impl Iterator for Subsets {
    type Item = SquareSet;

    #[inline]
    fn next(&mut self) -> Option<SquareSet> {
        if self.is_done {
            return None;
        }
        self.subset.0 = self.subset.0.wrapping_sub(self.target.0) & self.target.0;
        self.is_done = self.subset.is_empty();
        Some(self.subset)
    }
}

impl FromIterator<Square> for SquareSet {
    #[inline]
    fn from_iter<T>(iter: T) -> Self
    where
        T: IntoIterator<Item = Square>,
    {
        iter.into_iter()
            .map(SquareSet::from)
            .fold(SquareSet::EMPTY, |ss, sq| ss | sq)
    }
}

impl From<Square> for SquareSet {
    #[inline]
    fn from(square: Square) -> Self {
        SquareSet::from_square(square)
    }
}

impl From<File> for SquareSet {
    #[inline]
    fn from(file: File) -> Self {
        SquareSet::from_file(file)
    }
}

impl From<Rank> for SquareSet {
    #[inline]
    fn from(rank: Rank) -> Self {
        SquareSet::from_rank(rank)
    }
}

impl From<u64> for SquareSet {
    #[inline]
    fn from(value: u64) -> Self {
        Self(value)
    }
}

impl From<SquareSet> for u64 {
    #[inline]
    fn from(ss: SquareSet) -> Self {
        ss.0
    }
}

impl Not for SquareSet {
    type Output = Self;

    #[inline]
    fn not(self) -> Self {
        self.complement()
    }
}

impl BitOr for SquareSet {
    type Output = Self;

    #[inline]
    fn bitor(self, rhs: Self) -> Self {
        self.union(rhs)
    }
}

impl BitAnd for SquareSet {
    type Output = Self;

    #[inline]
    fn bitand(self, rhs: Self) -> Self {
        self.intersection(rhs)
    }
}

impl BitXor for SquareSet {
    type Output = Self;

    #[inline]
    fn bitxor(self, rhs: Self) -> Self {
        self.symmetric_difference(rhs)
    }
}

impl BitOrAssign for SquareSet {
    #[inline]
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl BitAndAssign for SquareSet {
    #[inline]
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl BitXorAssign for SquareSet {
    #[inline]
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl fmt::Display for SquareSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in Rank::ALL.into_iter().rev() {
            for file in File::ALL {
                if self.contains(Square::new(file, rank)) {
                    write!(f, "# ")?;
                } else {
                    write!(f, ". ")?;
                }
            }
            if rank != Rank::First {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{square::Square::*, squareset::SquareSet};

    #[test]
    fn squareset_iter() {
        let squares =
            SquareSet::from(A1) | SquareSet::from(B6) | SquareSet::from(D3) | SquareSet::from(H8);

        let squares_vec = squares.iter().collect::<Vec<_>>();
        assert_eq!(squares_vec, vec![A1, D3, B6, H8]);

        let rebuilt = squares.into_iter().collect::<SquareSet>();
        assert_eq!(squares, rebuilt);
    }
}
