use core::fmt;
use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign, BitXor, BitXorAssign, Not};

use crate::square::{File, Rank, Square};

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Hash)]
pub struct SquareSet(pub u64);

impl SquareSet {
    pub const EMPTY: Self = Self(0);
    pub const FULL: Self = Self(!0);
    pub const EDGES: Self = Self::from_file(File::A)
        .union(Self::from_file(File::H))
        .union(Self::from_rank(Rank::First))
        .union(Self::from_rank(Rank::Eighth));

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
    pub const fn contains(self, square: Square) -> bool {
        self.0 & SquareSet::from_square(square).0 != 0
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
    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    #[inline]
    pub const fn count(self) -> u32 {
        self.0.count_ones()
    }
    
    #[inline]
    pub const fn advance_files(self) -> SquareSet {
        SquareSet((self.0 & !SquareSet::from_file(File::H).0).wrapping_shl(1))
    }

    #[inline]
    pub const fn retreat_files(self) -> SquareSet {
        SquareSet((self.0 & !SquareSet::from_file(File::A).0).wrapping_shr(1))
    }

    #[inline]
    pub const fn advance_ranks(self) -> SquareSet {
        SquareSet(self.0.wrapping_shl(8))
    }

    #[inline]
    pub const fn retreat_ranks(self) -> SquareSet {
        SquareSet(self.0.wrapping_shr(8))
    }

    #[inline]
    pub fn as_square(self) -> Option<Square> {
        if self.count() > 1 {
            return None;
        }
        self.next_square()
    }

    #[inline]
    const fn next_square(self) -> Option<Square> {
        if self.is_empty() {
            return None;
        }
        Some(Square::ALL[self.0.trailing_zeros() as usize])
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
        if let Some(square) = self.remaining.next_square() {
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
    use crate::{squareset::SquareSet, square::Square::*};

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
