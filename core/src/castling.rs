use crate::square::File;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Castling {
    pub king_side: Option<File>,
    pub queen_side: Option<File>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CastlingSide {
    King,
    Queen,
}

impl Castling {
    pub const ALL_STANDARD: Castling = Castling {
        king_side: Some(File::H),
        queen_side: Some(File::A),
    };

    pub const NONE: Castling = Castling {
        king_side: None,
        queen_side: None,
    };

    #[inline]
    pub fn is_none(&self) -> bool {
        *self == Self::NONE
    }

    #[inline]
    pub fn is_some(&self) -> bool {
        *self != Self::NONE
    }

    #[inline]
    pub fn remove(&mut self, file: File) {
        if self.king_side == Some(file) {
            self.king_side = None;
        }
        if self.queen_side == Some(file) {
            self.queen_side = None;
        }
    }

    #[inline]
    pub fn contains(&self, file: File) -> bool {
        self.king_side == Some(file) || self.queen_side == Some(file)
    }

    #[inline]
    pub fn side(&self, file: File) -> Option<CastlingSide> {
        if self.king_side == Some(file) {
            return Some(CastlingSide::King);
        }
        if self.queen_side == Some(file) {
            return Some(CastlingSide::Queen);
        }
        None
    }
}
