use crate::{IllegalMoveError, Piece, Position, SanError, SanMove, Square, UciMove, Variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum MoveKind {
    Normal { promotion: Option<Piece> },
    EnPassant { target: Square },
    Castles { rook: Square },
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Move {
    pub kind: MoveKind,
    pub from: Square,
    pub to: Square,
}

impl Move {
    #[inline]
    pub fn new_normal(from: Square, to: Square) -> Self {
        Self {
            from,
            to,
            kind: MoveKind::Normal { promotion: None },
        }
    }

    #[inline]
    pub fn new_promotion(from: Square, to: Square, promotion: Piece) -> Self {
        Self {
            from,
            to,
            kind: MoveKind::Normal {
                promotion: Some(promotion),
            },
        }
    }

    #[inline]
    pub fn new_en_passant(from: Square, to: Square, target: Square) -> Self {
        Self {
            from,
            to,
            kind: MoveKind::EnPassant { target },
        }
    }

    #[inline]
    pub fn new_castles(from: Square, to: Square, rook: Square) -> Self {
        Self {
            from,
            to,
            kind: MoveKind::Castles { rook },
        }
    }

    #[inline]
    pub fn promotion(&self) -> Option<Piece> {
        if let MoveKind::Normal { promotion } = self.kind {
            promotion
        } else {
            None
        }
    }

    #[inline]
    pub fn to_uci_standard(self) -> UciMove {
        UciMove::from_move_standard(self)
    }

    #[inline]
    pub fn to_uci_chess960(self) -> UciMove {
        UciMove::from_move_chess960(self)
    }

    #[inline]
    pub fn to_uci(self, variant: Variant) -> UciMove {
        UciMove::from_move(self, variant)
    }

    #[inline]
    pub fn to_san(self, position: &Position) -> Result<SanMove, SanError> {
        SanMove::from_move(self, position)
    }

    #[inline]
    pub fn to_san_nosuffix(self, position: &Position) -> Result<SanMove, SanError> {
        SanMove::from_move_nosuffix(self, position)
    }
}

pub trait ToMove {
    type Error;

    fn to_move(&self, position: &Position) -> Result<Move, Self::Error>;
}

impl ToMove for Move {
    type Error = IllegalMoveError;

    #[inline]
    fn to_move(&self, position: &Position) -> Result<Move, IllegalMoveError> {
        if !position.is_legal(self) {
            return Err(IllegalMoveError);
        }
        Ok(*self)
    }
}
