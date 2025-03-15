use core::fmt;
use std::str::FromStr;

use crate::{
    IllegalMoveError, Move, MoveKind, Piece, PieceParseError, Position, Square, SquareParseError,
    ToMove, Variant, File
};
use thiserror::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct UciMove {
    pub from: Square,
    pub to: Square,
    pub promotion: Option<Piece>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Error)]
#[error("invalid move notation.")]
pub struct UciMoveParseError;

impl UciMove {
    #[inline]
    pub fn from_move_standard(mv: Move) -> Self {
        Self {
            from: mv.from,
            to: mv.to,
            promotion: mv.promotion(),
        }
    }

    #[inline]
    pub fn from_move_chess960(mv: Move) -> Self {
        match mv.kind {
            MoveKind::Castles { rook } => Self {
                from: mv.from,
                to: rook,
                promotion: None,
            },
            _ => Self {
                from: mv.from,
                to: mv.to,
                promotion: mv.promotion(),
            },
        }
    }

    #[inline]
    pub fn from_move(mv: Move, variant: Variant) -> Self {
        match variant {
            Variant::Standard => Self::from_move_standard(mv),
            Variant::Chess960 => Self::from_move_chess960(mv),
        }
    }
}

impl ToMove for UciMove {
    type Error = IllegalMoveError;

    fn to_move(&self, position: &Position) -> Result<Move, IllegalMoveError> {
        let moved = position.piece_at(self.from).ok_or(IllegalMoveError)?;
        let our_backrank = position.our_backrank();
        let their_backrank = position.their_backrank();

        let mv = if let Some(promotion) = self.promotion {
            if moved == Piece::Pawn && self.to.rank() == their_backrank {
                Move::new_promotion(self.from, self.to, promotion)
            } else {
                return Err(IllegalMoveError);
            }
        } else if moved == Piece::King
            && self.from.rank() == our_backrank
            && self.to.rank() == our_backrank
        {
            if position.our_castling().contains(self.to.file()) {
                if self.to.file() > self.from.file() {
                    Move::new_castles(self.from, self.to.with_file(File::G), self.to)
                } else {
                    Move::new_castles(self.from, self.to.with_file(File::C), self.to)
                }
            } else if self.from.file() == File::E && self.from.distance(self.to) == 2 {
                if self.to.file() > self.from.file() {
                    Move::new_castles(self.from, self.to, self.to.with_file(File::H))
                } else {
                    Move::new_castles(self.from, self.to, self.to.with_file(File::A))
                }
            } else {
                Move::new_normal(self.from, self.to)
            }
        } else if moved == Piece::Pawn && position.en_passant_square() == Some(self.to) {
            Move::new_en_passant(self.from, self.to, position.en_passant_target().unwrap())
        } else {
            Move::new_normal(self.from, self.to)
        };

        if !position.is_legal(&mv) {
            return Err(IllegalMoveError);
        }

        Ok(mv)
    }
}

impl FromStr for UciMove {
    type Err = UciMoveParseError;

    fn from_str(s: &str) -> Result<Self, UciMoveParseError> {
        let promotion = match s.len() {
            4 => None,
            5 => Some(s[4..5].parse()?),
            _ => return Err(UciMoveParseError),
        };

        Ok(UciMove {
            from: s[0..2].parse()?,
            to: s[2..4].parse()?,
            promotion,
        })
    }
}

impl fmt::Display for UciMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.from,
            self.to,
            match self.promotion {
                Some(Piece::Bishop) => "b",
                Some(Piece::Knight) => "n",
                Some(Piece::Rook) => "r",
                Some(Piece::Queen) => "q",
                _ => "",
            }
        )
    }
}

impl From<SquareParseError> for UciMoveParseError {
    fn from(_: SquareParseError) -> Self {
        UciMoveParseError
    }
}

impl From<PieceParseError> for UciMoveParseError {
    fn from(_: PieceParseError) -> Self {
        UciMoveParseError
    }
}

#[cfg(test)]
mod tests {
    use crate::{Piece::*, Position, Square::*, ToMove, UciMove};

    #[test]
    fn uci_move_parse() {
        let strs = ["e2e4", "g7g8q", "g1f3", "c7c5"];
        #[rustfmt::skip]
        let moves = [
            UciMove { from: E2, to: E4, promotion: None },
            UciMove { from: G7, to: G8, promotion: Some(Queen)},
            UciMove { from: G1, to: F3, promotion: None },
            UciMove { from: C7, to: C5, promotion: None },
        ];

        for (str, mv) in strs.into_iter().zip(moves.into_iter()) {
            assert_eq!(str.parse(), Ok(mv));
        }
    }

    #[test]
    fn uci_move_roundtrip_std() {
        let moves = [
            "g1f3", "d7d5", "g2g3", "c7c5", "f1g2", "b8c6", "d2d4", "e7e6", "e1g1", "c5d4", "f3d4",
            "g8e7", "c2c4", "c6d4", "d1d4", "e7c6", "d4d1", "d5d4", "e2e3", "f8c5", "e3d4", "c5d4",
            "b1c3", "e8g8", "c3b5", "d4b6", "b2b3", "a7a6", "b5c3", "b6d4", "c1b2", "e6e5", "d1d2",
            "c8e6", "c3d5", "b7b5", "c4b5", "a6b5", "d5f4", "e5f4", "g2c6", "d4b2", "d2b2", "a8b8",
            "f1d1", "d8b6", "c6f3", "f4g3", "h2g3", "b5b4", "a2a4", "b4a3", "a1a3", "g7g6", "b2d4",
            "b6b5", "b3b4", "b5b4", "d4b4", "b8b4", "a3a8", "f8a8", "f3a8", "g6g5", "a8d5", "e6f5",
            "d1c1", "g8g7", "c1c7", "f5g6", "c7c4", "b4b1", "g1g2", "b1e1", "c4b4", "h7h5", "b4a4",
            "e1e5", "d5f3", "g7h6", "g2g1", "e5e6", "a4c4", "g5g4", "f3d5", "e6d6", "d5b7", "h6g5",
            "f2f3", "f7f5", "f3g4", "h5g4", "c4b4", "g6f7", "g1f2", "d6d2", "f2g1", "g5f6", "b4b6",
            "f6g5", "b6b4", "f7e6", "b4a4", "d2b2", "b7a8", "g5f6", "a4f4", "f6e5", "f4f2", "b2f2",
            "g1f2", "e6d5", "a8d5", "e5d5", "f2e3", "d5e5", "e3d3", "f5f4", "d3e2", "f4f3", "e2e3",
            "e5f5", "e3f2", "f5e4", "f2f1", "f3f2", "f1f2", "e4d3", "f2f1", "d3e3", "f1g2", "e3e2",
            "g2g1", "e2f3", "g1h2", "f3f2", "h2h1", "f2g3", "h1g1", "g3h3", "g1f2", "h3h2", "f2f1",
            "g4g3", "f1e2", "g3g2", "e2d3", "g2g1q", "d3c4", "h2g3",
        ];

        let mut position = Position::new_initial();
        for mv in moves {
            let uci = mv.parse::<UciMove>().unwrap();
            let mv = uci.to_move(&position).unwrap_or_else(|_| {
                panic!("move: {}\npos:\n{}", uci, position);
            });
            position.play_unchecked(&mv);

            let uci_back = UciMove::from_move_standard(mv);
            assert_eq!(uci_back, uci);
        }
    }

    #[test]
    fn uci_move_roundtrip_960() {
        let moves = [
            "d2d4", "d7d5", "f2f3", "e8b5", "a2a4", "b5a6", "h1f2", "h8g6", "f2d3", "a6d3", "d1d3",
            "e7e5", "e2e3", "g8f6", "d3b3", "a7a6", "g2g4", "c7c5", "d4c5", "c8c5", "g1e2", "f8d6",
            "e2g3", "g6e7", "b3c3", "c5a7", "a4a5", "d6b4", "c3b3", "b4e1", "c1e1", "e7c6", "g4g5",
            "f6d7", "f1h3", "a7c5", "g3h5", "c5f8", "f3f4", "d7c5", "b3b6", "b8a7", "f4e5", "c6e5",
            "e1b4", "a8b8", "h5g7", "e5c6", "b4a3", "d5d4", "e3d4", "d8d4", "g7f5", "d4a4", "a3g3",
            "a4a1", "b1a1", "f8d8", "g3g1", "d8d5", "b2b4", "d5e5", "a1a2", "b8c8", "b4c5", "e5c3",
            "b6b3", "c3c2", "a2a3", "c6a5", "c5c6", "a7b8", "g1g3", "c8c7", "b3c3", "c2b1", "f5d4",
            "b7b6", "g3d6", "b1a1", "a3b4", "a5b7", "d6f8", "b8a7", "d4b5", "a6b5", "c3a3", "a1a3",
            "b4a3", "c7c6", "h3g2", "c6c7", "g2e4", "b5b4", "a3b4", "c7d7", "f8e8", "d7c7", "e4d5",
            "b6b5", "h2h4", "a7a6", "e8b5", "a6a7", "d5e4", "c7e7", "e4h7", "f7f6", "g5g6", "e7c7",
            "b5d5", "a7b8", "h4h5", "f6f5", "h5h6", "c7c1", "g6g7", "c1b1", "b4a3", "b1a1", "a3b2",
            "a1g1", "h7f5", "b7a5", "d5a5", "g1g2", "f5c2", "b8c8", "h6h7", "g2c2", "b2c2", "c8d7",
            "g7g8q", "d7d6", "h7h8q", "d6d7", "a5a6", "d7e7", "a6e6",
        ];

        let mut position =
            Position::from_fen("rkqrbbnn/pppppppp/8/8/8/8/PPPPPPPP/RKQRBBNN w KQkq - 0 1").unwrap();
        for mv in moves {
            let uci = mv.parse::<UciMove>().unwrap();
            let mv = uci.to_move(&position).unwrap();
            position.play_unchecked(&mv);

            let uci_back = UciMove::from_move_chess960(mv);
            assert_eq!(uci_back, uci);
        }
    }
}
