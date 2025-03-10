use core::fmt;
use std::str::FromStr;

use crate::{
    Piece, PieceParseError,
    Square, SquareParseError,
};
use thiserror::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Move {
    pub from: Square,
    pub to: Square,
    pub promotion: Option<Piece>,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Error)]
#[error("invalid move notation.")]
pub struct MoveParseError;

impl FromStr for Move {
    type Err = MoveParseError;

    fn from_str(s: &str) -> Result<Self, MoveParseError> {
        let promotion = match s.len() {
            4 => None,
            5 => Some(s[4..5].parse()?),
            _ => return Err(MoveParseError),
        };

        Ok(Move {
            from: s[0..2].parse()?,
            to: s[2..4].parse()?,
            promotion,
        })
    }
}

impl fmt::Display for Move {
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

impl From<SquareParseError> for MoveParseError {
    fn from(_: SquareParseError) -> Self {
        MoveParseError
    }
}

impl From<PieceParseError> for MoveParseError {
    fn from(_: PieceParseError) -> Self {
        MoveParseError
    }
}

#[cfg(test)]
mod tests {
    use crate::{mv::Move, Piece::*, Square::*};

    #[test]
    fn uci_move_parse() {
        let strs = ["e2e4", "g7g8q", "g1f3", "c7c5"];
        #[rustfmt::skip]
        let moves = [
            Move { from: E2, to: E4, promotion: None },
            Move { from: G7, to: G8, promotion: Some(Queen)},
            Move { from: G1, to: F3, promotion: None },
            Move { from: C7, to: C5, promotion: None },
        ];

        for (str, mv) in strs.into_iter().zip(moves.into_iter()) {
            assert_eq!(str.parse(), Ok(mv));
        }
    }
}
