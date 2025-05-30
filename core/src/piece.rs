use crate::helpers::mapped_enum;
use std::str::FromStr;
use thiserror::Error;

mapped_enum! {
    #[repr(u8)]
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Piece {
        Pawn,
        Knight,
        Bishop,
        Rook,
        Queen,
        King,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
    pub map ByPiece {
        Pawn => pawn,
        Knight => knight,
        Bishop => bishop,
        Rook => rook,
        Queen => queen,
        King => king
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Error)]
#[error("invalid piece character, expected 'p|n|b|r|q|k|P|N|B|R|Q|K'")]
pub struct PieceParseError;

impl FromStr for Piece {
    type Err = PieceParseError;
    fn from_str(s: &str) -> Result<Self, PieceParseError> {
        match s {
            "P" | "p" => Ok(Piece::Pawn),
            "N" | "n" => Ok(Piece::Knight),
            "B" | "b" => Ok(Piece::Bishop),
            "R" | "r" => Ok(Piece::Rook),
            "Q" | "q" => Ok(Piece::Queen),
            "K" | "k" => Ok(Piece::King),
            _ => Err(PieceParseError),
        }
    }
}

impl TryFrom<char> for Piece {
    type Error = PieceParseError;
    fn try_from(ch: char) -> Result<Self, PieceParseError> {
        match ch {
            'P' | 'p' => Ok(Piece::Pawn),
            'N' | 'n' => Ok(Piece::Knight),
            'B' | 'b' => Ok(Piece::Bishop),
            'R' | 'r' => Ok(Piece::Rook),
            'Q' | 'q' => Ok(Piece::Queen),
            'K' | 'k' => Ok(Piece::King),
            _ => Err(PieceParseError),
        }
    }
}
