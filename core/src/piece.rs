use std::str::FromStr;

use enum_map::{Enum, EnumMap};
use thiserror::Error;

pub type ByPiece<T> = EnumMap<Piece, T>;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Enum)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq, Error)]
#[error("invalid piece character, expected 'p|n|b|r|q|k|P|N|B|R|Q|K'")]
pub struct PieceParseError;

impl Piece {
    pub const ALL: [Piece; 6] = [
        Piece::Pawn,
        Piece::Knight,
        Piece::Bishop,
        Piece::Rook,
        Piece::Queen,
        Piece::King,
    ];
    pub const COUNT: usize = Self::ALL.len();
}

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
