mod squaresets;
mod movegen;
mod perft;
mod game;
pub mod fen;
pub mod pgn;
pub mod san;
pub mod position;

pub use dama_core::{
    squareset::{self, SquareSet},
    castling::{Castling, CastlingSide},
    color::{ByColor, Color},
    mv::Move,
    piece::{ByPiece, Piece, PieceParseError},
    square::{BySquare, File, Rank, Square, SquareParseError},
};
pub use squaresets::SquareSets;
pub use game::Outcome;
pub use fen::{Fen, FenError, FenParseError, FormattedFen};
pub use movegen::{MoveList, MAX_LEGAL_MOVES};
pub use perft::perft;
pub use position::{IllegalMoveError, InvalidPositionError, Pieces, Position, Variant};
pub use san::{San, SanError, SanParseError};
