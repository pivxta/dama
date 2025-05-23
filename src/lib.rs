pub mod fen;
mod game;
#[doc(hidden)]
pub mod helpers;
mod movegen;
mod mv;
mod perft;
pub mod pgn;
pub mod position;
pub mod san;
mod squaresets;
mod uci;
mod zobrist;

pub use dama_core::{
    castling::{Castling, CastlingSide},
    color::{ByColor, Color},
    piece::{ByPiece, Piece, PieceParseError},
    square::{BySquare, File, Rank, Square, SquareParseError},
    squareset::{self, SquareSet},
};
pub use fen::{Fen, FenError, FenParseError, FormattedFen};
pub use game::Outcome;
pub use movegen::{MoveList, MAX_LEGAL_MOVES};
pub use mv::{Move, MoveKind, ToMove};
pub use perft::perft;
pub use position::{IllegalMoveError, InvalidPositionError, Position, Variant};
pub use san::{SanError, SanKind, SanMove, SanParseError};
pub use squaresets::SquareSets;
pub use uci::{UciMove, UciMoveParseError};
