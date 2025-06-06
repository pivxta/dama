use crate::{Color, Rank, Square, SquareSet};
use dama_core::magic;

pub trait SquareSets {
    fn king_moves(square: Square) -> SquareSet;
    fn knight_moves(square: Square) -> SquareSet;
    fn bishop_moves(square: Square, occupied: SquareSet) -> SquareSet;
    fn rook_moves(square: Square, occupied: SquareSet) -> SquareSet;
    fn queen_moves(square: Square, occupied: SquareSet) -> SquareSet;

    fn pawn_pushes(color: Color, square: Square, occupied: SquareSet) -> SquareSet;
    fn pawn_attacks(color: Color, square: Square) -> SquareSet;
    fn all_pawn_attacks(color: Color, pawns: SquareSet) -> SquareSet;

    fn between(a: Square, b: Square) -> SquareSet;
    fn ray(a: Square, b: Square) -> SquareSet;
}

impl SquareSets for SquareSet {
    #[inline]
    fn king_moves(square: Square) -> SquareSet {
        SquareSet::from_bits(KING_MOVES[square as usize])
    }

    #[inline]
    fn knight_moves(square: Square) -> SquareSet {
        SquareSet::from_bits(KNIGHT_MOVES[square as usize])
    }

    #[inline]
    fn bishop_moves(square: Square, occupied: SquareSet) -> SquareSet {
        SquareSet::from_bits(SLIDING_TABLE[magic::bishop_table_index(square, occupied)])
    }

    #[inline]
    fn rook_moves(square: Square, occupied: SquareSet) -> SquareSet {
        SquareSet::from_bits(SLIDING_TABLE[magic::rook_table_index(square, occupied)])
    }

    #[inline]
    fn queen_moves(square: Square, occupied: SquareSet) -> SquareSet {
        Self::bishop_moves(square, occupied) | Self::rook_moves(square, occupied)
    }

    #[inline]
    fn pawn_attacks(color: Color, square: Square) -> SquareSet {
        match color {
            Color::White => SquareSet::from_bits(WHITE_PAWN_CAPTURES[square as usize]),
            Color::Black => SquareSet::from_bits(BLACK_PAWN_CAPTURES[square as usize]),
        }
    }

    #[inline]
    fn pawn_pushes(color: Color, square: Square, occupied: SquareSet) -> SquareSet {
        let single_push = match color {
            Color::White => SquareSet::from(square).offset_ranks_by(1) & !occupied,
            Color::Black => SquareSet::from(square).offset_ranks_by(-1) & !occupied,
        };
        if single_push.is_empty() || square.rank() != Rank::second_for(color) {
            return single_push;
        }
        let double_push = match color {
            Color::White => single_push.offset_ranks_by(1) & !occupied,
            Color::Black => single_push.offset_ranks_by(-1) & !occupied,
        };
        single_push | double_push
    }

    #[inline]
    fn all_pawn_attacks(color: Color, pawns: SquareSet) -> SquareSet {
        let front = match color {
            Color::White => pawns.offset_ranks_by(1),
            Color::Black => pawns.offset_ranks_by(-1),
        };
        front.shift_right() | front.shift_left()
    }

    #[inline]
    fn ray(a: Square, b: Square) -> SquareSet {
        SquareSet::from_bits(LINE_TABLE[a as usize][b as usize])
    }

    #[inline]
    fn between(a: Square, b: Square) -> SquareSet {
        SquareSet::from_bits(LINE_BETWEEN_TABLE[a as usize][b as usize])
    }
}

include!(concat!(env!("OUT_DIR"), "/squareset_tables.rs"));
