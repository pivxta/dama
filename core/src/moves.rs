use crate::{squareset::SquareSet, square::Square};

#[inline]
pub fn bishop(square: Square, occupied: SquareSet) -> SquareSet {
    const BISHOP_OFFSETS: [(i32, i32); 4] = [(-1, -1), (1, -1), (1, 1), (-1, 1)];
    slider(square, occupied, &BISHOP_OFFSETS)
}

#[inline]
pub fn rook(square: Square, occupied: SquareSet) -> SquareSet {
    const ROOK_OFFSETS: [(i32, i32); 4] = [(0, 1), (0, -1), (1, 0), (-1, 0)];
    slider(square, occupied, &ROOK_OFFSETS)
}

#[inline]
pub fn king(square: Square) -> SquareSet {
    const KING_OFFSETS: [(i32, i32); 8] = [
        (-1, -1),
        (-1, 0),
        (-1, 1),
        (0, 1),
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
    ];
    non_slider(square, &KING_OFFSETS)
}

#[inline]
pub fn knight(square: Square) -> SquareSet {
    const KNIGHT_OFFSETS: [(i32, i32); 8] = [
        (-1, 2),
        (-2, 1),
        (1, 2),
        (2, 1),
        (-1, -2),
        (-2, -1),
        (1, -2),
        (2, -1),
    ];
    non_slider(square, &KNIGHT_OFFSETS)
}

#[inline]
pub fn black_pawn_captures(square: Square) -> SquareSet {
    non_slider(square, &[(-1, -1), (1, -1)])
}

#[inline]
pub fn white_pawn_captures(square: Square) -> SquareSet {
    non_slider(square, &[(-1, 1), (1, 1)])
}

fn non_slider(square: Square, offsets: &[(i32, i32)]) -> SquareSet {
    let mut moves = SquareSet::EMPTY;
    for (file_offset, rank_offset) in offsets {
        if let Some(square) = square.offset_by(*file_offset, *rank_offset) {
            moves |= square.into();
        }
    }
    moves
}

fn slider(square: Square, occupied: SquareSet, offsets: &[(i32, i32)]) -> SquareSet {
    let mut moves = SquareSet::EMPTY;
    for (file_offset, rank_offset) in offsets {
        let mut current_square = square;
        while let Some(square) = current_square.offset_by(*file_offset, *rank_offset) {
            moves |= square.into();
            if occupied.contains(square) {
                break;
            }
            current_square = square;
        }
    }
    moves
}
