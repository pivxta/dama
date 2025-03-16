use core::fmt;
use std::str::FromStr;
use thiserror::Error;

use crate::{
    CastlingSide, Color, File, Move, MoveKind, Piece, Position, Rank, Square, SquareSet,
    SquareSets, ToMove,
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SanMove {
    pub kind: SanKind,
    pub postfix: Option<Postfix>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum SanKind {
    Simple {
        piece: Piece,
        from_file: Option<File>,
        from_rank: Option<Rank>,
        is_capture: bool,
        to: Square,
        promotion: Option<Piece>,
    },
    Castling(CastlingSide),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Postfix {
    Check,
    Checkmate,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
pub enum SanError {
    #[error("invalid SAN move.")]
    InvalidNotation,
    #[error("ambiguous SAN move.")]
    AmbiguousMove,
    #[error("illegal move.")]
    IllegalMove,
}

#[derive(Debug, Default, Clone, PartialEq, Eq, Error)]
#[error("invalid SAN move.")]
pub struct SanParseError;

impl SanMove {
    #[inline]
    pub fn moved_piece(&self) -> Piece {
        match self.kind {
            SanKind::Castling(_) => Piece::King,
            SanKind::Simple { piece, .. } => piece,
        }
    }

    #[inline]
    pub fn is_capture(&self) -> bool {
        match self.kind {
            SanKind::Simple { is_capture, .. } => is_capture,
            _ => false,
        }
    }

    #[inline]
    pub fn promotion(&self) -> Option<Piece> {
        match self.kind {
            SanKind::Simple { promotion, .. } => promotion,
            _ => None,
        }
    }
}

impl ToMove for SanMove {
    type Error = SanError;

    #[inline]
    fn to_move(&self, position: &Position) -> Result<Move, SanError> {
        match self.kind {
            SanKind::Castling(side) => {
                let mv = castling_move(position, side)?;
                if !position.is_legal(&mv) {
                    return Err(SanError::IllegalMove);
                }
                Ok(mv)
            }
            SanKind::Simple {
                piece,
                from_file,
                from_rank,
                is_capture,
                to,
                promotion,
            } => {
                let us = position.side_to_move();
                let them = !position.side_to_move();
                let mut candidates = position.pieces(piece) & position.us();

                candidates &= match piece {
                    Piece::Pawn if is_capture => SquareSet::pawn_attacks(them, to),
                    Piece::Pawn => reverse_pawn_push(us, to, position.occupied()),
                    _ => piece_moves(piece, to, position.occupied()),
                };

                if let Some(from_file) = from_file {
                    candidates &= from_file.into();
                }

                if let Some(from_rank) = from_rank {
                    candidates &= from_rank.into();
                }

                if candidates.count() > 1 {
                    return Err(SanError::AmbiguousMove);
                }

                let from = candidates.first().ok_or(SanError::IllegalMove)?;
                let (mv, is_move_capture) =
                    if piece == Piece::Pawn && position.en_passant() == Some(to) {
                        (
                            Move::new_en_passant(from, to, position.en_passant_target().unwrap()),
                            true,
                        )
                    } else {
                        (
                            Move {
                                kind: MoveKind::Normal { promotion },
                                from,
                                to,
                            },
                            position.piece_at(to).is_some(),
                        )
                    };

                if is_capture != is_move_capture {
                    return Err(SanError::InvalidNotation);
                }

                if !position.is_legal(&mv) {
                    return Err(SanError::IllegalMove);
                }

                Ok(mv)
            }
        }
    }
}

impl FromStr for SanMove {
    type Err = SanParseError;
    fn from_str(s: &str) -> Result<Self, SanParseError> {
        if s.is_empty() {
            return Err(SanParseError);
        }

        let mut chars = s.chars().rev().peekable();

        let postfix = chars
            .next_if(|c| matches!(c, '#' | '+'))
            .and_then(notation_to_postfix);

        if chars.next_if_eq(&'O').is_some() {
            if chars.next_if_eq(&'-').is_none() || chars.next_if_eq(&'O').is_none() {
                return Err(SanParseError);
            }

            let side = if chars.next_if_eq(&'-').is_some() && chars.next_if_eq(&'O').is_some() {
                CastlingSide::Queen
            } else if chars.peek().is_none() {
                CastlingSide::King
            } else {
                return Err(SanParseError);
            };

            return Ok(SanMove {
                kind: SanKind::Castling(side),
                postfix,
            });
        }

        let promotion = chars
            .next_if(|c| matches!(c, 'N' | 'B' | 'R' | 'Q'))
            .and_then(notation_to_piece);

        if promotion.is_some() && chars.next_if_eq(&'=').is_none() {
            return Err(SanParseError);
        }

        let to = match (
            chars.next_if(|c| matches!(c, '1'..='8')),
            chars.next_if(|c| matches!(c, 'a'..='h')),
        ) {
            (Some(rank), Some(file)) => {
                Square::new(File::try_from(file).unwrap(), Rank::try_from(rank).unwrap())
            }
            _ => return Err(SanParseError),
        };

        let is_capture = chars.next_if_eq(&'x').is_some();

        let from_rank = chars
            .next_if(|c| matches!(c, '1'..='8'))
            .map(|c| Rank::try_from(c).unwrap());
        let from_file = chars
            .next_if(|c| matches!(c, 'a'..='h'))
            .map(|c| File::try_from(c).unwrap());

        let piece = chars
            .next()
            .map(|c| notation_to_piece(c).ok_or(SanParseError))
            .unwrap_or(Ok(Piece::Pawn))?;

        if chars.peek().is_some() {
            return Err(SanParseError);
        }

        Ok(SanMove {
            kind: SanKind::Simple {
                piece,
                is_capture,
                from_file,
                from_rank,
                to,
                promotion,
            },
            postfix,
        })
    }
}

impl fmt::Display for Postfix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Postfix::Check => write!(f, "+"),
            Postfix::Checkmate => write!(f, "#"),
        }
    }
}

impl fmt::Display for SanMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            SanKind::Simple {
                piece,
                from_file,
                from_rank,
                is_capture,
                to,
                promotion,
            } => {
                write!(f, "{}", piece_to_notation(piece))?;
                if let Some(file) = from_file {
                    write!(f, "{}", file)?;
                }
                if let Some(rank) = from_rank {
                    write!(f, "{}", rank)?;
                }
                if is_capture {
                    write!(f, "x")?;
                }
                write!(f, "{}", to)?;
                if let Some(promotion) = promotion {
                    write!(f, "={}", piece_to_notation(promotion))?;
                }
            }
            SanKind::Castling(side) => {
                write!(
                    f,
                    "{}",
                    match side {
                        CastlingSide::King => "O-O",
                        CastlingSide::Queen => "O-O-O",
                    }
                )?;
            }
        }
        if let Some(postfix) = self.postfix {
            write!(f, "{}", postfix)?;
        }
        Ok(())
    }
}

fn notation_to_postfix(c: char) -> Option<Postfix> {
    match c {
        '+' => Some(Postfix::Check),
        '#' => Some(Postfix::Checkmate),
        _ => None,
    }
}

fn notation_to_piece(c: char) -> Option<Piece> {
    match c {
        'P' => Some(Piece::Pawn),
        'N' => Some(Piece::Knight),
        'B' => Some(Piece::Bishop),
        'R' => Some(Piece::Rook),
        'Q' => Some(Piece::Queen),
        'K' => Some(Piece::King),
        _ => None,
    }
}

fn piece_to_notation(piece: Piece) -> &'static str {
    match piece {
        Piece::Pawn => "",
        Piece::Knight => "N",
        Piece::Bishop => "B",
        Piece::Rook => "R",
        Piece::Queen => "Q",
        Piece::King => "K",
    }
}

#[inline]
fn castling_move(position: &Position, side: CastlingSide) -> Result<Move, SanError> {
    use SanError::*;
    let from = position.our_king();
    let castling = position.our_castling();
    let backrank = position.our_backrank();
    let to = match side {
        CastlingSide::King => Square::new(File::G, backrank),
        CastlingSide::Queen => Square::new(File::C, backrank),
    };
    let rook = match side {
        CastlingSide::King => Square::new(castling.king_side.ok_or(IllegalMove)?, backrank),
        CastlingSide::Queen => Square::new(castling.queen_side.ok_or(IllegalMove)?, backrank),
    };
    Ok(Move {
        from,
        to,
        kind: MoveKind::Castles { rook },
    })
}

#[inline]
fn piece_moves(piece: Piece, square: Square, occupied: SquareSet) -> SquareSet {
    match piece {
        Piece::Knight => SquareSet::knight_moves(square),
        Piece::King => SquareSet::king_moves(square),
        Piece::Bishop => SquareSet::bishop_moves(square, occupied),
        Piece::Rook => SquareSet::rook_moves(square, occupied),
        Piece::Queen => SquareSet::queen_moves(square, occupied),
        _ => SquareSet::default(),
    }
}

#[inline]
fn reverse_pawn_push(color: Color, square: Square, occupied: SquareSet) -> SquareSet {
    let pawn = SquareSet::from(square);
    let single_push = match color {
        Color::Black => pawn.shift_up(1) & occupied,
        Color::White => pawn.shift_down(1) & occupied,
    };
    if !single_push.is_empty() || square.rank() != Rank::fourth_for(color) {
        single_push
    } else {
        match color {
            Color::Black => pawn.shift_up(2) & occupied,
            Color::White => pawn.shift_down(2) & occupied,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        san::{Postfix, SanKind},
        CastlingSide, File, Piece, Rank, SanMove,
        Square::*,
    };

    #[test]
    fn san_pawn() {
        check_san(
            "e4",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Pawn,
                    from_file: None,
                    from_rank: None,
                    is_capture: false,
                    to: E4,
                    promotion: None,
                },
                postfix: None,
            },
        );
        check_san(
            "exd5",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Pawn,
                    from_file: Some(File::E),
                    from_rank: None,
                    is_capture: true,
                    to: D5,
                    promotion: None,
                },
                postfix: None,
            },
        );
        check_san(
            "fxg8=Q",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Pawn,
                    from_file: Some(File::F),
                    from_rank: None,
                    is_capture: true,
                    to: G8,
                    promotion: Some(Piece::Queen),
                },
                postfix: None,
            },
        );
        check_san(
            "f6#",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Pawn,
                    from_file: None,
                    from_rank: None,
                    is_capture: false,
                    to: F6,
                    promotion: None,
                },
                postfix: Some(Postfix::Checkmate),
            },
        );
        check_san(
            "bxc4+",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Pawn,
                    from_file: Some(File::B),
                    from_rank: None,
                    is_capture: true,
                    to: C4,
                    promotion: None,
                },
                postfix: Some(Postfix::Check),
            },
        );
    }

    #[test]
    fn san_castle() {
        check_san(
            "O-O",
            SanMove {
                kind: SanKind::Castling(CastlingSide::King),
                postfix: None,
            },
        );
        check_san(
            "O-O-O",
            SanMove {
                kind: SanKind::Castling(CastlingSide::Queen),
                postfix: None,
            },
        );
        check_san(
            "O-O-O+",
            SanMove {
                kind: SanKind::Castling(CastlingSide::Queen),
                postfix: Some(Postfix::Check),
            },
        );
        check_san(
            "O-O#",
            SanMove {
                kind: SanKind::Castling(CastlingSide::King),
                postfix: Some(Postfix::Checkmate),
            },
        );
    }

    #[test]
    fn san_pieces() {
        check_san(
            "N1d2",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Knight,
                    from_file: None,
                    from_rank: Some(Rank::First),
                    is_capture: false,
                    to: D2,
                    promotion: None,
                },
                postfix: None,
            },
        );
        check_san(
            "Rgxg7#",
            SanMove {
                kind: SanKind::Simple {
                    piece: Piece::Rook,
                    from_file: Some(File::G),
                    from_rank: None,
                    is_capture: true,
                    to: G7,
                    promotion: None,
                },
                postfix: Some(Postfix::Checkmate),
            },
        );
    }

    fn check_san(str: &str, san: SanMove) {
        assert_eq!(str.parse(), Ok(san));
        assert_eq!(format!("{}", san), str);
    }
}
