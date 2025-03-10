use core::fmt;
use std::str::FromStr;
use thiserror::Error;

use crate::{CastlingSide, File, Move, Piece, Position, Rank, Square, Variant};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct San {
    pub kind: Kind,
    pub postfix: Option<Postfix>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
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

impl San {
    #[inline]
    pub fn moved_piece(&self) -> Piece {
        match self.kind {
            Kind::Castling(_) => Piece::King,
            Kind::Simple { piece, .. } => piece
        }
    }

    #[inline]
    pub fn to_move(&self, position: &Position) -> Result<Move, SanError> {
        match self.kind {
            Kind::Castling(side) => {
                let mv = castling_move(position, side)?;
                if !position.is_legal(&mv) {
                    return Err(SanError::IllegalMove);
                }
                Ok(mv)
            }
            Kind::Simple {
                piece,
                from_file,
                from_rank,
                is_capture,
                to,
                promotion,
            } => {
                let mut candidates = position.legal_moves_for(piece);
                candidates.retain(|mv| {
                    mv.to == to
                        && mv.promotion == promotion
                        && (from_file.is_none() || from_file == Some(mv.from.file()))
                        && (from_rank.is_none() || from_rank == Some(mv.from.rank()))
                });

                if candidates.len() > 1 {
                    return Err(SanError::AmbiguousMove);
                }

                let mv = candidates.first().ok_or(SanError::IllegalMove)?;
                if is_capture != position.is_capture(mv) {
                    return Err(SanError::InvalidNotation);
                }

                Ok(*mv)
            }
        }
    }
}

impl FromStr for San {
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

            return Ok(San {
                kind: Kind::Castling(side),
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

        Ok(San {
            kind: Kind::Simple {
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

impl fmt::Display for San {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.kind {
            Kind::Simple {
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
            Kind::Castling(side) => {
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
    let to = match (position.variant(), side) {
        (Variant::Standard, CastlingSide::King) => File::G,
        (Variant::Standard, CastlingSide::Queen) => File::C,
        (Variant::Chess960, CastlingSide::King) => castling.king_side.ok_or(IllegalMove)?,
        (Variant::Chess960, CastlingSide::Queen) => castling.queen_side.ok_or(IllegalMove)?,
    };
    let to = Square::new(to, position.our_backrank());
    Ok(Move {
        from,
        to,
        promotion: None,
    })
}

#[cfg(test)]
mod tests {
    use crate::{
        san::{Kind, Postfix},
        CastlingSide, File, Piece, Rank, San,
        Square::*,
    };

    #[test]
    fn san_pawn() {
        check_san(
            "e4",
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Castling(CastlingSide::King),
                postfix: None,
            },
        );
        check_san(
            "O-O-O",
            San {
                kind: Kind::Castling(CastlingSide::Queen),
                postfix: None,
            },
        );
        check_san(
            "O-O-O+",
            San {
                kind: Kind::Castling(CastlingSide::Queen),
                postfix: Some(Postfix::Check),
            },
        );
        check_san(
            "O-O#",
            San {
                kind: Kind::Castling(CastlingSide::King),
                postfix: Some(Postfix::Checkmate),
            },
        );
    }

    #[test]
    fn san_pieces() {
        check_san(
            "N1d2",
            San {
                kind: Kind::Simple {
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
            San {
                kind: Kind::Simple {
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

    fn check_san(str: &str, san: San) {
        assert_eq!(str.parse(), Ok(san));
        assert_eq!(format!("{}", san), str);
    }
}
