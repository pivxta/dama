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
    pub suffix: Option<Suffix>,
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
pub enum Suffix {
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
    pub fn from_ascii(s: &[u8]) -> Result<Self, SanParseError> {
        if s.is_empty() {
            return Err(SanParseError);
        }

        let mut chars = s.iter().cloned().rev().peekable();

        let suffix = chars
            .next_if(|c| matches!(c, b'#' | b'+'))
            .and_then(notation_to_suffix);

        if chars.next_if_eq(&b'O').is_some() {
            if chars.next_if_eq(&b'-').is_none() || chars.next_if_eq(&b'O').is_none() {
                return Err(SanParseError);
            }

            let side = if chars.next_if_eq(&b'-').is_some() && chars.next_if_eq(&b'O').is_some() {
                CastlingSide::Queen
            } else if chars.peek().is_none() {
                CastlingSide::King
            } else {
                return Err(SanParseError);
            };

            return Ok(SanMove {
                kind: SanKind::Castling(side),
                suffix,
            });
        }

        let promotion = chars
            .next_if(|c| matches!(c, b'N' | b'B' | b'R' | b'Q'))
            .and_then(notation_to_piece);

        if promotion.is_some() && chars.next_if_eq(&b'=').is_none() {
            return Err(SanParseError);
        }

        let to = match (
            chars.next_if(|c| matches!(c, b'1'..=b'8')),
            chars.next_if(|c| matches!(c, b'a'..=b'h')),
        ) {
            (Some(rank), Some(file)) => Square::new(
                File::try_from(file as char).unwrap(),
                Rank::try_from(rank as char).unwrap(),
            ),
            _ => return Err(SanParseError),
        };

        let is_capture = chars.next_if_eq(&b'x').is_some();

        let from_rank = chars
            .next_if(|c| matches!(c, b'1'..=b'8'))
            .map(|c| Rank::try_from(c as char).unwrap());
        let from_file = chars
            .next_if(|c| matches!(c, b'a'..=b'h'))
            .map(|c| File::try_from(c as char).unwrap());

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
            suffix,
        })
    }

    #[inline]
    pub fn from_move(_mv: Move, _position: &Position) -> SanMove {
        todo!();
    }

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

                candidates.keep(match piece {
                    Piece::Pawn if is_capture => SquareSet::pawn_attacks(them, to),
                    Piece::Pawn => reverse_pawn_push(us, to, position.occupied()),
                    _ => piece_moves(piece, to, position.occupied()),
                });

                if let Some(from_file) = from_file {
                    candidates.keep(from_file.into());
                }

                if let Some(from_rank) = from_rank {
                    candidates.keep(from_rank.into());
                }

                let mut candidates = candidates.iter().filter_map(|from| {
                    let mv = if piece == Piece::Pawn && position.en_passant() == Some(to) {
                        Move::new_en_passant(from, to, position.en_passant_target().unwrap())
                    } else {
                        Move {
                            kind: MoveKind::Normal { promotion },
                            from,
                            to,
                        }
                    };

                    if position.is_legal(&mv) {
                        Some(mv)
                    } else {
                        None
                    }
                });

                let mv = candidates.next().ok_or(SanError::IllegalMove)?;
                if candidates.next().is_some() {
                    return Err(SanError::AmbiguousMove);
                }

                let is_move_capture = position.occupied().contains(to)
                    || (piece == Piece::Pawn && position.en_passant() == Some(to));

                if is_capture != is_move_capture {
                    return Err(SanError::InvalidNotation);
                }

                Ok(mv)
            }
        }
    }
}

impl FromStr for SanMove {
    type Err = SanParseError;
    fn from_str(s: &str) -> Result<Self, SanParseError> {
        SanMove::from_ascii(s.as_bytes())
    }
}

impl fmt::Display for Suffix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Suffix::Check => write!(f, "+"),
            Suffix::Checkmate => write!(f, "#"),
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
        if let Some(suffix) = self.suffix {
            write!(f, "{}", suffix)?;
        }
        Ok(())
    }
}

fn notation_to_suffix(c: u8) -> Option<Suffix> {
    match c {
        b'+' => Some(Suffix::Check),
        b'#' => Some(Suffix::Checkmate),
        _ => None,
    }
}

fn notation_to_piece(c: u8) -> Option<Piece> {
    match c {
        b'P' => Some(Piece::Pawn),
        b'N' => Some(Piece::Knight),
        b'B' => Some(Piece::Bishop),
        b'R' => Some(Piece::Rook),
        b'Q' => Some(Piece::Queen),
        b'K' => Some(Piece::King),
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
    let from = position.our_king().ok_or(SanError::IllegalMove)?;
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
        Color::Black => pawn.offset_ranks_by(1) & occupied,
        Color::White => pawn.offset_ranks_by(-1) & occupied,
    };
    if !single_push.is_empty() || square.rank() != Rank::fourth_for(color) {
        single_push
    } else {
        match color {
            Color::Black => pawn.offset_ranks_by(2) & occupied,
            Color::White => pawn.offset_ranks_by(-2) & occupied,
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        san::{SanKind, Suffix},
        CastlingSide, File, Piece, Position, Rank, SanMove,
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
                suffix: None,
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
                suffix: None,
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
                suffix: None,
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
                suffix: Some(Suffix::Checkmate),
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
                suffix: Some(Suffix::Check),
            },
        );
    }

    #[test]
    fn san_castle() {
        check_san(
            "O-O",
            SanMove {
                kind: SanKind::Castling(CastlingSide::King),
                suffix: None,
            },
        );
        check_san(
            "O-O-O",
            SanMove {
                kind: SanKind::Castling(CastlingSide::Queen),
                suffix: None,
            },
        );
        check_san(
            "O-O-O+",
            SanMove {
                kind: SanKind::Castling(CastlingSide::Queen),
                suffix: Some(Suffix::Check),
            },
        );
        check_san(
            "O-O#",
            SanMove {
                kind: SanKind::Castling(CastlingSide::King),
                suffix: Some(Suffix::Checkmate),
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
                suffix: None,
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
                suffix: Some(Suffix::Checkmate),
            },
        );
    }

    #[test]
    fn san_pseudolegal_ambiguity() {
        let mut position = Position::from_fen(
            "r1bqkb1r/ppp2ppp/2npn3/1B2p3/4P3/2NPBNP1/PPP2P1P/R2Q1RK1 b kq - 2 8",
        )
        .unwrap();
        position.play(&"Nd4".parse::<SanMove>().unwrap()).unwrap();
        assert_eq!(
            position.fen().to_string(),
            "r1bqkb1r/ppp2ppp/2np4/1B2p3/3nP3/2NPBNP1/PPP2P1P/R2Q1RK1 w kq - 3 9"
        );
    }

    fn check_san(str: &str, san: SanMove) {
        assert_eq!(str.parse(), Ok(san));
        assert_eq!(format!("{}", san), str);
    }

    #[test]
    fn san_play() {
        let moves = [
            "Nf3", "d5", "g3", "c5", "Bg2", "Nc6", "d4", "e6", "O-O", "cxd4", "Nxd4", "Nge7", "c4",
            "Nxd4", "Qxd4", "Nc6", "Qd1", "d4", "e3", "Bc5", "exd4", "Bxd4", "Nc3", "O-O", "Nb5",
            "Bb6", "b3", "a6", "Nc3", "Bd4", "Bb2", "e5", "Qd2", "Be6", "Nd5", "b5", "cxb5",
            "axb5", "Nf4", "exf4", "Bxc6", "Bxb2", "Qxb2", "Rb8", "Rfd1", "Qb6", "Bf3", "fxg3",
            "hxg3", "b4", "a4", "bxa3", "Rxa3", "g6", "Qd4", "Qb5", "b4", "Qxb4", "Qxb4", "Rxb4",
            "Ra8", "Rxa8", "Bxa8", "g5", "Bd5", "Bf5", "Rc1", "Kg7", "Rc7", "Bg6", "Rc4", "Rb1+",
            "Kg2", "Re1", "Rb4", "h5", "Ra4", "Re5", "Bf3", "Kh6", "Kg1", "Re6", "Rc4", "g4",
            "Bd5", "Rd6", "Bb7", "Kg5", "f3", "f5", "fxg4", "hxg4", "Rb4", "Bf7", "Kf2", "Rd2+",
            "Kg1", "Kf6", "Rb6+", "Kg5", "Rb4", "Be6", "Ra4", "Rb2", "Ba8", "Kf6", "Rf4", "Ke5",
            "Rf2", "Rxf2", "Kxf2", "Bd5", "Bxd5", "Kxd5", "Ke3", "Ke5",
        ];
        let mut position = Position::new_initial();

        for san in moves.into_iter().map(SanMove::from_str).map(Result::unwrap) {
            position.play(&san).unwrap();
        }

        assert_eq!(
            position.fen().to_string(),
            "8/8/8/4kp2/6p1/4K1P1/8/8 w - - 2 59"
        );
    }

    #[test]
    fn san_play960() {
        let moves = [
            "e4", "e5", "Nf3", "Nf6", "a4", "c6", "b4", "Qc7", "Qb3", "Ng6", "Rb1", "d5", "Ng3",
            "O-O-O", "Bd3", "dxe4", "Nxe4", "Nf4", "Nxf6", "gxf6", "Bf5+", "Kb8", "g3", "Nd5",
            "O-O", "Nxb4", "d4", "c5", "dxe5", "b6", "Rfd1", "Qc6", "exf6", "Bb7", "Bg4", "c4",
            "Qc3", "a5", "Rxd8+", "Rxd8", "Qe5+", "Ka7", "Qf5", "Bc5", "Bc3", "Nxc2", "Rc1", "Ne3",
            "fxe3", "Bxe3+", "Kg2", "Bxc1", "Kh3", "Rd3", "Bd4", "Qd5",
        ];
        let mut position =
            Position::from_fen("bqrkrbnn/pppppppp/8/8/8/8/PPPPPPPP/BQRKRBNN w KQkq - 0 1").unwrap();

        for san in moves.into_iter().map(SanMove::from_str).map(Result::unwrap) {
            position.play(&san).unwrap();
        }

        assert_eq!(
            position.fen().to_string(),
            "8/kb3p1p/1p3P2/p2q1Q2/P1pB2B1/3r1NPK/7P/2b5 w - - 4 29"
        );
    }
}
