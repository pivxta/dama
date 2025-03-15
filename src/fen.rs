use crate::{
    position, ByColor, BySquare, Castling, CastlingSide, Color, File, InvalidPositionError, Piece,
    PieceParseError, Pieces, Position, Rank, Square, SquareParseError, Variant,
};
use core::fmt;
use dama_core::enum_map;
use std::{cmp::Ordering, str::FromStr};
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fen {
    pub setup: position::Setup,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FormattedFen {
    pub format: Format,
    pub fen: Fen,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum Format {
    #[default]
    Fen,
    XFen,
    ShredderFen,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
pub enum FenError {
    #[error("{0}")]
    InvalidFen(FenParseError),
    #[error("{0}")]
    InvalidPosition(InvalidPositionError),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
pub enum FenParseError {
    #[error("invalid number of files in a row, expected 8.")]
    InvalidFileCount,
    #[error("invalid number of rows, expected 8.")]
    InvalidRankCount,
    #[error("invalid side to move, expected 'w' or 'b'.")]
    InvalidSideToMove,
    #[error("invalid castling rights, expected 'K?Q?k?q?' or '-'.")]
    InvalidCastlingRights,
    #[error("invalid en passant square.")]
    InvalidEnPassantSquare,
    #[error("invalid section count.")]
    InvalidSectionCount,
    #[error("invalid halfmove clock number.")]
    InvalidHalfmoveClock,
    #[error("invalid fullmove number.")]
    InvalidFullmoveNumber,
    #[error("unexpected piece character found.")]
    UnexpectedPieceChar,
    #[error("no king found in FEN string.")]
    NoKingFound(Color),
    #[error("no castling rook found in FEN string.")]
    NoCastlingRookFound(Color),
}

impl Fen {
    #[inline]
    pub fn format(self, format: Format) -> FormattedFen {
        FormattedFen { fen: self, format }
    }

    #[inline]
    pub fn into_position(self) -> Result<Position, InvalidPositionError> {
        self.setup.into_position()
    }
}

impl position::Setup {
    pub fn from_fen(fen: &str) -> Result<Self, FenParseError> {
        Ok(Fen::from_str(fen)?.setup)
    }
}

impl FromStr for Fen {
    type Err = FenParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut sections = s.split_whitespace();

        use FenParseError::*;
        let pieces = parse_pieces(sections.next().ok_or(InvalidSectionCount)?)?;
        let side_to_move = parse_side_to_move(sections.next().ok_or(InvalidSectionCount)?)?;
        let castling = parse_castling(&pieces, sections.next().ok_or(InvalidSectionCount)?)?;
        let en_passant = match sections.next() {
            Some(s) => parse_en_passant(s)?,
            None => None,
        };
        let halfmove_clock: u32 = match sections.next() {
            Some(s) => s.parse().map_err(|_| InvalidHalfmoveClock)?,
            None => 0,
        };
        let fullmove_number: u32 = match sections.next() {
            Some(s) => s.parse().map_err(|_| InvalidFullmoveNumber)?,
            None => 0,
        };

        Ok(Fen {
            setup: position::Setup::new_empty()
                .with_pieces(pieces)
                .with_side_to_move(side_to_move)
                .with_castling(Color::White, castling[Color::White])
                .with_castling(Color::Black, castling[Color::Black])
                .with_en_passant(en_passant)
                .with_halfmove_clock(halfmove_clock)
                .with_fullmove_number(fullmove_number),
        })
    }
}

fn parse_en_passant(s: &str) -> Result<Option<Square>, FenParseError> {
    if s == "-" {
        return Ok(None);
    }

    Ok(Some(s.parse()?))
}

fn parse_castling(pieces: &Pieces, s: &str) -> Result<ByColor<Castling>, FenParseError> {
    if s == "-" {
        return Ok(ByColor::from_fn(|_| Castling::NONE));
    }

    if s.len() > 4 {
        return Err(FenParseError::InvalidCastlingRights);
    }

    let king_file = enum_map! {
        Color::White => king_file(pieces, Color::White)?,
        Color::Black => king_file(pieces, Color::Black)?,
    };

    let mut castling: ByColor<Castling> = Default::default();
    for c in s.chars() {
        let color = if c.is_ascii_uppercase() {
            Color::White
        } else {
            Color::Black
        };
        match c.to_ascii_lowercase() {
            'k' => {
                let rook = outermost_rook(pieces, color, CastlingSide::King)?;
                castling[color].king_side = Some(rook);
            }
            'q' => {
                let rook = outermost_rook(pieces, color, CastlingSide::Queen)?;
                castling[color].queen_side = Some(rook);
            }
            c @ 'a'..='h' => {
                let file = File::try_from(c).map_err(|_| FenParseError::InvalidCastlingRights)?;
                match file.cmp(&king_file[color]) {
                    Ordering::Less => castling[color].queen_side = Some(file),
                    Ordering::Greater => castling[color].king_side = Some(file),
                    _ => {}
                }
            }
            _ => return Err(FenParseError::InvalidCastlingRights),
        }
    }
    Ok(castling)
}

fn outermost_rook(
    pieces: &Pieces,
    color: Color,
    side: CastlingSide,
) -> Result<File, FenParseError> {
    match side {
        CastlingSide::King => pieces
            .iter()
            .filter(|(sq, _)| sq.rank() == Rank::back_rank(color))
            .rev()
            .take_while(|(_, &p)| p != Some((color, Piece::King)))
            .find(|(_, &p)| p == Some((color, Piece::Rook)))
            .map(|(sq, _)| sq.file())
            .ok_or(FenParseError::NoCastlingRookFound(color)),
        CastlingSide::Queen => pieces
            .iter()
            .filter(|(sq, _)| sq.rank() == Rank::back_rank(color))
            .take_while(|(_, &p)| p != Some((color, Piece::King)))
            .find(|(_, &p)| p == Some((color, Piece::Rook)))
            .map(|(sq, _)| sq.file())
            .ok_or(FenParseError::NoCastlingRookFound(color)),
    }
}

fn king_file(pieces: &Pieces, color: Color) -> Result<File, FenParseError> {
    pieces
        .iter()
        .find(|(_, &p)| p == Some((color, Piece::King)))
        .map(|(sq, _)| sq.file())
        .ok_or(FenParseError::NoKingFound(color))
}

fn parse_side_to_move(s: &str) -> Result<Color, FenParseError> {
    match s {
        "w" => Ok(Color::White),
        "b" => Ok(Color::Black),
        _ => Err(FenParseError::InvalidSideToMove),
    }
}

fn parse_pieces(s: &str) -> Result<Pieces, FenParseError> {
    let mut pieces = [None; 64];
    let mut index = 0;

    for (n, rank) in s.rsplit('/').enumerate() {
        if n >= 8 {
            return Err(FenParseError::InvalidRankCount);
        }

        pieces[index..index + 8].copy_from_slice(&parse_rank(rank)?);
        index += 8;
    }

    if index != 64 {
        return Err(FenParseError::InvalidRankCount);
    }

    Ok(BySquare::from_array(pieces))
}

fn parse_rank(s: &str) -> Result<[Option<(Color, Piece)>; 8], FenParseError> {
    let mut rank = [None; 8];
    let mut index = 0;

    for ch in s.chars() {
        if index >= 8 {
            return Err(FenParseError::InvalidFileCount);
        }

        match ch {
            '0'..='9' => index += ch.to_digit(10).unwrap() as usize,
            _ => {
                let piece = Piece::try_from(ch)?;
                let color = if ch.is_ascii_uppercase() {
                    Color::White
                } else {
                    Color::Black
                };
                rank[index] = Some((color, piece));
                index += 1;
            }
        }
    }

    if index != 8 {
        return Err(FenParseError::InvalidFileCount);
    }

    Ok(rank)
}

impl fmt::Display for Fen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let format = match self.setup.variant {
            Some(Variant::Standard) => Format::Fen,
            Some(Variant::Chess960) | None => Format::XFen,
        };
        self.write_format(format, f)
    }
}

impl fmt::Display for FormattedFen {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.fen.write_format(self.format, f)
    }
}

impl Fen {
    fn write_format(&self, format: Format, f: &mut fmt::Formatter) -> fmt::Result {
        self.write_pieces(f)?;
        self.write_side_to_move(f)?;
        self.write_castling(format, f)?;
        self.write_en_passant(format, f)?;
        self.write_halfmove_clock(f)?;
        self.write_fullmove_number(f)?;

        Ok(())
    }
    fn write_pieces(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for rank in Rank::ALL.into_iter().rev() {
            self.write_rank(rank, f)?;
            if rank != Rank::First {
                write!(f, "/")?;
            }
        }

        write!(f, " ")
    }

    fn write_rank(&self, rank: Rank, f: &mut fmt::Formatter) -> fmt::Result {
        let piece_chars = enum_map! {
            Piece::Pawn => 'p',
            Piece::Knight => 'n',
            Piece::Bishop => 'b',
            Piece::Rook => 'r',
            Piece::Queen => 'q',
            Piece::King => 'k',
        };

        let mut empty = 0;
        for file in File::ALL {
            let sq = Square::new(file, rank);

            if self.setup.pieces[sq].is_some() && empty != 0 {
                write!(f, "{}", empty)?;
                empty = 0;
            }

            match self.setup.pieces[sq] {
                Some((Color::White, piece)) => {
                    write!(f, "{}", piece_chars[piece].to_ascii_uppercase())?
                }
                Some((Color::Black, piece)) => write!(f, "{}", piece_chars[piece])?,
                _ => {
                    empty += 1;
                    if file == File::H {
                        write!(f, "{}", empty)?;
                    }
                }
            }
        }
        Ok(())
    }

    fn write_side_to_move(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} ",
            match self.setup.side_to_move {
                Color::White => "w",
                Color::Black => "b",
            }
        )
    }

    fn write_castling(&self, format: Format, f: &mut fmt::Formatter) -> fmt::Result {
        if self.setup.castling[Color::White].is_none()
            && self.setup.castling[Color::Black].is_none()
        {
            write!(f, "- ")?;
            return Ok(());
        }

        self.write_castling_side(Color::White, format, f)?;
        self.write_castling_side(Color::Black, format, f)?;

        write!(f, " ")
    }

    fn write_castling_side(
        &self,
        color: Color,
        format: Format,
        f: &mut fmt::Formatter,
    ) -> fmt::Result {
        let (write_queen_side_file, write_king_side_file) =
            self.write_castling_files(format, color);

        let castling = self.setup.castling[color];
        if let Some(king_side) = castling.king_side {
            if write_king_side_file {
                write!(f, "{}", file_char(color, king_side))?;
            } else {
                write!(f, "{}", side_char(color, true))?;
            }
        }
        if let Some(queen_side) = castling.queen_side {
            if write_queen_side_file {
                write!(f, "{}", file_char(color, queen_side))?;
            } else {
                write!(f, "{}", side_char(color, false))?;
            }
        }

        Ok(())
    }

    fn write_castling_files(&self, format: Format, color: Color) -> (bool, bool) {
        match format {
            Format::Fen => return (false, false),
            Format::ShredderFen => return (true, true),
            _ => {}
        }
        let castling = self.setup.castling[color];
        let pieces = &self.setup.pieces;
        let king_file = pieces
            .iter()
            .find(|(_, &p)| p == Some((color, Piece::King)))
            .map(|(sq, _)| sq.file())
            .expect("no king found.");
        let backrank = Rank::back_rank(color);
        let backrank_rooks = pieces
            .iter()
            .filter(|(sq, &p)| p == Some((color, Piece::Rook)) && sq.rank() == backrank)
            .map(|(sq, _)| sq.file());

        let mut queen_side = 0;
        let mut king_side = 0;

        for rook_file in backrank_rooks {
            if rook_file < king_file
                && castling.queen_side.is_some()
                && castling.queen_side.unwrap() > rook_file
            {
                queen_side += 1;
            }
            if rook_file > king_file
                && castling.king_side.is_some()
                && castling.king_side.unwrap() < rook_file
            {
                king_side += 1;
            }
        }

        (queen_side > 0, king_side > 0)
    }

    fn write_en_passant(&self, format: Format, f: &mut fmt::Formatter) -> fmt::Result {
        match self.setup.en_passant_square {
            Some(sq) => {
                if format == Format::XFen {
                    let capture_offset = match self.setup.side_to_move {
                        Color::White => -1,
                        Color::Black => 1,
                    };

                    let left_attacker = sq
                        .offset_by(-1, capture_offset)
                        .and_then(|sq| self.setup.pieces[sq]);
                    let right_attacker = sq
                        .offset_by(1, capture_offset)
                        .and_then(|sq| self.setup.pieces[sq]);

                    if left_attacker != Some((self.setup.side_to_move, Piece::Pawn))
                        && right_attacker != Some((self.setup.side_to_move, Piece::Pawn))
                    {
                        write!(f, "- ")?;
                        return Ok(());
                    }
                }
                write!(f, "{} ", sq)
            }
            None => write!(f, "- "),
        }
    }

    fn write_halfmove_clock(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} ", self.setup.halfmove_clock)
    }

    fn write_fullmove_number(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.setup.fullmove_number)
    }
}

fn side_char(color: Color, is_king: bool) -> &'static str {
    match (color, is_king) {
        (Color::White, false) => "Q",
        (Color::White, true) => "K",
        (Color::Black, false) => "q",
        (Color::Black, true) => "k",
    }
}
fn file_char(color: Color, file: File) -> String {
    match color {
        Color::White => file.to_string().to_ascii_uppercase(),
        Color::Black => file.to_string().to_ascii_lowercase(),
    }
}

impl TryFrom<Fen> for Position {
    type Error = InvalidPositionError;
    fn try_from(fen: Fen) -> Result<Self, InvalidPositionError> {
        fen.setup.into_position()
    }
}

impl From<Fen> for position::Setup {
    fn from(fen: Fen) -> Self {
        fen.setup
    }
}

impl From<PieceParseError> for FenParseError {
    fn from(_err: PieceParseError) -> Self {
        FenParseError::UnexpectedPieceChar
    }
}

impl From<SquareParseError> for FenParseError {
    fn from(_err: SquareParseError) -> Self {
        FenParseError::InvalidEnPassantSquare
    }
}

impl From<FenParseError> for FenError {
    fn from(err: FenParseError) -> Self {
        FenError::InvalidFen(err)
    }
}

impl From<InvalidPositionError> for FenError {
    fn from(err: InvalidPositionError) -> Self {
        FenError::InvalidPosition(err)
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        fen::Format,
        Castling, Color, Fen, FenParseError, Move, Piece,
        Rank::{self, *},
        Square::{self, *},
        SquareSet,
    };

    #[test]
    fn chess960_ambiguous_castle() {
        use crate::Position;
        let mut pos =
            Position::from_fen("n1rnbbk1/p1ppppp1/qr6/1p5p/1P5P/1RQ5/P1PPPPP1/N1RNBBK1 w Qq - 6 7")
                .unwrap();

        pos.play_unchecked(&Move::new_normal(B3, B1));

        assert_eq!(
            pos.fen().to_string(),
            "n1rnbbk1/p1ppppp1/qr6/1p5p/1P5P/2Q5/P1PPPPP1/NRRNBBK1 b Cq - 7 7"
        );

        pos.play_unchecked(&Move::new_normal(B6, B8));

        assert_eq!(
            pos.fen().to_string(),
            "nrrnbbk1/p1ppppp1/q7/1p5p/1P5P/2Q5/P1PPPPP1/NRRNBBK1 w Cc - 8 8"
        );
    }

    #[test]
    fn shredder_fen_initial() {
        let pos =
            Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        assert_eq!(
            pos.format(Format::ShredderFen).to_string(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1"
        );
    }

    #[test]
    fn xfen_enpassant() {
        use crate::Position;
        let mut pos =
            Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        pos.play_unchecked(&Move::new_normal(E2, E4));

        assert_eq!(
            pos.fen().format(Format::XFen).to_string(),
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
        );

        let pos = Fen::from_str("rnbqkb1r/pppppppp/8/8/4P1nP/5N2/PPPP1PP1/RNBQKB1R b KQkq h3 0 3")
            .unwrap();

        assert_eq!(
            pos.format(Format::XFen).to_string(),
            "rnbqkb1r/pppppppp/8/8/4P1nP/5N2/PPPP1PP1/RNBQKB1R b KQkq - 0 3"
        );

        let pos =
            Fen::from_str("rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3").unwrap();

        assert_eq!(
            pos.format(Format::XFen).to_string(),
            "rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
        );
    }

    #[test]
    fn to_fen_initial() {
        let pos =
            Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        assert_eq!(
            pos.to_string(),
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        );
    }

    #[test]
    fn to_fen_testpos() {
        let pos =
            Fen::from_str("r1b1k2r/2qnbppp/p2pp3/1p3PP1/3NP3/2N2Q2/PPP4P/2KR1B1R b kq - 0 13")
                .unwrap();

        assert_eq!(
            pos.to_string(),
            "r1b1k2r/2qnbppp/p2pp3/1p3PP1/3NP3/2N2Q2/PPP4P/2KR1B1R b kq - 0 13"
        );
    }

    #[test]
    fn from_fen_err() {
        let pos = Fen::from_str("rnbqkbnr/ppppppp/7/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(pos, Err(FenParseError::InvalidFileCount));

        let pos = Fen::from_str("rnbqkbnr/pppppppx/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
        assert_eq!(pos, Err(FenParseError::UnexpectedPieceChar));

        let pos = Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP w KQkq - 0 1");
        assert_eq!(pos, Err(FenParseError::InvalidRankCount));

        let pos = Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR W KQkq - 0 1");
        assert_eq!(pos, Err(FenParseError::InvalidSideToMove));

        let pos = Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkx - 0 1");
        assert_eq!(pos, Err(FenParseError::InvalidCastlingRights));

        let pos = Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq D6 0 1");
        assert_eq!(pos, Err(FenParseError::InvalidEnPassantSquare));

        let pos =
            Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - notanumber 1");
        assert_eq!(pos, Err(FenParseError::InvalidHalfmoveClock));

        let pos =
            Fen::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 notanumber");
        assert_eq!(pos, Err(FenParseError::InvalidFullmoveNumber));
    }

    #[test]
    fn from_fen_initial() {
        use crate::Position;
        let pos =
            Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();
        assert_eq!(pos.colored(Color::White), rank(First) | rank(Second));
        assert_eq!(pos.colored(Color::Black), rank(Seventh) | rank(Eighth));
        assert_eq!(pos.pieces(Piece::Pawn), rank(Second) | rank(Seventh));
        assert_eq!(pos.pieces(Piece::Bishop), sq(C1) | sq(C8) | sq(F1) | sq(F8));
        assert_eq!(pos.pieces(Piece::Knight), sq(B1) | sq(B8) | sq(G1) | sq(G8));
        assert_eq!(pos.pieces(Piece::Rook), sq(A1) | sq(A8) | sq(H1) | sq(H8));
        assert_eq!(pos.pieces(Piece::Queen), sq(D1) | sq(D8));
        assert_eq!(pos.pieces(Piece::King), sq(E1) | sq(E8));
        assert_eq!(pos.side_to_move(), Color::White);
        assert_eq!(pos.castling(Color::White), Castling::ALL);
        assert_eq!(pos.castling(Color::Black), Castling::ALL);
        assert_eq!(pos.en_passant_square(), None);
        assert_eq!(pos.halfmove_clock(), 0);
        assert_eq!(pos.fullmove_number(), 1);
    }

    #[test]
    fn from_fen_testpos() {
        use crate::Position;
        let pos =
            Position::from_fen("r1b1k2r/2qnbppp/p2pp3/1p3PP1/3NP3/2N2Q2/PPP4P/2KR1B1R b kq - 0 13")
                .unwrap();
        assert_eq!(pos.colored(Color::White), SquareSet(0x00000060182487AC));
        assert_eq!(pos.colored(Color::Black), SquareSet(0x95FC190200000000));
        #[rustfmt::skip]
        assert_eq!(
            pos.pieces(Piece::Pawn), 
            sq(A2) | sq(B2) | sq(C2) | sq(E4) | sq(F5) | sq(G5) | sq(H2)
                | sq(A6) | sq(B5) | sq(D6) | sq(E6) | sq(F7) | sq(G7) | sq(H7)
        );
        assert_eq!(pos.pieces(Piece::Bishop), sq(C8) | sq(F1) | sq(E7));
        assert_eq!(pos.pieces(Piece::Knight), sq(C3) | sq(D4) | sq(D7));
        assert_eq!(pos.pieces(Piece::Rook), sq(D1) | sq(H1) | sq(A8) | sq(H8));
        assert_eq!(pos.pieces(Piece::Queen), sq(F3) | sq(C7));
        assert_eq!(pos.pieces(Piece::King), sq(C1) | sq(E8));
        assert_eq!(pos.side_to_move(), Color::Black);
        assert_eq!(pos.castling(Color::White), Castling::NONE);
        assert_eq!(pos.castling(Color::Black), Castling::ALL);
        assert_eq!(pos.en_passant_square(), None);
        assert_eq!(pos.halfmove_clock(), 0);
        assert_eq!(pos.fullmove_number(), 13);
    }

    #[test]
    fn from_fen_enpassant() {
        let pos =
            Fen::from_str("rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3").unwrap();
        assert_eq!(pos.setup.en_passant_square, Some(D6));
    }

    fn rank(rank: Rank) -> SquareSet {
        SquareSet::from_rank(rank)
    }

    fn sq(square: Square) -> SquareSet {
        SquareSet::from_square(square)
    }
}
