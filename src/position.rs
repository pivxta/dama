use crate::{
    SquareSets, SquareSet, ByColor, ByPiece, BySquare, Castling, CastlingSide, Color, Fen, FenError, File, Move, Piece, Rank, San, SanError, Square
};
use core::fmt;
use std::str::FromStr;
use dama_core::enum_map;
use thiserror::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Variant {
    #[default]
    Standard,
    Chess960,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Position {
    occupied: SquareSet,
    pieces: ByPiece<SquareSet>,
    colors: ByColor<SquareSet>,
    castling: ByColor<Castling>,
    en_passant_square: Option<Square>,
    checkers: SquareSet,
    pinned: SquareSet,
    side_to_move: Color,
    halfmove_clock: u32,
    fullmove_number: u32,
    variant: Variant,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
#[error("illegal move.")]
pub struct IllegalMoveError;


#[derive(Clone, Copy, Debug, PartialEq, Eq, Error)]
pub enum InvalidPositionError {
    #[error("pawns cannot be in the first or eighth rank.")]
    PawnsInBackRank,
    #[error("impossible en passant square.")]
    EnPassantSquare,
    #[error("en passant square currently has a piece on it.")]
    OccupiedEnPassantSquare,
    #[error("no pawn to capture en passant.")]
    NoEnPassantTarget,
    #[error("inconsistent castling rights.")]
    CastlingRights,
    #[error("{0} king's position is not consistent with the position's castling rights.")]
    CastlingKingPosition(Color),
    #[error("{0} rook's position is not consistent with the position's castling rights.")]
    CastlingRookPosition(Color),
    #[error("no position can have more than 2 checkers, this one has {0}.")]
    TooManyCheckers(u32),
    #[error("expected one king per side, but found {count} kings for {color}.")]
    KingCount { color: Color, count: u32 },
    #[error("king can be captured this turn.")]
    ExposedKing,
}

impl Default for Position {
    fn default() -> Self {
        Self::new_initial()
    }
}

impl Position {
    pub fn new_initial() -> Self {
        Self::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap()
    }

    pub fn from_fen(fen: &str) -> Result<Self, FenError> {
        Ok(Fen::from_str(fen)?.into_position()?)
    }

    #[inline]
    pub fn variant(&self) -> Variant {
        self.variant
    }

    #[inline]
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    #[inline]
    pub fn castling(&self, color: Color) -> Castling {
        self.castling[color]
    }

    #[inline]
    pub fn our_castling(&self) -> Castling {
        self.castling[self.side_to_move]
    }

    #[inline]
    pub fn en_passant_square(&self) -> Option<Square> {
        self.en_passant_square
    }

    #[inline]
    pub fn halfmove_clock(&self) -> u32 {
        self.halfmove_clock
    }

    #[inline]
    pub fn fullmove_number(&self) -> u32 {
        self.fullmove_number
    }

    #[inline]
    pub fn occupied(&self) -> SquareSet {
        self.occupied
    }

    #[inline]
    pub fn us(&self) -> SquareSet {
        self.colors[self.side_to_move]
    }

    #[inline]
    pub fn them(&self) -> SquareSet {
        self.colors[!self.side_to_move]
    }

    #[inline]
    pub fn white(&self) -> SquareSet {
        self.colors[Color::White]
    }

    #[inline]
    pub fn black(&self) -> SquareSet {
        self.colors[Color::Black]
    }

    #[inline]
    pub fn pawns(&self) -> SquareSet {
        self.pieces(Piece::Pawn)
    }

    #[inline]
    pub fn knights(&self) -> SquareSet {
        self.pieces(Piece::Knight)
    }

    #[inline]
    pub fn bishops(&self) -> SquareSet {
        self.pieces(Piece::Bishop)
    }

    #[inline]
    pub fn rooks(&self) -> SquareSet {
        self.pieces(Piece::Rook)
    }

    #[inline]
    pub fn queens(&self) -> SquareSet {
        self.pieces(Piece::Queen)
    }

    #[inline]
    pub fn colored(&self, color: Color) -> SquareSet {
        self.colors[color]
    }

    #[inline]
    pub fn pieces(&self, piece: Piece) -> SquareSet {
        self.pieces[piece]
    }

    #[inline]
    pub fn piece_at(&self, square: Square) -> Option<Piece> {
        Piece::ALL
            .into_iter()
            .find(|&p| self.pieces(p).contains(square))
    }

    #[inline]
    pub fn color_at(&self, square: Square) -> Option<Color> {
        if self.colored(Color::White).contains(square) {
            return Some(Color::White);
        }
        if self.colored(Color::Black).contains(square) {
            return Some(Color::Black);
        }
        None
    }

    #[inline]
    pub fn checkers(&self) -> SquareSet {
        self.checkers
    }

    #[inline]
    pub fn pinned(&self) -> SquareSet {
        self.pinned
    }

    #[inline]
    pub fn is_in_check(&self) -> bool {
        !self.checkers().is_empty()
    }

    #[inline]
    pub fn our_king(&self) -> Square {
        self.king(self.side_to_move)
    }

    #[inline]
    pub fn their_king(&self) -> Square {
        self.king(!self.side_to_move)
    }

    #[inline]
    pub(crate) fn our_backrank(&self) -> Rank {
        Rank::back_rank(self.side_to_move)
    }

    #[inline]
    pub(crate) fn their_backrank(&self) -> Rank {
        Rank::back_rank(!self.side_to_move)
    }

    #[inline]
    pub fn king(&self, color: Color) -> Square {
        (self.pieces[Piece::King] & self.colors[color])
            .iter()
            .next()
            .unwrap_or_else(|| panic!("no {:?} king present in the board.", color))
    }

    #[inline]
    pub fn attacking(&self, square: Square) -> SquareSet {
        (self.knights() & SquareSet::knight_moves(square))
            | ((self.bishops() | self.queens()) & SquareSet::bishop_moves(square, self.occupied))
            | ((self.rooks() | self.queens()) & SquareSet::rook_moves(square, self.occupied))
            | (self.pawns() & self.white() & SquareSet::pawn_attacks(Color::Black, square))
            | (self.pawns() & self.black() & SquareSet::pawn_attacks(Color::White, square))
            | (self.pieces(Piece::King) & SquareSet::king_moves(square))
    }

    #[inline]
    pub fn is_legal(&self, mv: &Move) -> bool {
        if self.color_at(mv.from) != Some(self.side_to_move) {
            return false;
        }

        // If the color bitboard shows there is a piece there but the
        // piece ones don't, there is probably a serious problem going
        // on. So, the best thing to do here is panic, allowing the bug
        // to be properly caught.
        let moved_piece = self
            .piece_at(mv.from)
            .expect("inconsistent piece and color bitboards.");

        self.legal_moves_for(moved_piece).contains(mv)
    }

    /// Checks if a move is a capture. This function assumes that the given
    /// move is at least pseudolegal.
    #[inline]
    pub fn is_capture(&self, mv: &Move) -> bool {
        let captured_color = self.color_at(mv.to);
        if captured_color.is_some() && captured_color != Some(self.side_to_move) {
            return true;
        }
        self.piece_at(mv.from) == Some(Piece::Pawn) && self.en_passant_square() == Some(mv.to)
    }

    #[inline]
    pub fn setup(&self) -> Setup {
        Setup {
            pieces: BySquare::from_fn(|sq| self.color_at(sq).zip(self.piece_at(sq))),
            side_to_move: self.side_to_move,
            en_passant_square: self.en_passant_square,
            castling: self.castling,
            halfmove_clock: self.halfmove_clock,
            fullmove_number: self.fullmove_number,
            variant: Some(self.variant),
        }
    }

    pub fn fen(&self) -> Fen {
        Fen {
            setup: self.setup(),
        }
    }

    #[inline]
    pub fn play_san(&self, san: &San) -> Result<Self, SanError> {
        Ok(self.play_unchecked(&san.to_move(self)?))
    }

    #[inline]
    pub fn play(&self, mv: &Move) -> Result<Self, IllegalMoveError> {
        if !self.is_legal(mv) {
            return Err(IllegalMoveError);
        }
        Ok(self.play_unchecked(mv))
    }

    #[must_use]
    pub fn play_unchecked(&self, mv: &Move) -> Self {
        let mut pos = self.clone();

        let (us, them) = (pos.side_to_move, !pos.side_to_move);
        let (moved, captured) = pos.move_pieces(mv);

        if captured == Some(Piece::Rook) && mv.to.rank() == self.their_backrank() {
            pos.disable_castling(them, mv.to.file());
        }

        let mut new_ep_square = None;
        match moved {
            Piece::Pawn if pos.is_double_push(mv) => {
                debug_assert_eq!(mv.from.file(), mv.to.file());
                new_ep_square = Some(mv.from.with_rank(Rank::third_for(us)));
            }
            Piece::Pawn if Some(mv.to) == pos.en_passant_square => {
                debug_assert!(captured.is_none());
                let capture_square = mv.to.with_rank(Rank::fifth_for(us));
                let (captured_color, captured) = pos
                    .grab_piece(capture_square)
                    .expect("no pawn to capture en passant.");
                debug_assert!(captured == Piece::Pawn && captured_color == them);
            }
            Piece::Pawn if mv.to.rank() == self.their_backrank() => {
                let promotion = mv
                    .promotion
                    .expect("pawn moving to last rank but not promoting.");
                pos.grab_piece(mv.to);
                pos.put_piece(mv.to, us, promotion);
            }
            Piece::King => {
                if self.variant == Variant::Standard {
                    pos.try_castling_standard(mv);
                }
                pos.castling[us] = Castling::default();
            }
            Piece::Rook if mv.from.rank() == self.our_backrank() => {
                pos.disable_castling(us, mv.from.file())
            }
            _ => {}
        }

        if moved != Piece::Pawn && captured.is_none() {
            pos.halfmove_clock += 1;
        } else {
            pos.halfmove_clock = 0;
        }

        if us == Color::Black {
            pos.fullmove_number += 1;
        }

        pos.side_to_move = them;
        pos.en_passant_square = new_ep_square;
        pos.update_checkers_and_pinners();
        pos
    }

    #[inline]
    fn move_pieces(&mut self, mv: &Move) -> (Piece, Option<Piece>) {
        let (_, moved) = self.grab_piece(mv.from).expect("no piece to be moved.");
        let captured = match self.grab_piece(mv.to) {
            Some((color, Piece::Rook))
                if self.variant == Variant::Chess960
                    && moved == Piece::King
                    && color == self.side_to_move
                    && mv.from.rank() == self.our_backrank()
                    && mv.to.rank() == self.our_backrank()
                    && self.our_castling().contains(mv.to.file()) =>
            {
                self.try_castling_960(mv);
                None
            }
            Some((color, captured)) if color != self.side_to_move => {
                self.put_piece(mv.to, self.side_to_move, moved);
                Some(captured)
            }
            None => {
                self.put_piece(mv.to, self.side_to_move, moved);
                None
            }
            _ => panic!("cannot capture pieces of same color."),
        };
        (moved, captured)
    }

    #[inline]
    fn put_piece(&mut self, sq: Square, color: Color, piece: Piece) {
        debug_assert!(self.piece_at(sq).is_none(), "{:?} {}\n{}", piece, sq, self);
        let sq = sq.into();
        self.pieces[piece] ^= sq;
        self.colors[color] ^= sq;
        self.occupied ^= sq;
    }

    #[inline]
    fn grab_piece(&mut self, sq: Square) -> Option<(Color, Piece)> {
        let piece = self.piece_at(sq)?;
        let color = self.color_at(sq)?;

        let sq = sq.into();
        self.pieces[piece] ^= sq;
        self.colors[color] ^= sq;
        self.occupied ^= sq;

        Some((color, piece))
    }

    #[inline]
    fn try_castling_standard(&mut self, mv: &Move) {
        let castling = self.castling(self.side_to_move);

        match mv.to.file() {
            File::G if castling.king_side.is_some() => self.castle(mv, File::H, File::F),
            File::C if castling.queen_side.is_some() => self.castle(mv, File::A, File::D),
            _ => {}
        }
    }

    #[inline]
    fn try_castling_960(&mut self, mv: &Move) {
        let castling = self.castling(self.side_to_move);
        let backrank = self.our_backrank();
        let rook_file = mv.to.file();
        debug_assert_eq!(mv.to.rank(), backrank);
        debug_assert_eq!(mv.from.rank(), backrank);

        let side = castling
            .side(rook_file)
            .expect("no rights to castle in this file.");
        let (king_to, rook_to) = match side {
            CastlingSide::King => (File::G, File::F),
            CastlingSide::Queen => (File::C, File::D),
        };
        let (king_to, rook_to) = (
            Square::new(king_to, backrank),
            Square::new(rook_to, backrank),
        );
        self.put_piece(king_to, self.side_to_move, Piece::King);
        self.put_piece(rook_to, self.side_to_move, Piece::Rook);
    }

    #[inline]
    fn castle(&mut self, mv: &Move, rook_from: File, rook_to: File) {
        let us = self.side_to_move;

        let rank = self.our_backrank();
        debug_assert_eq!(rank, mv.from.rank());
        debug_assert_eq!(rank, mv.to.rank());
        debug_assert_eq!(mv.from.file(), File::E);

        let rook_sq_from = Square::new(rook_from, rank);
        let rook_sq_to = Square::new(rook_to, rank);
        let (_, rook) = self
            .grab_piece(rook_sq_from)
            .expect("no rook found for castling.");
        debug_assert_eq!(rook, Piece::Rook);

        self.put_piece(rook_sq_to, us, rook);
    }

    #[inline]
    fn disable_castling(&mut self, color: Color, file: File) {
        self.castling[color].remove(file);
    }

    #[inline]
    fn is_double_push(&self, mv: &Move) -> bool {
        let second_rank = Rank::second_for(self.side_to_move);
        let fourth_rank = Rank::fourth_for(self.side_to_move);
        mv.from.rank() == second_rank && mv.to.rank() == fourth_rank
    }

    #[inline]
    pub fn update_checkers_and_pinners(&mut self) {
        let king = self.our_king();

        self.pinned = SquareSet::EMPTY;
        self.checkers = self.them()
            & ((SquareSet::knight_moves(king) & self.knights())
                | (SquareSet::pawn_attacks(self.side_to_move, king) & self.pawns()));

        for attacker in self.sliding_king_attackers() {
            let between = SquareSet::between(attacker, king) & self.occupied();
            match between.count() {
                0 => self.checkers |= attacker.into(),
                1 => self.pinned |= between,
                _ => {}
            }
        }
    }

    #[inline]
    fn sliding_king_attackers(&self) -> SquareSet {
        let king = self.our_king();

        let attackers = self.them();
        let attacking_bishops = self.bishops() & attackers;
        let attacking_rooks = self.rooks() & attackers;
        let attacking_queens = self.queens() & attackers;

        let bishop_rays = SquareSet::bishop_moves(king, SquareSet::EMPTY);
        let rook_rays = SquareSet::rook_moves(king, SquareSet::EMPTY);

        (attacking_bishops & bishop_rays)
            | (attacking_rooks & rook_rays)
            | (attacking_queens & (bishop_rays | rook_rays))
    }

    #[inline]
    pub fn validate(&self) -> Result<(), InvalidPositionError> {
        for color in Color::ALL {
            let count = self.king_count(color);
            if count != 1 {
                return Err(InvalidPositionError::KingCount { color, count });
            }
        }

        if self.backrank_pawns() > 0 {
            return Err(InvalidPositionError::PawnsInBackRank);
        }

        if (self.attacking(self.their_king()) & self.us()).count() > 0 {
            return Err(InvalidPositionError::ExposedKing);
        }

        if self.checkers().count() > 2 {
            return Err(InvalidPositionError::TooManyCheckers(
                self.checkers().count(),
            ));
        }

        for color in Color::ALL {
            if !self.are_castling_rights_valid() {
                return Err(InvalidPositionError::CastlingRights);
            }
            if !self.is_castling_king_valid(color) {
                return Err(InvalidPositionError::CastlingKingPosition(color));
            }
            if !self.are_castling_rooks_valid(color) {
                return Err(InvalidPositionError::CastlingRookPosition(color));
            }
        }

        let ep_rank = self.en_passant_rank();
        if (self.side_to_move == Color::White && ep_rank.is_some() && ep_rank != Some(Rank::Sixth))
            || (self.side_to_move == Color::Black
                && ep_rank.is_some()
                && ep_rank != Some(Rank::Third))
        {
            return Err(InvalidPositionError::EnPassantSquare);
        }

        if self
            .en_passant_square
            .and_then(|sq| self.piece_at(sq))
            .is_some()
        {
            return Err(InvalidPositionError::OccupiedEnPassantSquare);
        }

        let ep_target = self.en_passant_target();
        if ep_target.is_some() && self.color_at(ep_target.unwrap()).is_none() {
            return Err(InvalidPositionError::NoEnPassantTarget);
        }

        Ok(())
    }

    #[inline]
    fn king_count(&self, color: Color) -> u32 {
        (self.pieces(Piece::King) & self.colored(color)).count()
    }

    #[inline]
    fn are_castling_rights_valid(&self) -> bool {
        let white = self.castling(Color::White);
        let black = self.castling(Color::Black);
        if white.king_side.is_some() 
            && black.king_side.is_some() 
            && white.king_side != black.king_side {
            return false;
        }
        if white.queen_side.is_some() 
            && black.queen_side.is_some() 
            && white.queen_side != black.queen_side {
            return false;
        }
        true
    }

    #[inline]
    fn is_castling_king_valid(&self, color: Color) -> bool {
        if self.castling(color).is_none() {
            return true;
        }

        let king = self.king(color);
        king.rank() == Rank::back_rank(color)
            && (self.variant != Variant::Standard || king.file() == File::E)
    }

    #[inline]
    fn are_castling_rooks_valid(&self, color: Color) -> bool {
        if self.castling(color).is_none() {
            return true;
        }

        let backrank = Rank::back_rank(color);
        let castling = self.castling(color);
        let king_file = self.king(color).file();

        if let Some(king_rook) = castling.king_side.map(|file| Square::new(file, backrank)) {
            if king_rook.file() < king_file
                || !self.is_valid_castling_rook(CastlingSide::King, king_rook, color)
            {
                return false;
            }
        }

        if let Some(queen_rook) = castling.queen_side.map(|file| Square::new(file, backrank)) {
            if queen_rook.file() > king_file
                || !self.is_valid_castling_rook(CastlingSide::Queen, queen_rook, color)
            {
                return false;
            }
        }

        true
    }

    #[inline]
    fn is_valid_castling_rook(&self, side: CastlingSide, rook: Square, color: Color) -> bool {
        (self.variant != Variant::Standard || rook.file() == Self::standard_rook_file(side))
            && (self.piece_at(rook) == Some(Piece::Rook))
            && (self.color_at(rook) == Some(color))
    }

    #[inline]
    fn standard_rook_file(side: CastlingSide) -> File {
        match side {
            CastlingSide::King => File::H,
            CastlingSide::Queen => File::A,
        }
    }

    #[inline]
    fn backrank_pawns(&self) -> u32 {
        (self.pawns() & (SquareSet::from(Rank::First) | SquareSet::from(Rank::Eighth))).count()
    }

    #[inline]
    fn en_passant_rank(&self) -> Option<Rank> {
        self.en_passant_square.map(Square::rank)
    }

    #[inline]
    fn en_passant_target(&self) -> Option<Square> {
        self.en_passant_square
            .map(|ep| ep.with_rank(Rank::fifth_for(self.side_to_move)))
    }
}

pub type Pieces = BySquare<Option<(Color, Piece)>>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Setup {
    pub pieces: Pieces,
    pub castling: ByColor<Castling>,
    pub en_passant_square: Option<Square>,
    pub side_to_move: Color,
    pub halfmove_clock: u32,
    pub fullmove_number: u32,
    pub variant: Option<Variant>,
}

impl Setup {
    #[inline]
    pub fn new_empty() -> Self {
        Setup {
            pieces: BySquare::default(),
            side_to_move: Color::White,
            castling: Default::default(),
            en_passant_square: None,
            halfmove_clock: 0,
            fullmove_number: 1,
            variant: None
        }
    }

    #[inline]
    pub fn with_variant(mut self, variant: Variant) -> Self {
        self.variant = Some(variant);
        self
    }

    #[inline]
    pub fn with_pieces(mut self, pieces: Pieces) -> Self {
        self.pieces = pieces;
        self
    }

    #[inline]
    pub fn with_side_to_move(mut self, side_to_move: Color) -> Self {
        self.side_to_move = side_to_move;
        self
    }

    #[inline]
    pub fn with_castling(mut self, color: Color, castling: Castling) -> Self {
        self.castling[color] = castling;
        self
    }

    #[inline]
    pub fn with_en_passant(mut self, square: Option<Square>) -> Self {
        self.en_passant_square = square;
        self
    }

    #[inline]
    pub fn with_halfmove_clock(mut self, halfmove_clock: u32) -> Self {
        self.halfmove_clock = halfmove_clock;
        self
    }

    #[inline]
    pub fn with_fullmove_number(mut self, fullmove_number: u32) -> Self {
        self.fullmove_number = fullmove_number;
        self
    }

    pub fn into_position(&self) -> Result<Position, InvalidPositionError> {
        let mut pieces = ByPiece::from_fn(|_| SquareSet::EMPTY);
        let mut colors = ByColor::from_fn(|_| SquareSet::EMPTY);
        let mut king_squares = ByColor::from_fn(|_| None);
        for (sq, piece) in self.pieces {
            if let Some((color, piece)) = piece {
                let sqset = SquareSet::from(sq);
                pieces[piece] |= sqset;
                colors[color] |= sqset;
                if piece == Piece::King {
                    king_squares[color] = Some(sq);
                }
            }
        }
        use Color::*;
        let king_squares = enum_map! {
            White => king_squares[White].ok_or(InvalidPositionError::no_king(White))?,
            Black => king_squares[Black].ok_or(InvalidPositionError::no_king(Black))?
        };

        let occupied = colors[White] | colors[Black];

        let mut position = Position {
            pieces,
            colors,
            occupied,
            variant: self.get_variant(king_squares),
            castling: self.castling,
            en_passant_square: self.en_passant_square,
            halfmove_clock: self.halfmove_clock,
            side_to_move: self.side_to_move,
            fullmove_number: self.fullmove_number,
            checkers: SquareSet::EMPTY,
            pinned: SquareSet::EMPTY,
        };
        position.update_checkers_and_pinners();
        position.validate()?;
        Ok(position)
    }

    #[inline]
    fn get_variant(&self, king_squares: ByColor<Square>) -> Variant {
        match self.variant {
            Some(variant) => variant,
            None if is_chess960(self.castling[Color::White], king_squares[Color::White])
                || is_chess960(self.castling[Color::Black], king_squares[Color::Black]) => {
                Variant::Chess960
            }
            None => Variant::Standard,
        }
    }
}

#[inline]
fn is_chess960(castling: Castling, king_square: Square) -> bool {
    castling.is_some() 
        && (king_square.file() != File::E
            || (castling.king_side.is_some() && castling.king_side != Some(File::H))
            || (castling.queen_side.is_some() && castling.queen_side != Some(File::A)))
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for rank in Rank::ALL.into_iter().rev() {
            write!(f, "{} | ", rank)?;
            for file in File::ALL {
                let sq = Square::new(file, rank);
                match (self.piece_at(sq), self.color_at(sq)) {
                    (Some(piece), Some(color)) => write!(f, "{} ", piece_char(color, piece))?,
                    _ => write!(f, ". ")?,
                }
            }
            writeln!(f)?;
        }
        writeln!(f, "  └————————————————")?;
        write!(f, "    ")?;
        for file in File::ALL {
            write!(f, "{} ", file)?;
        }

        Ok(())
    }
}

fn piece_char(color: Color, piece: Piece) -> char {
    match (color, piece) {
        (Color::Black, Piece::Pawn) => '♙',
        (Color::Black, Piece::Knight) => '♘',
        (Color::Black, Piece::Bishop) => '♗',
        (Color::Black, Piece::Rook) => '♖',
        (Color::Black, Piece::Queen) => '♕',
        (Color::Black, Piece::King) => '♔',
        (Color::White, Piece::Pawn) => '♟',
        (Color::White, Piece::Knight) => '♞',
        (Color::White, Piece::Bishop) => '♝',
        (Color::White, Piece::Rook) => '♜',
        (Color::White, Piece::Queen) => '♛',
        (Color::White, Piece::King) => '♚',
    }
}

impl InvalidPositionError {
    #[inline]
    fn no_king(color: Color) -> InvalidPositionError {
        Self::KingCount { color, count: 0 }
    }
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;

    use crate::{
        position::Setup, Color, FenError::*, InvalidPositionError::{self, *}, Move, Position, San, Square::*, SquareSet, Variant
    };

    #[test]
    fn validation() {
        let pos = Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPP1/RNBQKBNP w Qkq - 0 1");
        assert_eq!(pos, Err(InvalidPosition(PawnsInBackRank)));
        let pos = Position::from_fen("rnbqkbPr/pppppppp/8/8/8/8/PPPPPPP1/RNBQKBNR w KQkq - 0 1");
        assert_eq!(pos, Err(InvalidPosition(PawnsInBackRank)));
        let pos = Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e7 0 1");
        assert_eq!(pos, Err(InvalidPosition(EnPassantSquare)));
        let pos = Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq e6 0 1");
        assert_eq!(pos, Err(InvalidPosition(NoEnPassantTarget)));
        let pos =
            Setup::from_fen("r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ2KR w KQkq - 0 1")
                .unwrap()
                .with_variant(Variant::Standard)
                .into_position();
        assert_eq!(pos, Err(CastlingKingPosition(Color::White)));
        let pos =
            Setup::from_fen("r1bq1bkr/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1")
                .unwrap()
                .with_variant(Variant::Standard)
                .into_position();
        assert_eq!(pos, Err(CastlingKingPosition(Color::Black)));
        let pos = Setup::from_fen("rnbqkbrn/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBRN w KQkq - 0 1")
            .unwrap()
            .with_variant(Variant::Standard)
            .into_position();
        assert!(matches!(pos, Err(CastlingRookPosition(_))));
        let pos = Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBRN w KQkq - 0 1");
        assert_eq!(pos, Err(InvalidPosition(InvalidPositionError::CastlingRights)));
        let pos =
            Position::from_fen("rnbqkbnr/pppp2pp/8/4pp1Q/4P3/8/PPPP1PPP/RNB1KBNR w KQkq - 0 1");
        assert_eq!(pos, Err(InvalidPosition(ExposedKing)));
        let pos =
            Position::from_fen("rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNKQKBNR w KQkq - 0 1");
        assert_eq!(
            pos,
            Err(InvalidPosition(KingCount {
                color: Color::White,
                count: 2
            }))
        );
        let pos =
            Position::from_fen("r3kbnr/pbp3pp/np1Nqp2/4p2B/Q3P3/2P5/PP1P1PPP/R1B1K2R b KQkq - 0 1");
        assert_eq!(pos, Err(InvalidPosition(TooManyCheckers(3))));
    }

    #[test]
    fn pins() {
        let position = Position::from_fen(
            "r1bqkbnr/ppp2ppp/2np4/1B2p3/3PP3/5N2/PPP2PPP/RNBQK2R b KQkq d3 0 4",
        )
        .unwrap();

        assert_eq!(position.pinned(), SquareSet::from(C6));
    }

    #[test]
    fn checks() {
        let position = Position::from_fen(
            "rnbqkb1r/pp2pppp/5n2/1Bpp4/3P1B2/4P3/PPP2PPP/RN1QK1NR b KQkq - 2 4",
        )
        .unwrap();

        assert_eq!(position.checkers(), SquareSet::from(B5));
    }

    #[test]
    fn en_passant() {
        let position = Position::new_initial()
            .play(&mv("e2e4"))
            .unwrap()
            .play(&mv("h7h6"))
            .unwrap()
            .play(&mv("e4e5"))
            .unwrap()
            .play(&mv("d7d5"))
            .unwrap()
            .play(&mv("e5d6"))
            .unwrap();

        assert_eq!(
            position.fen().to_string(),
            "rnbqkbnr/ppp1ppp1/3P3p/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3"
        );
    }

    #[test]
    fn short_castle() {
        let position = Position::new_initial()
            .play_unchecked(&mv("e2e4"))
            .play_unchecked(&mv("e7e5"))
            .play_unchecked(&mv("g1f3"))
            .play_unchecked(&mv("b8c6"))
            .play_unchecked(&mv("f1b5"))
            .play_unchecked(&mv("g8f6"))
            .play_unchecked(&mv("e1g1"));

        assert_eq!(
            position.fen().to_string(),
            "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 5 4"
        );
    }

    #[test]
    fn long_castle() {
        let position =
            Position::from_fen("r1bq1rk1/pppp1ppp/2n2n2/8/1b2P3/2N1Q3/PPPB1PPP/R3KBNR w KQ - 7 7")
                .unwrap();

        let position = position.play(&mv("e1c1"))
                .unwrap();

        assert_eq!(
            position.fen().to_string(),
            "r1bq1rk1/pppp1ppp/2n2n2/8/1b2P3/2N1Q3/PPPB1PPP/2KR1BNR b - - 8 7"
        );
    }

    #[test]
    fn promotion() {
        let position = Position::from_fen("5r2/6P1/k6K/8/8/8/8/8 w - - 1 1")
            .unwrap()
            .play(&mv("g7g8q"))
            .unwrap();

        assert_eq!(position.fen().to_string(), "5rQ1/8/k6K/8/8/8/8/8 b - - 0 1")
    }

    #[test]
    fn promotion_capture() {
        let position = Position::from_fen("5r2/6P1/k6K/8/8/8/8/8 w - - 1 1")
            .unwrap()
            .play(&mv("g7f8q"))
            .unwrap();

        assert_eq!(position.fen().to_string(), "5Q2/8/k6K/8/8/8/8/8 b - - 0 1")
    }

    #[test]
    fn san_play() {
        #[rustfmt::skip]
        let moves = [
            "Nf3", "d5",  "g3", "c5",  "Bg2", "Nc6",  "d4", "e6",  "O-O", "cxd4",  "Nxd4", "Nge7",  "c4",
            "Nxd4", "Qxd4", "Nc6",  "Qd1", "d4",  "e3", "Bc5",  "exd4", "Bxd4",  "Nc3", "O-O",  "Nb5",
            "Bb6", "b3", "a6",  "Nc3", "Bd4",  "Bb2", "e5",  "Qd2", "Be6",  "Nd5", "b5",  "cxb5",
            "axb5",  "Nf4", "exf4",  "Bxc6", "Bxb2",  "Qxb2", "Rb8",  "Rfd1", "Qb6",  "Bf3", "fxg3",  
            "hxg3", "b4",  "a4", "bxa3",  "Rxa3", "g6",  "Qd4", "Qb5",  "b4", "Qxb4",  "Qxb4", "Rxb4",  
            "Ra8", "Rxa8",  "Bxa8", "g5",  "Bd5", "Bf5",  "Rc1", "Kg7",  "Rc7", "Bg6",  "Rc4", "Rb1+",  
            "Kg2", "Re1",  "Rb4", "h5",  "Ra4", "Re5",  "Bf3", "Kh6",  "Kg1", "Re6",  "Rc4", "g4",  
            "Bd5", "Rd6",  "Bb7", "Kg5",  "f3", "f5",  "fxg4", "hxg4",  "Rb4", "Bf7",  "Kf2", "Rd2+", 
            "Kg1", "Kf6",  "Rb6+", "Kg5",  "Rb4", "Be6",  "Ra4", "Rb2",  "Ba8", "Kf6",  "Rf4", "Ke5", 
            "Rf2", "Rxf2",  "Kxf2", "Bd5",  "Bxd5", "Kxd5",  "Ke3", "Ke5"
        ];
        let mut position = Position::new_initial();

        for san in moves.into_iter().map(San::from_str).map(Result::unwrap) {
            position = position.play_san(&san).unwrap();
        }

        assert_eq!(
            position.fen().to_string(),
            "8/8/8/4kp2/6p1/4K1P1/8/8 w - - 2 59"
        );
    }

    #[test]
    fn san_play960() {
        #[rustfmt::skip]
        let moves = [
            "e4", "e5", "Nf3", "Nf6", "a4", "c6", "b4", "Qc7", "Qb3", "Ng6", "Rb1", "d5", "Ng3", "O-O-O", 
            "Bd3", "dxe4", "Nxe4", "Nf4", "Nxf6", "gxf6", "Bf5+", "Kb8", "g3", "Nd5", "O-O", "Nxb4", "d4",
            "c5", "dxe5", "b6", "Rfd1", "Qc6", "exf6", "Bb7", "Bg4", "c4", "Qc3", "a5", "Rxd8+", 
            "Rxd8", "Qe5+", "Ka7", "Qf5", "Bc5", "Bc3", "Nxc2", "Rc1", "Ne3", "fxe3", "Bxe3+", "Kg2", 
            "Bxc1", "Kh3", "Rd3", "Bd4", "Qd5",
        ];
        let mut position =
            Position::from_fen("bqrkrbnn/pppppppp/8/8/8/8/PPPPPPPP/BQRKRBNN w KQkq - 0 1").unwrap();

        for san in moves.into_iter().map(San::from_str).map(Result::unwrap) {
            position = position.play_san(&san).unwrap();
        }

        assert_eq!(
            position.fen().to_string(),
            "8/kb3p1p/1p3P2/p2q1Q2/P1pB2B1/3r1NPK/7P/2b5 w - - 4 29"
        );
    }

    fn mv(mv: &str) -> Move {
        mv.parse().unwrap()
    }
}
