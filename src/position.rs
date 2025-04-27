use crate::{
    zobrist::Zobrist, ByColor, ByPiece, BySquare, Castling, CastlingSide, Color, Fen, FenError,
    FenParseError, File, Move, MoveKind, Piece, Rank, Square, SquareSet, SquareSets, ToMove,
};
use core::fmt;
use std::str::FromStr;
use thiserror::Error;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum Variant {
    #[default]
    Standard,
    Chess960,
}

#[derive(Clone, Debug, Eq)]
pub struct Position {
    zobrist: Zobrist,
    occupied: SquareSet,
    pieces: ByPiece<SquareSet>,
    colors: ByColor<SquareSet>,
    castling: ByColor<Castling>,
    en_passant: Option<Square>,
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
    #[inline]
    fn default() -> Self {
        Self::new_initial()
    }
}

impl PartialEq for Position {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.colors == other.colors
            && self.pieces == other.pieces
            && self.castling == other.castling
            && self.side_to_move == other.side_to_move
            && self.legal_en_passant() == other.legal_en_passant()
    }
}

impl Position {
    #[inline]
    pub fn new_initial() -> Self {
        let mut position = Self {
            pieces: INITIAL_PIECES,
            colors: INITIAL_COLORS,
            occupied: INITIAL_OCCUPIED,
            side_to_move: Color::White,
            castling: ByColor::from_fn(|_| Castling::ALL_STANDARD),
            checkers: SquareSet::EMPTY,
            pinned: SquareSet::EMPTY,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_number: 1,
            variant: Variant::Standard,
            zobrist: Zobrist::new(),
        };
        position.update_hash();
        position
    }

    #[inline]
    pub fn new_chess960(scharnagl_number: u32) -> Self {
        let (pieces, castling) = initial_chess960(scharnagl_number);
        let mut position = Self {
            pieces,
            castling,
            colors: INITIAL_COLORS,
            occupied: INITIAL_OCCUPIED,
            side_to_move: Color::White,
            checkers: SquareSet::EMPTY,
            pinned: SquareSet::EMPTY,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_number: 1,
            variant: Variant::Chess960,
            zobrist: Zobrist::new(),
        };
        position.update_hash();
        position
    }

    pub fn from_fen(fen: &str) -> Result<Self, FenError> {
        Ok(Fen::from_str(fen)?.into_position()?)
    }

    #[inline]
    pub fn variant(&self) -> Variant {
        self.variant
    }

    #[inline]
    pub fn hash(&self) -> u64 {
        self.zobrist.get()
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
    pub fn en_passant(&self) -> Option<Square> {
        self.en_passant
    }

    #[inline]
    pub fn pseudolegal_en_passant(&self) -> Option<Square> {
        self.en_passant.filter(|&ep| {
            let attackers =
                SquareSet::pawn_attacks(!self.side_to_move, ep) & self.pawns() & self.us();

            !attackers.is_empty()
        })
    }

    #[inline]
    pub fn legal_en_passant(&self) -> Option<Square> {
        self.en_passant.filter(|&ep| {
            let target = ep.with_rank(Rank::fifth_for(self.side_to_move));
            let mut attackers =
                SquareSet::pawn_attacks(!self.side_to_move, ep) & self.pawns() & self.us();

            attackers.retain(|from| self.is_safe(&Move::new_en_passant(from, ep, target)));
            !attackers.is_empty()
        })
    }

    #[inline]
    pub fn en_passant_target(&self) -> Option<Square> {
        self.en_passant
            .map(|ep| ep.with_rank(Rank::fifth_for(self.side_to_move())))
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
    pub fn ply_number(&self) -> u32 {
        match self.side_to_move() {
            Color::White => self.fullmove_number * 2,
            Color::Black => self.fullmove_number * 2 + 1,
        }
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
    pub fn kings(&self) -> SquareSet {
        self.pieces(Piece::King)
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
        if !self.occupied().contains(square) {
            return None;
        }

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
    pub fn color_piece_at(&self, square: Square) -> Option<(Color, Piece)> {
        Some((self.color_at(square)?, self.piece_at(square)?))
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
    pub fn has_non_pawn_material(&self, color: Color) -> bool {
        !self.colored(color).is_subset(self.pawns() | self.kings())
    }

    /// Returns whether there is no sequence of moves for `color` that could result in checkmate.
    #[inline]
    pub fn has_insufficient_material(&self, color: Color) -> bool {
        let pieces = self.colored(color);
        if !(pieces & (self.queens() | self.rooks() | self.pawns())).is_empty() {
            return false;
        }

        if !(pieces & self.knights()).is_empty() {
            return pieces.count() <= 2
                && self
                    .colored(!color)
                    .without(self.kings() | self.queens())
                    .is_empty();
        }

        if !(pieces & self.bishops()).is_empty() {
            return ((self.bishops() & SquareSet::DARK_SQUARES).is_empty()
                || (self.bishops() & SquareSet::LIGHT_SQUARES).is_empty())
                && self.knights().is_empty()
                && self.pawns().is_empty();
        }

        false
    }

    /// Returns whether there is no sequence of moves that could result in checkmate.
    #[inline]
    pub fn is_insufficient_material(&self) -> bool {
        self.has_insufficient_material(Color::White) && self.has_insufficient_material(Color::Black)
    }

    #[inline]
    pub fn is_in_check(&self) -> bool {
        !self.checkers().is_empty()
    }

    #[inline]
    pub fn our_king(&self) -> Option<Square> {
        self.king(self.side_to_move)
    }

    #[inline]
    pub fn their_king(&self) -> Option<Square> {
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
    pub fn king(&self, color: Color) -> Option<Square> {
        (self.pieces[Piece::King] & self.colors[color]).single()
        //.unwrap_or_else(|| panic!("invalid number of {:?} kings in the board", color))
    }

    #[inline]
    pub fn attacking_with(&self, square: Square, occupied: SquareSet) -> SquareSet {
        (self.knights() & SquareSet::knight_moves(square))
            | ((self.bishops() | self.queens()) & SquareSet::bishop_moves(square, occupied))
            | ((self.rooks() | self.queens()) & SquareSet::rook_moves(square, occupied))
            | (self.pawns() & self.white() & SquareSet::pawn_attacks(Color::Black, square))
            | (self.pawns() & self.black() & SquareSet::pawn_attacks(Color::White, square))
            | (self.pieces(Piece::King) & SquareSet::king_moves(square))
    }

    #[inline]
    pub fn attacking(&self, square: Square) -> SquareSet {
        self.attacking_with(square, self.occupied)
    }

    pub fn is_legal(&self, mv: &Move) -> bool {
        self.is_pseudolegal(mv) && self.is_safe(mv)
    }

    #[inline]
    pub fn is_pseudolegal(&self, mv: &Move) -> bool {
        let us = self.side_to_move();

        if self.color_at(mv.from) != Some(us) {
            return false;
        }

        match mv.kind {
            MoveKind::Normal { promotion } => {
                // If the color bitboard shows there is a piece there but the
                // piece ones don't, there is probably a serious problem going
                // on. So, the best thing to do here is panic, allowing the bug
                // to be properly caught.
                let moved = self
                    .piece_at(mv.from)
                    .expect("inconsistent piece and color bitboards.");

                let is_capture = self.occupied.contains(mv.to);
                if is_capture && self.us().contains(mv.to) {
                    return false;
                }

                if promotion.is_some()
                    && (moved != Piece::Pawn || mv.to.rank() != self.their_backrank())
                {
                    return false;
                }

                let moves = match moved {
                    Piece::Pawn if is_capture => SquareSet::pawn_attacks(us, mv.from),
                    Piece::Pawn => SquareSet::pawn_pushes(us, mv.from, self.occupied()),
                    _ => piece_moves(moved, mv.from, self.occupied()),
                };

                moves.contains(mv.to)
            }
            MoveKind::EnPassant { target } => {
                if let Some(ep_target) = self.en_passant_target() {
                    if ep_target != target {
                        return false;
                    }
                    SquareSet::pawn_attacks(us, mv.from).contains(mv.to)
                } else {
                    false
                }
            }
            MoveKind::Castles { rook } => {
                if self.is_in_check() {
                    return false;
                }

                let our_backrank = self.our_backrank();
                if mv.from.rank() != our_backrank
                    || mv.to.rank() != our_backrank
                    || rook.rank() != our_backrank
                    || !self.our_castling().contains(rook.file())
                    || (self.variant() == Variant::Standard && mv.from.file() != File::E)
                {
                    return false;
                }

                let (king_to, rook_to) = if mv.to.file() > mv.from.file() {
                    (File::G, File::F)
                } else {
                    (File::C, File::D)
                };

                let (king_to, rook_to) = (
                    Square::new(king_to, our_backrank),
                    Square::new(rook_to, our_backrank),
                );

                if mv.to != king_to {
                    return false;
                }

                let must_be_empty = (SquareSet::between(rook, rook_to)
                    | SquareSet::between(mv.from, mv.to))
                .with(rook_to)
                .with(king_to)
                .without(mv.from)
                .without(rook);

                if !(must_be_empty & self.occupied()).is_empty() {
                    return false;
                }

                let must_be_safe = SquareSet::between(mv.from, mv.to).with(mv.to);
                for square in must_be_safe {
                    if !(self.attacking(square) & self.them()).is_empty() {
                        return false;
                    }
                }

                true
            }
        }
    }

    #[inline]
    pub fn is_safe(&self, mv: &Move) -> bool {
        if let Some(king) = self.our_king() {
            match mv.kind {
                MoveKind::Normal { .. } if self.piece_at(mv.from) == Some(Piece::King) => {
                    (self.attacking(mv.to) & self.them()).is_empty()
                }
                MoveKind::Normal { .. } => {
                    !self.pinned().contains(mv.from)
                        || SquareSet::ray(mv.from, king).contains(mv.to)
                }
                MoveKind::EnPassant { target } => {
                    let queens_and_bishops = (self.queens() | self.bishops()) & self.them();
                    let queens_and_rooks = (self.queens() | self.rooks()) & self.them();
                    let occupied_after = self
                        .occupied()
                        .toggled(mv.from)
                        .toggled(mv.to)
                        .toggled(target);

                    (SquareSet::bishop_moves(king, occupied_after) & queens_and_bishops).is_empty()
                        && (SquareSet::rook_moves(king, occupied_after) & queens_and_rooks)
                            .is_empty()
                }
                _ => true,
            }
        } else {
            true
        }
    }

    /// Checks if a move is a capture. This function assumes that the given
    /// move is at least pseudolegal.
    #[inline]
    pub fn is_capture(&self, mv: &Move) -> bool {
        let captured_color = self.color_at(mv.to);
        if captured_color.is_some() && captured_color != Some(self.side_to_move) {
            return true;
        }
        self.piece_at(mv.from) == Some(Piece::Pawn) && self.en_passant() == Some(mv.to)
    }

    #[inline]
    pub fn is_quiet(&self, mv: &Move) -> bool {
        !self.is_capture(mv)
    }

    #[inline]
    pub fn setup(&self) -> Setup {
        Setup {
            pieces: self.pieces,
            colors: self.colors,
            side_to_move: self.side_to_move,
            en_passant: self.en_passant,
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
    pub fn skip(&mut self) {
        self.halfmove_clock = 0;

        if self.side_to_move == Color::Black {
            self.fullmove_number += 1;
        }

        self.toggle_side_to_move();
        self.set_en_passant(None);

        if let Some(king) = self.our_king() {
            self.update_checkers_and_pinners(king);
        }
    }

    #[inline]
    pub fn play<M: ToMove>(&mut self, mv: &M) -> Result<(), M::Error> {
        self.play_unchecked(&mv.to_move(self)?);
        Ok(())
    }

    pub fn play_unchecked(&mut self, mv: &Move) {
        let us = self.side_to_move;
        let them = !self.side_to_move;
        let mut ep_square = None;

        let (moved, captured) = match mv.kind {
            MoveKind::Normal { promotion } => {
                let moved = self.piece_at(mv.from).expect("no piece to be moved");
                if moved == Piece::Pawn && self.is_double_push(mv) {
                    ep_square = Some(mv.from.with_rank(Rank::third_for(us)));
                    self.grab_piece(mv.from, us, Piece::Pawn);
                    self.put_piece(mv.to, us, Piece::Pawn);

                    (moved, None)
                } else {
                    let captured = self.piece_at(mv.to);
                    if let Some(captured) = captured {
                        if captured == Piece::Rook && mv.to.rank() == self.their_backrank() {
                            self.remove_castling(them, mv.to.file());
                        }
                        self.grab_piece(mv.to, them, captured);
                    }

                    self.grab_piece(mv.from, us, moved);
                    if let Some(promotion) = promotion {
                        self.put_piece(mv.to, us, promotion);
                    } else {
                        self.put_piece(mv.to, us, moved);
                    }

                    match moved {
                        Piece::King => self.clear_castling(us),
                        Piece::Rook if mv.from.rank() == self.our_backrank() => {
                            self.remove_castling(us, mv.from.file());
                        }
                        _ => {}
                    }

                    (moved, captured)
                }
            }
            MoveKind::Castles { rook } => {
                self.clear_castling(us);
                self.grab_piece(mv.from, us, Piece::King);
                self.grab_piece(rook, us, Piece::Rook);
                self.put_piece(mv.to, us, Piece::King);

                if mv.to.file() > mv.from.file() {
                    self.put_piece(rook.with_file(File::F), us, Piece::Rook);
                } else {
                    self.put_piece(rook.with_file(File::D), us, Piece::Rook);
                }

                (Piece::King, None)
            }
            MoveKind::EnPassant { target } => {
                self.grab_piece(mv.from, us, Piece::Pawn);
                self.grab_piece(target, them, Piece::Pawn);
                self.put_piece(mv.to, us, Piece::Pawn);

                (Piece::Pawn, Some(Piece::Pawn))
            }
        };

        if moved != Piece::Pawn && captured.is_none() {
            self.halfmove_clock += 1;
        } else {
            self.halfmove_clock = 0;
        }

        if us == Color::Black {
            self.fullmove_number += 1;
        }

        self.toggle_side_to_move();
        self.set_en_passant(ep_square);

        if let Some(king) = self.our_king() {
            self.update_checkers_and_pinners(king);
        }
    }

    #[inline]
    fn put_piece(&mut self, sq: Square, color: Color, piece: Piece) {
        debug_assert!(self.piece_at(sq).is_none(), "{:?} {}\n{}", piece, sq, self);
        self.pieces[piece].insert(sq);
        self.colors[color].insert(sq);
        self.occupied.insert(sq);
        self.zobrist.toggle_piece(color, piece, sq);
    }

    #[inline]
    fn grab_piece(&mut self, sq: Square, color: Color, piece: Piece) {
        self.pieces[piece].toggle(sq);
        self.colors[color].toggle(sq);
        self.occupied.toggle(sq);
        self.zobrist.toggle_piece(color, piece, sq);
    }

    #[inline]
    fn clear_castling(&mut self, color: Color) {
        if let Some(file) = self.castling[color].queen_side {
            self.zobrist.toggle_castling(color, file);
        }
        if let Some(file) = self.castling[color].king_side {
            self.zobrist.toggle_castling(color, file);
        }

        self.castling[color].queen_side = None;
        self.castling[color].king_side = None;
    }

    #[inline]
    fn remove_castling(&mut self, color: Color, file: File) {
        if self.castling[color].remove(file) {
            self.zobrist.toggle_castling(color, file);
        }
    }

    #[inline]
    fn toggle_side_to_move(&mut self) {
        self.side_to_move = !self.side_to_move;
        self.zobrist.toggle_side_to_move();
    }

    #[inline]
    fn set_en_passant(&mut self, ep_square: Option<Square>) {
        if let Some(old_ep_square) = self.en_passant {
            self.zobrist.toggle_en_passant(old_ep_square);
        }
        if let Some(ep_square) = ep_square {
            self.zobrist.toggle_en_passant(ep_square);
        }
        self.en_passant = ep_square;
    }

    #[inline]
    fn is_double_push(&self, mv: &Move) -> bool {
        let second_rank = Rank::second_for(self.side_to_move);
        let fourth_rank = Rank::fourth_for(self.side_to_move);
        mv.from.rank() == second_rank && mv.to.rank() == fourth_rank
    }

    #[inline]
    fn update_hash(&mut self) {
        self.zobrist = Zobrist::new();

        if self.side_to_move() == Color::Black {
            self.zobrist.toggle_side_to_move();
        }

        if let Some(ep_square) = self.legal_en_passant() {
            self.zobrist.toggle_en_passant(ep_square);
        }

        for color in Color::ALL {
            if let Some(file) = self.castling[color].queen_side {
                self.zobrist.toggle_castling(color, file);
            }
            if let Some(file) = self.castling[color].king_side {
                self.zobrist.toggle_castling(color, file);
            }
        }

        for color in Color::ALL {
            for piece in Piece::ALL {
                for square in self.colored(color) & self.pieces(piece) {
                    self.zobrist.toggle_piece(color, piece, square);
                }
            }
        }
    }

    #[inline]
    fn update_checkers_and_pinners(&mut self, king: Square) {
        self.pinned = SquareSet::EMPTY;
        self.checkers = self.them()
            & ((SquareSet::knight_moves(king) & self.knights())
                | (SquareSet::pawn_attacks(self.side_to_move, king) & self.pawns()));

        for attacker in self.sliding_king_attackers(king) {
            let between = SquareSet::between(attacker, king) & self.occupied();
            match between.count() {
                0 => {
                    self.checkers.insert(attacker);
                }
                1 => self.pinned |= between,
                _ => {}
            }
        }
    }

    #[inline]
    fn sliding_king_attackers(&self, king: Square) -> SquareSet {
        self.them()
            & (((self.queens() | self.bishops()) & SquareSet::bishop_moves(king, SquareSet::EMPTY))
                | ((self.queens() | self.rooks()) & SquareSet::rook_moves(king, SquareSet::EMPTY)))
    }

    #[inline]
    pub fn validate(&self) -> Result<(), InvalidPositionError> {
        for color in Color::ALL {
            let count = self.king_count(color);
            if count != 1 {
                return Err(InvalidPositionError::KingCount { color, count });
            }
        }

        let our_king = self.our_king().unwrap();
        let their_king = self.their_king().unwrap();

        if self.backrank_pawns() > 0 {
            return Err(InvalidPositionError::PawnsInBackRank);
        }

        if (self.attacking(their_king) & self.us()).count() > 0 {
            return Err(InvalidPositionError::ExposedKing);
        }

        let checkers = self.attacking(our_king) & self.them();
        if checkers.count() > 2 {
            return Err(InvalidPositionError::TooManyCheckers(checkers.count()));
        }

        for color in Color::ALL {
            if !self.are_castling_rights_valid() {
                return Err(InvalidPositionError::CastlingRights);
            }

            let king = self.king(color).unwrap();
            if !self.is_castling_king_valid(king, color) {
                return Err(InvalidPositionError::CastlingKingPosition(color));
            }
            if !self.are_castling_rooks_valid(king, color) {
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

        if self.en_passant.and_then(|sq| self.piece_at(sq)).is_some() {
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

        if (white.king_side.is_some() && white.king_side == white.queen_side)
            || (black.king_side.is_some() && black.king_side == black.queen_side)
        {
            return false;
        }

        if white.king_side.is_some()
            && black.king_side.is_some()
            && white.king_side != black.king_side
        {
            return false;
        }
        if white.queen_side.is_some()
            && black.queen_side.is_some()
            && white.queen_side != black.queen_side
        {
            return false;
        }
        true
    }

    #[inline]
    fn is_castling_king_valid(&self, king: Square, color: Color) -> bool {
        if self.castling(color).is_none() {
            return true;
        }

        king.rank() == Rank::back_rank(color)
            && (self.variant != Variant::Standard || king.file() == File::E)
    }

    #[inline]
    fn are_castling_rooks_valid(&self, king: Square, color: Color) -> bool {
        if self.castling(color).is_none() {
            return true;
        }

        let backrank = Rank::back_rank(color);
        let castling = self.castling(color);

        if let Some(king_rook) = castling.king_side.map(|file| Square::new(file, backrank)) {
            if king_rook.file() < king.file()
                || !self.is_valid_castling_rook(CastlingSide::King, king_rook, color)
            {
                return false;
            }
        }

        if let Some(queen_rook) = castling.queen_side.map(|file| Square::new(file, backrank)) {
            if queen_rook.file() > king.file()
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
        self.en_passant.map(Square::rank)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Setup {
    pieces: ByPiece<SquareSet>,
    colors: ByColor<SquareSet>,
    pub castling: ByColor<Castling>,
    pub en_passant: Option<Square>,
    pub side_to_move: Color,
    pub halfmove_clock: u32,
    pub fullmove_number: u32,
    pub variant: Option<Variant>,
}

impl Setup {
    #[inline]
    pub fn new_empty() -> Self {
        Setup {
            pieces: ByPiece::default(),
            colors: ByColor::default(),
            castling: Default::default(),
            side_to_move: Color::White,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_number: 1,
            variant: None,
        }
    }

    #[inline]
    pub fn new_initial() -> Self {
        Setup {
            variant: None,
            pieces: INITIAL_PIECES,
            colors: INITIAL_COLORS,
            castling: ByColor::from_fn(|_| Castling::ALL_STANDARD),
            side_to_move: Color::White,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_number: 1,
        }
    }

    #[inline]
    pub fn new_chess960(scharnagl_number: u32) -> Self {
        let (pieces, castling) = initial_chess960(scharnagl_number);
        Self {
            pieces,
            castling,
            variant: None,
            colors: INITIAL_COLORS,
            side_to_move: Color::White,
            en_passant: None,
            halfmove_clock: 0,
            fullmove_number: 1,
        }
    }

    pub fn from_fen(fen: &str) -> Result<Self, FenParseError> {
        Ok(Fen::from_str(fen)?.setup)
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
    pub fn color_piece_at(&self, square: Square) -> Option<(Color, Piece)> {
        Some((self.color_at(square)?, self.piece_at(square)?))
    }

    #[inline]
    pub fn occupied(&self) -> SquareSet {
        self.colored(Color::White) | self.colored(Color::Black)
    }

    #[inline]
    pub fn king(&self, color: Color) -> Option<Square> {
        (self.pieces[Piece::King] & self.colors[color]).single()
    }

    #[inline]
    pub fn set_variant(&mut self, variant: Variant) -> &mut Self {
        self.variant = Some(variant);
        self
    }

    #[inline]
    pub fn put_piece(&mut self, square: Square, color: Color, piece: Piece) -> &mut Self {
        self.remove_piece(square);
        self.pieces[piece].insert(square);
        self.colors[color].insert(square);
        self
    }

    #[inline]
    pub fn remove_piece(&mut self, square: Square) -> &mut Self {
        if let (Some(color), Some(piece)) = (self.color_at(square), self.piece_at(square)) {
            self.pieces[piece].remove(square);
            self.colors[color].remove(square);
        }
        self
    }

    #[inline]
    pub fn set_board(&mut self, board: &BySquare<Option<(Color, Piece)>>) -> &mut Self {
        for (square, spot) in board {
            if let Some((color, piece)) = spot {
                self.put_piece(square, *color, *piece);
            } else {
                self.remove_piece(square);
            }
        }
        self
    }

    #[inline]
    pub fn set_side_to_move(&mut self, side_to_move: Color) -> &mut Self {
        self.side_to_move = side_to_move;
        self
    }

    #[inline]
    pub fn set_castling(&mut self, color: Color, castling: Castling) -> &mut Self {
        self.castling[color] = castling;
        self
    }

    #[inline]
    pub fn set_en_passant(&mut self, square: Option<Square>) -> &mut Self {
        self.en_passant = square;
        self
    }

    #[inline]
    pub fn set_halfmove_clock(&mut self, halfmove_clock: u32) -> &mut Self {
        self.halfmove_clock = halfmove_clock;
        self
    }

    #[inline]
    pub fn set_fullmove_number(&mut self, fullmove_number: u32) -> &mut Self {
        self.fullmove_number = fullmove_number;
        self
    }

    pub fn into_position(&self) -> Result<Position, InvalidPositionError> {
        let mut position = Position {
            pieces: self.pieces,
            colors: self.colors,
            occupied: self.occupied(),
            variant: self.deduce_variant(),
            castling: self.castling,
            en_passant: self.en_passant,
            halfmove_clock: self.halfmove_clock,
            side_to_move: self.side_to_move,
            fullmove_number: self.fullmove_number,
            checkers: SquareSet::EMPTY,
            pinned: SquareSet::EMPTY,
            zobrist: Zobrist::new(),
        };
        position.validate()?;
        position.update_checkers_and_pinners(position.our_king().unwrap());
        position.update_hash();
        Ok(position)
    }

    #[inline]
    fn deduce_variant(&self) -> Variant {
        let (white_king, black_king) = match (self.king(Color::White), self.king(Color::Black)) {
            (Some(white_king), Some(black_king)) => (white_king, black_king),
            _ => return Variant::Standard,
        };

        match self.variant {
            Some(variant) => variant,
            None if is_chess960(self.castling[Color::White], white_king)
                || is_chess960(self.castling[Color::Black], black_king) =>
            {
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

fn initial_chess960(seed: u32) -> (ByPiece<SquareSet>, ByColor<Castling>) {
    assert!(seed < 960);

    let seed = seed as usize;
    let (seed, light_bishop) = (seed / 4, seed % 4);
    let (seed, dark_bishop) = (seed / 4, seed % 4);
    let (knights, queen) = (seed / 6, seed % 6);
    let (knight1, knight2) = match knights {
        0 => (0, 1),
        1 => (0, 2),
        2 => (0, 3),
        3 => (0, 4),
        4 => (1, 2),
        5 => (1, 3),
        6 => (1, 4),
        7 => (2, 3),
        8 => (2, 4),
        9 => (3, 4),
        _ => unreachable!(),
    };

    let mut free_squares = SquareSet::from_rank(Rank::First);

    use Square::*;
    const LIGHT_BISHOP_SQS: [Square; 4] = [B1, D1, F1, H1];
    const DARK_BISHOP_SQS: [Square; 4] = [A1, C1, E1, G1];

    let bishop1 = LIGHT_BISHOP_SQS[light_bishop];
    let bishop2 = DARK_BISHOP_SQS[dark_bishop];
    free_squares.toggle(bishop1);
    free_squares.toggle(bishop2);

    let queen = free_squares.iter().nth(queen).unwrap();
    free_squares.toggle(queen);

    let knight1 = free_squares.iter().nth(knight1).unwrap();
    let knight2 = free_squares.iter().nth(knight2).unwrap();
    free_squares.toggle(knight1);
    free_squares.toggle(knight2);

    let rook1 = free_squares.first().unwrap();
    let rook2 = free_squares.last().unwrap();
    free_squares.toggle(rook1);
    free_squares.toggle(rook2);

    let king = free_squares.single().unwrap();

    let mut pieces = ByPiece::from_array([
        INITIAL_PIECES[Piece::Pawn],
        SquareSet::from([knight1, knight2]),
        SquareSet::from([bishop1, bishop2]),
        SquareSet::from([rook1, rook2]),
        SquareSet::from(queen),
        SquareSet::from(king),
    ]);
    let castling = Castling {
        king_side: Some(rook2.file()),
        queen_side: Some(rook1.file()),
    };

    for squares in pieces.values_mut() {
        *squares |= squares.flip_vertical();
    }

    (pieces, ByColor::from_fn(|_| castling))
}

const INITIAL_PIECES: ByPiece<SquareSet> = ByPiece::from_array([
    // Pawn
    SquareSet::from_bits(0x00ff00000000ff00),
    // Knight
    SquareSet::from_bits(0x4200000000000042),
    // Bishop
    SquareSet::from_bits(0x2400000000000024),
    // Rook
    SquareSet::from_bits(0x8100000000000081),
    // Queen
    SquareSet::from_bits(0x0800000000000008),
    // King
    SquareSet::from_bits(0x1000000000000010),
]);

const INITIAL_COLORS: ByColor<SquareSet> = ByColor::from_array([
    // White
    SquareSet::from_bits(0x000000000000ffff),
    // Black
    SquareSet::from_bits(0xffff000000000000),
]);

const INITIAL_OCCUPIED: SquareSet = SquareSet::from_bits(0xffff00000000ffff);

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use crate::{
        position::Setup,
        Castling, Color,
        FenError::*,
        File,
        InvalidPositionError::{self, *},
        Move, Piece, Position, SanMove,
        Square::*,
        SquareSet, UciMove, Variant,
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
                .set_variant(Variant::Standard)
                .into_position();
        assert_eq!(pos, Err(CastlingKingPosition(Color::White)));
        let pos =
            Setup::from_fen("r1bq1bkr/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 1")
                .unwrap()
                .set_variant(Variant::Standard)
                .into_position();
        assert_eq!(pos, Err(CastlingKingPosition(Color::Black)));
        let pos = Setup::from_fen("rnbqkbrn/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBRN w KQkq - 0 1")
            .unwrap()
            .set_variant(Variant::Standard)
            .into_position();
        assert!(matches!(pos, Err(CastlingRookPosition(_))));
        let pos = Position::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBRN w KQkq - 0 1");
        assert_eq!(
            pos,
            Err(InvalidPosition(InvalidPositionError::CastlingRights))
        );
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
        let mut position = Position::new_initial();
        position.play_unchecked(&Move::new_normal(E2, E4));
        position.play_unchecked(&Move::new_normal(H7, H6));
        position.play_unchecked(&Move::new_normal(E4, E5));
        position.play_unchecked(&Move::new_normal(D7, D5));
        position.play_unchecked(&Move::new_en_passant(E5, D6, D5));

        assert_eq!(
            position.fen().to_string(),
            "rnbqkbnr/ppp1ppp1/3P3p/8/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 3"
        );
    }

    #[test]
    fn short_castle() {
        let mut position = Position::new_initial();
        position.play_unchecked(&Move::new_normal(E2, E4));
        position.play_unchecked(&Move::new_normal(E7, E5));
        position.play_unchecked(&Move::new_normal(G1, F3));
        position.play_unchecked(&Move::new_normal(B8, C6));
        position.play_unchecked(&Move::new_normal(F1, B5));
        position.play_unchecked(&Move::new_normal(G8, F6));
        position.play_unchecked(&Move::new_castles(E1, G1, H1));

        assert_eq!(
            position.fen().to_string(),
            "r1bqkb1r/pppp1ppp/2n2n2/1B2p3/4P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 5 4"
        );
    }

    #[test]
    fn long_castle() {
        let mut position =
            Position::from_fen("r1bq1rk1/pppp1ppp/2n2n2/8/1b2P3/2N1Q3/PPPB1PPP/R3KBNR w KQ - 7 7")
                .unwrap();

        position.play_unchecked(&Move::new_castles(E1, C1, A1));

        assert_eq!(
            position.fen().to_string(),
            "r1bq1rk1/pppp1ppp/2n2n2/8/1b2P3/2N1Q3/PPPB1PPP/2KR1BNR b - - 8 7"
        );
    }

    #[test]
    fn promotion() {
        let mut position = Position::from_fen("5r2/6P1/k6K/8/8/8/8/8 w - - 1 1").unwrap();
        position.play_unchecked(&Move::new_promotion(G7, G8, Piece::Queen));

        assert_eq!(position.fen().to_string(), "5rQ1/8/k6K/8/8/8/8/8 b - - 0 1")
    }

    #[test]
    fn promotion_capture() {
        let mut position = Position::from_fen("5r2/6P1/k6K/8/8/8/8/8 w - - 1 1").unwrap();
        position.play_unchecked(&Move::new_promotion(G7, F8, Piece::Queen));

        assert_eq!(position.fen().to_string(), "5Q2/8/k6K/8/8/8/8/8 b - - 0 1")
    }

    #[test]
    fn chess960_seed() {
        assert_eq!(Position::new_chess960(518), Position::new_initial());
        let pos1 = Position::new_chess960(22);
        assert_eq!(
            pos1.fen().to_string(),
            "nqbnrbkr/pppppppp/8/8/8/8/PPPPPPPP/NQBNRBKR w KQkq - 0 1"
        );
        assert_eq!(
            pos1.castling(Color::White),
            Castling {
                king_side: Some(File::H),
                queen_side: Some(File::E),
            }
        );

        let pos2 = Position::new_chess960(420);
        assert_eq!(
            pos2.fen().to_string(),
            "rbbnqnkr/pppppppp/8/8/8/8/PPPPPPPP/RBBNQNKR w KQkq - 0 1",
        );
        assert_eq!(
            pos2.castling(Color::White),
            Castling {
                king_side: Some(File::H),
                queen_side: Some(File::A),
            }
        );
    }

    #[test]
    fn hash_game1() {
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
            position.hash(),
            Position::from_fen("8/8/8/4kp2/6p1/4K1P1/8/8 w - - 2 59")
                .unwrap()
                .hash()
        );
    }

    #[test]
    fn hash_game2() {
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
            position.hash(),
            Position::from_fen("8/kb3p1p/1p3P2/p2q1Q2/P1pB2B1/3r1NPK/7P/2b5 w - - 4 29")
                .unwrap()
                .hash()
        );
    }

    #[test]
    fn hash_transpositions() {
        let position = Position::from_fen(
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
        )
        .unwrap();

        const MOVES: &[[[&str; 4]; 2]] = &[
            [
                ["a2a4", "e8h8", "e1h1", "e7d8"],
                ["e1h1", "e8h8", "a2a4", "e7d8"],
            ],
            [
                ["h1g1", "f6g4", "d2h6", "b4b3"],
                ["h1g1", "b4b3", "d2h6", "f6g4"],
            ],
            [
                ["e1d1", "f6d5", "b2b3", "a8c8"],
                ["e1d1", "a8c8", "b2b3", "f6d5"],
            ],
            [
                ["e2c4", "h8h5", "f3f5", "e7d8"],
                ["f3f5", "h8h5", "e2c4", "e7d8"],
            ],
            [
                ["a2a3", "h8h5", "c3b1", "a8d8"],
                ["a2a3", "a8d8", "c3b1", "h8h5"],
            ],
            [
                ["e2d3", "c7c6", "g2g4", "h8h6"],
                ["e2d3", "h8h6", "g2g4", "c7c6"],
            ],
            [
                ["e1d1", "e8f8", "e5c6", "h8h5"],
                ["e1d1", "h8h5", "e5c6", "e8f8"],
            ],
            [
                ["g2h3", "e7d8", "e5g4", "b6c8"],
                ["e5g4", "b6c8", "g2h3", "e7d8"],
            ],
            [
                ["b2b3", "e8f8", "g2g3", "a6b7"],
                ["b2b3", "a6b7", "g2g3", "e8f8"],
            ],
            [
                ["e5g4", "e8d8", "d2e3", "a6d3"],
                ["d2e3", "a6d3", "e5g4", "e8d8"],
            ],
            [
                ["e5g4", "h8h5", "f3f5", "e6f5"],
                ["f3f5", "e6f5", "e5g4", "h8h5"],
            ],
            [
                ["e2c4", "h8f8", "d2h6", "b4b3"],
                ["e2c4", "b4b3", "d2h6", "h8f8"],
            ],
            [
                ["a1c1", "c7c5", "c3a4", "a6e2"],
                ["c3a4", "c7c5", "a1c1", "a6e2"],
            ],
            [
                ["g2g3", "a8c8", "e5d3", "e7f8"],
                ["e5d3", "a8c8", "g2g3", "e7f8"],
            ],
            [
                ["c3a4", "f6g8", "e1d1", "a8c8"],
                ["c3a4", "a8c8", "e1d1", "f6g8"],
            ],
            [
                ["e5d3", "a6b7", "g2g3", "h8h6"],
                ["e5d3", "h8h6", "g2g3", "a6b7"],
            ],
            [
                ["e2d3", "g6g5", "d2f4", "b6d5"],
                ["d2f4", "g6g5", "e2d3", "b6d5"],
            ],
            [
                ["f3e3", "e8h8", "a2a4", "a8c8"],
                ["a2a4", "a8c8", "f3e3", "e8h8"],
            ],
            [
                ["d5d6", "e8h8", "f3f6", "a6c4"],
                ["f3f6", "a6c4", "d5d6", "e8h8"],
            ],
            [
                ["f3h5", "f6h7", "c3b1", "g7f6"],
                ["c3b1", "f6h7", "f3h5", "g7f6"],
            ],
        ];

        for (n, [moves1, moves2]) in MOVES.iter().enumerate() {
            let mut pos1 = position.clone();
            let mut pos2 = position.clone();
            for mv in moves1 {
                pos1.play(&mv.parse::<UciMove>().unwrap()).unwrap();
            }
            for mv in moves2 {
                pos2.play(&mv.parse::<UciMove>().unwrap()).unwrap();
            }
            assert_eq!(
                pos1.hash(),
                pos2.hash(),
                "transposition failed at test {}",
                n + 1
            );
        }
    }

    #[test]
    fn insufficient_material() {
        assert!(!Position::new_initial().is_insufficient_material());
        assert!(Position::from_fen("8/8/3b4/6B1/8/1K6/8/k7 w - - 0 1")
            .unwrap()
            .is_insufficient_material());
        assert!(!Position::from_fen("8/8/8/7k/8/6PK/6PP/1b6 w - - 0 1")
            .unwrap()
            .has_insufficient_material(Color::Black));
    }
}
