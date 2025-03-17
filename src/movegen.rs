use crate::{Color, File, Move, Piece, Position, Rank, Square, SquareSet, SquareSets};
use arrayvec::ArrayVec;
pub const MAX_LEGAL_MOVES: usize = 218;

pub type MoveList = ArrayVec<Move, MAX_LEGAL_MOVES>;

impl Position {
    pub fn legal_moves(&self) -> MoveList {
        let mut list = MoveList::new();

        if let Some(king) = self.our_king() {
            match self.checkers().count() {
                0 | 1 => {
                    let targets = self.target_squares(king);
                    self.add_en_passant(&mut list, king);
                    self.add_pawn_moves(&mut list, king, targets);
                    self.add_knight_moves(&mut list, targets);
                    self.add_slider_moves::<Bishop>(&mut list, king, targets);
                    self.add_slider_moves::<Rook>(&mut list, king, targets);
                    self.add_slider_moves::<Queen>(&mut list, king, targets);
                    self.add_king_moves(&mut list, king, SquareSet::ALL);
                }
                _ => self.add_king_moves(&mut list, king, SquareSet::ALL),
            }
        }

        list
    }

    pub fn legal_quiets(&self) -> MoveList {
        let mut list = MoveList::new();

        if let Some(king) = self.our_king() {
            match self.checkers().count() {
                0 | 1 => {
                    let targets = self.target_squares(king) & !self.them();
                    self.add_pawn_quiets(&mut list, king, targets);
                    self.add_knight_moves(&mut list, targets);
                    self.add_slider_moves::<Bishop>(&mut list, king, targets);
                    self.add_slider_moves::<Rook>(&mut list, king, targets);
                    self.add_slider_moves::<Queen>(&mut list, king, targets);
                    self.add_king_moves(&mut list, king, !self.them());
                }
                _ => self.add_king_moves(&mut list, king, !self.them()),
            }
        }

        list
    }

    pub fn legal_captures(&self) -> MoveList {
        let mut list = MoveList::new();

        if let Some(king) = self.our_king() {
            match self.checkers().count() {
                0 | 1 => {
                    let targets = self.them() & self.target_squares(king);
                    self.add_en_passant(&mut list, king);
                    self.add_pawn_captures(&mut list, king, targets);
                    self.add_knight_moves(&mut list, targets);
                    self.add_slider_moves::<Bishop>(&mut list, king, targets);
                    self.add_slider_moves::<Rook>(&mut list, king, targets);
                    self.add_slider_moves::<Queen>(&mut list, king, targets);
                    self.add_king_moves(&mut list, king, self.them());
                }
                _ => self.add_king_moves(&mut list, king, self.them()),
            }
        }

        list
    }

    #[inline]
    fn add_pawn_captures(&self, list: &mut MoveList, king: Square, targets: SquareSet) {
        self.add_safe_pawn_captures(list, targets);

        if !self.is_in_check() {
            let pinned_pawns = self.pawns() & self.us() & self.pinned();
            if pinned_pawns.is_empty() {
                return;
            }
            let their_pieces = self.them();
            let their_backrank = SquareSet::from(self.their_backrank());

            for from in pinned_pawns {
                let targets = targets & SquareSet::ray(from, king);
                let moves = SquareSet::pawn_attacks(self.side_to_move(), from) & their_pieces;
                add_moves(list, from, moves & targets & !their_backrank);
                add_promotions(list, from, moves & targets & their_backrank);
            }
        }
    }

    #[inline]
    fn add_pawn_quiets(&self, list: &mut MoveList, king: Square, targets: SquareSet) {
        self.add_safe_pawn_pushes(list, targets);

        if !self.is_in_check() {
            let pinned_pawns = self.pawns() & self.us() & self.pinned();
            if pinned_pawns.is_empty() {
                return;
            }
            let their_backrank = SquareSet::from(self.their_backrank());

            for from in pinned_pawns {
                let targets = targets & SquareSet::ray(from, king);
                let moves = SquareSet::pawn_pushes(self.side_to_move(), from, self.occupied());
                add_moves(list, from, moves & targets & !their_backrank);
                add_promotions(list, from, moves & targets & their_backrank);
            }
        }
    }

    #[inline]
    fn add_pawn_moves(&self, list: &mut MoveList, king: Square, targets: SquareSet) {
        self.add_safe_pawn_pushes(list, targets);
        self.add_safe_pawn_captures(list, targets);

        if !self.is_in_check() {
            let pinned_pawns = self.pawns() & self.us() & self.pinned();
            if pinned_pawns.is_empty() {
                return;
            }
            let their_pieces = self.them();
            let their_backrank = SquareSet::from(self.their_backrank());

            for from in pinned_pawns {
                let targets = targets & SquareSet::ray(from, king);
                let moves = SquareSet::pawn_pushes(self.side_to_move(), from, self.occupied())
                    | (SquareSet::pawn_attacks(self.side_to_move(), from) & their_pieces);
                add_moves(list, from, moves & targets & !their_backrank);
                add_promotions(list, from, moves & targets & their_backrank);
            }
        }
    }

    #[inline]
    fn add_safe_pawn_pushes(&self, list: &mut MoveList, targets: SquareSet) {
        let our_pawns = self.pawns() & self.us() & !self.pinned();
        let their_backrank = SquareSet::from(self.their_backrank());
        let (pushes, from_offset) = match self.side_to_move() {
            Color::White => (our_pawns.offset_ranks_by(1) & !self.occupied(), -8),
            Color::Black => (our_pawns.offset_ranks_by(-1) & !self.occupied(), 8),
        };

        add_pawn_moves(list, from_offset, pushes & targets & !their_backrank);
        add_pawn_promotions(list, from_offset, pushes & targets & their_backrank);

        let (double_pushes, from_offset) = match self.side_to_move() {
            Color::White => (pushes.offset_ranks_by(1) & !self.occupied(), -16),
            Color::Black => (pushes.offset_ranks_by(-1) & !self.occupied(), 16),
        };
        let double_pushes = double_pushes & Rank::fourth_for(self.side_to_move()).into();

        add_pawn_moves(list, from_offset, double_pushes & targets);
    }

    #[inline]
    fn add_safe_pawn_captures(&self, list: &mut MoveList, targets: SquareSet) {
        let our_pawns = self.pawns() & self.us() & !self.pinned();
        let their_backrank = SquareSet::from(self.their_backrank());
        let (forward, left_from_offset, right_from_offset) = match self.side_to_move() {
            Color::White => (our_pawns.offset_ranks_by(1), -7, -9),
            Color::Black => (our_pawns.offset_ranks_by(-1), 9, 7),
        };
        let (left_captures, right_captures) = (
            forward.shift_left() & self.them(),
            forward.shift_right() & self.them(),
        );

        add_pawn_moves(
            list,
            left_from_offset,
            left_captures & targets & !their_backrank,
        );
        add_pawn_promotions(
            list,
            left_from_offset,
            left_captures & targets & their_backrank,
        );

        add_pawn_moves(
            list,
            right_from_offset,
            right_captures & targets & !their_backrank,
        );
        add_pawn_promotions(
            list,
            right_from_offset,
            right_captures & targets & their_backrank,
        );
    }

    #[inline]
    fn add_en_passant(&self, list: &mut MoveList, king: Square) {
        if let Some(ep_square) = self.en_passant() {
            let them = !self.side_to_move();
            let target = ep_square.with_rank(Rank::fourth_for(them));
            let diagonal_attackers = (self.queens() | self.bishops()) & self.them();
            let orthogonal_attackers = (self.queens() | self.rooks()) & self.them();

            let attackers = self.pawns() & self.us() & SquareSet::pawn_attacks(them, ep_square);
            for from in attackers {
                let blockers_after_move = self
                    .occupied()
                    .toggled(from)
                    .toggled(target)
                    .toggled(ep_square);

                let king_attackers_after_move =
                    SquareSet::bishop_moves(king, blockers_after_move) & diagonal_attackers;
                if !king_attackers_after_move.is_empty() {
                    continue;
                }

                let king_attackers_after_move =
                    SquareSet::rook_moves(king, blockers_after_move) & orthogonal_attackers;
                if !king_attackers_after_move.is_empty() {
                    continue;
                }

                list.push(Move::new_en_passant(from, ep_square, target));
            }
        }
    }

    #[inline]
    fn add_knight_moves(&self, list: &mut MoveList, targets: SquareSet) {
        for from in self.knights() & self.us() & !self.pinned() {
            add_moves(list, from, SquareSet::knight_moves(from) & targets);
        }
    }

    #[inline]
    fn add_slider_moves<S>(&self, list: &mut MoveList, king: Square, targets: SquareSet)
    where
        S: Slider,
    {
        let occupied = self.occupied();
        let sliders = self.pieces(S::PIECE) & self.us();

        for from in sliders & !self.pinned() {
            add_moves(list, from, S::moves(from, occupied) & targets);
        }

        if !self.is_in_check() {
            for from in sliders & self.pinned() {
                let moves = S::moves(from, occupied);
                let targets = targets & SquareSet::ray(from, king);
                add_moves(list, from, moves & targets);
            }
        }
    }

    #[inline]
    fn add_king_moves(&self, list: &mut MoveList, king: Square, targets: SquareSet) {
        let danger = self.danger_squares(king);

        add_moves(
            list,
            king,
            SquareSet::king_moves(king) & !self.us() & !danger & targets,
        );

        if !self.is_in_check() {
            let castling = self.our_castling();
            let backrank = self.our_backrank();
            if let Some(rook) = castling.king_side {
                if self.can_castle(danger, king, File::G, rook, File::F) {
                    let king_to = Square::new(File::G, backrank);
                    if targets.contains(king_to) {
                        list.push(Move::new_castles(
                            king,
                            king_to,
                            Square::new(rook, backrank),
                        ));
                    }
                }
            }
            if let Some(rook) = castling.queen_side {
                if self.can_castle(danger, king, File::C, rook, File::D) {
                    let king_to = Square::new(File::C, backrank);
                    if targets.contains(king_to) {
                        list.push(Move::new_castles(
                            king,
                            king_to,
                            Square::new(rook, backrank),
                        ));
                    }
                }
            }
        }
    }

    #[inline]
    fn can_castle(
        &self,
        danger: SquareSet,
        king_from: Square,
        king_to: File,
        rook_from: File,
        rook_to: File,
    ) -> bool {
        let backrank = self.our_backrank();
        let king_to = Square::new(king_to, backrank);
        let rook_from = Square::new(rook_from, backrank);
        let rook_to = Square::new(rook_to, backrank);

        let must_be_safe = SquareSet::between(king_from, king_to) | king_to.into();
        let must_be_empty = (SquareSet::between(rook_from, rook_to)
            | SquareSet::between(king_from, king_to))
        .with(rook_to)
        .with(king_to)
        .without(king_from)
        .without(rook_from);

        (must_be_safe & danger).is_empty() && (must_be_empty & self.occupied()).is_empty()
    }

    #[inline]
    fn target_squares(&self, king: Square) -> SquareSet {
        if !self.is_in_check() {
            return !self.us();
        }

        let checkers = self.checkers();
        (checkers | SquareSet::between(checkers.single().unwrap(), king)) & !self.us()
    }

    #[inline]
    fn danger_squares(&self, king: Square) -> SquareSet {
        let occupied = self.occupied().toggled(king);
        let attacked =
            SquareSet::all_pawn_attacks(!self.side_to_move(), self.pawns() & self.them());
        let attacked = (self.knights() & self.them())
            .iter()
            .fold(attacked, |att, sq| att | SquareSet::knight_moves(sq));
        let attacked = (self.bishops() & self.them())
            .iter()
            .fold(attacked, |att, sq| {
                att | SquareSet::bishop_moves(sq, occupied)
            });
        let attacked = (self.rooks() & self.them())
            .iter()
            .fold(attacked, |att, sq| {
                att | SquareSet::rook_moves(sq, occupied)
            });
        let attacked = (self.queens() & self.them())
            .iter()
            .fold(attacked, |att, sq| {
                att | SquareSet::queen_moves(sq, occupied)
            });

        if let Some(their_king) = self.their_king() {
            attacked | SquareSet::king_moves(their_king)
        } else {
            attacked
        }
    }
}

#[inline]
fn add_pawn_moves(list: &mut MoveList, from_offset: i32, moves: SquareSet) {
    for to in moves {
        let from = unsafe { to.add_unchecked(from_offset) };
        list.push(Move::new_normal(from, to));
    }
}

#[inline]
fn add_pawn_promotions(list: &mut MoveList, from_offset: i32, moves: SquareSet) {
    for to in moves {
        let from = unsafe { to.add_unchecked(from_offset) };
        add_promotion_pieces(list, from, to);
    }
}

#[inline]
fn add_promotion_pieces(list: &mut MoveList, from: Square, to: Square) {
    list.push(Move::new_promotion(from, to, Piece::Queen));
    list.push(Move::new_promotion(from, to, Piece::Rook));
    list.push(Move::new_promotion(from, to, Piece::Knight));
    list.push(Move::new_promotion(from, to, Piece::Bishop));
}

#[inline]
fn add_promotions(list: &mut MoveList, from: Square, moves: SquareSet) {
    for to in moves {
        add_promotion_pieces(list, from, to);
    }
}

#[inline]
fn add_moves(list: &mut MoveList, from: Square, moves: SquareSet) {
    for to in moves {
        list.push(Move::new_normal(from, to))
    }
}

struct Queen;
struct Rook;
struct Bishop;

trait Slider {
    const PIECE: Piece;
    fn moves(square: Square, occupied: SquareSet) -> SquareSet;
}

impl Slider for Bishop {
    const PIECE: Piece = Piece::Bishop;

    #[inline]
    fn moves(square: Square, occupied: SquareSet) -> SquareSet {
        SquareSet::bishop_moves(square, occupied)
    }
}

impl Slider for Rook {
    const PIECE: Piece = Piece::Rook;

    #[inline]
    fn moves(square: Square, occupied: SquareSet) -> SquareSet {
        SquareSet::rook_moves(square, occupied)
    }
}

impl Slider for Queen {
    const PIECE: Piece = Piece::Queen;

    #[inline]
    fn moves(square: Square, occupied: SquareSet) -> SquareSet {
        SquareSet::queen_moves(square, occupied)
    }
}

#[cfg(test)]
mod tests {
    use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

    use crate::Position;

    #[test]
    fn chess960_positions() {
        let pos =
            Position::from_fen("b1nrkrqb/1p1npppp/p2p4/2p5/5P2/4P2P/PPPP1RP1/BNNRK1QB w Dfd - 1 9")
                .unwrap();
        assert_eq!(perft(&pos, 1), 25);
        assert_eq!(perft(&pos, 2), 475);
        assert_eq!(perft(&pos, 3), 12603);
        assert_eq!(perft(&pos, 4), 270909);
        assert_eq!(perft(&pos, 5), 7545536);
        assert_eq!(perft(&pos, 6), 179579818);

        let pos =
            Position::from_fen("nnrkbbrq/1pp2p1p/p2pp1p1/2P5/8/8/PP1PPPPP/NNRKBBRQ w Ggc - 0 9")
                .unwrap();
        assert_eq!(perft(&pos, 1), 24);
        assert_eq!(perft(&pos, 2), 762);
        assert_eq!(perft(&pos, 3), 19283);
        assert_eq!(perft(&pos, 4), 624598);
        assert_eq!(perft(&pos, 5), 16838099);
        assert_eq!(perft(&pos, 6), 555230555);

        let pos = Position::from_fen(
            "nqbr1bkr/p1p1ppp1/1p1n4/3pN2p/1P6/8/P1PPPPPP/NQBR1BKR w HDhd - 0 9",
        )
        .unwrap();
        assert_eq!(perft(&pos, 1), 29);
        assert_eq!(perft(&pos, 2), 898);
        assert_eq!(perft(&pos, 3), 26532);
        assert_eq!(perft(&pos, 4), 809605);
        assert_eq!(perft(&pos, 5), 24703467);

        let pos =
            Position::from_fen("nrqnbrkb/pppp1p2/4p2p/3B2p1/8/1P4P1/PQPPPP1P/NR1NBKR1 w GB - 0 9")
                .unwrap();
        assert_eq!(perft(&pos, 1), 37);
        assert_eq!(perft(&pos, 2), 764);
        assert_eq!(perft(&pos, 3), 27073);
        assert_eq!(perft(&pos, 4), 610950);
        assert_eq!(perft(&pos, 5), 21284835);

        let pos =
            Position::from_fen("nrbbnk1r/pp2pppq/8/2pp3p/3P2P1/1N6/PPP1PP1P/1RBBNKQR w HBhb - 0 9")
                .unwrap();
        assert_eq!(perft(&pos, 1), 29);
        assert_eq!(perft(&pos, 2), 1036);
        assert_eq!(perft(&pos, 3), 31344);
        assert_eq!(perft(&pos, 4), 1139166);
        assert_eq!(perft(&pos, 5), 35627310);
    }

    #[test]
    fn initial_position() {
        let pos = Position::new_initial();
        assert_eq!(perft(&pos, 1), 20);
        assert_eq!(perft(&pos, 2), 400);
        assert_eq!(perft(&pos, 3), 8902);
        assert_eq!(perft(&pos, 4), 197281);
        assert_eq!(perft(&pos, 5), 4865609);
        assert_eq!(perft(&pos, 6), 119060324);
        assert_eq!(perft(&pos, 7), 3195901860);
    }

    #[test]
    fn kiwipete() {
        let pos =
            Position::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - ")
                .unwrap();
        assert_eq!(perft(&pos, 1), 48);
        assert_eq!(perft(&pos, 2), 2039);
        assert_eq!(perft(&pos, 3), 97862);
        assert_eq!(perft(&pos, 4), 4085603);
        assert_eq!(perft(&pos, 5), 193690690);
    }

    #[test]
    fn position3() {
        let pos = Position::from_fen("8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - ").unwrap();
        assert_eq!(perft(&pos, 1), 14);
        assert_eq!(perft(&pos, 2), 191);
        assert_eq!(perft(&pos, 3), 2812);
        assert_eq!(perft(&pos, 4), 43238);
        assert_eq!(perft(&pos, 5), 674624);
        assert_eq!(perft(&pos, 6), 11030083);
        assert_eq!(perft(&pos, 7), 178633661);
        assert_eq!(perft(&pos, 8), 3009794393);
    }

    #[test]
    fn position4() {
        let pos =
            Position::from_fen("r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1")
                .unwrap();
        assert_eq!(perft(&pos, 1), 6);
        assert_eq!(perft(&pos, 2), 264);
        assert_eq!(perft(&pos, 3), 9467);
        assert_eq!(perft(&pos, 4), 422333);
        assert_eq!(perft(&pos, 5), 15833292);
        assert_eq!(perft(&pos, 6), 706045033);
    }

    #[test]
    fn position5() {
        let pos = Position::from_fen("rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8  ")
            .unwrap();
        assert_eq!(perft(&pos, 1), 44);
        assert_eq!(perft(&pos, 2), 1486);
        assert_eq!(perft(&pos, 3), 62379);
        assert_eq!(perft(&pos, 4), 2103487);
        assert_eq!(perft(&pos, 5), 89941194);
    }

    #[test]
    fn position6() {
        let pos = Position::from_fen(
            "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10 ",
        )
        .unwrap();
        assert_eq!(perft(&pos, 1), 46);
        assert_eq!(perft(&pos, 2), 2079);
        assert_eq!(perft(&pos, 3), 89890);
        assert_eq!(perft(&pos, 4), 3894594);
        assert_eq!(perft(&pos, 5), 164075551);
    }

    fn perft(position: &Position, depth: u32) -> u64 {
        if depth == 0 {
            return 1;
        }
        if depth == 1 {
            return position.legal_moves().len() as u64;
        }
        position
            .legal_moves()
            .par_iter()
            .map(|mv| {
                let mut child = position.clone();
                child.play_unchecked(mv);
                perft_internal(&child, depth - 1)
            })
            .sum()
    }

    fn perft_internal(position: &Position, depth: u32) -> u64 {
        if depth == 1 {
            return position.legal_moves().len() as u64;
        }

        let legal_moves = position.legal_moves();
        let quiets = position.legal_quiets();
        let captures = position.legal_captures();

        assert_eq!(legal_moves.len(), quiets.len() + captures.len());

        legal_moves
            .iter()
            .map(|mv| {
                let mut child = position.clone();
                child.play_unchecked(mv);
                perft_internal(&child, depth - 1)
            })
            .sum()
    }
}
