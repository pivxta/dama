use dama::{perft, Move, Position, SanMove, Square::*, ToMove, UciMove};

fn main() {
    let san_moves = [
        "d4", "d5", "f3", "Bb5", "a4", "Ba6", "Nf2", "Ng6", "Nd3", "Bxd3", "Rxd3", "e5", "e3",
        "Nf6", "Rb3", "a6", "g4", "c5", "dxc5", "Qxc5", "Ne2", "Bd6", "Ng3", "Ne7", "Rc3", "Qa7",
        "a5", "Bb4", "Rb3", "Bxe1", "Qxe1", "Nc6", "g5", "Nd7", "Bh3", "Qc5", "Nh5", "Qf8", "f4",
        "Nc5", "Rb6", "Ka7", "fxe5", "Nxe5", "Qb4", "Rab8", "Nxg7", "Nc6", "Qa3", "d4", "exd4",
        "Rxd4", "Nf5", "Ra4", "Qg3", "Rxa1+", "Kxa1", "Qd8", "Qg1", "Qd5", "b4", "Qe5+", "Ka2",
        "Rc8", "bxc5", "Qc3", "Rb3", "Qxc2+", "Ka3", "Nxa5", "c6+", "Kb8", "Qg3+", "Rc7", "Rc3",
        "Qb1", "Nd4", "b6", "Qd6", "Qa1+", "Kb4", "Nb7", "Qf8+", "Ka7", "Nb5+", "axb5", "Ra3+",
        "Qxa3+", "Kxa3", "Rxc6", "Bg2", "Rc7", "Be4", "b4+", "Kxb4", "Rd7", "Qe8", "Rc7", "Bd5",
        "b5", "h4", "Ka6", "Qxb5+", "Ka7", "Be4", "Re7", "Bxh7", "f6", "g6", "Rc7", "Qd5", "Kb8",
        "h5", "f5", "h6", "Rc1", "g7", "Rb1+", "Ka3", "Ra1+", "Kb2", "Rg1", "Bxf5", "Na5", "Qxa5",
        "Rg2+", "Bc2", "Kc8", "h7", "Rxc2+", "Kxc2", "Kd7", "g8=Q", "Kd6", "h8=Q", "Kd7", "Qa6",
        "Ke7", "Qae6#",
    ];
    let mut uci_moves = vec![];
    let mut position =
        Position::from_fen("rkqrbbnn/pppppppp/8/8/8/8/PPPPPPPP/RKQRBBNN w KQkq - 0 1").unwrap();

    for san in san_moves {
        let san = san
            .parse::<SanMove>()
            .unwrap()
            .to_move(&position)
            .unwrap_or_else(|_| panic!("error parsing {}", san));
        let uci = UciMove::from_move_chess960(san);
        uci_moves.push(uci.to_string());
        position.play(&uci).unwrap();
    }

    println!("{:?}", uci_moves)

    /*let depth = 1;

    for mv in position.legal_moves() {
        let mut child = position.clone();
        child.play_unchecked(&mv);
        println!("{}: {}", UciMove {
            from: mv.from,
            to: mv.to,
            promotion: mv.promotion()
        }, perft(&child, depth - 1));
    }*/
}
