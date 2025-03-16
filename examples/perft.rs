use std::hint::black_box;

use dama::{Fen, Position};

fn main() {
    println!("{}", Position::new_chess960(0));
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
