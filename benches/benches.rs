use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use dama::{perft, Position, San};

fn movegen_bench(c: &mut Criterion) {
    c.bench_function(
        "movegen", 
        |b| {
            let position = Position::new_initial();
            b.iter(|| black_box(&position).legal_moves())
        }
    );
    c.bench_function(
        "perft_shallow", 
        |b| {
            let position = Position::new_initial();
            b.iter(|| perft(black_box(&position), 3))
        }
    );
    c.bench_function(
        "perft_deep", 
        |b| {
            let position = Position::new_initial();
            b.iter(|| perft(black_box(&position), 5))
        }
    );

}

fn playmove_bench(c: &mut Criterion) {
    const MOVES: [&str; 116] = [
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

    c.bench_function("playmove", |b| {
        let sans = MOVES
            .into_iter()
            .map(San::from_str)
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        let mut position = Position::new_initial();
        let mut mvs = vec![];
        for san in sans.iter() {
            let mv = san.to_move(&position).unwrap();
            mvs.push(mv);
            position = position.play_unchecked(&mv);
        }

        b.iter_batched(|| Position::new_initial(), |mut pos| {
            for mv in mvs.iter() {
                pos = black_box(pos.play_unchecked(black_box(mv)));
            }
        }, BatchSize::PerIteration)
    });
}

fn san_bench(c: &mut Criterion) {
    const MOVES: [&str; 116] = [
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

    c.bench_function("san_to_move", |b| {
        let sans = MOVES
            .into_iter()
            .collect::<Vec<_>>();

        let mut positions = vec![Position::new_initial()];
        for san in sans.iter() {
            positions.push(
                positions
                    .last()
                    .unwrap()
                    .play_san(&san.parse().unwrap())
                    .unwrap()
            );
        }

        b.iter(|| {
            for (pos, san) in positions.iter().zip(sans.iter()) {
                black_box(
                    black_box(&san)
                        .parse::<San>()
                        .unwrap()
                        .to_move(&pos)
                        .unwrap()
                );
            }
        })
    });
}

criterion_group! { 
    name = benches;
    config = Criterion::default();
    targets = san_bench, movegen_bench, playmove_bench
}
criterion_main!(benches);
