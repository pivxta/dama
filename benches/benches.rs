use std::str::FromStr;

use criterion::{black_box, criterion_group, criterion_main, BatchSize, Criterion};
use dama::{perft, Position, SanMove, ToMove, UciMove};

const MOVES: [&str; 116] = [
    "Nf3", "d5", "g3", "c5", "Bg2", "Nc6", "d4", "e6", "O-O", "cxd4", "Nxd4", "Nge7", "c4", "Nxd4",
    "Qxd4", "Nc6", "Qd1", "d4", "e3", "Bc5", "exd4", "Bxd4", "Nc3", "O-O", "Nb5", "Bb6", "b3",
    "a6", "Nc3", "Bd4", "Bb2", "e5", "Qd2", "Be6", "Nd5", "b5", "cxb5", "axb5", "Nf4", "exf4",
    "Bxc6", "Bxb2", "Qxb2", "Rb8", "Rfd1", "Qb6", "Bf3", "fxg3", "hxg3", "b4", "a4", "bxa3",
    "Rxa3", "g6", "Qd4", "Qb5", "b4", "Qxb4", "Qxb4", "Rxb4", "Ra8", "Rxa8", "Bxa8", "g5", "Bd5",
    "Bf5", "Rc1", "Kg7", "Rc7", "Bg6", "Rc4", "Rb1+", "Kg2", "Re1", "Rb4", "h5", "Ra4", "Re5",
    "Bf3", "Kh6", "Kg1", "Re6", "Rc4", "g4", "Bd5", "Rd6", "Bb7", "Kg5", "f3", "f5", "fxg4",
    "hxg4", "Rb4", "Bf7", "Kf2", "Rd2+", "Kg1", "Kf6", "Rb6+", "Kg5", "Rb4", "Be6", "Ra4", "Rb2",
    "Ba8", "Kf6", "Rf4", "Ke5", "Rf2", "Rxf2", "Kxf2", "Bd5", "Bxd5", "Kxd5", "Ke3", "Ke5",
];

fn movegen_bench(c: &mut Criterion) {
    c.bench_function("movegen", |b| {
        let mut positions = vec![Position::new_initial()];
        for san in MOVES.iter() {
            let mut position = positions.last().unwrap().clone();
            position.play(&SanMove::from_str(san).unwrap()).unwrap();
            positions.push(position);
        }

        b.iter(|| {
            for position in &positions {
                black_box(black_box(position).legal_moves());
            }
        })
    });
    c.bench_function("perft(3)", |b| {
        let position = Position::new_initial();
        b.iter(|| perft(black_box(&position), 3))
    });
    c.bench_function("perft(5)", |b| {
        let position = Position::new_initial();
        b.iter(|| perft(black_box(&position), 5))
    });
}

fn playmove_bench(c: &mut Criterion) {
    c.bench_function("playmove", |b| {
        let mut position = Position::new_initial();
        let mut mvs = vec![];
        for san in MOVES.iter() {
            let mv = san.parse::<SanMove>().unwrap().to_move(&position).unwrap();

            mvs.push(mv);
            position.play_unchecked(&mv);
        }

        b.iter_batched(
            || Position::new_initial(),
            |mut pos| {
                for mv in mvs.iter() {
                    black_box(&mut pos).play_unchecked(black_box(mv));
                }
            },
            BatchSize::PerIteration,
        )
    });
}

fn notation_bench(c: &mut Criterion) {
    c.bench_function("san_to_move", |b| {
        let mut positions = vec![Position::new_initial()];
        for san in MOVES.iter() {
            let mut position = positions.last().unwrap().clone();
            position.play(&SanMove::from_str(san).unwrap()).unwrap();
            positions.push(position);
        }

        b.iter(|| {
            for (pos, san) in positions.iter().zip(MOVES.iter()) {
                black_box(
                    black_box(&san)
                        .parse::<SanMove>()
                        .unwrap()
                        .to_move(&pos)
                        .unwrap(),
                );
            }
        })
    });

    c.bench_function("uci_to_move", |b| {
        let ucis = [
            "g1f3", "d7d5", "g2g3", "c7c5", "f1g2", "b8c6", "d2d4", "e7e6", "e1g1", "c5d4", "f3d4",
            "g8e7", "c2c4", "c6d4", "d1d4", "e7c6", "d4d1", "d5d4", "e2e3", "f8c5", "e3d4", "c5d4",
            "b1c3", "e8g8", "c3b5", "d4b6", "b2b3", "a7a6", "b5c3", "b6d4", "c1b2", "e6e5", "d1d2",
            "c8e6", "c3d5", "b7b5", "c4b5", "a6b5", "d5f4", "e5f4", "g2c6", "d4b2", "d2b2", "a8b8",
            "f1d1", "d8b6", "c6f3", "f4g3", "h2g3", "b5b4", "a2a4", "b4a3", "a1a3", "g7g6", "b2d4",
            "b6b5", "b3b4", "b5b4", "d4b4", "b8b4", "a3a8", "f8a8", "f3a8", "g6g5", "a8d5", "e6f5",
            "d1c1", "g8g7", "c1c7", "f5g6", "c7c4", "b4b1", "g1g2", "b1e1", "c4b4", "h7h5", "b4a4",
            "e1e5", "d5f3", "g7h6", "g2g1", "e5e6", "a4c4", "g5g4", "f3d5", "e6d6", "d5b7", "h6g5",
            "f2f3", "f7f5", "f3g4", "h5g4", "c4b4", "g6f7", "g1f2", "d6d2", "f2g1", "g5f6", "b4b6",
            "f6g5", "b6b4", "f7e6", "b4a4", "d2b2", "b7a8", "g5f6", "a4f4", "f6e5", "f4f2", "b2f2",
            "g1f2", "e6d5", "a8d5", "e5d5", "f2e3", "d5e5", "e3d3", "f5f4", "d3e2", "f4f3", "e2e3",
            "e5f5", "e3f2", "f5e4", "f2f1", "f3f2", "f1f2", "e4d3", "f2f1", "d3e3", "f1g2", "e3e2",
            "g2g1", "e2f3", "g1h2", "f3f2", "h2h1", "f2g3", "h1g1", "g3h3", "g1f2", "h3h2", "f2f1",
            "g4g3", "f1e2", "g3g2", "e2d3", "g2g1q", "d3c4", "h2g3",
        ];

        let mut positions = vec![Position::new_initial()];
        for uci in ucis.iter() {
            let mut position = positions.last().unwrap().clone();
            position.play(&UciMove::from_str(uci).unwrap()).unwrap();
            positions.push(position);
        }

        b.iter(|| {
            for (pos, uci) in positions.iter().zip(ucis.iter()) {
                black_box(
                    black_box(&uci)
                        .parse::<UciMove>()
                        .unwrap()
                        .to_move(&pos)
                        .unwrap(),
                );
            }
        })
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = notation_bench, movegen_bench, playmove_bench
}
criterion_main!(benches);
