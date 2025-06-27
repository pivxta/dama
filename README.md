# dama

A general-purpose rust library for standard chess and chess960.

# Capabilities

- Parsing and outputting FEN, XFEN and Shredder FEN notation:

```rust
use dama::Position;

let position = 
    Position::from_fen("r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3").unwrap();
assert_eq!(
    position.fen().to_string(), 
    "r1bqkbnr/pppp1ppp/2n5/1B2p3/4P3/5N2/PPPP1PPP/RNBQK2R b KQkq - 3 3"
);
```

- Generating legal moves:

```rust
use dama::Position;

let position = Position::new_initial();
assert_eq!(position.legal_moves().len(), 20);
assert_eq!(position.legal_quiets().len(), 20);
assert_eq!(position.legal_captures().len(), 0);
```

- Playing moves and reading UCI and SAN move notation:

```rust
use dama::{Position, Square, UciMove, SanMove};

let position = Position::new_initial();
position.play("e2e4".parse::<UciMove>().unwrap()).unwrap();
position.play(Move::new_normal(Square::E7, Square::E5)).unwrap();
position.play("Nf3".parse::<SanMove>().unwrap()).unwrap();
```

- Parsing PGN games with error recovery. 

- Supports only standard and fischer-random chess (chess960), with no plans of supporting other variants.

- Fast and compact representation of square sets with bitboards and magic attack tables.

- Quick zobrist hashing.


