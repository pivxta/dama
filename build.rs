use dama_core::{magic, moves, square::Square, squareset::SquareSet};
use std::{
    array, env,
    fs::File,
    io::{BufWriter, Write},
    path::PathBuf,
};

fn main() -> anyhow::Result<()> {
    println!("cargo:rerun-if-changed=build.rs");

    let king_table: [_; 64] = array::from_fn(|n| moves::king(Square::from_index(n)));
    let knight_table: [_; 64] = array::from_fn(|n| moves::knight(Square::from_index(n)));
    let black_pawn_cap_table: [_; 64] =
        array::from_fn(|n| moves::black_pawn_captures(Square::from_index(n)));
    let white_pawn_cap_table: [_; 64] =
        array::from_fn(|n| moves::white_pawn_captures(Square::from_index(n)));
    let line_between_table: [[_; 64]; 64] = array::from_fn(|sa| {
        array::from_fn(|sb| between(Square::from_index(sa), Square::from_index(sb)))
    });
    let line_table: [[_; 64]; 64] = array::from_fn(|sa| {
        array::from_fn(|sb| ray(Square::from_index(sa), Square::from_index(sb)))
    });

    let mut sliding_table = vec![SquareSet::EMPTY; magic::SLIDING_TABLE_SIZE];

    Bishop::write(&mut sliding_table);
    Rook::write(&mut sliding_table);

    let path = PathBuf::from(env::var("OUT_DIR")?).join("squareset_tables.rs");
    let mut writer = BufWriter::new(File::create(path)?);

    write_square_table(&mut writer, "KING_MOVES", &king_table)?;
    write_square_table(&mut writer, "KNIGHT_MOVES", &knight_table)?;
    write_square_table(&mut writer, "WHITE_PAWN_CAPTURES", &white_pawn_cap_table)?;
    write_square_table(&mut writer, "BLACK_PAWN_CAPTURES", &black_pawn_cap_table)?;
    write_nested_table(&mut writer, "LINE_TABLE", &line_table)?;
    write_nested_table(&mut writer, "LINE_BETWEEN_TABLE", &line_between_table)?;
    write_sliding_table(&mut writer, &sliding_table)?;

    Ok(())
}

fn write_nested_table(
    w: &mut impl Write,
    name: &str,
    table: &[[SquareSet; 64]; 64],
) -> anyhow::Result<()> {
    writeln!(w, "#[rustfmt::skip]")?;
    writeln!(w, "const {}: [[u64; 64]; 64] = [", name)?;
    for row in table.iter() {
        write!(w, "    [")?;
        for &entry in row.iter() {
            write!(w, "0x{:x}, ", entry.0)?;
        }
        writeln!(w, "],")?;
    }
    writeln!(w, "];")?;
    writeln!(w)?;
    Ok(())
}

fn write_square_table(
    w: &mut impl Write,
    name: &str,
    table: &[SquareSet; 64],
) -> anyhow::Result<()> {
    writeln!(w, "#[rustfmt::skip]")?;
    writeln!(w, "const {}: [u64; {}] = [", name, table.len())?;
    for entry in table.iter() {
        writeln!(w, "    0x{:x},", entry.0)?;
    }
    writeln!(w, "];")?;
    writeln!(w)?;
    Ok(())
}

fn write_sliding_table(w: &mut impl Write, sliding_table: &[SquareSet]) -> anyhow::Result<()> {
    writeln!(w, "#[rustfmt::skip]")?;
    writeln!(
        w,
        "static SLIDING_TABLE: [u64; {}] = [",
        sliding_table.len()
    )?;
    for entry in sliding_table.iter() {
        writeln!(w, "    0x{:x},", entry.0)?;
    }
    writeln!(w, "];")?;
    writeln!(w)?;
    Ok(())
}

#[inline]
fn between(a: Square, b: Square) -> SquareSet {
    let diagonal = moves::bishop(a, b.into());
    if diagonal.contains(b) {
        return diagonal & moves::bishop(b, a.into());
    }

    let orthogonal = moves::rook(a, b.into());
    if orthogonal.contains(b) {
        return orthogonal & moves::rook(b, a.into());
    }

    SquareSet::EMPTY
}

#[inline]
fn ray(a: Square, b: Square) -> SquareSet {
    let diagonal = moves::bishop(a, SquareSet::EMPTY) | SquareSet::from(a);
    if diagonal.contains(b) {
        return diagonal & (moves::bishop(b, SquareSet::EMPTY) | SquareSet::from(b));
    }

    let orthogonal = moves::rook(a, SquareSet::EMPTY) | SquareSet::from(a);
    if orthogonal.contains(b) {
        return orthogonal & (moves::rook(b, SquareSet::EMPTY) | SquareSet::from(b));
    }

    SquareSet::EMPTY
}

trait Slider {
    fn relevant_blockers(square: Square) -> SquareSet;
    fn moves(square: Square, occupied: SquareSet) -> SquareSet;
    fn index(square: Square, occupied: SquareSet) -> usize;

    fn write(table: &mut [SquareSet]) {
        for square in Square::ALL {
            for occupied in Self::relevant_blockers(square).subsets() {
                table[Self::index(square, occupied)] = Self::moves(square, occupied);
            }
        }
    }
}

struct Rook;
struct Bishop;

impl Slider for Rook {
    #[inline]
    fn relevant_blockers(square: Square) -> SquareSet {
        magic::rook_relevant_blockers(square)
    }

    #[inline]
    fn index(square: Square, occupied: SquareSet) -> usize {
        magic::rook_table_index(square, occupied)
    }

    #[inline]
    fn moves(square: Square, occupied: SquareSet) -> SquareSet {
        moves::rook(square, occupied)
    }
}

impl Slider for Bishop {
    #[inline]
    fn relevant_blockers(square: Square) -> SquareSet {
        magic::bishop_relevant_blockers(square)
    }

    #[inline]
    fn index(square: Square, occupied: SquareSet) -> usize {
        magic::bishop_table_index(square, occupied)
    }

    #[inline]
    fn moves(square: Square, occupied: SquareSet) -> SquareSet {
        moves::bishop(square, occupied)
    }
}
