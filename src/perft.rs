use crate::Position;

pub fn perft(position: &Position, depth: u32) -> u64 {
    if depth == 0 {
        return 1;
    }
    if depth == 1 {
        return position.legal_moves().len() as u64;
    }
    position
        .legal_moves()
        .iter()
        .map(|mv| {
            let mut child = position.clone();
            child.play_unchecked(mv);
            perft(&child, depth - 1)
        })
        .sum()
}
