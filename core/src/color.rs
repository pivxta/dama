use core::fmt;
use std::ops::Not;

use enum_map::{Enum, EnumMap};

pub type ByColor<T> = EnumMap<Color, T>;

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Enum)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub const ALL: [Color; 2] = [Color::White, Color::Black];
    pub const COUNT: usize = 2;
}

impl Not for Color {
    type Output = Color;

    #[inline]
    fn not(self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

impl fmt::Display for Color {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match *self {
                Color::White => "white",
                Color::Black => "black",
            }
        )
    }
}
