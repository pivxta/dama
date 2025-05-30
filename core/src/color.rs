use crate::helpers::mapped_enum_u8;
use core::fmt;
use std::ops::Not;

mapped_enum_u8! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub enum Color [all: Colors] {
        White,
        Black,
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Default)]
    pub map ByColor {
        White => white,
        Black => black
    }
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
