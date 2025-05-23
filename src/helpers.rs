#[doc(hidden)]
pub use enum_map::enum_map as _by;

#[macro_export]
macro_rules! by_color {
    { $($color:ident => $value:expr),*$(,)? } => {{
        use $crate::Color;
        $crate::helpers::_by! {
            $(Color::$color => $value),*
        }
    }};
}

#[macro_export]
macro_rules! by_piece {
    { $($piece:ident => $value:expr),*$(,)? } => {{
        use $crate::Piece;
        $crate::helpers::_by! {
            $(Piece::$piece => $value),*
        }
    }};
}
