use core::fmt;
use std::str::FromStr;
use thiserror::Error;

use crate::Color;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Winner(Color),
    Draw,
}

#[derive(Clone, Copy, Debug, Error)]
#[error("invalid outcome string.")]
pub struct OutcomeParseError;

impl Outcome {
    #[inline]
    pub fn winner(self) -> Option<Color> {
        match self {
            Outcome::Winner(winner) => Some(winner),
            Outcome::Draw => None,
        }
    }
}

impl FromStr for Outcome {
    type Err = OutcomeParseError;

    #[inline]
    fn from_str(s: &str) -> Result<Self, OutcomeParseError> {
        match s {
            "1-0" => Ok(Outcome::Winner(Color::White)),
            "0-1" => Ok(Outcome::Winner(Color::Black)),
            "1/2-1/2" => Ok(Outcome::Draw),
            _ => Err(OutcomeParseError),
        }
    }
}

impl fmt::Display for Outcome {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Winner(Color::White) => write!(f, "1-0"),
            Self::Winner(Color::Black) => write!(f, "0-1"),
            Self::Draw => write!(f, "1/2-1/2"),
        }
    }
}
