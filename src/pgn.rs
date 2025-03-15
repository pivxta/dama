use core::{result, str};
use std::{
    fmt::Debug,
    io::{self, Bytes, Read},
    ops::Range,
};
use thiserror::Error;

use crate::{Color, Outcome, SanMove, SanParseError};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ControlFlow {
    Continue,
    Skip,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Annotation {
    pub id: u8,
}

pub trait Visitor {
    type Error;

    fn prepare(&mut self) {}

    fn enter_tag_pairs(&mut self) -> ControlFlow {
        ControlFlow::Continue
    }

    fn visit_tag_pair(&mut self, _name: &str, _value: &str) -> result::Result<(), Self::Error> {
        Ok(())
    }

    fn enter_game(&mut self) -> ControlFlow {
        ControlFlow::Continue
    }

    fn visit_move(
        &mut self,
        _number: Option<u32>,
        _mv: SanMove,
    ) -> result::Result<(), Self::Error> {
        Ok(())
    }
    fn visit_annotation(&mut self, _annotation: Annotation) -> result::Result<(), Self::Error> {
        Ok(())
    }
    fn visit_comment(&mut self, _comment: &[u8]) -> result::Result<(), Self::Error> {
        Ok(())
    }
    fn visit_outcome(&mut self, _outcome: Option<Outcome>) -> result::Result<(), Self::Error> {
        Ok(())
    }

    fn enter_variation(&mut self) -> ControlFlow;
    fn leave_variation(&mut self) {}

    fn finish(&mut self) {}
}

#[derive(Debug)]
pub struct Reader<R>
where
    R: Read,
{
    bytes: Bytes<R>,
    next: Option<u8>,
    unrecoverable: bool,
    line: u32,
    column: u32,
    scratch: Vec<u8>,
}

#[derive(Error, Debug)]
pub enum Error<VisitorError> {
    #[error("{0}")]
    Visitor(VisitorError),
    #[error("{0}")]
    Parse(ParseError),
}

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Error)]
#[error("parse error at {line}:{column}: {kind}")]
pub struct ParseError {
    pub line: u32,
    pub column: u32,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("IO error: {0}")]
    Io(io::Error),
    #[error("invalid UTF8 string: {0}")]
    Utf8(str::Utf8Error),
    #[error("parse error: {0}")]
    Message(String),
    #[error("invalid SAN move: {0}")]
    InvalidMove(SanParseError),
    #[error("unexpected end of file")]
    UnexpectedEof,
}

impl Annotation {
    pub const NULL: Self = Self { id: 0 };
    pub const GOOD_MOVE: Self = Self { id: 1 };
    pub const POOR_MOVE: Self = Self { id: 2 };
    pub const GREAT_MOVE: Self = Self { id: 3 };
    pub const BLUNDER: Self = Self { id: 4 };
    pub const SPECULATIVE_MOVE: Self = Self { id: 5 };
    pub const QUESTIONABLE_MOVE: Self = Self { id: 6 };
    pub const FORCED_MOVE: Self = Self { id: 7 };
    pub const SINGULAR_MOVE: Self = Self { id: 8 };
    pub const WORST_MOVE: Self = Self { id: 9 };
}

impl Reader<&[u8]> {
    pub fn from_string(s: &str) -> Reader<&[u8]> {
        Reader::new(s.as_bytes())
    }
}

impl<R> Reader<R>
where
    R: Read,
{
    pub fn new(read: R) -> Self {
        Self {
            bytes: read.bytes(),
            next: None,
            unrecoverable: false,
            line: 1,
            column: 1,
            scratch: Vec::with_capacity(64),
        }
    }

    pub fn visit_game<V>(&mut self, visitor: &mut V) -> Result<bool, Error<V::Error>>
    where
        V: Visitor,
    {
        if self.unrecoverable {
            return Ok(false);
        }

        self.skip_whitespace()?;
        if self.peek()?.is_none() {
            return Ok(false);
        }

        visitor.prepare();
        if let Err(err) = self.read_tag_pairs(visitor) {
            self.recover_tag_pairs()?;
            return Err(err);
        }
        if let Err(err) = self.read_movetext(visitor) {
            self.recover_movetext()?;
            return Err(err);
        }
        visitor.finish();
        Ok(true)
    }

    pub fn read_tag_pairs<V>(&mut self, visitor: &mut V) -> Result<(), Error<V::Error>>
    where
        V: Visitor,
    {
        visitor.enter_tag_pairs();

        self.skip_whitespace()?;
        while self.next_if_eq(b'[')?.is_some() {
            self.scratch.clear();

            let tag_name = self.parse_tag_name()?;
            let tag_value = self.parse_tag_value()?;

            if self.next_if_eq(b']')?.is_none() {
                return Err(self.error_msg("expected matching ']'").into());
            }

            let (name, value) = (
                str::from_utf8(&self.scratch[tag_name])
                    .map_err(|err| self.error(ParseErrorKind::Utf8(err)))?,
                str::from_utf8(&self.scratch[tag_value])
                    .map_err(|err| self.error(ParseErrorKind::Utf8(err)))?,
            );

            visitor
                .visit_tag_pair(name, value)
                .map_err(Error::Visitor)?;
        }

        Ok(())
    }

    fn recover_tag_pairs(&mut self) -> ParseResult<()> {
        if self.unrecoverable {
            return Ok(());
        }

        while let Some(c) = self.peek()? {
            match c {
                b'[' => break,
                _ => {
                    self.next()?;
                    if c == b']' {
                        break;
                    }
                }
            }
        }

        'outer: while self.next_if_eq(b'[')?.is_some() {
            while let Some(c) = self.peek()? {
                match c {
                    b'[' => continue 'outer,
                    _ => {
                        self.next()?;
                        if c == b']' {
                            continue 'outer;
                        }
                    }
                }
            }
        }

        self.recover_movetext()
    }

    fn recover_movetext(&mut self) -> ParseResult<()> {
        if self.unrecoverable {
            return Ok(());
        }

        let mut comment = false;
        // Tries to skip until the next tag pair
        while let Some(c) = self.peek()? {
            match c {
                b'[' if !comment => return Ok(()),
                b'{' if !comment => comment = true,
                b'}' => comment = false,
                _ => {}
            }
            self.next()?;
        }
        Ok(())
    }

    pub fn parse_tag_name(&mut self) -> ParseResult<Range<usize>> {
        let scratch_begin = self.scratch.len();
        self.take_while(|c| c.is_ascii_alphanumeric() || c == b'_')?;
        let scratch_end = self.scratch.len();

        if scratch_begin == scratch_end {
            return Err(self.error(ParseErrorKind::Message("expected tag name".into())));
        }

        Ok(scratch_begin..scratch_end)
    }

    pub fn parse_tag_value(&mut self) -> ParseResult<Range<usize>> {
        if self.next_if_eq(b'"')?.is_none() {
            return Err(self.error_msg("expected string token for tag value"));
        }

        let scratch_begin = self.scratch.len();
        while let Some(c) = self.next()? {
            match c {
                b'"' => break,
                b'\\' => match self.next()? {
                    Some(b'"') => self.scratch.push(b'"'),
                    Some(b'\\') => self.scratch.push(b'\\'),
                    _ => return Err(self.error_msg("invalid escape character")),
                },
                b'\n' => return Err(self.error_msg("unterminated string")),
                _ if c.is_ascii_control() => {
                    return Err(self.error_msg("control characters are not allowed inside strings."))
                }
                _ => self.scratch.push(c),
            }
        }
        let scratch_end = self.scratch.len();
        Ok(scratch_begin..scratch_end)
    }

    pub fn read_movetext<V>(&mut self, visitor: &mut V) -> Result<(), Error<V::Error>>
    where
        V: Visitor,
    {
        let mut variation = VariationInfo {
            depth: 0,
            skip: match visitor.enter_game() {
                ControlFlow::Continue => None,
                ControlFlow::Skip => Some(0),
            },
        };

        loop {
            self.skip_whitespace()?;
            match self.read_token(&mut variation, visitor)? {
                Continue(false) => break Ok(()),
                Continue(true) => {}
            }
        }
    }

    #[inline]
    fn read_token<V>(
        &mut self,
        variation: &mut VariationInfo,
        visitor: &mut V,
    ) -> Result<Continue, Error<V::Error>>
    where
        V: Visitor,
    {
        if matches!(self.peek()?, Some(b'[') | None) {
            return Ok(Continue(false));
        }

        let should_visit = variation.should_visit();
        match self.next()? {
            Some(b'*') => {
                if should_visit {
                    visitor.visit_outcome(None).map_err(Error::Visitor)?;
                }
                if variation.depth == 0 {
                    return Ok(Continue(false));
                }
            }
            Some(b'(') if should_visit => match visitor.enter_variation() {
                ControlFlow::Skip => variation.enter_skip(),
                ControlFlow::Continue => variation.enter(),
            },
            Some(b'(') => {
                variation.enter();
            }
            Some(b')') if variation.depth != 0 => {
                if should_visit {
                    visitor.leave_variation();
                }
                variation.leave();
            }
            Some(b')') => {
                return Err(self.error_msg("unexpected ')'").into());
            }
            Some(c @ b'0'..=b'9') => {
                if matches!(c, b'0' | b'1') {
                    if let Some(d) = self.next_if(|c| matches!(c, b'-' | b'/'))? {
                        let outcome = self.parse_outcome(c, d)?;
                        if should_visit {
                            visitor
                                .visit_outcome(Some(outcome))
                                .map_err(Error::Visitor)?;
                        }
                        if variation.depth == 0 {
                            return Ok(Continue(false));
                        }
                        return Ok(Continue(true));
                    }
                }

                let move_number = self.parse_move_number(c)?;
                let move_ch = self
                    .next()?
                    .ok_or(self.error(ParseErrorKind::UnexpectedEof))?;
                let mv = self.parse_move(move_ch)?;

                if should_visit {
                    visitor
                        .visit_move(Some(move_number), mv)
                        .map_err(Error::Visitor)?;
                }

                if let Some(suffix) = self.parse_move_suffix()? {
                    if should_visit {
                        visitor.visit_annotation(suffix).map_err(Error::Visitor)?;
                    }
                }
            }
            Some(c) if is_symbol(c) => {
                let mv = self.parse_move(c)?;
                if should_visit {
                    visitor.visit_move(None, mv).map_err(Error::Visitor)?;
                }

                if let Some(suffix) = self.parse_move_suffix()? {
                    if should_visit {
                        visitor.visit_annotation(suffix).map_err(Error::Visitor)?;
                    }
                }
            }
            Some(b'{') => {
                let comment = self.read_until_after(b'}')?;
                if should_visit {
                    visitor.visit_comment(comment).map_err(Error::Visitor)?;
                }
            }
            Some(b';') => {
                let comment = self.read_until_after(b'\n')?;
                if should_visit {
                    visitor.visit_comment(comment).map_err(Error::Visitor)?;
                }
            }
            Some(b'$') if should_visit => {
                visitor
                    .visit_annotation(Annotation {
                        id: self.parse_number()? as u8,
                    })
                    .map_err(Error::Visitor)?;
            }
            Some(c) => {
                return Err(self
                    .error_msg(format!("unexpected character '{}'", c as char))
                    .into());
            }
            None => unreachable!(),
        }
        Ok(Continue(true))
    }

    fn read_until_after(&mut self, until: u8) -> ParseResult<&[u8]> {
        self.scratch.clear();
        let scratch_start = 0;
        while let Some(c) = self.next()? {
            if c == until {
                return Ok(&self.scratch[scratch_start..]);
            }
            self.scratch.push(c);
        }
        Err(self.error(ParseErrorKind::UnexpectedEof))
    }

    fn parse_outcome(&mut self, c: u8, d: u8) -> ParseResult<Outcome> {
        let e = self
            .next_if(|c| matches!(c, b'0' | b'1' | b'2'))?
            .ok_or(self.error_msg("invalid game result"))?;

        match (c, d, e) {
            (b'1', b'-', b'0') => Ok(Outcome::Winner(Color::White)),
            (b'0', b'-', b'1') => Ok(Outcome::Winner(Color::Black)),
            (b'1', b'/', b'2') => {
                self.next_if_eq(b'-')?
                    .ok_or(self.error_msg("invalid game result"))?;
                self.next_if_eq(b'1')?
                    .ok_or(self.error_msg("invalid game result"))?;
                self.next_if_eq(b'/')?
                    .ok_or(self.error_msg("invalid game result"))?;
                self.next_if_eq(b'2')?
                    .ok_or(self.error_msg("invalid game result"))?;
                Ok(Outcome::Draw)
            }
            _ => Err(self.error_msg("invalid game result")),
        }
    }

    fn parse_number(&mut self) -> ParseResult<u32> {
        if !self.peek()?.map(|c| c.is_ascii_digit()).unwrap_or(false) {
            return Err(self.error_msg("expected number"));
        }

        let mut number = 0;
        while let Some(c) = self.peek()? {
            if !c.is_ascii_digit() {
                break;
            }
            number = number * 10 + ((c - b'0') as u32);
            self.next()?;
        }
        Ok(number)
    }

    fn parse_move_number(&mut self, c: u8) -> ParseResult<u32> {
        let mut number = (c - b'0') as u32;
        while let Some(c) = self.peek()? {
            if !c.is_ascii_digit() {
                break;
            }
            let digit = (c - b'0') as u32;
            number = number * 10 + digit;
            self.next()?;
        }
        loop {
            self.skip_whitespace()?;
            if self.next_if_eq(b'.')?.is_none() {
                break;
            }
        }
        Ok(number)
    }

    fn parse_move_suffix(&mut self) -> ParseResult<Option<Annotation>> {
        let id = match self.next_if(|c| matches!(c, b'!' | b'?'))? {
            Some(b'!') => match self.next_if(|c| matches!(c, b'!' | b'?'))? {
                Some(b'!') => 3,
                Some(b'?') => 5,
                _ => 1,
            },
            Some(b'?') => match self.next_if(|c| matches!(c, b'!' | b'?'))? {
                Some(b'!') => 6,
                Some(b'?') => 4,
                _ => 2,
            },
            _ => return Ok(None),
        };
        Ok(Some(Annotation { id }))
    }

    fn parse_move(&mut self, c: u8) -> ParseResult<SanMove> {
        if !is_symbol(c) {
            return Err(self.error_msg(format!(
                "expected SAN move, found character '{}'",
                c as char
            )));
        }

        self.scratch.clear();
        self.scratch.push(c);
        self.take_while(is_symbol)?;
        str::from_utf8(&self.scratch)
            .unwrap()
            .parse::<SanMove>()
            .map_err(|err| self.error(ParseErrorKind::InvalidMove(err)))
    }

    #[inline]
    fn take_while(&mut self, f: impl Fn(u8) -> bool) -> ParseResult<()> {
        while let Some(c) = self.peek()? {
            if !f(c) {
                return Ok(());
            }
            self.scratch.push(c);
            self.next()?;
        }
        Ok(())
    }

    #[inline]
    fn skip_whitespace(&mut self) -> ParseResult<()> {
        while let Some(c) = self.peek()? {
            if !is_whitespace(c) {
                return Ok(());
            }
            self.next()?;
        }
        Ok(())
    }

    #[inline]
    fn next_if_eq(&mut self, c: u8) -> ParseResult<Option<u8>> {
        self.skip_whitespace()?;
        if self.peek()? == Some(c) {
            self.next()?;
            return Ok(Some(c));
        }
        Ok(None)
    }

    #[inline]
    fn next_if(&mut self, f: impl Fn(u8) -> bool) -> ParseResult<Option<u8>> {
        self.skip_whitespace()?;
        if let Some(c) = self.peek()?.filter(|&c| f(c)) {
            self.next()?;
            return Ok(Some(c));
        }
        Ok(None)
    }

    #[inline]
    fn next(&mut self) -> ParseResult<Option<u8>> {
        let next = if self.next.is_some() {
            self.next.take()
        } else {
            self.bytes.next().transpose().map_err(|err| {
                self.unrecoverable = true;
                self.error(ParseErrorKind::Io(err))
            })?
        };
        match next {
            Some(b'\n') => {
                self.line += 1;
                self.column = 1;
            }
            Some(_) => {
                self.column += 1;
            }
            _ => {}
        }
        Ok(next)
    }

    #[inline]
    fn peek(&mut self) -> ParseResult<Option<u8>> {
        if self.next.is_none() {
            self.next = self
                .bytes
                .next()
                .transpose()
                .map_err(|err| self.error(ParseErrorKind::Io(err)))?;
        }
        Ok(self.next)
    }

    #[inline]
    fn error_msg(&self, msg: impl Into<String>) -> ParseError {
        self.error(ParseErrorKind::Message(msg.into()))
    }

    #[inline]
    fn error(&self, kind: ParseErrorKind) -> ParseError {
        ParseError {
            line: self.line,
            column: self.column,
            kind,
        }
    }
}

pub struct Continue(bool);

#[derive(Debug)]
struct VariationInfo {
    depth: u32,
    skip: Option<u32>,
}

impl VariationInfo {
    #[inline]
    fn enter(&mut self) {
        self.depth += 1;
    }

    #[inline]
    fn leave(&mut self) {
        if self.skip == Some(self.depth) {
            self.skip = None;
        }
        self.depth -= 1;
    }

    #[inline]
    fn enter_skip(&mut self) {
        self.depth += 1;
        self.skip = Some(self.depth);
    }

    #[inline]
    fn should_visit(&self) -> bool {
        self.skip.is_none()
    }
}

#[inline]
fn is_whitespace(c: u8) -> bool {
    matches!(c, b' ' | b'\t' | b'\r' | b'\n')
}

#[inline]
fn is_symbol(c: u8) -> bool {
    c.is_ascii_alphanumeric() || matches!(c, b'+' | b'#' | b'=' | b'_' | b'-' | b':')
}

impl<E> From<ParseError> for Error<E> {
    #[inline]
    fn from(err: ParseError) -> Self {
        Self::Parse(err)
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        pgn::{self, ControlFlow, Reader, Visitor},
        Outcome, Position, SanMove, Color
    };

    #[test]
    fn pgn_game() {
        let freestyle_magnus_vs_javokhir = r#"
[Event "Freestyle Chess Grand Slam Weissenhaus"]
[Site "Chess.com"]
[Date "2025.02.12"]
[Round "03-01"]
[White "Sindarov, Javokhir"]
[Black "Carlsen, Magnus"]
[Result "*"]
[Variant "Chess960"]
[SetUp "1"]
[FEN "bqrkrbnn/pppppppp/8/8/8/8/PPPPPPPP/BQRKRBNN w ECec - 0 1"]
[WhiteElo "2700"]
[BlackElo "2833"]
[TimeControl "5400+30"]
[Link "https://www.chess.com/news/view/2025-freestyle-chess-grand-slam-weissenhaus-day-7"]

1. e4 e5 2. Nf3 Nf6 3. a4 c6 4. b4 Qc7 5. Qb3 Ng6 $2 {Carlsen sacrificed the pawn
on f7, a \"total bluff,\" and Sindarov didn't grab it.} 6. Rb1? {A non-commital
move, a little too sophisticated-looking to be good.} (6. Qxf7 {is the best move
and Black wouldn't objectively have much compensation for it.}) 6... d5 (6...
c5 $1 {is the best, with an advantage, but the move in the game is enough for
equality.}) 7. Ng3 O-O-O 8. Bd3 dxe4 9. Nxe4 Nf4 10. Nxf6 gxf6 11. Bf5+ Kb8 12.
g3 Nd5 13. O-O Nxb4 14. d4? {This is where Carlsen takes over, from a game that
was otherwise close before this.} (14. c3 $1 {is best, as ugly-looking as it is at
first.} 14... Nd3 15. c4 e4 16. Ne1 {with equal chances according to the engine,
although after} 16... Bc5 {with ...Nxf2 threatened next, Black is going after
the white king.}) (14. d3 {is also playable.}) 14... c5 $1 15. dxe5 b6 {The bishop
on a8, which was once an \"ugly duckling,\" will become the best minor piece on
the board.} 16. Rfd1 Qc6 17. exf6 {White is lost, but he has a big threat of
Be5+ with checkmating ideas. Black has only one move to still be winning.} 17...
Bb7 $1 {A move that looks so simple yet is not so easy to find. Black simply frees
up the a8-square for the king, so no none of these Be5+ attacks work.} (17...
Qxf3 $4 {loses and perfectly illustrates White's idea.} 18. Be5+ $1 Bd6 (18... Rxe5
19. Rxd8+) 19. Bxd6+ Rxd6 20. Qxf3 Bxf3 21. Rxd6 {White has a winning material
advantage.}) (17... c4 {was another move brought up in the commentary, but here
White is even better after} 18. Rxd8+ Rxd8 19. Be5+ Bd6 20. Qe3 Nxc2 (20...
Qxf3 $4 21. Bxd6+ Rxd6 22. Qe8+ {is mate.}) 21. Bxd6+ Rxd6 22. Qe7 $1 {and it's the
black king that's less safe.}) 18. Bg4 c4 19. Qc3 a5 {The black king is safe and
the white king is forever unstable. Carlsen carries out the advantage without
any mistakes.} 20. Rxd8+ Rxd8 21. Qe5+ Ka7 22. Qf5 Bc5 23. Bc3 Nxc2 $1
{Dismantling the house of cards.} 24. Rc1 (24. Qxc2 Rd3 {And there's no
defending both the knight and c3-bishop.} 25. Qe2 Rxc3) 24... Ne3 $1 {As pretty as
it is effective.} 25. fxe3 Bxe3+ 26. Kg2 Bxc1 27. Kh3 Rd3 28. Bd4 Qd5 {Black is
already up material and with the safer king. White resigns.} 0-1
        "#;

        struct Game {
            tag_count: u32,
            comments: u32,
            position: Position,
            outcome: Option<Outcome>,
            variation: u32,
        }

        impl Visitor for Game {
            type Error = anyhow::Error;

            fn visit_tag_pair(&mut self, name: &str, value: &str) -> anyhow::Result<()> {
                self.tag_count += 1;
                if name == "FEN" {
                    self.position = Position::from_fen(value).unwrap();
                }
                Ok(())
            }

            fn visit_comment(&mut self, _comment: &[u8]) -> anyhow::Result<()> {
                self.comments += 1;
                Ok(())
            }

            fn visit_move(&mut self, _number: Option<u32>, mv: SanMove) -> anyhow::Result<()> {
                if self.variation == 0 {
                    self.position.play(&mv)?;
                }
                Ok(())
            }

            fn visit_outcome(&mut self, outcome: Option<Outcome>) -> anyhow::Result<()> {
                self.outcome = outcome;
                Ok(())
            }

            fn enter_variation(&mut self) -> ControlFlow {
                self.variation += 1;
                ControlFlow::Continue
            }

            fn leave_variation(&mut self) {
                self.variation -= 1;
            }
        }

        let mut reader = Reader::from_string(freestyle_magnus_vs_javokhir);
        let mut game = Game {
            tag_count: 0,
            comments: 0,
            position: Position::new_initial(),
            outcome: None,
            variation: 0,
        };
        reader.visit_game(&mut game).unwrap();

        assert_eq!(game.tag_count, 14);
        assert_eq!(game.comments, 22);
        assert_eq!(
            game.position.fen().to_string(),
            "8/kb3p1p/1p3P2/p2q1Q2/P1pB2B1/3r1NPK/7P/2b5 w - - 4 29"
        );
        assert_eq!(game.outcome, Some(Outcome::Winner(Color::Black)));
        assert_eq!(game.variation, 0);
    }

    #[test]
    fn pgn_error_recovery() {
        let pgn1 = r#"
[Event "ch-UZB 1st League 2014"]
[Site "Tashkent UZB"]
[Date "2014.03.01"]
[Round "1.5"]
[White "Abdusattorov,Nodirbek"]
[Black "Alikulov,Elbek"]
[Result "1-0"]
[WhiteElo "2024"]
[BlackElo "2212"]
[ECO "B28"]

1.e4 c5 2.Nf3 a6 3.d3 g6 4.g3 Bg7 5.Bg2 b5 6.O-O Bb7 7.c3 e5 8.a3 Ne7 9.b4 d6
10.Nbd2 O-O 11.Nb3 Nd7 12.Be3 Rc8 13.Rc1 h6 14.Nfd2 f5 15.f4 Kh7 16.Qe2 cxb4
17.axb4 exf4 18.Bxf4 Rxc3 19.Rxc3 Bxc3 20.Bxd6 Qb6+ 21.Bc5 Nxc5 22.bxc5 Qe6
23.d4 Rd8 24.something sneaky Bxd2 25.Nxd2 fxe4 26.Nxe4 Nf5 27.d5 Qe5 28.g4 Ne7 29.Rf7+ Kg8
30.Qf1 Nxd5 31.Rxb7 Qd4+ 32.Kh1 Rf8 33.Qg1 Ne3 34.Re7 a5 35.c6 a4 36.Qxe3 Qxe3
37.Nf6+ Rxf6 38.Rxe3 Rd6 39.h4 Rd1+ 40.Kh2 b4 41.c7  1-0

[Event "Wch U16"]
[Site "Wattignies"]
[Date "1976.08.27" wow]
[Round "?"]
[White "Chandler, Murray G"]
[Black "Kasparov, Gary"
[Result "1-0"]
[WhiteElo "" test test]
[BlackElo ""]
[ECO "B22"]

1.e4 c5 2.c3 Nf6 3.e5 Nd5 4.d4 Nc6 5.Nf3 cxd4 6.cxd4 e6 7.a3 d6 8.Bd3 Qa5+
9.Bd2 Qb6 10.Nc3 Nxc3 11.Bxc3 dxe5 12.dxe5 Be7 13.O-O Bd7 14.Nd2 Qc7 15.Qg4 O-O-O
16.Rfc1 Kb8 17.Qc4 Rc8 18.b4 f6 19.Nf3 Qb6 20.Qe4 f5 21.Qe1 a6 22.Rab1 g5
23.Nd2 Nd4 24.Qe3 Rxc3 25.Rxc3 f4 26.Qe1 g4 27.Ne4 Bc6 28.Nc5 Ka7 29.a4 Bf3
30.a5 Qd8 31.Bc4 Bxc5 32.bxc5 Qh4 33.gxf3 gxf3 34.Kh1 Rg8 35.Qe4 Rg7 36.Qxd4 Qg5
37.c6+ Kb8 38.c7+ Rxc7 39.Rg1 Qh5 40.Rg8+ Rc8 41.Qd6+ Ka7 1-0

[Event "ch-UZB 1st League 2014"]
[Site "Tashkent UZB"]
[Date "2014.03.02"]
[Round "2.3"]
[White "Jumanov,E"]
[Black "Abdusattorov,Nodirbek"]
[Result "1-0"]
[WhiteElo "2090"]
[BlackElo "2024"]
[ECO "A15"]

1.Nf3 Nf6 2.c4 g6 3.Nc3 d5 4.cxd5 Nxd5 5.g3 Bg7 6.Nxd5 Qxd5 7.Bg2 O-O 8.O-O Nc6
9.d3 Qd8 10.a3 e5 11.Bg5 Qd6 12.Qc2 Bg4 13.Be3 Rfe8 14.Rac1 Rac8 15.Rfe1 Ne7
16.Ng5 Nd5 17.Qb3 c6 18.Bxa7 Qe7 19.h4 h6 20.Bc5 Qd7 21.Ne4 b6 22.Bb4 Be6
23.Qa4 Red8 24.Bd2 f5 25.Nc3 Ne7 26.Red1 Kh7 27.Be3 Rb8 28.b4 Ra8 29.Qc2 Rxa3
30.Bxb6 Rb8 31.Bc5 Nd5 32.Nxd5 cxd5 33.Ra1 Raa8 34.Rxa8 Rxa8 35.d4 f4 36.dxe5 fxg3
37.fxg3 Bxe5 38.h5 Qf7 39.hxg6+ Qxg6 40.Qxg6+ Kxg6 41.Bxd5 Bxd5 42.Rxd5 Bxg3
43.Kg2 Bf4 44.b5 Ra2 45.Kf3 Bh2 46.b6 Rb2 47.Be3 Rb4 48.Rd8 Rb5 49.Rh8 h5
50.Kg2 Be5 51.Rh6+ Kf5 52.Rxh5+ Ke4 53.Bf2 Rb2 54.Rh4+ Kd5 55.e4+ Kc4 56.Kf3 Rb3+
57.Kg4 Rb5 58.Rh6 Bc3 59.Rc6+ Kd3 60.Kf3 Rb1 61.Rd6+ Kc4 62.Rd7 Kb5 63.Rc7 Ba5
64.Kg4 Rb2 65.Bd4 Rb4 66.Rd7 Kc6 67.Rd8 Bxb6 68.Bxb6 Rxe4+ 69.Kf3 Re7 70.Be3 Kc7
71.Ra8 Re6 72.Ke2 Kc6 73.Kd3 Rd6+ 74.Kc4 Rd1 75.Ra6+ Kb7 76.Rf6 Re1 77.Bc5 Rc1+
78.Kb5 Rb1+ 79.Bb4 Rc1 80.Bc5 Rb1+ 81.Kc4 Rh1 82.Rb6+ Kc7 83.Ra6 Kb7 84.Re6 Rd1
85.Kb5 Rb1+ 86.Bb4 Rc1 87.Ra6 Rc2 88.Ra1 Rh2 89.Bc5 Rb2+ 90.Kc4 Rh2 91.Ra7+ Kc6
92.Ra6+ Kb7 93.Rb6+ Kc7 94.Rg6 Rd2 95.Bd4 Kb7 96.Kd5 Rc2 97.Rb6+ Kc7 98.Ra6 Kd7
99.Rg6 Re2 100.Rd6+ Ke7 101.Rg6 Kd7 102.Rh6 Rc2 103.Rd6+ Kc7 104.Rg6 Kb7
105.Bc5 Rd2+ 106.Ke5 Rd1 107.Rg7+ Ka6 108.Bd4 Rc1 109.Kd5 Rc8 110.Ra7+ Kb5
111.Rb7+ Ka6 112.Rb6+ Ka5 113.Bc5 Rd8+ 114.Kc4 Rd1 115.Rb2 Rc1+ 116.Kd5 Ka4
117.Rb4+ Ka5 118.Kc6 Rc3 119.Rb8 Ka4 120.Rb7 Rc2 121.Kd5 Rd2+ 122.Bd4 Rc2
123.Ke4 Ka5 124.Rb6 Rc4 125.Rb2 Rc1 126.Kd5 Ka4 127.Bc5 Ka5 128.Kc6 Rc4 129.Rb3 Rg4
130.Ra3+ Ra4 131.Rd3 Rg4 132.Kd5 Kb5 133.Rb3+ Ka4 134.Rb2 Rh4 135.Bd4 Ka3
136.Rb7 Ka2 137.Kc4 Rg4 138.Rb2+ Ka3 139.Rb1 Rg2 140.Rb7  1-0
        "#;

        let pgn2 = r#"
[Event "Wch U16"]
[Site "Wattignies"]
[Date "1976.08.27"]
[Round "?"]
[White "Chandler, Murray G"]
[Black "Kasparov, Gary"]
[Result "1-"]
[WhiteElo ""]
[BlackElo ""]
[ECO "B22"]

1.e4 c5 2.c3 Nf6 3.e5 Nd5 4.d4 Nc6 5.Nf3 cxd4 6.cxd4 e6 7.a3 d6 8.Bd3 Qa5+
9.Bd2 Qb6 10.Nc3 Nxc3 11.Bxc3 dxe5 12.dxe5 Be7 13.O-O Bd7 14.Nd2 Qc7 15.Qg4 O-O-O
16.Rfc1 Kb8 17.Qc4 Rc8 18.b4 f6 19.Nf3 Qb6 20.Qe4 f5 21.Qe1 a6 22.Rab1 g5
23.Nd2 Nd4 24.Qe3 Rxc3 25.Rxc3 f4 26.Qe1 g4 27.Ne4 Bc6 28.Nc5 Ka7 29.a4 Bf3
30.a5 Qd8 31.Bc4 Bxc5 32.bxc5 Qh4 33.gxf3 gxf3 34.Kh1 Rg8 35.Qe4 Rg7 36.Qxd4 Qg5
37.c6+ Kb8 38.c7+ Rxc7 39.Rg1 Qh5 40.Rg8+ Rc8 41.Qd6+ Ka7 1-0

[Event "Wch U16"]
[Site "Wattignies"]
[Date "1976.??.??"]
[Round "?"]
[White "Kasparov, Gary"]
[Black "Galle, Andre"]
[Result "1-0"]
[WhiteElo ""]
[BlackElo ""]
[ECO "C11"]

1.e4 e6 2.d4 d5 3.Nd2 Nf6 4.e5 Ne4 5.Nxe4 dxe4 6.Be3 b6 7.Ne2 Bb7 8.Ng3 c5
9.dxc5 Qxd1+ 10.Rxd1 Bxc5 11.Bxc5 bxc5 12.Bb5+ Ke7 13.O-O Bc6 14.Bxc6 Nxc6
15.Nxe4 Nxe5 16.Nxc5 Rac8 17.b4 Rhd8 18.f3 a5 19.c3 axb4 20.cxb4 Nc6 21.Rxd8 Rxd8
22.b5 Nb4 23.Rb1 Nd5 24.b6 Rb8 25.b7 Nc7 26.Rb6 Kd8 27.Kf2 Nd5 28.Rd6+ Kc7
29.Rxd5 exd5 30.Na6+ Kxb7 31.Nxb8 Kxb8 32.Ke3 Kc7 33.Kd4 Kc6 34.a4 Kb6 35.Kxd5 Ka5
36.Kd6 Kxa4 37.Ke7 f5 38.Kf7 1-0
        "#;

        struct Game {
            position: Position,
        }

        impl Visitor for Game {
            type Error = anyhow::Error;

            fn prepare(&mut self) {
                self.position = Position::new_initial();
            }

            fn visit_tag_pair(&mut self, name: &str, value: &str) -> anyhow::Result<()> {
                match (name, value) {
                    ("Result", "1-0" | "0-1" | "1/2-1/2") => Ok(()),
                    ("Result", _) => Err(anyhow::Error::msg("invalid result.")),
                    _ => Ok(()),
                }
            }

            fn visit_move(&mut self, _number: Option<u32>, mv: SanMove) -> anyhow::Result<()> {
                self.position.play(&mv)?;
                Ok(())
            }

            fn enter_variation(&mut self) -> ControlFlow {
                ControlFlow::Skip
            }
        }

        let mut reader = Reader::from_string(&pgn1);
        let mut game = Game {
            position: Position::new_initial(),
        };

        assert!(reader.visit_game(&mut game).is_err());
        assert!(reader.visit_game(&mut game).is_err());
        assert!(reader.visit_game(&mut game).is_ok());

        let mut reader = Reader::from_string(&pgn2);
        let mut game = Game {
            position: Position::new_initial(),
        };

        assert!(matches!(
            reader.visit_game(&mut game),
            Err(pgn::Error::Visitor(_))
        ));

        reader.visit_game(&mut game).unwrap();
        assert_eq!(
            game.position.fen().to_string(),
            "8/5Kpp/8/5p2/k7/5P2/6PP/8 b - - 1 38"
        );
    }
}
