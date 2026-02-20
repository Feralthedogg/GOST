// Purpose: Tokenize source text into lexical tokens with span metadata.
// Inputs/Outputs: Consumes UTF-8 source string and emits ordered token stream.
// Invariants: Token boundaries and keyword classification must match parser grammar assumptions.
// Gotchas: Numeric/range/operator edge cases can silently shift parse trees if changed.

use super::ast::Span;
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    IntLit(String),
    FloatLit(String),
    StringLit(String),
    CharLit(char),
    LexError(LexErrorKind),
    Unknown(char),
    Keyword(Keyword),
    Symbol(Symbol),
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexErrorKind {
    InvalidIntegerLiteral(String),
    InvalidFloatLiteral(String),
    UnterminatedStringLiteral,
    UnterminatedCharLiteral,
    EmptyCharLiteral,
    CharLiteralTooLong,
    InvalidEscapeSequence(char),
    UnterminatedBlockComment,
}

impl LexErrorKind {
    // Postcondition: Returns a user-facing diagnostic message for this lexing failure.
    pub fn message(&self) -> String {
        match self {
            Self::InvalidIntegerLiteral(detail) => {
                format!("invalid integer literal: {detail}")
            }
            Self::InvalidFloatLiteral(detail) => {
                format!("invalid float literal: {detail}")
            }
            Self::UnterminatedStringLiteral => "unterminated string literal".to_string(),
            Self::UnterminatedCharLiteral => "unterminated character literal".to_string(),
            Self::EmptyCharLiteral => {
                "character literal must contain exactly one character".to_string()
            }
            Self::CharLiteralTooLong => {
                "character literal must contain exactly one character".to_string()
            }
            Self::InvalidEscapeSequence(ch) => {
                format!("invalid escape sequence `\\{ch}`")
            }
            Self::UnterminatedBlockComment => "unterminated block comment".to_string(),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Keyword {
    Module,
    Import,
    Pub,
    Private,
    Const,
    Type,
    Extern,
    Unsafe,
    Fn,
    Struct,
    Enum,
    Copy,
    Let,
    If,
    Else,
    Match,
    While,
    Loop,
    For,
    In,
    Mut,
    Return,
    Break,
    Continue,
    Select,
    Case,
    Default,
    Go,
    Defer,
    Send,
    Recv,
    Close,
    After,
    True,
    False,
    Nil,
    Interface,
    Trait,
    Impl,
    As,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Symbol {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Comma,
    Semi,
    Colon,
    Dot,
    DotDot,
    DotDotEq,
    Arrow,
    FatArrow,
    Pipe,
    PipeGt,
    Amp,
    Star,
    Plus,
    Minus,
    Slash,
    Percent,
    Caret,
    Tilde,
    Bang,
    Eq,
    PlusEq,
    MinusEq,
    StarEq,
    SlashEq,
    PercentEq,
    AmpEq,
    PipeEq,
    CaretEq,
    Shl,
    Shr,
    ShlEq,
    ShrEq,
    EqEq,
    NotEq,
    Lt,
    Lte,
    Gt,
    Gte,
    AndAnd,
    OrOr,
    Question,
}

#[derive(Clone, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

pub struct Lexer<'a> {
    src: &'a str,
    idx: usize,
    line: usize,
    col: usize,
    prev_can_insert_semi: bool,
    pending_semi: bool,
}

impl<'a> Lexer<'a> {
    // Precondition: `src` is valid UTF-8 source text.
    // Postcondition: Lexer state is reset to the first source location.
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            idx: 0,
            line: 1,
            col: 1,
            prev_can_insert_semi: false,
            pending_semi: false,
        }
    }

    // Postcondition: Returned stream always ends with exactly one EOF token.
    pub fn lex_all(mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        loop {
            let tok = self.next_token();
            let is_eof = matches!(tok.kind, TokenKind::Eof);
            tokens.push(tok);
            if is_eof {
                break;
            }
        }
        tokens
    }

    fn next_token(&mut self) -> Token {
        if self.pending_semi {
            self.pending_semi = false;
            return self.inserted_semi_token();
        }

        if let Some(tok) = self.skip_whitespace_and_comments() {
            return tok;
        }

        if self.pending_semi {
            self.pending_semi = false;
            return self.inserted_semi_token();
        }

        let start = self.idx;
        let (line, column) = (self.line, self.col);
        if self.idx >= self.src.len() {
            return Token {
                kind: TokenKind::Eof,
                span: Span {
                    start,
                    end: start,
                    line,
                    column,
                },
            };
        }

        let ch = self.peek_char();
        if is_ident_start(ch) {
            let ident = self.read_while(is_ident_continue);
            let kind = match ident.as_str() {
                "module" => TokenKind::Keyword(Keyword::Module),
                "import" => TokenKind::Keyword(Keyword::Import),
                "pub" => TokenKind::Keyword(Keyword::Pub),
                "private" => TokenKind::Keyword(Keyword::Private),
                "priv" => TokenKind::Keyword(Keyword::Private),
                "const" => TokenKind::Keyword(Keyword::Const),
                "type" => TokenKind::Keyword(Keyword::Type),
                "extern" => TokenKind::Keyword(Keyword::Extern),
                "unsafe" => TokenKind::Keyword(Keyword::Unsafe),
                "fn" => TokenKind::Keyword(Keyword::Fn),
                "struct" => TokenKind::Keyword(Keyword::Struct),
                "enum" => TokenKind::Keyword(Keyword::Enum),
                "copy" => TokenKind::Keyword(Keyword::Copy),
                "let" => TokenKind::Keyword(Keyword::Let),
                "if" => TokenKind::Keyword(Keyword::If),
                "else" => TokenKind::Keyword(Keyword::Else),
                "match" => TokenKind::Keyword(Keyword::Match),
                "while" => TokenKind::Keyword(Keyword::While),
                "loop" => TokenKind::Keyword(Keyword::Loop),
                "for" => TokenKind::Keyword(Keyword::For),
                "in" => TokenKind::Keyword(Keyword::In),
                "mut" => TokenKind::Keyword(Keyword::Mut),
                "return" => TokenKind::Keyword(Keyword::Return),
                "break" => TokenKind::Keyword(Keyword::Break),
                "continue" => TokenKind::Keyword(Keyword::Continue),
                "select" => TokenKind::Keyword(Keyword::Select),
                "case" => TokenKind::Keyword(Keyword::Case),
                "default" => TokenKind::Keyword(Keyword::Default),
                "go" => TokenKind::Keyword(Keyword::Go),
                "defer" => TokenKind::Keyword(Keyword::Defer),
                "send" => TokenKind::Keyword(Keyword::Send),
                "recv" => TokenKind::Keyword(Keyword::Recv),
                "close" => TokenKind::Keyword(Keyword::Close),
                "after" => TokenKind::Keyword(Keyword::After),
                "true" => TokenKind::Keyword(Keyword::True),
                "false" => TokenKind::Keyword(Keyword::False),
                "nil" => TokenKind::Keyword(Keyword::Nil),
                "interface" => TokenKind::Keyword(Keyword::Interface),
                "trait" => TokenKind::Keyword(Keyword::Trait),
                "impl" => TokenKind::Keyword(Keyword::Impl),
                "as" => TokenKind::Keyword(Keyword::As),
                _ => TokenKind::Ident(ident),
            };
            return self.make_token(start, line, column, kind);
        }

        if ch.is_ascii_digit() {
            let kind = match self.read_number() {
                Ok(number) => {
                    if number.contains('.') || number.contains('e') || number.contains('E') {
                        TokenKind::FloatLit(number)
                    } else {
                        TokenKind::IntLit(number)
                    }
                }
                Err(err) => TokenKind::LexError(err),
            };
            return self.make_token(start, line, column, kind);
        }

        let kind = match ch {
            '"' => match self.read_string() {
                Ok(s) => TokenKind::StringLit(s),
                Err(err) => TokenKind::LexError(err),
            },
            '\'' => match self.read_char_lit() {
                Ok(c) => TokenKind::CharLit(c),
                Err(err) => TokenKind::LexError(err),
            },
            '(' => {
                self.advance();
                TokenKind::Symbol(Symbol::LParen)
            }
            ')' => {
                self.advance();
                TokenKind::Symbol(Symbol::RParen)
            }
            '{' => {
                self.advance();
                TokenKind::Symbol(Symbol::LBrace)
            }
            '}' => {
                self.advance();
                TokenKind::Symbol(Symbol::RBrace)
            }
            '[' => {
                self.advance();
                TokenKind::Symbol(Symbol::LBracket)
            }
            ']' => {
                self.advance();
                TokenKind::Symbol(Symbol::RBracket)
            }
            ',' => {
                self.advance();
                TokenKind::Symbol(Symbol::Comma)
            }
            ';' => {
                self.advance();
                TokenKind::Symbol(Symbol::Semi)
            }
            ':' => {
                self.advance();
                TokenKind::Symbol(Symbol::Colon)
            }
            '.' => {
                self.advance();
                if self.peek_char() == '.' {
                    self.advance();
                    if self.peek_char() == '=' {
                        self.advance();
                        TokenKind::Symbol(Symbol::DotDotEq)
                    } else {
                        TokenKind::Symbol(Symbol::DotDot)
                    }
                } else {
                    TokenKind::Symbol(Symbol::Dot)
                }
            }
            '|' => {
                self.advance();
                if self.peek_char() == '>' {
                    self.advance();
                    TokenKind::Symbol(Symbol::PipeGt)
                } else if self.peek_char() == '|' {
                    self.advance();
                    TokenKind::Symbol(Symbol::OrOr)
                } else if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::PipeEq)
                } else {
                    TokenKind::Symbol(Symbol::Pipe)
                }
            }
            '&' => {
                self.advance();
                if self.peek_char() == '&' {
                    self.advance();
                    TokenKind::Symbol(Symbol::AndAnd)
                } else if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::AmpEq)
                } else {
                    TokenKind::Symbol(Symbol::Amp)
                }
            }
            '*' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::StarEq)
                } else {
                    TokenKind::Symbol(Symbol::Star)
                }
            }
            '+' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::PlusEq)
                } else {
                    TokenKind::Symbol(Symbol::Plus)
                }
            }
            '-' => {
                self.advance();
                if self.peek_char() == '>' {
                    self.advance();
                    TokenKind::Symbol(Symbol::Arrow)
                } else if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::MinusEq)
                } else {
                    TokenKind::Symbol(Symbol::Minus)
                }
            }
            '/' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::SlashEq)
                } else {
                    TokenKind::Symbol(Symbol::Slash)
                }
            }
            '%' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::PercentEq)
                } else {
                    TokenKind::Symbol(Symbol::Percent)
                }
            }
            '^' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::CaretEq)
                } else {
                    TokenKind::Symbol(Symbol::Caret)
                }
            }
            '~' => {
                self.advance();
                TokenKind::Symbol(Symbol::Tilde)
            }
            '!' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::NotEq)
                } else {
                    TokenKind::Symbol(Symbol::Bang)
                }
            }
            '=' => {
                self.advance();
                if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::EqEq)
                } else if self.peek_char() == '>' {
                    self.advance();
                    TokenKind::Symbol(Symbol::FatArrow)
                } else {
                    TokenKind::Symbol(Symbol::Eq)
                }
            }
            '<' => {
                self.advance();
                if self.peek_char() == '<' {
                    self.advance();
                    if self.peek_char() == '=' {
                        self.advance();
                        TokenKind::Symbol(Symbol::ShlEq)
                    } else {
                        TokenKind::Symbol(Symbol::Shl)
                    }
                } else if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::Lte)
                } else {
                    TokenKind::Symbol(Symbol::Lt)
                }
            }
            '>' => {
                self.advance();
                if self.peek_char() == '>' {
                    self.advance();
                    if self.peek_char() == '=' {
                        self.advance();
                        TokenKind::Symbol(Symbol::ShrEq)
                    } else {
                        TokenKind::Symbol(Symbol::Shr)
                    }
                } else if self.peek_char() == '=' {
                    self.advance();
                    TokenKind::Symbol(Symbol::Gte)
                } else {
                    TokenKind::Symbol(Symbol::Gt)
                }
            }
            '?' => {
                self.advance();
                TokenKind::Symbol(Symbol::Question)
            }
            _ => {
                self.advance();
                TokenKind::Unknown(ch)
            }
        };
        self.make_token(start, line, column, kind)
    }

    fn inserted_semi_token(&self) -> Token {
        Token {
            kind: TokenKind::Symbol(Symbol::Semi),
            span: Span {
                start: self.idx,
                end: self.idx,
                line: self.line,
                column: self.col,
            },
        }
    }

    fn make_token(&mut self, start: usize, line: usize, column: usize, kind: TokenKind) -> Token {
        self.prev_can_insert_semi = can_insert_semi_after(&kind);
        Token {
            kind,
            span: Span {
                start,
                end: self.idx,
                line,
                column,
            },
        }
    }

    fn skip_whitespace_and_comments(&mut self) -> Option<Token> {
        loop {
            if self.idx >= self.src.len() {
                return None;
            }
            let ch = self.peek_char();
            match ch {
                ' ' | '\t' | '\r' => {
                    self.advance();
                }
                '\n' => {
                    self.advance();
                    if self.prev_can_insert_semi {
                        self.prev_can_insert_semi = false;
                        self.pending_semi = true;
                        return None;
                    }
                }
                '/' if self.peek_next_char() == '/' => {
                    self.advance();
                    self.advance();
                    while self.idx < self.src.len() && self.peek_char() != '\n' {
                        self.advance();
                    }
                }
                '/' if self.peek_next_char() == '*' => {
                    let start = self.idx;
                    let (line, column) = (self.line, self.col);
                    match self.consume_block_comment() {
                        Ok(saw_newline) => {
                            if saw_newline && self.prev_can_insert_semi {
                                self.prev_can_insert_semi = false;
                                self.pending_semi = true;
                                return None;
                            }
                        }
                        Err(err) => {
                            return Some(self.make_token(
                                start,
                                line,
                                column,
                                TokenKind::LexError(err),
                            ));
                        }
                    }
                }
                _ => return None,
            }
        }
    }

    fn consume_block_comment(&mut self) -> Result<bool, LexErrorKind> {
        self.advance(); // '/'
        self.advance(); // '*'
        let mut depth = 1usize;
        let mut saw_newline = false;
        while self.idx < self.src.len() && depth > 0 {
            let ch = self.peek_char();
            if ch == '\n' {
                saw_newline = true;
                self.advance();
                continue;
            }
            if ch == '/' && self.peek_next_char() == '*' {
                self.advance();
                self.advance();
                depth += 1;
                continue;
            }
            if ch == '*' && self.peek_next_char() == '/' {
                self.advance();
                self.advance();
                depth -= 1;
                continue;
            }
            self.advance();
        }
        if depth == 0 {
            Ok(saw_newline)
        } else {
            Err(LexErrorKind::UnterminatedBlockComment)
        }
    }

    fn read_string(&mut self) -> Result<String, LexErrorKind> {
        self.advance(); // opening quote
        let mut s = String::new();
        while self.idx < self.src.len() {
            let ch = self.peek_char();
            if ch == '"' {
                self.advance();
                return Ok(s);
            }
            if ch == '\n' {
                self.advance();
                return Err(LexErrorKind::UnterminatedStringLiteral);
            }
            if ch == '\\' {
                self.advance();
                if self.idx >= self.src.len() {
                    return Err(LexErrorKind::UnterminatedStringLiteral);
                }
                let esc = self.peek_char();
                self.advance();
                let actual = match esc {
                    'n' => '\n',
                    'r' => '\r',
                    't' => '\t',
                    '\\' => '\\',
                    '"' => '"',
                    '\'' => '\'',
                    _ => return Err(LexErrorKind::InvalidEscapeSequence(esc)),
                };
                s.push(actual);
            } else {
                s.push(ch);
                self.advance();
            }
        }
        Err(LexErrorKind::UnterminatedStringLiteral)
    }

    fn read_char_lit(&mut self) -> Result<char, LexErrorKind> {
        self.advance(); // opening quote
        if self.idx >= self.src.len() {
            return Err(LexErrorKind::UnterminatedCharLiteral);
        }
        if self.peek_char() == '\n' {
            self.advance();
            return Err(LexErrorKind::UnterminatedCharLiteral);
        }
        if self.peek_char() == '\'' {
            self.advance();
            return Err(LexErrorKind::EmptyCharLiteral);
        }

        let ch = if self.peek_char() == '\\' {
            self.advance();
            if self.idx >= self.src.len() {
                return Err(LexErrorKind::UnterminatedCharLiteral);
            }
            let esc = self.peek_char();
            self.advance();
            match esc {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                _ => return Err(LexErrorKind::InvalidEscapeSequence(esc)),
            }
        } else {
            let c = self.peek_char();
            self.advance();
            c
        };

        if self.peek_char() != '\'' {
            let mut saw_closing_quote = false;
            while self.idx < self.src.len() {
                let c = self.peek_char();
                if c == '\'' {
                    self.advance();
                    saw_closing_quote = true;
                    break;
                }
                if c == '\n' {
                    break;
                }
                self.advance();
            }
            if saw_closing_quote {
                return Err(LexErrorKind::CharLiteralTooLong);
            }
            return Err(LexErrorKind::UnterminatedCharLiteral);
        }

        self.advance(); // closing quote
        Ok(ch)
    }

    fn read_number(&mut self) -> Result<String, LexErrorKind> {
        if self.peek_char() == '0' {
            let next = self.peek_next_char();
            if next == 'x' || next == 'X' {
                return self.read_prefixed_int(16, 'x');
            }
            if next == 'o' || next == 'O' {
                return self.read_prefixed_int(8, 'o');
            }
            if next == 'b' || next == 'B' {
                return self.read_prefixed_int(2, 'b');
            }
        }

        let mut s = String::new();
        self.read_decimal_digits(&mut s);

        if self.peek_char() == '.' {
            // Why: Keep range operators (`..`, `..=`) out of numeric literals.
            let next = self.peek_next_char();
            if next != '.' && next.is_ascii_digit() {
                s.push('.');
                self.advance();
                self.read_decimal_digits(&mut s);
            }
        }

        let exp = self.peek_char();
        if exp == 'e' || exp == 'E' {
            if !self.has_valid_exponent_tail() {
                // Why: `1e`, `1e+`, and similar forms should fail at lexing so users get
                // precise literal diagnostics instead of parser/type-check cascades.
                let suffix = self.consume_invalid_exponent_suffix();
                return Err(LexErrorKind::InvalidFloatLiteral(format!(
                    "expected at least one digit after exponent marker, found `{suffix}`"
                )));
            }
            s.push('e');
            self.advance();
            if self.peek_char() == '+' || self.peek_char() == '-' {
                s.push(self.peek_char());
                self.advance();
            }
            self.read_decimal_digits(&mut s);
        }

        Ok(s)
    }

    fn read_prefixed_int(&mut self, radix: u32, prefix: char) -> Result<String, LexErrorKind> {
        self.advance(); // 0
        self.advance(); // x/o/b
        let mut digits = String::new();
        let mut trailing_separator = false;
        while self.idx < self.src.len() {
            let ch = self.peek_char();
            if ch == '_' {
                trailing_separator = true;
                self.advance();
                continue;
            }
            if ch.is_digit(radix) {
                digits.push(ch);
                trailing_separator = false;
                self.advance();
                continue;
            }
            break;
        }
        if digits.is_empty() {
            return Err(LexErrorKind::InvalidIntegerLiteral(format!(
                "expected at least one digit after `0{prefix}`"
            )));
        }
        if trailing_separator {
            return Err(LexErrorKind::InvalidIntegerLiteral(format!(
                "trailing `_` is not allowed in `0{prefix}` literal"
            )));
        }
        if is_numeric_suffix_char(self.peek_char()) {
            // Why: Reject invalid radix digits/suffixes at lexing boundary to avoid later
            // mis-parsing as separate identifiers or decimal fragments.
            let suffix = self.read_while(is_numeric_suffix_char);
            return Err(LexErrorKind::InvalidIntegerLiteral(format!(
                "invalid digit or suffix `{suffix}` in `0{prefix}` literal"
            )));
        }
        u128::from_str_radix(&digits, radix).map_or_else(
            |_| {
                Err(LexErrorKind::InvalidIntegerLiteral(format!(
                    "value after `0{prefix}` is too large (max 128-bit)"
                )))
            },
            |v| Ok(v.to_string()),
        )
    }

    fn read_decimal_digits(&mut self, out: &mut String) -> usize {
        let mut count = 0usize;
        while self.idx < self.src.len() {
            let ch = self.peek_char();
            if ch == '_' {
                self.advance();
                continue;
            }
            if ch.is_ascii_digit() {
                out.push(ch);
                self.advance();
                count += 1;
                continue;
            }
            break;
        }
        count
    }

    fn has_valid_exponent_tail(&self) -> bool {
        let mut iter = self.src[self.idx..].chars();
        let _ = iter.next(); // e/E
        let Some(mut ch) = iter.next() else {
            return false;
        };
        if ch == '+' || ch == '-' {
            let Some(next) = iter.next() else {
                return false;
            };
            ch = next;
        }
        ch.is_ascii_digit()
    }

    fn consume_invalid_exponent_suffix(&mut self) -> String {
        let mut suffix = String::new();
        let marker = self.peek_char();
        suffix.push(marker);
        self.advance();
        if self.peek_char() == '+' || self.peek_char() == '-' {
            suffix.push(self.peek_char());
            self.advance();
        }
        suffix.push_str(&self.read_while(is_numeric_suffix_char));
        suffix
    }

    fn read_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        while self.idx < self.src.len() {
            let ch = self.peek_char();
            if !f(ch) {
                break;
            }
            s.push(ch);
            self.advance();
        }
        s
    }

    fn advance(&mut self) {
        if self.idx >= self.src.len() {
            return;
        }
        let ch = self.peek_char();
        self.idx += ch.len_utf8();
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn peek_char(&self) -> char {
        self.src[self.idx..].chars().next().unwrap_or('\0')
    }

    fn peek_next_char(&self) -> char {
        let mut it = self.src[self.idx..].chars();
        let _ = it.next();
        it.next().unwrap_or('\0')
    }
}

fn is_ident_start(ch: char) -> bool {
    ch == '_' || is_xid_start(ch)
}

fn is_ident_continue(ch: char) -> bool {
    ch == '_' || is_xid_continue(ch)
}

fn is_numeric_suffix_char(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn can_insert_semi_after(kind: &TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Ident(_)
            | TokenKind::IntLit(_)
            | TokenKind::FloatLit(_)
            | TokenKind::StringLit(_)
            | TokenKind::CharLit(_)
            | TokenKind::Keyword(Keyword::Return)
            | TokenKind::Keyword(Keyword::Break)
            | TokenKind::Keyword(Keyword::Continue)
            | TokenKind::Keyword(Keyword::True)
            | TokenKind::Keyword(Keyword::False)
            | TokenKind::Keyword(Keyword::Nil)
            | TokenKind::Symbol(Symbol::RParen)
            | TokenKind::Symbol(Symbol::RBracket)
            | TokenKind::Symbol(Symbol::RBrace)
    )
}
