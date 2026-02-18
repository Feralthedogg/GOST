use super::ast::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Ident(String),
    IntLit(String),
    FloatLit(String),
    StringLit(String),
    CharLit(char),
    Unknown(char),
    Keyword(Keyword),
    Symbol(Symbol),
    Eof,
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
    bytes: &'a [u8],
    idx: usize,
    line: usize,
    col: usize,
    prev_can_insert_semi: bool,
    pending_semi: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            bytes: src.as_bytes(),
            idx: 0,
            line: 1,
            col: 1,
            prev_can_insert_semi: false,
            pending_semi: false,
        }
    }

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
            return Token {
                kind: TokenKind::Symbol(Symbol::Semi),
                span: Span {
                    start: self.idx,
                    end: self.idx,
                    line: self.line,
                    column: self.col,
                },
            };
        }
        self.skip_whitespace_and_comments();
        if self.pending_semi {
            self.pending_semi = false;
            return Token {
                kind: TokenKind::Symbol(Symbol::Semi),
                span: Span {
                    start: self.idx,
                    end: self.idx,
                    line: self.line,
                    column: self.col,
                },
            };
        }
        let start = self.idx;
        let (line, column) = (self.line, self.col);
        if self.idx >= self.bytes.len() {
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
            let end = self.idx;
            self.prev_can_insert_semi = can_insert_semi_after(&kind);
            return Token {
                kind,
                span: Span {
                    start,
                    end,
                    line,
                    column,
                },
            };
        }
        if ch.is_ascii_digit() {
            let number = self.read_number();
            let kind = if number.contains('.') || number.contains('e') || number.contains('E') {
                TokenKind::FloatLit(number)
            } else {
                TokenKind::IntLit(number)
            };
            let end = self.idx;
            self.prev_can_insert_semi = can_insert_semi_after(&kind);
            return Token {
                kind,
                span: Span {
                    start,
                    end,
                    line,
                    column,
                },
            };
        }
        let kind = match ch {
            '"' => {
                let s = self.read_string();
                TokenKind::StringLit(s)
            }
            '\'' => {
                let c = self.read_char_lit();
                TokenKind::CharLit(c)
            }
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
        let end = self.idx;
        self.prev_can_insert_semi = can_insert_semi_after(&kind);
        Token {
            kind,
            span: Span {
                start,
                end,
                line,
                column,
            },
        }
    }

    fn skip_whitespace_and_comments(&mut self) {
        loop {
            if self.idx >= self.bytes.len() {
                return;
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
                        return;
                    }
                }
                '/' if self.peek_next_char() == '/' => {
                    self.advance();
                    self.advance();
                    while self.idx < self.bytes.len() && self.peek_char() != '\n' {
                        self.advance();
                    }
                }
                '/' if self.peek_next_char() == '*' => {
                    let saw_newline = self.consume_block_comment();
                    if saw_newline && self.prev_can_insert_semi {
                        self.prev_can_insert_semi = false;
                        self.pending_semi = true;
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    fn consume_block_comment(&mut self) -> bool {
        self.advance(); // '/'
        self.advance(); // '*'
        let mut depth = 1usize;
        let mut saw_newline = false;
        while self.idx < self.bytes.len() && depth > 0 {
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
        saw_newline
    }

    fn read_string(&mut self) -> String {
        self.advance(); // opening quote
        let mut s = String::new();
        while self.idx < self.bytes.len() {
            let ch = self.peek_char();
            if ch == '"' {
                self.advance();
                break;
            }
            if ch == '\\' {
                self.advance();
                if self.idx >= self.bytes.len() {
                    break;
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
                    _ => esc,
                };
                s.push(actual);
            } else {
                s.push(ch);
                self.advance();
            }
        }
        s
    }

    fn read_char_lit(&mut self) -> char {
        self.advance();
        let ch = if self.peek_char() == '\\' {
            self.advance();
            let esc = self.peek_char();
            self.advance();
            match esc {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                '\'' => '\'',
                _ => esc,
            }
        } else {
            let c = self.peek_char();
            self.advance();
            c
        };
        if self.peek_char() == '\'' {
            self.advance();
        }
        ch
    }

    fn read_number(&mut self) -> String {
        if self.peek_char() == '0' {
            let next = self.peek_next_char();
            if next == 'x' || next == 'X' {
                return self.read_prefixed_int(16);
            }
            if next == 'o' || next == 'O' {
                return self.read_prefixed_int(8);
            }
            if next == 'b' || next == 'B' {
                return self.read_prefixed_int(2);
            }
        }

        let mut s = String::new();
        self.read_decimal_digits(&mut s);

        if self.peek_char() == '.' {
            // Keep range operators (`..`, `..=`) out of numeric literals.
            let next = self.peek_next_char();
            if next != '.' && next.is_ascii_digit() {
                s.push('.');
                self.advance();
                self.read_decimal_digits(&mut s);
            }
        }

        let exp = self.peek_char();
        if (exp == 'e' || exp == 'E') && self.has_valid_exponent_tail() {
            s.push('e');
            self.advance();
            if self.peek_char() == '+' || self.peek_char() == '-' {
                s.push(self.peek_char());
                self.advance();
            }
            self.read_decimal_digits(&mut s);
        }

        s
    }

    fn read_prefixed_int(&mut self, radix: u32) -> String {
        self.advance(); // 0
        self.advance(); // x/o/b
        let mut digits = String::new();
        while self.idx < self.bytes.len() {
            let ch = self.peek_char();
            if ch == '_' {
                self.advance();
                continue;
            }
            if ch.is_digit(radix) {
                digits.push(ch);
                self.advance();
                continue;
            }
            break;
        }
        if digits.is_empty() {
            return "0".to_string();
        }
        u128::from_str_radix(&digits, radix)
            .map(|v| v.to_string())
            .unwrap_or_else(|_| "0".to_string())
    }

    fn read_decimal_digits(&mut self, out: &mut String) -> usize {
        let mut count = 0usize;
        while self.idx < self.bytes.len() {
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
        let mut i = self.idx + 1;
        if i >= self.bytes.len() {
            return false;
        }
        let mut ch = self.bytes[i] as char;
        if ch == '+' || ch == '-' {
            i += 1;
            if i >= self.bytes.len() {
                return false;
            }
            ch = self.bytes[i] as char;
        }
        if !ch.is_ascii_digit() {
            return false;
        }
        true
    }

    fn read_while<F>(&mut self, f: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut s = String::new();
        while self.idx < self.bytes.len() {
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
        if self.idx >= self.bytes.len() {
            return;
        }
        let ch = self.peek_char();
        self.idx += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
    }

    fn peek_char(&self) -> char {
        self.bytes.get(self.idx).copied().unwrap_or(b'\0') as char
    }

    fn peek_next_char(&self) -> char {
        self.bytes.get(self.idx + 1).copied().unwrap_or(b'\0') as char
    }
}

fn is_ident_start(ch: char) -> bool {
    ch.is_ascii_alphabetic() || ch == '_'
}

fn is_ident_continue(ch: char) -> bool {
    ch.is_ascii_alphanumeric() || ch == '_'
}

fn can_insert_semi_after(kind: &TokenKind) -> bool {
    match kind {
        TokenKind::Ident(_) => true,
        TokenKind::IntLit(_) => true,
        TokenKind::FloatLit(_) => true,
        TokenKind::StringLit(_) => true,
        TokenKind::CharLit(_) => true,
        TokenKind::Keyword(Keyword::Return)
        | TokenKind::Keyword(Keyword::Break)
        | TokenKind::Keyword(Keyword::Continue)
        | TokenKind::Keyword(Keyword::True)
        | TokenKind::Keyword(Keyword::False)
        | TokenKind::Keyword(Keyword::Nil) => true,
        TokenKind::Symbol(Symbol::RParen)
        | TokenKind::Symbol(Symbol::RBracket)
        | TokenKind::Symbol(Symbol::RBrace) => true,
        _ => false,
    }
}
