use super::ast::Span;

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: Option<Span>,
}

impl Diagnostic {
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
        }
    }
}

#[derive(Default)]
pub struct Diagnostics {
    pub items: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn push(&mut self, message: impl Into<String>, span: Option<Span>) {
        self.items.push(Diagnostic::new(message, span));
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

pub fn format_diagnostic(diag: &Diagnostic, source: &str) -> String {
    if let Some(span) = &diag.span {
        let line = span.line;
        let col = span.column;
        let line_text = source
            .lines()
            .nth(line.saturating_sub(1))
            .unwrap_or("");
        format!(
            "error:{}:{}: {}\n  {}\n  {}^",
            line,
            col,
            diag.message,
            line_text,
            " ".repeat(col.saturating_sub(1))
        )
    } else {
        format!("error: {}", diag.message)
    }
}
