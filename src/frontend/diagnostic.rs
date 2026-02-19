// Purpose: Define diagnostic codes/messages and rendering helpers for user-facing errors.
// Inputs/Outputs: Converts spans and error metadata into stable, explainable diagnostics.
// Invariants: Diagnostic codes are externally referenced and should remain backward-compatible.
// Gotchas: Message wording changes can break tests and docs tied to exact phrasing.

use super::ast::Span;

pub const E_UNDEFINED_NAME: &str = "E1002";
pub const E_UNKNOWN_TYPE: &str = "E1003";
pub const E_UNKNOWN_FUNCTION: &str = "E1004";
pub const E_UNKNOWN_FIELD: &str = "E1101";
pub const E_UNKNOWN_METHOD: &str = "E1102";
pub const E_UNKNOWN_VARIANT: &str = "E1103";
pub const E1104: &str = "E1104"; // receiver not addressable
pub const E1105: &str = "E1105"; // need mutable receiver

#[derive(Clone, Debug)]
pub enum HelpMsg {
    Plain(String),
    Snippet { span: Span, template: String },
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub message: String,
    pub span: Option<Span>,
    pub code: Option<String>,
    pub label: Option<String>,
    pub notes: Vec<String>,
    pub helps: Vec<HelpMsg>,
}

impl Diagnostic {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn new(message: impl Into<String>, span: Option<Span>) -> Self {
        Self {
            message: message.into(),
            span,
            code: None,
            label: None,
            notes: Vec::new(),
            helps: Vec::new(),
        }
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn code(mut self, code: &str) -> Self {
        self.code = Some(code.to_string());
        self
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn label(mut self, span: Span, message: impl Into<String>) -> Self {
        self.span = Some(span);
        self.label = Some(message.into());
        self
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn help(mut self, message: impl Into<String>) -> Self {
        self.helps.push(HelpMsg::Plain(message.into()));
        self
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn with_help(self, message: impl Into<String>) -> Self {
        self.help(message)
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn with_help_snippet(mut self, span: Span, template: impl Into<String>) -> Self {
        self.helps.push(HelpMsg::Snippet {
            span,
            template: template.into(),
        });
        self
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn note(mut self, message: impl Into<String>) -> Self {
        self.notes.push(message.into());
        self
    }
}

#[derive(Default)]
pub struct Diagnostics {
    pub items: Vec<Diagnostic>,
}

impl Diagnostics {
    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn push(&mut self, message: impl Into<String>, span: Option<Span>) {
        self.items.push(Diagnostic::new(message, span));
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn push_diag(&mut self, diag: Diagnostic) {
        self.items.push(diag);
    }

    // Precondition: Inputs satisfy semantic and structural invariants expected by this API.
    // Postcondition: Returns a value/state transition that preserves module invariants.
    // Side effects: May read/write filesystem, caches, diagnostics, globals, or process state.
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

pub fn format_diagnostic(diag: &Diagnostic, source: &str, file_name: Option<&str>) -> String {
    if let Some(span) = &diag.span {
        let file_display = file_name.unwrap_or("<input>");
        let code = diag
            .code
            .as_ref()
            .map(|c| format!("[{}]", c))
            .unwrap_or_default();
        let mut out = String::new();
        let err = colorize("error", "1;31");
        let code_col = if code.is_empty() {
            "".to_string()
        } else {
            colorize(&code, "1;33")
        };

        let start = span.start.min(source.len());
        let end = span.end.min(source.len());
        let (start_line, start_col) = line_col_at(source, start);
        let (end_line, end_col) = line_col_at(source, end);

        let msg = diag.message.trim_start();
        if code.is_empty() {
            out.push_str(&format!(
                "{}:{}:{}: {}: {}\n",
                file_display, start_line, start_col, err, msg
            ));
        } else {
            out.push_str(&format!(
                "{}:{}:{}: {}{}: {}\n",
                file_display, start_line, start_col, err, code_col, msg
            ));
        }
        out.push_str(&format!(
            "  --> {}:{}:{}\n",
            file_display, start_line, start_col
        ));

        let line_w = source.lines().count().to_string().len().max(1);

        if start_line == end_line {
            let line_text = source
                .lines()
                .nth(start_line.saturating_sub(1))
                .unwrap_or("");
            out.push_str(&format!("{:>width$} |\n", "", width = line_w));
            out.push_str(&format!(
                "{:>width$} | {}\n",
                start_line,
                line_text,
                width = line_w
            ));
            let caret_len = (end.saturating_sub(start)).max(1);
            let caret = colorize(&"^".repeat(caret_len), "1;31");
            out.push_str(&format!(
                "{:>width$} | {}{}",
                "",
                " ".repeat(start_col.saturating_sub(1)),
                caret,
                width = line_w
            ));
            if let Some(label) = &diag.label {
                out.push(' ');
                out.push_str(label);
            }
            out.push('\n');
        } else {
            // multi-line span
            out.push_str(&format!("{:>width$} |\n", "", width = line_w));

            for line_no in start_line..=end_line {
                let line_text = source.lines().nth(line_no.saturating_sub(1)).unwrap_or("");
                out.push_str(&format!(
                    "{:>width$} | {}\n",
                    line_no,
                    line_text,
                    width = line_w
                ));

                if line_no == start_line {
                    let caret = colorize(
                        &"^".repeat(line_text.len().saturating_sub(start_col - 1).max(1)),
                        "1;31",
                    );
                    out.push_str(&format!(
                        "{:>width$} | {}{}",
                        "",
                        " ".repeat(start_col.saturating_sub(1)),
                        caret,
                        width = line_w
                    ));
                } else if line_no == end_line {
                    let caret = colorize(&"^".repeat(end_col.saturating_sub(1).max(1)), "1;31");
                    out.push_str(&format!(
                        "{:>width$} | {}{}",
                        "",
                        " ".repeat(0),
                        caret,
                        width = line_w
                    ));
                } else {
                    let wave = colorize(&"~".repeat(line_text.len().max(1)), "1;31");
                    out.push_str(&format!("{:>width$} | {}", "", wave, width = line_w));
                }
                if let Some(label) = &diag.label
                    && line_no == start_line
                {
                    out.push(' ');
                    out.push_str(label);
                }
                out.push('\n');
            }
        }

        for h in diag.helps.iter().take(3) {
            out.push_str(&format!("{}: ", colorize("help", "1;36")));
            out.push_str(&render_help(h, source));
            out.push('\n');
        }
        for n in diag.notes.iter().take(2) {
            out.push_str(&format!("{}: {}\n", colorize("note", "1;36"), n));
        }
        out
    } else {
        let code = diag
            .code
            .as_ref()
            .map(|c| format!("[{}]", c))
            .unwrap_or_default();
        let err = colorize("error", "1;31");
        let code_col = if code.is_empty() {
            "".to_string()
        } else {
            colorize(&code, "1;33")
        };
        let msg = diag.message.trim_start();
        let mut out = if code.is_empty() {
            format!("{}: {}", err, msg)
        } else {
            format!("{}{}: {}", err, code_col, msg)
        };
        out.push('\n');
        for h in diag.helps.iter().take(3) {
            out.push_str(&format!("{}: ", colorize("help", "1;36")));
            out.push_str(&render_help(h, source));
            out.push('\n');
        }
        for n in diag.notes.iter().take(2) {
            out.push_str(&format!("{}: {}\n", colorize("note", "1;36"), n));
        }
        out
    }
}

fn render_help(h: &HelpMsg, source: &str) -> String {
    match h {
        HelpMsg::Plain(s) => s.clone(),
        HelpMsg::Snippet { span, template } => {
            let snip = snippet_one_line(source, span.clone());
            template.replace("{snippet}", &snip)
        }
    }
}

fn line_col_at(source: &str, byte: usize) -> (usize, usize) {
    let mut line = 1usize;
    let mut col = 1usize;
    let mut i = 0usize;
    for ch in source.chars() {
        if i >= byte {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
        i += ch.len_utf8();
    }
    (line, col)
}

fn colorize(s: &str, code: &str) -> String {
    if std::env::var_os("GOST_COLOR").is_some() {
        format!("\x1b[{}m{}\x1b[0m", code, s)
    } else {
        s.to_string()
    }
}

fn snippet_one_line(source: &str, span: Span) -> String {
    let start = span.start.min(source.len());
    let end = span.end.min(source.len());
    let (lo, hi) = if start <= end {
        (start, end)
    } else {
        (end, start)
    };
    let mut s = source.get(lo..hi).unwrap_or("<expr>").to_string();
    s = s.replace(['\r', '\n'], " ");
    s = s.split_whitespace().collect::<Vec<_>>().join(" ");
    const LIM: usize = 80;
    if s.len() > LIM {
        s.truncate(LIM);
        s.push_str("...");
    }
    s
}
