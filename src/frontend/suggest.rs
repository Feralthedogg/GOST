use strsim::levenshtein;

/// Return up to 3 suggestions by edit distance.
pub fn suggest(needle: &str, candidates: impl IntoIterator<Item = String>) -> Vec<String> {
    let needle = needle.trim();
    if needle.is_empty() {
        return vec![];
    }

    let mut scored: Vec<(usize, String)> = candidates
        .into_iter()
        .filter(|c| !c.is_empty() && c != needle)
        .map(|c| (levenshtein(needle, &c), c))
        .collect();

    let max_dist = match needle.len() {
        0..=3 => 1,
        4..=6 => 2,
        7..=10 => 3,
        _ => 4,
    };

    scored.retain(|(d, _)| *d <= max_dist);
    scored.sort_by(|(da, a), (db, b)| da.cmp(db).then(a.len().cmp(&b.len())).then(a.cmp(b)));

    scored.into_iter().take(3).map(|(_, s)| s).collect()
}

pub fn did_you_mean(needle: &str, candidates: impl IntoIterator<Item = String>) -> Option<String> {
    let v = suggest(needle, candidates);
    match v.len() {
        0 => None,
        1 => Some(format!("did you mean `{}`?", v[0])),
        _ => Some(format!(
            "did you mean one of: {}?",
            v.iter()
                .map(|s| format!("`{}`", s))
                .collect::<Vec<_>>()
                .join(", ")
        )),
    }
}
