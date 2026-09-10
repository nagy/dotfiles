#!/usr/bin/env rust-script
//! ```cargo
//! [dependencies]
//! anyhow = "1"
//! clap = { version = "4", features = ["derive"] }
//! clap_complete = "4"
//! regex = "1"
//! serde_json = "1"
//! ```

//! Convert AI-chat HTML into clean markdown.
//!
//! Each supported site provides a [`ChatExtractor`] implementation. The default
//! registry holds one ([`ChatGptExtractor`]); the CLI auto-detects the site
//! with [`ChatExtractor::matches`] and can be forced with `--source`.
//!
//! ```text
//! chat2md page.html > chat.md
//! bruvtab html <TAB_ID> | chat2md > chat.md
//! chat2md --url https://chatgpt.com/share/... page.html
//! chat2md --strict page.html       # fail instead of the lossy DOM fallback
//! chat2md --no-citations p.html    # strip markers, emit no Sources
//! chat2md --source chatgpt p.html  # skip auto-detection
//! chat2md sources                  # list available extractors
//! ```
//!
//! Public share links can be fetched without a browser; smoke-test with:
//!
//! ```text
//! curl -sSL -A 'Mozilla/5.0' \
//!   https://chatgpt.com/share/6a5fdc7a-d6f8-83e8-bbea-8deb42cfed56 > page.html
//! chat2md --strict page.html > chat.md
//! ```
//!
//! That conversation ("Jacobian Conjecture Counterexample", model `gpt-5-5`)
//! exercises multi-chunk RSC streams, the pointer graph, and code fences. Do not
//! commit the downloaded HTML or its markdown; the link is the fixture. Live
//! logged-in tabs still need `bruvtab html <TAB_ID>`.
//!
//! Exit codes: 0 ok, 1 no conversation found, 2 bad usage.

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    io::Read,
    path::{Path, PathBuf},
    sync::OnceLock,
};

use anyhow::{bail, Context, Result};
use clap::{CommandFactory, Parser, Subcommand};
use clap_complete::Shell;
use regex::Regex;
use serde_json::{Map, Value};

// --- Domain -----------------------------------------------------------------

/// Who authored a message.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    User,
    Assistant,
}

impl Role {
    /// Parse a site-reported role string, ignoring plumbing roles.
    fn parse(raw: &str) -> Option<Self> {
        match raw {
            "user" => Some(Role::User),
            "assistant" => Some(Role::Assistant),
            _ => None,
        }
    }

    /// Capitalised heading label, e.g. `User`.
    fn label(self) -> &'static str {
        match self {
            Role::User => "User",
            Role::Assistant => "Assistant",
        }
    }
}

/// A web citation attached to an assistant turn.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Source {
    pub label: String,
    pub url: String,
}

/// One turn of a conversation.
#[derive(Debug, Clone)]
pub struct Message {
    pub role: Role,
    pub markdown: String,
    /// Cited pages, numbered in order. The first [`Message::cited_count`] were
    /// referenced by an inline marker; the rest are plain per-turn sources.
    pub sources: Vec<Source>,
    pub cited_count: usize,
}

/// How faithfully the source preserved the original text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fidelity {
    /// Text (including code newlines) came from the site's data payload.
    Exact,
    /// Text was reconstructed from rendered HTML and is best-effort.
    Lossy,
}

/// A parsed conversation, site-independent.
#[derive(Debug, Clone)]
pub struct Conversation {
    pub title: Option<String>,
    pub create_time: Option<i64>,
    pub update_time: Option<i64>,
    pub model: Option<String>,
    pub messages: Vec<Message>,
    pub fidelity: Fidelity,
}

/// Knobs passed to an extractor.
#[derive(Debug, Clone, Copy)]
pub struct ExtractOptions {
    /// Resolve inline citation markers into footnotes.
    pub citations: bool,
}

// --- Extractor trait --------------------------------------------------------

/// Turn one site's HTML into a [`Conversation`].
///
/// Implementors recognise their own pages ([`ChatExtractor::matches`]) and
/// decode them ([`ChatExtractor::extract`]). Keep site internals (routes,
/// embedded payloads, DOM selectors) private to the implementation; the rest of
/// the program only sees [`Conversation`].
pub trait ChatExtractor {
    /// Stable identifier used by `--source` and `sources`.
    fn name(&self) -> &'static str;

    /// One-line description for `chat2md sources`.
    fn description(&self) -> &'static str;

    /// Cheap check that this extractor can probably handle `html`.
    fn matches(&self, html: &str) -> bool;

    /// Decode `html`, or `None` when no conversation is present.
    fn extract(&self, html: &str, options: &ExtractOptions) -> Option<Conversation>;
}

/// Built-in extractors, checked in order during auto-detection.
fn registry() -> Vec<Box<dyn ChatExtractor>> {
    vec![Box::new(ChatGptExtractor)]
}

// --- ChatGPT extractor ------------------------------------------------------

/// Extractor for chatgpt.com (share links and logged-in tabs).
struct ChatGptExtractor;

impl ChatExtractor for ChatGptExtractor {
    fn name(&self) -> &'static str {
        "chatgpt"
    }

    fn description(&self) -> &'static str {
        "chatgpt.com (React-Router RSC payload, DOM fallback)"
    }

    fn matches(&self, html: &str) -> bool {
        html.contains("__reactRouterContext") || html.contains("data-message-author-role")
    }

    fn extract(&self, html: &str, options: &ExtractOptions) -> Option<Conversation> {
        if let Some(raw) = rsc_payload(html) {
            if let Some(arr) = parse_rsc_array(&raw) {
                if let Some(conv) = conversation_from_rsc(&arr, options.citations) {
                    return Some(conv);
                }
            }
        }
        let turns = dom_turns(html);
        if turns.is_empty() {
            return None;
        }
        let messages = turns
            .into_iter()
            .map(|(role, markdown)| Message {
                role,
                markdown,
                sources: Vec::new(),
                cited_count: 0,
            })
            .collect();
        Some(Conversation {
            title: None,
            create_time: None,
            update_time: None,
            model: None,
            messages,
            fidelity: Fidelity::Lossy,
        })
    }
}

// --- RSC payload ------------------------------------------------------------

/// Stream chunks pushed via `window.__reactRouterContext...enqueue("…")`.
fn re_enqueue() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"(?s)streamController\.enqueue\(\s*("(?:[^"\\]|\\.)*")\s*\)"#).unwrap()
    })
}

/// Zero-width private-use glyphs wrapping web-search citations.
const CITATION_MARKERS: [char; 5] = ['\u{e200}', '\u{e201}', '\u{e202}', '\u{e203}', '\u{e204}'];

/// Tracking query parameters stripped from recovered URLs by default.
const TRACKING_PARAMS: [&str; 11] = [
    "utm_source",
    "utm_medium",
    "utm_campaign",
    "utm_term",
    "utm_content",
    "utm_id",
    "gclid",
    "fbclid",
    "mc_cid",
    "mc_eid",
    "igshid",
];

/// Join every RSC stream chunk into one decoded string.
///
/// The stream is a sequence of JS string literals; concatenating their decoded
/// values yields the RSC row data (usually one JSON array, sometimes followed
/// by `P<n>:` rows trimmed later).
fn rsc_payload(doc: &str) -> Option<String> {
    let mut any = false;
    let mut out = String::new();
    for caps in re_enqueue().captures_iter(doc) {
        any = true;
        let decoded: String = serde_json::from_str(caps.get(1)?.as_str()).ok()?;
        out.push_str(&decoded);
    }
    if any {
        Some(out)
    } else {
        None
    }
}

/// Parse the RSC row stream into the leading JSON array.
fn parse_rsc_array(raw: &str) -> Option<Vec<Value>> {
    if let Ok(Value::Array(arr)) = serde_json::from_str::<Value>(raw) {
        return Some(arr);
    }
    // Rows may look like `[ … ]\nP554:[{}]`; keep only the leading array.
    let start = raw.find('[')?;
    let bytes = raw.as_bytes();
    let mut depth = 0i64;
    let mut in_str = false;
    let mut esc = false;
    for i in start..bytes.len() {
        let ch = bytes[i];
        if in_str {
            if esc {
                esc = false;
            } else if ch == b'\\' {
                esc = true;
            } else if ch == b'"' {
                in_str = false;
            }
            continue;
        }
        match ch {
            b'"' => in_str = true,
            b'[' => depth += 1,
            b']' => {
                depth -= 1;
                if depth == 0 {
                    return serde_json::from_str::<Vec<Value>>(&raw[start..=i]).ok();
                }
            }
            _ => {}
        }
    }
    None
}

/// Resolve the RSC array's pointer graph into plain JSON.
///
/// Non-negative integers are pointers to `arr[n]`; negative ones are
/// null/undefined. Object keys like `_53` name the field held at `arr[53]`.
/// Cycles are guarded with a visited cache.
fn make_resolver(arr: &[Value]) -> impl Fn(&Value) -> Value + '_ {
    let cache: RefCell<HashMap<usize, Value>> = RefCell::new(HashMap::new());
    move |v| deref(v, arr, &cache)
}

fn deref(v: &Value, arr: &[Value], cache: &RefCell<HashMap<usize, Value>>) -> Value {
    match v {
        Value::Bool(_) => v.clone(),
        Value::Number(n) => {
            let Some(i) = n.as_i64() else {
                return v.clone();
            };
            if i < 0 || i as usize >= arr.len() {
                return Value::Null;
            }
            let i = i as usize;
            if let Some(cached) = cache.borrow().get(&i) {
                return cached.clone();
            }
            cache.borrow_mut().insert(i, Value::Null);
            let resolved = deref(&arr[i], arr, cache);
            cache.borrow_mut().insert(i, resolved.clone());
            resolved
        }
        Value::Object(map) => {
            let mut out = Map::new();
            for (k, val) in map {
                let key = match k.strip_prefix('_').and_then(|r| r.parse::<usize>().ok()) {
                    Some(idx) => deref(&Value::from(idx as i64), arr, cache)
                        .as_str()
                        .map(str::to_string)
                        .unwrap_or_else(|| k.clone()),
                    None => k.clone(),
                };
                out.insert(key, deref(val, arr, cache));
            }
            Value::Object(out)
        }
        Value::Array(items) => Value::Array(items.iter().map(|x| deref(x, arr, cache)).collect()),
        _ => v.clone(),
    }
}

/// Locate the conversation object inside the resolved loader data.
///
/// Tries the share route first, then any route exposing `serverResponse.data`,
/// then a generic recursive search.
fn find_conversation_data(root: &Value) -> Option<Value> {
    if let Some(Value::Object(loader)) = root.get("loaderData") {
        for (key, val) in loader {
            if key.contains("share") {
                if let Some(data) = route_data(val) {
                    if looks_like_conversation(&data) {
                        return Some(data);
                    }
                }
            }
        }
        for val in loader.values() {
            if let Some(data) = route_data(val) {
                if looks_like_conversation(&data) {
                    return Some(data);
                }
            }
        }
    }
    search_for_conversation(root, &mut HashSet::new())
}

fn route_data(route: &Value) -> Option<Value> {
    route.get("serverResponse")?.get("data").cloned()
}

fn looks_like_conversation(obj: &Value) -> bool {
    obj.get("linear_conversation").is_some_and(Value::is_array)
        || obj.get("mapping").is_some_and(Value::is_object)
}

fn search_for_conversation(obj: &Value, seen: &mut HashSet<usize>) -> Option<Value> {
    match obj {
        Value::Object(map) => {
            if !seen.insert(obj as *const Value as usize) {
                return None;
            }
            if looks_like_conversation(obj) {
                return Some(obj.clone());
            }
            map.values().find_map(|v| search_for_conversation(v, seen))
        }
        Value::Array(items) => {
            if !seen.insert(obj as *const Value as usize) {
                return None;
            }
            items.iter().find_map(|v| search_for_conversation(v, seen))
        }
        _ => None,
    }
}

/// Assemble a conversation from a parsed RSC array.
fn conversation_from_rsc(arr: &[Value], citations: bool) -> Option<Conversation> {
    let deref = make_resolver(arr);
    let root = deref(arr.first()?);
    let data = find_conversation_data(&root)?;
    let messages = messages_from_data(&data, citations);
    if messages.is_empty() {
        return None;
    }
    Some(Conversation {
        title: data
            .get("title")
            .and_then(Value::as_str)
            .map(str::to_string),
        create_time: data
            .get("create_time")
            .and_then(Value::as_f64)
            .map(|f| f as i64),
        update_time: data
            .get("update_time")
            .and_then(Value::as_f64)
            .map(|f| f as i64),
        model: data
            .get("default_model_slug")
            .and_then(Value::as_str)
            .map(str::to_string),
        messages,
        fidelity: Fidelity::Exact,
    })
}

/// Conversation turns, in order.
///
/// `mapping` walked from `current_node` is preferred because
/// `linear_conversation` lists every branch, so an edited or retried turn shows
/// up as a duplicate. Fall back to `linear_conversation` only when the mapping
/// cannot be walked.
fn linear_nodes(data: &Value) -> Vec<Value> {
    if let Some(Value::Object(mapping)) = data.get("mapping") {
        if let Some(current) = data.get("current_node").and_then(Value::as_str) {
            let mut chain = Vec::new();
            let mut seen = HashSet::new();
            let mut id = Some(current.to_string());
            while let Some(node_id) = id {
                if seen.contains(&node_id) {
                    break;
                }
                let Some(node) = mapping.get(&node_id) else {
                    break;
                };
                seen.insert(node_id.clone());
                chain.push(node.clone());
                id = node
                    .get("parent")
                    .and_then(Value::as_str)
                    .map(str::to_string);
            }
            if !chain.is_empty() {
                chain.reverse();
                return chain;
            }
        }
    }
    data.get("linear_conversation")
        .and_then(Value::as_array)
        .cloned()
        .unwrap_or_default()
}

fn messages_from_data(data: &Value, citations: bool) -> Vec<Message> {
    let mut messages = Vec::new();
    for node in linear_nodes(data) {
        let Some(msg) = node.get("message").filter(|m| m.is_object()) else {
            continue;
        };
        let Some(role) = msg
            .get("author")
            .and_then(|a| a.get("role"))
            .and_then(Value::as_str)
            .and_then(Role::parse)
        else {
            continue;
        };
        let Some(content) = msg.get("content") else {
            continue;
        };
        if content.get("content_type").and_then(Value::as_str) != Some("text") {
            continue;
        }
        let mut text = match content.get("parts").and_then(Value::as_array) {
            Some(parts) => parts.iter().filter_map(Value::as_str).collect::<String>(),
            None => String::new(),
        };
        let index = if citations {
            citation_index(msg)
        } else {
            HashMap::new()
        };
        let sources = if citations {
            let (rewritten, mut sources) = render_citations(&text, &index, true);
            text = rewritten;
            let cited_count = sources.len();
            let extra = footnote_sources(msg, &sources);
            sources.extend(extra);
            Some((sources, cited_count))
        } else {
            let (rewritten, _) = render_citations(&text, &index, false);
            text = rewritten;
            None
        };
        let text = text.trim().to_string();
        if text.is_empty() {
            continue;
        }
        let (sources, cited_count) = sources.unwrap_or_default();
        messages.push(Message {
            role,
            markdown: text,
            sources,
            cited_count,
        });
    }
    messages
}

/// Map each full citation marker to the sources it cites.
///
/// `metadata.content_references` is authoritative: each `grouped_webpages`
/// entry repeats the marker in `matched_text` and lists cited pages, first in
/// `items` then in `supporting_websites`. A marker naming two tokens therefore
/// maps to two sources, in order.
fn citation_index(msg: &Value) -> HashMap<String, Vec<Source>> {
    let mut index = HashMap::new();
    let Some(meta) = msg.get("metadata") else {
        return index;
    };
    let Some(refs) = meta.get("content_references").and_then(Value::as_array) else {
        return index;
    };
    for reference in refs {
        if reference.get("type").and_then(Value::as_str) != Some("grouped_webpages") {
            continue;
        }
        let Some(marker) = reference.get("matched_text").and_then(Value::as_str) else {
            continue;
        };
        if marker.is_empty() {
            continue;
        }
        let mut sources = Vec::new();
        for key in ["items", "fallback_items"] {
            if let Some(items) = reference.get(key).and_then(Value::as_array) {
                for item in items {
                    sources.extend(source_pages(item));
                }
            }
        }
        if !sources.is_empty() {
            index.insert(marker.to_string(), sources);
        }
    }
    index
}

/// Split one reference item into an ordered list of pages.
fn source_pages(entry: &Value) -> Vec<Source> {
    let mut pages = Vec::new();
    if let Some(url) = entry
        .get("url")
        .and_then(Value::as_str)
        .filter(|u| !u.is_empty())
    {
        pages.push(Source {
            label: entry
                .get("title")
                .and_then(Value::as_str)
                .filter(|t| !t.is_empty())
                .unwrap_or(url)
                .to_string(),
            url: clean_url(url),
        });
    }
    if let Some(sites) = entry.get("supporting_websites").and_then(Value::as_array) {
        for site in sites {
            let Some(url) = site
                .get("url")
                .and_then(Value::as_str)
                .filter(|u| !u.is_empty())
            else {
                continue;
            };
            pages.push(Source {
                label: site
                    .get("title")
                    .and_then(Value::as_str)
                    .filter(|t| !t.is_empty())
                    .unwrap_or(url)
                    .to_string(),
                url: clean_url(url),
            });
        }
    }
    pages
}

/// Pages listed in a `sources_footnote` but missed by the markers.
fn footnote_sources(msg: &Value, already: &[Source]) -> Vec<Source> {
    let seen: HashSet<&str> = already.iter().map(|s| s.url.as_str()).collect();
    let mut extra = Vec::new();
    let Some(refs) = msg
        .get("metadata")
        .and_then(|m| m.get("content_references"))
        .and_then(Value::as_array)
    else {
        return extra;
    };
    for reference in refs {
        if reference.get("type").and_then(Value::as_str) != Some("sources_footnote") {
            continue;
        }
        let Some(sources) = reference.get("sources").and_then(Value::as_array) else {
            continue;
        };
        for source in sources {
            let Some(url) = source
                .get("url")
                .and_then(Value::as_str)
                .filter(|u| !u.is_empty())
            else {
                continue;
            };
            let url = clean_url(url);
            if seen.contains(url.as_str()) {
                continue;
            }
            extra.push(Source {
                label: source
                    .get("title")
                    .and_then(Value::as_str)
                    .filter(|t| !t.is_empty())
                    .unwrap_or(&url)
                    .to_string(),
                url,
            });
        }
    }
    extra
}

/// Drop tracking query parameters while preserving the rest of the URL.
fn clean_url(url: &str) -> String {
    let (rest, fragment) = match url.split_once('#') {
        Some((r, f)) => (r, Some(f)),
        None => (url, None),
    };
    let (base, query) = match rest.split_once('?') {
        Some((b, q)) => (b, q),
        None => return url.to_string(),
    };
    let kept: Vec<&str> = query
        .split('&')
        .filter(|pair| {
            let key = pair.split('=').next().unwrap_or("").to_ascii_lowercase();
            !TRACKING_PARAMS.contains(&key.as_str())
        })
        .collect();
    let mut out = base.to_string();
    if !kept.is_empty() {
        out.push('?');
        out.push_str(&kept.join("&"));
    }
    if let Some(f) = fragment {
        out.push('#');
        out.push_str(f);
    }
    out
}

fn re_cite_group() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"\x{E200}(?:cite)?\x{E202}([^\x{E201}]*)\x{E201}").unwrap())
}

fn re_cite_bare() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"\x{E202}([^\x{E201}]*)\x{E201}").unwrap())
}

/// Rewrite inline citation markers as per-message markdown footnotes.
///
/// Markers absent from the index, and any leftover private-use glyphs, are
/// dropped so no dead link survives.
fn render_citations(
    text: &str,
    index: &HashMap<String, Vec<Source>>,
    keep: bool,
) -> (String, Vec<Source>) {
    if !CITATION_MARKERS.iter().any(|m| text.contains(*m)) {
        return (text.to_string(), Vec::new());
    }
    let mut sources = Vec::new();
    let first = replace_citations(re_cite_group(), text, index, keep, &mut sources);
    let second = replace_citations(re_cite_bare(), &first, index, keep, &mut sources);
    let mut out = second;
    for marker in CITATION_MARKERS {
        out = out.replace(marker, "");
    }
    (out, sources)
}

fn replace_citations(
    re: &Regex,
    text: &str,
    index: &HashMap<String, Vec<Source>>,
    keep: bool,
    sources: &mut Vec<Source>,
) -> String {
    if !keep {
        return re.replace_all(text, "").into_owned();
    }
    let mut out = String::new();
    let mut last = 0;
    for caps in re.captures_iter(text) {
        let whole = caps.get(0).unwrap();
        out.push_str(&text[last..whole.start()]);
        let fallback = format!("\u{e200}cite\u{e202}{}\u{e201}", &caps[1]);
        if let Some(cites) = index.get(whole.as_str()).or_else(|| index.get(&fallback)) {
            for source in cites {
                sources.push(source.clone());
                out.push_str(&format!("[^{}]", sources.len()));
            }
        }
        last = whole.end();
    }
    out.push_str(&text[last..]);
    out
}

// --- Minimal DOM (fallback) -------------------------------------------------

const VOID_TAGS: [&str; 14] = [
    "area", "base", "br", "col", "embed", "hr", "img", "input", "link", "meta", "param", "source",
    "track", "wbr",
];

const AUTO_CLOSE: [&str; 8] = ["p", "li", "tr", "td", "th", "option", "thead", "tbody"];

fn re_tag() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"<(/?)([a-zA-Z][a-zA-Z0-9-]*)((?:"[^"]*"|'[^']*'|[^>"'])*?)(/?)>"#).unwrap()
    })
}

fn re_attr() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(
            r#"([a-zA-Z_:][-a-zA-Z0-9_:.]*)(?:\s*=\s*(?:"([^"]*)"|'([^']*)'|([^\s"'=<>`]+)))?"#,
        )
        .unwrap()
    })
}

fn re_message_open() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| {
        Regex::new(r#"<div[^>]*\bdata-message-author-role="(user|assistant)"[^>]*>"#).unwrap()
    })
}

/// A forgiving DOM node; text nodes carry an empty tag and data instead.
struct RawNode {
    tag: String,
    attrs: HashMap<String, String>,
    data: String,
    children: Vec<usize>,
}

#[derive(Default)]
struct Arena {
    nodes: Vec<RawNode>,
}

impl Arena {
    fn push(&mut self, tag: &str, attrs: HashMap<String, String>) -> usize {
        self.nodes.push(RawNode {
            tag: tag.to_string(),
            attrs,
            data: String::new(),
            children: Vec::new(),
        });
        self.nodes.len() - 1
    }

    fn add_child(&mut self, parent: usize, child: usize) {
        self.nodes[parent].children.push(child);
    }
}

/// Parse an HTML fragment into an arena; node 0 is the `#root`.
fn parse_html(fragment: &str) -> Arena {
    let tag_re = re_tag();
    let attr_re = re_attr();
    let mut arena = Arena::default();
    arena.push("#root", HashMap::new());
    let mut stack = vec![0usize];
    let mut pos = 0usize;
    let mut raw_text_tag: Option<String> = None;

    while pos < fragment.len() {
        if let Some(tag) = raw_text_tag.clone() {
            let needle = format!("</{tag}");
            match fragment[pos..].to_ascii_lowercase().find(&needle) {
                Some(offset) => {
                    let end = pos + offset;
                    add_text(&mut arena, *stack.last().unwrap(), &fragment[pos..end]);
                    raw_text_tag = None;
                    pos = end;
                    continue;
                }
                None => {
                    add_text(&mut arena, *stack.last().unwrap(), &fragment[pos..]);
                    break;
                }
            }
        }
        let Some(caps) = tag_re.captures(&fragment[pos..]) else {
            add_text(&mut arena, *stack.last().unwrap(), &fragment[pos..]);
            break;
        };
        let whole = caps.get(0).unwrap();
        let start = pos + whole.start();
        let end = pos + whole.end();
        add_text(&mut arena, *stack.last().unwrap(), &fragment[pos..start]);

        let closing = caps.get(1).is_some_and(|g| g.as_str() == "/");
        let name = caps.get(2).unwrap().as_str().to_ascii_lowercase();
        let attrs = parse_attrs(attr_re, caps.get(3).map(|g| g.as_str()).unwrap_or(""));
        let self_closing = caps.get(4).is_some_and(|g| g.as_str() == "/");

        if closing {
            close_through(&arena, &mut stack, &name);
        } else if VOID_TAGS.contains(&name.as_str()) {
            let idx = arena.push(&name, attrs);
            let parent = *stack.last().unwrap();
            arena.add_child(parent, idx);
        } else {
            if AUTO_CLOSE.contains(&name.as_str()) {
                close_through(&arena, &mut stack, &name);
            }
            let idx = arena.push(&name, attrs);
            let parent = *stack.last().unwrap();
            arena.add_child(parent, idx);
            if !self_closing {
                stack.push(idx);
            }
            if name == "script" || name == "style" {
                raw_text_tag = Some(name);
            }
        }
        pos = end;
    }
    arena
}

fn close_through(arena: &Arena, stack: &mut Vec<usize>, tag: &str) {
    if let Some(i) = stack.iter().rposition(|&n| arena.nodes[n].tag == tag) {
        stack.truncate(i);
    }
}

fn parse_attrs(re: &Regex, raw: &str) -> HashMap<String, String> {
    let mut map = HashMap::new();
    for caps in re.captures_iter(raw) {
        let key = caps.get(1).unwrap().as_str().to_ascii_lowercase();
        let value = caps
            .get(2)
            .or_else(|| caps.get(3))
            .or_else(|| caps.get(4))
            .map(|g| g.as_str())
            .unwrap_or("");
        map.insert(key, value.to_string());
    }
    map
}

fn add_text(arena: &mut Arena, parent: usize, raw: &str) {
    if raw.is_empty() {
        return;
    }
    let idx = arena.push("", HashMap::new());
    arena.nodes[idx].data = decode_entities(raw);
    arena.add_child(parent, idx);
}

fn decode_entities(s: &str) -> String {
    if !s.contains('&') {
        return s.to_string();
    }
    let mut out = String::with_capacity(s.len());
    let mut i = 0;
    while i < s.len() {
        if s.as_bytes()[i] == b'&' {
            if let Some(semi) = s[i + 1..].find(';') {
                let entity = &s[i + 1..i + 1 + semi];
                if let Some(ch) = decode_entity(entity) {
                    out.push(ch);
                    i += semi + 2;
                    continue;
                }
            }
        }
        let ch = s[i..].chars().next().unwrap();
        out.push(ch);
        i += ch.len_utf8();
    }
    out
}

fn decode_entity(entity: &str) -> Option<char> {
    match entity {
        "amp" => Some('&'),
        "lt" => Some('<'),
        "gt" => Some('>'),
        "quot" => Some('"'),
        "apos" => Some('\''),
        "nbsp" => Some('\u{a0}'),
        _ => {
            if let Some(hex) = entity
                .strip_prefix("#x")
                .or_else(|| entity.strip_prefix("#X"))
            {
                char::from_u32(u32::from_str_radix(hex, 16).ok()?)
            } else {
                char::from_u32(entity.strip_prefix('#')?.parse().ok()?)
            }
        }
    }
}

/// Return the substring of the element whose opening tag starts at `start`.
fn balanced_span(doc: &str, start: usize) -> Option<String> {
    let re = re_tag();
    let first = re.captures(&doc[start..])?;
    if first.get(0).unwrap().start() != 0 {
        return None;
    }
    let name = first.get(2).unwrap().as_str().to_ascii_lowercase();
    let mut depth = 0i64;
    let mut pos = start;
    while pos < doc.len() {
        let caps = re.captures(&doc[pos..])?;
        let end = pos + caps.get(0).unwrap().end();
        let tag = caps.get(2).unwrap().as_str().to_ascii_lowercase();
        if tag == name {
            if caps.get(1).is_some_and(|g| g.as_str() == "/") {
                depth -= 1;
                if depth == 0 {
                    return Some(doc[start..end].to_string());
                }
            } else if !(caps.get(4).is_some_and(|g| g.as_str() == "/")
                || VOID_TAGS.contains(&tag.as_str()))
            {
                depth += 1;
            }
        }
        pos = end;
    }
    None
}

/// Lossy fallback: extract turns straight from the rendered DOM.
fn dom_turns(doc: &str) -> Vec<(Role, String)> {
    let mut turns = Vec::new();
    for caps in re_message_open().captures_iter(doc) {
        let role = caps.get(1).unwrap().as_str();
        let Some(span) = balanced_span(doc, caps.get(0).unwrap().start()) else {
            continue;
        };
        let body = render_dom_body(&span);
        let body = body.trim().to_string();
        if !body.is_empty() {
            let role = if role == "user" {
                Role::User
            } else {
                Role::Assistant
            };
            turns.push((role, body));
        }
    }
    turns
}

/// Convert a turn's DOM into markdown.
fn render_dom_body(span: &str) -> String {
    let arena = parse_html(span);
    let mut md = Markdown::default();
    match find_class(&arena, 0, "markdown") {
        Some(root) => render_node(&arena, root, &mut md),
        None => {
            for &child in &arena.nodes[0].children {
                render_node(&arena, child, &mut md);
            }
        }
    }
    md.finish()
}

fn find_class(arena: &Arena, idx: usize, needle: &str) -> Option<usize> {
    for &child in &arena.nodes[idx].children {
        if arena.nodes[child]
            .attrs
            .get("class")
            .is_some_and(|c| c.contains(needle))
        {
            return Some(child);
        }
        if let Some(hit) = find_class(arena, child, needle) {
            return Some(hit);
        }
    }
    None
}

fn find_tag(arena: &Arena, idx: usize, tag: &str) -> Option<usize> {
    for &child in &arena.nodes[idx].children {
        if arena.nodes[child].tag == tag {
            return Some(child);
        }
        if let Some(hit) = find_tag(arena, child, tag) {
            return Some(hit);
        }
    }
    None
}

fn raw_text(arena: &Arena, idx: usize) -> String {
    let mut out = String::new();
    raw_text_into(arena, idx, &mut out);
    out
}

fn raw_text_into(arena: &Arena, idx: usize, out: &mut String) {
    if arena.nodes[idx].tag.is_empty() {
        out.push_str(&arena.nodes[idx].data);
    }
    for &child in &arena.nodes[idx].children {
        raw_text_into(arena, child, out);
    }
}

/// Accumulates rendered markdown chunks and normalises spacing.
#[derive(Default)]
struct Markdown {
    parts: String,
}

impl Markdown {
    fn inline(&mut self, text: &str) {
        self.parts.push_str(text);
    }

    fn block(&mut self, text: &str) {
        self.parts.push_str("\n\n");
        self.parts.push_str(text);
        self.parts.push_str("\n\n");
    }

    fn finish(&self) -> String {
        let out = re_horizontal_space()
            .replace_all(&self.parts, " ")
            .into_owned();
        let out = re_line_padding().replace_all(&out, "\n").into_owned();
        re_blank_runs().replace_all(&out, "\n\n").trim().to_string()
    }
}

fn render_node(arena: &Arena, idx: usize, md: &mut Markdown) {
    let node = &arena.nodes[idx];
    if node.tag.is_empty() {
        md.inline(&node.data);
        return;
    }
    let tag = node.tag.as_str();
    match tag {
        "strong" | "b" => render_wrapped(arena, idx, md, "**"),
        "em" | "i" => render_wrapped(arena, idx, md, "*"),
        "code" => render_wrapped(arena, idx, md, "`"),
        "del" | "s" => render_wrapped(arena, idx, md, "~~"),
        "br" => md.inline("\n"),
        "a" => {
            let href = node.attrs.get("href").map(String::as_str).unwrap_or("");
            let inner = render_children(arena, idx);
            let label = if inner.is_empty() {
                href.to_string()
            } else {
                inner
            };
            md.inline(&format!("[{label}]({href})"));
        }
        "h1" | "h2" | "h3" | "h4" | "h5" | "h6" => {
            let level = tag.as_bytes()[1] - b'0';
            let inner = render_children(arena, idx);
            md.block(&format!("{} {inner}", "#".repeat(level as usize)));
        }
        "pre" => md.block(&render_code_block(arena, idx)),
        "hr" => md.block("---"),
        "ul" => md.block(&render_list(arena, idx, false)),
        "ol" => md.block(&render_list(arena, idx, true)),
        "blockquote" => {
            let inner = render_children(arena, idx);
            let quoted = inner
                .lines()
                .map(|line| format!("> {line}"))
                .collect::<Vec<_>>()
                .join("\n");
            md.block(&quoted);
        }
        "table" => md.block(&render_table(arena, idx)),
        "p" => {
            let inner = render_children(arena, idx);
            md.block(&inner);
        }
        "script" | "style" | "svg" | "button" | "form" | "textarea" => {}
        _ => {
            for &child in &arena.nodes[idx].children {
                render_node(arena, child, md);
            }
        }
    }
}

fn render_wrapped(arena: &Arena, idx: usize, md: &mut Markdown, marker: &str) {
    let inner = render_children(arena, idx);
    if !inner.is_empty() {
        md.inline(&format!("{marker}{inner}{marker}"));
    }
}

fn render_children(arena: &Arena, idx: usize) -> String {
    let mut md = Markdown::default();
    for &child in &arena.nodes[idx].children {
        render_node(arena, child, &mut md);
    }
    md.finish()
}

/// Render `<pre>` to a fenced block, recovering newlines heuristically.
fn render_code_block(arena: &Arena, idx: usize) -> String {
    let code = find_tag(arena, idx, "code").unwrap_or(idx);
    let raw = raw_text(arena, code);
    let language = detect_language(arena, idx)
        .or_else(|| detect_language(arena, code))
        .unwrap_or_default();
    let body = reflow_code(&raw);
    format!("```{language}\n{body}\n```")
}

/// Language label from the code block's header div, if any.
fn detect_language(arena: &Arena, idx: usize) -> Option<String> {
    for &child in &arena.nodes[idx].children {
        if arena.nodes[child]
            .attrs
            .get("class")
            .is_some_and(|c| c.contains("text-token-text-primary"))
        {
            let label = raw_text(arena, child);
            let label = label.trim();
            if !label.is_empty() && label.chars().count() <= 24 {
                return Some(label.to_lowercase().replace(' ', ""));
            }
        }
        if let Some(found) = detect_language(arena, child) {
            return Some(found);
        }
    }
    None
}

fn render_list(arena: &Arena, idx: usize, ordered: bool) -> String {
    let mut lines = Vec::new();
    let mut index = 1;
    for &child in &arena.nodes[idx].children {
        if arena.nodes[child].tag != "li" {
            continue;
        }
        let mut item = Markdown::default();
        let mut nested = Vec::new();
        for &grand in &arena.nodes[child].children {
            match arena.nodes[grand].tag.as_str() {
                "ul" => nested.push(render_list(arena, grand, false)),
                "ol" => nested.push(render_list(arena, grand, true)),
                _ => render_node(arena, grand, &mut item),
            }
        }
        let bullet = if ordered {
            format!("{index}. ")
        } else {
            "- ".to_string()
        };
        lines.push(format!(
            "{bullet}{}",
            item.finish().replace('\n', " ").trim()
        ));
        for block in nested {
            lines.extend(block.lines().map(|line| format!("  {line}")));
        }
        index += 1;
    }
    lines.join("\n")
}

fn render_table(arena: &Arena, idx: usize) -> String {
    let mut rows: Vec<Vec<String>> = Vec::new();
    let mut trs = Vec::new();
    iter_tags(arena, idx, "tr", &mut trs);
    for tr in trs {
        let mut cells = Vec::new();
        for &cell in &arena.nodes[tr].children {
            if arena.nodes[cell].tag == "td" || arena.nodes[cell].tag == "th" {
                cells.push(
                    render_children(arena, cell)
                        .replace('\n', " ")
                        .trim()
                        .to_string(),
                );
            }
        }
        if !cells.is_empty() {
            rows.push(cells);
        }
    }
    if rows.is_empty() {
        return String::new();
    }
    let width = rows.iter().map(Vec::len).max().unwrap_or(0);
    for row in &mut rows {
        row.resize(width, String::new());
    }
    let mut out = vec![format!("| {} |", rows[0].join(" | "))];
    out.push(format!("| {} |", vec!["---"; width].join(" | ")));
    for row in &rows[1..] {
        out.push(format!("| {} |", row.join(" | ")));
    }
    out.join("\n")
}

fn iter_tags(arena: &Arena, idx: usize, tag: &str, out: &mut Vec<usize>) {
    for &child in &arena.nodes[idx].children {
        if arena.nodes[child].tag == tag {
            out.push(child);
        }
        iter_tags(arena, child, tag, out);
    }
}

/// Approximate newline recovery for CodeMirror-rendered code.
///
/// The rendered DOM collapses a code block onto one line, preserving only the
/// original indentation runs; break there, then split runs at `;` `{` `}`
/// boundaries. Best-effort only: the RSC source keeps exact newlines.
fn reflow_code(text: &str) -> String {
    let text = text.trim_matches('\n');
    if text.is_empty() {
        return String::new();
    }
    let text = re_indent_run()
        .replace_all(text, |caps: &regex::Captures| {
            format!("\n{}", " ".repeat(caps[0].len()))
        })
        .into_owned();
    let mut out = Vec::new();
    for line in text.lines() {
        let stripped = line.trim();
        if stripped.is_empty() {
            continue;
        }
        let indent = &line[..line.len() - line.trim_start_matches(' ').len()];
        let mut piece = String::new();
        for ch in stripped.chars() {
            piece.push(ch);
            if matches!(ch, ';' | '{' | '}') {
                push_code_piece(&mut out, indent, &piece);
                piece.clear();
            }
        }
        push_code_piece(&mut out, indent, &piece);
    }
    out.join("\n")
}

fn push_code_piece(out: &mut Vec<String>, indent: &str, piece: &str) {
    let piece = piece.trim();
    if !piece.is_empty() {
        out.push(format!("{indent}{piece}"));
    }
}

fn re_indent_run() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r" {4,}").unwrap())
}

fn re_horizontal_space() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"[ \t]+").unwrap())
}

fn re_line_padding() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r" *\n *").unwrap())
}

fn re_blank_runs() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r"\n{3,}").unwrap())
}

// --- Front matter / output --------------------------------------------------

/// Serialise `value` as a JSON/YAML string literal.
fn yaml_string(value: &str) -> String {
    serde_json::to_string(value).unwrap_or_else(|_| format!("{value:?}"))
}

/// Format a Unix timestamp as local-time ISO 8601 (UTC offset reported as UTC).
fn iso_timestamp(ts: i64) -> String {
    let days = ts.div_euclid(86_400);
    let secs = ts.rem_euclid(86_400);
    let (year, month, day) = civil_from_days(days);
    let (hour, minute, second) = (secs / 3600, (secs % 3600) / 60, secs % 60);
    format!("{year:04}-{month:02}-{day:02}T{hour:02}:{minute:02}:{second:02}+00:00")
}

/// Convert a day count since the Unix epoch to a civil date.
fn civil_from_days(z: i64) -> (i64, i64, i64) {
    let z = z + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let d = doy - (153 * mp + 2) / 5 + 1;
    let m = if mp < 10 { mp + 3 } else { mp - 9 };
    (if m <= 2 { y + 1 } else { y }, m, d)
}

/// Render a conversation as markdown with YAML front matter.
fn to_markdown(conv: &Conversation, url: Option<&str>) -> String {
    let mut lines = vec!["---".to_string()];
    let title = conv.title.as_deref().unwrap_or("Chat conversation");
    if let Some(title) = conv.title.as_deref() {
        lines.push(format!("title: {}", yaml_string(title)));
    }
    if let Some(url) = url {
        lines.push(format!("source: {url}"));
    }
    if let Some(ts) = conv.create_time {
        lines.push(format!("date: {}", iso_timestamp(ts)));
    }
    if let Some(model) = conv.model.as_deref() {
        lines.push(format!("model: {model}"));
    }
    lines.push("---".to_string());
    lines.push(String::new());
    lines.push(format!("# {title}"));
    lines.push(String::new());
    for msg in &conv.messages {
        lines.push(format!("## {}", msg.role.label()));
        lines.push(String::new());
        lines.push(msg.markdown.clone());
        if !msg.sources.is_empty() {
            lines.push(String::new());
            lines.push("### Sources".to_string());
            lines.push(String::new());
            for (number, source) in msg.sources.iter().enumerate() {
                if number < msg.cited_count {
                    lines.push(format!("[^{}]: {}", number + 1, source.label));
                } else {
                    lines.push(format!("- {}", source.label));
                }
                lines.push(format!("  {}", source.url));
            }
        }
        lines.push(String::new());
    }
    lines.join("\n").trim_end().to_string() + "\n"
}

// --- CLI --------------------------------------------------------------------

#[derive(Parser)]
#[command(about, version)]
struct Cli {
    /// HTML file, or - for stdin (default: -)
    input: Option<PathBuf>,

    /// Source URL for front matter (default: canonical/og:url)
    #[arg(long)]
    url: Option<String>,

    /// Output markdown file (default: stdout)
    #[arg(long, short)]
    output: Option<PathBuf>,

    /// Fail instead of using the lossy DOM fallback
    #[arg(long)]
    strict: bool,

    /// Extractor to use (default: auto-detect): see `chat2md sources`
    #[arg(long, value_name = "NAME", default_value = "auto")]
    source: String,

    /// Drop inline citation markers instead of resolving them
    #[arg(long)]
    no_citations: bool,

    #[command(subcommand)]
    cmd: Option<Cmd>,
}

#[derive(Subcommand)]
enum Cmd {
    /// Generate shell completions
    Completions {
        /// The shell to generate completions for
        #[arg(value_enum)]
        shell: Shell,
    },
    /// List available extractors
    Sources,
}

fn read_input(path: Option<&Path>) -> Result<String> {
    match path {
        Some(p) if p != Path::new("-") => {
            std::fs::read_to_string(p).with_context(|| format!("reading {}", p.display()))
        }
        _ => {
            let mut buf = String::new();
            std::io::stdin()
                .read_to_string(&mut buf)
                .context("reading stdin")?;
            Ok(buf)
        }
    }
}

/// Drop bruvtab's `tab<TAB>title<TAB>url<TAB>payload` prefix when present.
///
/// Only the first three tabs are split on: the payload itself may contain tabs.
fn strip_bruvtab_prefix(doc: &str) -> String {
    if let Some(rest) = doc.split_once('\t') {
        let mut it = rest.1.splitn(3, '\t');
        if let (Some(_title), Some(_url), Some(payload)) = (it.next(), it.next(), it.next()) {
            return payload.to_string();
        }
    }
    doc.to_string()
}

fn re_canonical() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"<link[^>]+rel="canonical"[^>]+href="([^"]+)""#).unwrap())
}

fn re_og_url() -> &'static Regex {
    static RE: OnceLock<Regex> = OnceLock::new();
    RE.get_or_init(|| Regex::new(r#"<meta[^>]+property="og:url"[^>]+content="([^"]+)""#).unwrap())
}

fn detect_url(doc: &str) -> Option<String> {
    let caps = re_canonical()
        .captures(doc)
        .or_else(|| re_og_url().captures(doc))?;
    Some(decode_entities(caps.get(1)?.as_str()))
}

fn select_extractor<'a>(
    registry: &'a [Box<dyn ChatExtractor>],
    source: &str,
    html: &str,
) -> Result<&'a dyn ChatExtractor> {
    if source == "auto" {
        return registry
            .iter()
            .find(|e| e.matches(html))
            .map(Box::as_ref)
            .ok_or_else(|| {
                anyhow::anyhow!("no extractor recognized the input; see `chat2md sources`")
            });
    }
    registry
        .iter()
        .find(|e| e.name() == source)
        .map(Box::as_ref)
        .ok_or_else(|| anyhow::anyhow!("unknown source `{source}`; see `chat2md sources`"))
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match cli.cmd {
        Some(Cmd::Completions { shell }) => {
            let mut cmd = Cli::command();
            let name = cmd.get_name().to_string();
            clap_complete::generate(shell, &mut cmd, name, &mut std::io::stdout());
            return Ok(());
        }
        Some(Cmd::Sources) => {
            for extractor in registry() {
                println!("{}\t{}", extractor.name(), extractor.description());
            }
            return Ok(());
        }
        None => {}
    }

    let doc = strip_bruvtab_prefix(&read_input(cli.input.as_deref())?);
    let registry = registry();
    let extractor = select_extractor(&registry, &cli.source, &doc)?;
    let options = ExtractOptions {
        citations: !cli.no_citations,
    };
    let Some(conv) = extractor.extract(&doc, &options) else {
        bail!("no conversation found in input");
    };
    if cli.strict && conv.fidelity == Fidelity::Lossy {
        bail!("data payload unavailable; used lossy DOM fallback (--strict)");
    }
    if conv.fidelity == Fidelity::Lossy {
        eprintln!(
            "warning: {} used the lossy DOM fallback (code block newlines are heuristic)",
            extractor.name()
        );
    }

    let url = cli.url.or_else(|| detect_url(&doc));
    let markdown = to_markdown(&conv, url.as_deref());
    match cli.output.as_deref() {
        Some(path) => {
            std::fs::write(path, markdown).with_context(|| format!("writing {}", path.display()))?
        }
        None => print!("{markdown}"),
    }
    Ok(())
}
