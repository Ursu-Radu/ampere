use std::{collections::HashMap, fmt::Display, fs, ops::Range, path::PathBuf};

use slotmap::{new_key_type, SlotMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeSpan {
    pub start: usize,
    pub end: usize,
}

impl CodeSpan {
    pub fn extend(self, other: CodeSpan) -> CodeSpan {
        (self.start..other.end).into()
    }
}

impl From<Range<usize>> for CodeSpan {
    fn from(r: Range<usize>) -> Self {
        Self {
            start: r.start,
            end: r.end,
        }
    }
}
impl From<CodeSpan> for Range<usize> {
    fn from(s: CodeSpan) -> Self {
        s.start..s.end
    }
}
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AmpereSource {
    File(PathBuf),
}

impl AmpereSource {
    pub fn read(&self) -> String {
        match self {
            AmpereSource::File(p) => fs::read_to_string(p).unwrap(),
        }
    }
    pub fn name(&self) -> String {
        match self {
            AmpereSource::File(p) => p.display().to_string(),
        }
    }
}

new_key_type! {
    pub struct SourceKey;
}

impl Display for SourceKey {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

pub type SourceMap = SlotMap<SourceKey, AmpereSource>;

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct CodeArea {
    pub span: CodeSpan,
    pub src: SourceKey,
}

#[derive(Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub span: CodeSpan,
}
impl<T> Spanned<T> {
    pub fn split(self) -> (T, CodeSpan) {
        (self.value, self.span)
    }
}

pub trait Spannable {
    fn spanned(self, span: CodeSpan) -> Spanned<Self>
    where
        Self: Sized;
}

impl<T> Spannable for T {
    fn spanned(self, span: CodeSpan) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned { value: self, span }
    }
}
