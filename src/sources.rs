use std::{collections::HashMap, fs, ops::Range, path::PathBuf};

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
    pub fn str(&self) -> String {
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

#[derive(Debug, Clone)]
pub struct CodeArea {
    pub span: CodeSpan,
    pub source: AmpereSource,
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
