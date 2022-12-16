use std::{
    collections::HashMap,
    fmt::Display,
    fs,
    ops::{Index, IndexMut, Range},
    path::PathBuf,
};

use ahash::AHashMap;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use crate::runtime::interpreter::ValueKey;

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
    pub fn read(&self) -> Option<String> {
        match self {
            AmpereSource::File(p) => fs::read_to_string(p).ok(),
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

// pub type SourceMap = SlotMap<SourceKey, AmpereSource>;

pub struct SrcData {
    pub src: AmpereSource,
    pub exports: AHashMap<Spur, ValueKey>,
}

pub struct SourceMap {
    pub slotmap: SlotMap<SourceKey, SrcData>,
    pub hashmap: AHashMap<AmpereSource, SourceKey>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            slotmap: SlotMap::default(),
            hashmap: AHashMap::new(),
        }
    }
    pub fn insert(&mut self, src: AmpereSource) -> SourceKey {
        match self.hashmap.get(&src) {
            Some(k) => *k,
            None => {
                let k = self.slotmap.insert(SrcData {
                    src: src.clone(),
                    exports: AHashMap::new(),
                });
                self.hashmap.insert(src, k);
                k
            }
        }
    }
}

impl Index<SourceKey> for SourceMap {
    type Output = SrcData;

    fn index(&self, index: SourceKey) -> &Self::Output {
        &self.slotmap[index]
    }
}
impl IndexMut<SourceKey> for SourceMap {
    fn index_mut(&mut self, index: SourceKey) -> &mut Self::Output {
        &mut self.slotmap[index]
    }
}
// impl Index<AmpereSource> for SourceMap {
//     type Output = AHashMap<Spur, ValueKey>;

//     fn index(&self, index: AmpereSource) -> &Self::Output {
//         &self.slotmap[self.hashmap[&index]].exports
//     }
// }

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
