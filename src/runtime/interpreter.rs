use ahash::AHashMap;
use lasso::Spur;
use slotmap::{new_key_type, SlotMap};

use super::value::Value;

new_key_type! {
   pub struct ValueKey;
   pub struct ScopeKey;
}

pub struct Scope {
    vars: AHashMap<Spur, ValueKey>,
    parent: Option<ScopeKey>,
}

pub struct Globals {
    memory: SlotMap<ValueKey, Value>,
    scopes: SlotMap<ScopeKey, Scope>,
}
