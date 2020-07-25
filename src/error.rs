use crate::types::{Value, Arity};
use crate::names::{Name};

#[derive(Clone)]
pub enum Error {
    Reason(String),
    ArgErr(Option<Name>, Arity, u16),
    KwArgErr(Option<Name>),
    TypeErr(&'static str, Option<Value>),
    CallErr(Option<Value>),
    KeyErr(Name),
    BindErr(Name),
    PairErr(Option<&'static str>),
    Throw(Option<Value>),
}

impl From<&str> for Error { fn from(s: &str) -> Error { Error::Reason(s.to_string()) }}
impl From<String> for Error { fn from(s: String) -> Error { Error::Reason(s) }}