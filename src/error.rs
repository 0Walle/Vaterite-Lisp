use crate::types::{Value, Arity};

#[derive(Clone)]
pub enum Error {
    Reason(String),
    ArgErr(Option<String>, Arity, u16),
    TypeErr(&'static str, Option<Value>),
    BindErr(String),
    Throw(Option<Value>),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
            Error::Reason(s) => write!(f, "{}", s),
            Error::BindErr(s) => write!(f, "name {} not found", s),
            Error::ArgErr(name, arity, got) => if let Some(name) = name {
                write!(f, "{} expected {} but got {}", name, arity, got)
            } else {
                write!(f, "expected {} but got {}", arity, got)
            }
            Error::TypeErr(expected, got) => if let Some(v) = got {
                write!(f, "expected type '{}' but got {:?}", expected, v)
            } else {
                write!(f, "expected type '{}'", expected)
            },
            Error::Throw(v) => write!(f, "thrown value '{:?}'", v.clone().unwrap_or(Value::Nil)),
        }
    }
}

impl From<&str> for Error { fn from(s: &str) -> Error { Error::Reason(s.to_string()) }}
impl From<String> for Error { fn from(s: String) -> Error { Error::Reason(s) }}