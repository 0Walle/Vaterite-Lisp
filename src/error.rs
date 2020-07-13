use crate::types::{Value, Arity};

#[derive(Clone)]
pub enum Error {
    Reason(String),
    ArgErr(Option<String>, Arity, u16),
    KwArgErr(Option<String>),
    TypeErr(&'static str, Option<Value>),
    BindErr(String),
    PairErr(Option<String>),
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
            Error::BindErr(s) => write!(f, "Name {} not found", s),
            Error::KwArgErr(name) => if let Some(name) = name {
                write!(f, "Keyword arguments are not in pairs calling {}", name)
            } else {
                write!(f, "Keyword arguments are not in pairs")
            }
            Error::ArgErr(name, arity, got) => if let Some(name) = name {
                write!(f, "{} expected {} but got {}", name, arity, got)
            } else {
                write!(f, "Expected {} but got {}", arity, got)
            }
            Error::TypeErr(expected, got) => if let Some(v) = got {
                write!(f, "Expected value of type '{}' but got {:?}", expected, v)
            } else {
                write!(f, "Expected value of type '{}'", expected)
            },
            Error::PairErr(name) => if let Some(v) = name {
                write!(f, "{} must be a pair", v)
            } else {
                write!(f, "Expected a pair")
            },
            Error::Throw(v) => write!(f, "Thrown value '{:?}'", v.clone().unwrap_or(Value::Nil)),
        }
    }
}

impl From<&str> for Error { fn from(s: &str) -> Error { Error::Reason(s.to_string()) }}
impl From<String> for Error { fn from(s: String) -> Error { Error::Reason(s) }}