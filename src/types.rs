use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use std::ops::{Deref};

use crate::error::{Error};
use crate::printer::Printer;
use crate::names::{Name, NamePool};

pub struct FuncData {
    pub ast: Value,
    pub params: Vec<Name>,
    pub opt_params: Vec<(Name, Value)>,
    pub has_kwargs: bool,
    pub rest_param: Option<Name>,
    pub is_macro: bool,
    pub name: Option<Name>,
    pub arity: Arity,
}

pub struct LazyData {
    pub head: Value,
    pub tail: Value,
}

pub struct StructData {
    pub fields: Vec<Name>,
    pub name: Name,
}

#[derive(Clone)]
pub enum Arity {
    Exact(u16),
    Min(u16),
    Range(u16, u16),
}

#[derive(Clone)]
pub struct NatFunc {
    pub name: Name,
    pub arity: Arity,
    pub func: fn(Vec<Value>, &NamePool) -> Result<Value, Error>
}

/// Enum for the types used by vaterite
#[derive(Clone)]
pub enum Value {
    /// Nil value represents an empty list
    Nil,
    /// True value
    True,
    /// False value
    False,
    /// Number value is 64 bit float
    Num(f64),
    /// Char value
    Char(char),
    /// String value
    Str(SliceString),
    /// Symbol value, used to bind a value to env
    Sym(Name),
    /// Keyword value, like symbols that evaluate to themselves
    Keyword(Name),
    /// List value, the basic container of vaterite
    List(SliceList),
    /// Native function are used to define functions in rust that can be used in vaterite
    NatFunc(NatFunc),
    /// Vaterite function, defined by vaterite code
    Func {
        env: Env,
        eval: fn(Value, Env, &NamePool) -> Result<Value, Error>,
        func: Rc<FuncData>,
    },
    /// Box refers to a vaterite value and is mutable
    Box(Rc<RefCell<Value>>),
    /// Lazy is used to implement lazy sequences
    Lazy{
        env: Env,
        eval: fn(Value, Env, &NamePool) -> Result<Value, Error>,
        data: Rc<LazyData>,
    },
    Map(Rc<HashMap<Name, Value>>),
    StructDef(Rc<StructData>),
    Struct(Rc<StructData>, Rc<Vec<Value>>)
}

pub type ValueList = Vec<Value>;
type ValueResult = Result<Value, Error>;

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (True, True) => true,
            (False, False) => true,
            (Num(a), Num(b)) => a == b,
            (Str(a), Str(b)) => a == b,
            (Sym(a), Sym(b)) => a == b,
            (Keyword(a), Keyword(b)) => a == b,
            (List(a), List(b)) => a == b,
            (List(a), Nil) => a.len() == 0,
            (Nil, List(a)) => a.len() == 0,
            (Str(a), Nil) => a.len() == 0,
            (Nil, Str(a)) => a.len() == 0,
            (Func{func: a, ..}, Func{func: b, ..}) => Rc::ptr_eq(a, b),
            (NatFunc(a), NatFunc(b)) => a.name == b.name,
            (Box(a), Box(b)) => Rc::ptr_eq(a, b),
            (StructDef(a), StructDef(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl From<&str> for Value { fn from(string: &str) -> Self { Value::Str(string.into()) }}
impl From<String> for Value { fn from(string: String) -> Self { Value::Str(string.into()) }}
impl From<SliceString> for Value { fn from(string: SliceString) -> Self { Value::Str(string) }}
impl From<SliceList> for Value { fn from(list: SliceList) -> Self { Value::List(list) }}
impl From<Name> for Value { fn from(name: Name) -> Self { Value::Sym(name) }}
impl From<f64> for Value { fn from(number: f64) -> Self { Value::Num(number) }}
impl From<char> for Value { fn from(c: char) -> Self { Value::Char(c) }}
impl From<bool> for Value { fn from(b: bool) -> Self { if b { Value::True } else { Value::False }}}
impl From<Vec<Value>> for Value { fn from(ls: Vec<Value>) -> Self { if ls.len() == 0 { Value::Nil } else { Value::List(ls.into()) }}}

impl<T> From<Option<T>> for Value
    where T: Into<Value> { 
    fn from(opt: Option<T>) -> Self { 
        if let Some(v) = opt {
            v.into()
        } else { 
            Value::Nil
        }
    }
}

impl Value {
    /// Apply arguments to a vaterite expression that must be a function
    pub fn apply(&self, args: ValueList, names: &NamePool) -> ValueResult {
        match self {
            Value::NatFunc(f) => {
                if !match f.arity {
                    Arity::Exact(n) => args.len() == n.into(),
                    Arity::Min(n) => args.len() >= n.into(),
                    Arity::Range(min, max) => min as usize <= args.len() && args.len() <= max.into(),
                } {
                    return Err(Error::ArgErr(Some(f.name), f.arity.clone(), args.len() as u16))
                }
                match (f.func)(args, names) {
                    Err(err) => Err(format!("{}\n\tat {}", Printer::str_error(&err, names), names.get(f.name)).into()),
                    Ok(x) => Ok(x)
                }
            },
            Value::StructDef(data) => {
                if data.fields.len() != args.len() {
                    return Err("Invalid number of fields in struct".into())
                }
                Ok(Value::Struct(data.clone(),Rc::new(args)))
            }
            Value::Func{
                func, eval, env, ..
            } => {
                let ast = &(**func).ast;
                let binds = &*func;
                let local_env = EnvStruct::bind(Some(env.clone()), binds, args, *eval, names)?;
                return Ok(match eval(ast.clone(), local_env, names) {
                    Ok(val) => val,
                    Err(err) => return match func.name {
                        Some(name) => Err(format!("{}\n\tat {}", Printer::str_error(&err, names), names.get(name)).into()),
                        None => Err(format!("{}\n\tat lambda", Printer::str_error(&err, names)).into())
                    }
                })
            },
            _ => Err(format!("Attempt to call non-function {}", Printer::repr_name(self, names)).into()),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::List(l) => l.len() == 0,
            _ => false,
        }
    }

    pub fn is_false(&self) -> bool {
        match self {
            Value::False => true,
            Value::Nil => true,
            Value::List(l) => l.len() == 0,
            _ => false,
        }
    }

    pub fn first(&self) -> ValueResult {
        match self {
            Value::List(l) => Ok(l.head().into()),
            Value::Nil => Ok(Value::Nil),
            Value::Str(s) => Ok(s.head().into()),
            Value::Lazy{data, ..} => Ok(data.head.clone()),
            x => Err(Error::TypeErr("collection", Some(x.clone()))),
        }
    }

    pub fn rest(&self, names: &NamePool) -> ValueResult {
        match self {
            Value::List(l) => Ok(l.tail().into()),
            Value::Str(s) => Ok(s.tail().into()),
            Value::Nil => Ok(Value::Nil),
            Value::Lazy{eval, data, env} => eval(data.tail.clone(), env.clone(), names),
            x => Err(Error::TypeErr("collection", Some(x.clone())))
        }
    }

    pub fn to_vec(&self) -> Option<&[Value]> {
        match &self {
            Value::List(ls) => Some(ls),
            Value::Nil => Some(&[]),
            _ => None
        }
    }

    pub fn to_pair(&self) -> Option<(Value, Value)> {
        match &self {
            Value::List(ls) if ls.len() == 2 => Some((ls[0].clone(), ls[1].clone())),
            _ => None
        }
    }

    pub fn to_name(&self) -> Option<Name> {
        match &self {
            Value::Sym(name) => Some(*name),
            _ => None
        }
    }
}

/// Helper to make native functions
pub fn func(name: Name, arity: Arity, func: fn(ValueList, &NamePool) -> Result<Value, Error>) -> Value {
    Value::NatFunc(NatFunc { name, func, arity })
}

impl std::fmt::Display for Arity {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match &self {
           Arity::Exact(n) => write!(f, "{} arguments", n),
           Arity::Min(n) => write!(f, "at least {} arguments", n),
           Arity::Range(min, max) => write!(f, "{} to {} arguments", min, max),
        }
    }
}

/// An vaterite environment maps expressions to strings
#[derive(Clone)]
pub struct EnvStruct {
    pub data: RefCell<HashMap<Name, Value>>,
    pub access: Option<Env>,
}

impl EnvStruct {
    /// Creates a new environment optionaly with another environment to access
    pub fn new(access: Option<Env>) -> Env {
        Rc::new(EnvStruct {
            access,
            data: RefCell::new(HashMap::default())
        })
    }

    /// Creates a new environment and bind values acordingly to a lambda list
    pub fn bind(access: Option<Env>, binds: &FuncData, exprs: ValueList, eval: fn(Value, Env, &NamePool) -> ValueResult, names: &NamePool) -> Result<Env, Error> {
        let env = EnvStruct::new(access);
        let (req, opt, keys, rest) = (&binds.params, &binds.opt_params, &binds.has_kwargs, &binds.rest_param);
        let req_len = req.len();
        let opt_len = opt.len() * if *keys { 2 } else { 1 };
        let args_len = exprs.len();

        if !match binds.arity {
            Arity::Exact(n) => args_len == n.into(),
            Arity::Min(n) => args_len >= n.into(),
            Arity::Range(min, max) => min as usize <= args_len && args_len <= max.into(),
        } {
            return Err(Error::ArgErr(binds.name.clone(), binds.arity.clone(), args_len as u16))
        }

        if *keys && (args_len - req_len) % 2 != 0 {
            return Err(Error::KwArgErr(binds.name))
        }

        for (i, b) in req.iter().enumerate() {
            env.set(*b, exprs[i].clone());
        }
        let exprs = &exprs[req_len..];
        if *keys {
            'next_key: for (_, b) in opt.iter().enumerate() {
                let key = &b.0;
                for i in (0..exprs.len()).step_by(2) {
                    if let Value::Keyword(kw) = &exprs[i] {
                        if key == kw {
                            env.set(*key, exprs[i + 1].clone());
                            continue 'next_key;
                        }
                    }
                }
                env.set(*key, eval(b.1.clone(), env.clone(), names)?);
            }
        } else {
            for (i, (name, value)) in opt.iter().enumerate() {
                match exprs.get(i) {
                    Some(expr) => {
                        env.set(*name, expr.clone());
                    }
                    None => {
                        env.set(*name, eval(value.clone(), env.clone(), names)?);
                    }
                }
            }
        }
        if let Some(rest) = rest {
            if exprs.len() > opt_len {
                env.set(*rest, exprs[opt_len..].to_vec().into());
            } else {
                env.set(*rest, Value::Nil);
            }
        };
        Ok(env)
    }

    /// Get an binding from the environment
    pub fn get(&self, key: Name) -> ValueResult {
        match self.data.borrow().get(&key) {
            Some(e) => Ok(e.clone()),
            None => {
                if let Some(env) = &self.access {
                    match env.get(key) {
                        Ok(e) => Ok(e),
                        Err(err) => Err(err)
                    }
                }else {
                    Err(Error::BindErr(key))
                }
            }
        }
    }

    /// Set an binding in the environment
    pub fn set(&self, key: Name, value: Value){
        self.data.borrow_mut().insert(key, value);
    }

    /// Search and set an binding in the environment
    pub fn assign(&self, key: Name, value: Value) -> ValueResult {
        match self.data.borrow_mut().insert(key.clone(), value.clone()) {
            Some(v) => Ok(v),
            None => if let Some(env) = &self.access {
                env.assign(key, value)
            } else {
                Err(Error::BindErr(key))
            }
        }
    }
}

pub type Env = Rc<EnvStruct>;

#[derive(Clone)]
pub struct SliceString {
    string: Rc<String>,
    start: usize
}

impl From<String> for SliceString {
    fn from(s: String) -> Self {
        SliceString {
            string: Rc::new(s),
            start: 0
        }
    }
}
impl From<&str> for SliceString {
    fn from(s: &str) -> Self {
        SliceString {
            string: Rc::new(s.to_owned()),
            start: 0
        }
    }
}

impl PartialEq for SliceString {
    fn eq(&self, other: &SliceString) -> bool {
        self.inner() == other.inner()
    }
}

impl SliceString {
    pub fn len(&self) -> usize {
        self.string.len() - self.start
    }

    pub fn inner(&self) -> &str {
        &self.string[self.start..]
    }

    pub fn slice(&self, start: usize) -> Option<SliceString> {
        let st = self.start + start;

        if st > self.string.len() {
            return None
        }

        if !self.string.is_char_boundary(st) {
            return None
        }

        Some(SliceString {
            string: self.string.clone(),
            start: st,
        })
    }

    pub fn head(&self) -> Option<char> {
        (&self.string[self.start..]).chars().next()
    }

    pub fn tail(&self) -> Option<SliceString> {
        match self.string.chars().next() {
            Some(ch) => {
                if self.start + ch.len_utf8() == self.string.len() {
                    return None
                };
                Some(SliceString {
                    string: self.string.clone(),
                    start: self.start + ch.len_utf8(),
                })
            },
            None => None
        }
    }
}

#[derive(Clone)]
pub struct SliceList {
    data: Rc<Vec<Value>>,
    start: usize
}

impl PartialEq for SliceList {
    fn eq(&self, other: &Self) -> bool {
        self.inner() == other.inner()
    }
}

impl From<Vec<Value>> for SliceList {
    fn from(l: Vec<Value>) -> Self {
        SliceList {
            data: Rc::new(l),
            start: 0
        }
    }
}

impl Deref for SliceList {
    type Target = [Value];

    fn deref(&self) -> &[Value] {
        self.inner()
    }
}

// impl<'a> IntoIterator for &'a SliceList {
//     type Item = &'a Value;
//     type IntoIter = Iter<'a, Value>;

//     fn into_iter(self) -> Iter<'a, Value> {
//         self.data.iter()
//     }
// }

impl SliceList {
    pub fn len(&self) -> usize {
        self.data.len() - self.start
    }

    pub fn inner(&self) -> &[Value] {
        &self.data[self.start..]
    }

    pub fn get(&self, i: usize) -> Option<&Value> {
        self.data.get(self.start + i)
    }

    pub fn head(&self) -> Option<Value> {
        (&self.data[self.start..]).iter().next().cloned()
    }

    pub fn tail(&self) -> Option<SliceList> {
        match self.data.iter().next() {
            Some(_) => {
                if self.start + 1 == self.data.len() {
                    return None
                };
                Some(SliceList {
                    data: self.data.clone(),
                    start: self.start + 1,
                })
            },
            None => None
        }
    }
}