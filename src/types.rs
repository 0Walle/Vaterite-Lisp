use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use crate::error::{Error};
use crate::printer::Printer;
use crate::names::{Name, NamePool};

pub struct FuncData {
    pub ast: Value,
    pub params: Vec<String>,
    pub opt_params: Vec<(String, Value)>,
    pub has_kwargs: bool,
    pub rest_param: Option<String>,
    pub name: Option<String>,
    pub arity: Arity,
}

#[derive(Clone)]
pub enum Arity {
    Exact(u16),
    Min(u16),
    Range(u16, u16),
}

#[derive(Clone)]
pub struct NatFunc {
    pub name: Rc<String>,
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
    /// Chars value
    Chars(Box<[char]>),
    /// String value
    Str(String),
    /// Symbol value, used to bind a value to env
    // Sym(String),
    Sym(Name),
    /// Keyword value, like symbols that evaluate to themselves
    Keyword(Name),
    // Keyword(String),
    /// List value, the basic container of vaterite
    List(Rc<Vec<Value>>),
    /// Native function are used to define functions in rust that can be used in vaterite
    NatFunc(NatFunc),
    /// Vaterite function, defined by vaterite code
    Func {
        env: Env,
        eval: fn(Value, Env, &NamePool) -> Result<Value, Error>,
        func: Rc<FuncData>,
        is_macro: bool,
    },
    /// Box refers to a vaterite value and is mutable
    Box(Rc<RefCell<Value>>),
    /// Lazy is used to implement lazy sequences
    Lazy{
        env: Env,
        eval: fn(Value, Env, &NamePool) -> Result<Value, Error>,
        head: Rc<Value>,
        tail: Rc<Value>
    },
    // Map(Rc<HashMap<String,Value>>),
    Map(Rc<HashMap<Name, Value>>),
    Struct(Rc<String>, Rc<Vec<Value>>)
}

pub type ValueList = Vec<Value>;
type ValueResult = Result<Value, Error>;

// impl std::fmt::Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(f, "{}", Printer::str_(self))
//     }
// }

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", Printer::repr_(self, 0))
    }
}

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
            (Chars(a), Nil) => a.len() == 0,
            (Nil, Chars(a)) => a.len() == 0,
            (Func{func: a, ..}, Func{func: b, ..}) => Rc::ptr_eq(a, b),
            (NatFunc(a), NatFunc(b)) => a.name == b.name,
            (Box(a), Box(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl From<&str> for Value { fn from(string: &str) -> Self { Value::Str(string.to_string()) }}
impl From<String> for Value { fn from(string: String) -> Self { Value::Str(string) }}
impl From<Name> for Value { fn from(name: Name) -> Self { Value::Sym(name) }}
impl From<f64> for Value { fn from(number: f64) -> Self { Value::Num(number) }}
impl From<bool> for Value { fn from(b: bool) -> Self { if b { Value::True } else { Value::False }}}
impl From<Vec<Value>> for Value { fn from(ls: Vec<Value>) -> Self { if ls.len() == 0 { Value::Nil } else { Value::List(Rc::new(ls)) }}}

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
                    return Err(Error::ArgErr(Some((*f.name).clone()), f.arity.clone(), args.len() as u16))
                }
                match (f.func)(args, names) {
                    Err(err) => Err(format!("{}\n\tat {}", err, f.name).into()),
                    Ok(x) => Ok(x)
                }
            },
            Value::Func{
                func, eval, env, ..
            } => {
                let ast = &(**func).ast;
                let binds = &*func;
                let local_env = EnvStruct::bind(Some(env.clone()), binds, args, *eval, names)?;
                return Ok(match eval(ast.clone(), local_env, names) {
                    Ok(val) => val,
                    Err(err) => return Err(format!("{}\n\tat {}", err, (func.name).as_ref().unwrap_or(&"lambda".to_string())).into())
                })
            },
            _ => Err(format!("Attempt to call non-function {}", Printer::repr_name(self, 0, names)).into()),
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
            Value::List(l) => {
                if l.len() == 0 {
                    Ok(Value::Nil)
                }else{
                    Ok(l[0].clone())
                }
            },
            Value::Nil => Ok(Value::Nil),
            Value::Lazy{head, ..} => Ok((**head).clone()),
            Value::Chars(s) => match s.get(0) {
                // Some(ch) => Ok(Value::Num(*ch as i32 as f64)),
                Some(ch) => Ok(Value::Char(*ch)),
                None => Ok(Value::Nil)
            },
            // _ => Err("Value is not a ordered collection".to_string()),
            x => Err(Error::TypeErr("collection", Some(x.clone()))),
        }
    }

    pub fn rest(&self, names: &NamePool) -> ValueResult {
        match self {
            Value::List(l) => {
                if l.len() == 0 {
                    Ok(Value::Nil)
                }else{
                    Ok(l[1..].to_vec().into())
                }
            },
            Value::Nil => Ok(Value::Nil),
            Value::Lazy{env, eval, tail, ..} => eval((**tail).clone(), env.clone(), names),
            Value::Chars(s) => if s.len() != 1 {
                Ok(Value::Chars(s[1..].into()))
            } else {
                Ok(Value::Nil)
            },
            // _ => Err("Value is not a ordered collection".to_string()),
            x => Err(Error::TypeErr("collection", Some(x.clone())))
        }
    }

    pub fn to_vec(&self) -> Option<Rc<ValueList>> {
        match &self {
            Value::List(ls) => Some(ls.clone()),
            Value::Nil => Some(Rc::new(Vec::default())),
            _ => None
        }
    }

    pub fn to_pair(&self) -> Option<(Value, Value)> {
        match &self {
            Value::List(ls) if ls.len() == 2 => Some((ls[0].clone(), ls[1].clone())),
            _ => None
        }
    }

    pub fn from_sym(&self, names: &NamePool) -> Option<String> {
        match &self {
            Value::Sym(s) => Some(names.get(*s)),
            _ => None
        }
    }
}

/// Helper to make native functions
pub fn func(name: &str, arity: Arity, func: fn(ValueList, &NamePool) -> Result<Value, Error>) -> Value {
    Value::NatFunc(NatFunc {
        name: Rc::new(String::from(name)),
        func,
        arity,
    })
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
    pub data: RefCell<HashMap<String, Value>>,
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
            return Err(Error::KwArgErr(binds.name.clone()))
        }

        for (i, b) in req.iter().enumerate() {
            env.set(b.clone(), exprs[i].clone());
        }
        // let exprs = exprs[req_len..].to_vec();
        let exprs = &exprs[req_len..];
        if *keys {
            'next_key: for (_, b) in opt.iter().enumerate() {
                let key = &b.0;
                for i in (0..exprs.len()).step_by(2) {
                    if let Value::Keyword(kw) = &exprs[i] {
                        if key == &names.get(*kw) {
                            env.set(key.clone(), exprs[i + 1].clone());
                            continue 'next_key;
                        }
                    }
                }
                env.set(key.clone(), eval(b.1.clone(), env.clone(), names)?);
            }
        } else {
            for (i, b) in opt.iter().enumerate() {
                match exprs.get(i) {
                    Some(expr) => {
                        env.set(b.0.clone(), expr.clone());
                    }
                    None => {
                        env.set(b.0.clone(), eval(b.1.clone(), env.clone(), names)?);
                    }
                }
            }
        }
        if let Some(rest) = rest {
            if exprs.len() > opt_len {
                env.set(rest.clone(), exprs[opt_len..].to_vec().into());
            } else {
                env.set(rest.clone(), Value::Nil);
            }
        };
        Ok(env)
    }

    /// Get an binding from the environment
    pub fn get(&self, key: String) -> ValueResult {
        match self.data.borrow().get(&key) {
            Some(e) => Ok(e.clone()),
            None => {
                if let Some(env) = &self.access {
                    match env.get(key) {
                        Ok(e) => Ok(e),
                        Err(err) => Err(err)
                    }
                }else {
                    // Err(format!("'{}' not found", key))
                    Err(Error::BindErr(key))
                }
            }
        }
    }

    /// Set an binding in the environment
    pub fn set(&self, key: String, value: Value){
        self.data.borrow_mut().insert(key, value);
    }

    /// Search and set an binding in the environment
    pub fn assign(&self, key: String, value: Value) -> ValueResult {
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