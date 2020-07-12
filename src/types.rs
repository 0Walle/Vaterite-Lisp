use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;
use crate::error::{Error};

pub struct FuncData {
    pub ast: Value,
    // pub params: Value,
    pub params: Vec<String>,
    pub opt_params: Vec<(String, Value)>,
    pub has_kwargs: bool,
    pub rest_param: Option<String>,
    pub name: String
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
    // pub func: fn(Vec<Value>) -> Result<Value, String>
    pub func: fn(Vec<Value>) -> Result<Value, Error>
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
    Sym(String),
    /// Keyword value, like symbols that evaluate to themselves
    Keyword(String),
    /// List value, the basic container of vaterite
    List(Rc<Vec<Value>>),
    /// Native function are used to define functions in rust that can be used in vaterite
    // NatFunc(fn(Vec<Value>) -> Result<Value, String>),
    NatFunc(NatFunc),
    /// Vaterite function, defined by vaterite code
    Func {
        env: Env,
        // eval: fn(Value, Env) -> Result<Value, String>,
        eval: fn(Value, Env) -> Result<Value, Error>,
        func: Rc<FuncData>,
        is_macro: bool,
    },
    /// Box refers to a vaterite value and is mutable
    Box(Rc<RefCell<Value>>),
    /// Lazy is used to implement lazy sequences
    Lazy{
        env: Env,
        // eval: fn(Value, Env) -> Result<Value, String>,
        eval: fn(Value, Env) -> Result<Value, Error>,
        head: Rc<Value>,
        tail: Rc<Value>
    },
    Map(Rc<HashMap<String,Value>>),
    Struct(Rc<String>, Rc<Vec<Value>>)
}

pub type ValueList = Vec<Value>;
type ValueResult = Result<Value, Error>;

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", Printer::str_(self))
    }
}

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
            (NatFunc(a), NatFunc(b)) => a.func == b.func,
            (Box(a), Box(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl From<&str> for Value { fn from(string: &str) -> Self { Value::Str(string.to_string()) }}
impl From<f64> for Value { fn from(number: f64) -> Self { Value::Num(number) }}
impl From<bool> for Value { fn from(b: bool) -> Self { if b { Value::True } else { Value::False }}}
impl From<Vec<Value>> for Value { fn from(ls: Vec<Value>) -> Self { if ls.len() == 0 { Value::Nil } else { Value::List(Rc::new(ls)) }}}

impl<T> From<Option<T>> for Value where T: Into<Value> { 
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
    pub fn apply(&self, args: ValueList) -> ValueResult {
        match self {
            Value::NatFunc(f) => {
                if !match f.arity {
                    Arity::Exact(n) => args.len() == n.into(),
                    Arity::Min(n) => args.len() >= n.into(),
                    Arity::Range(min, max) => min as usize <= args.len() && args.len() <= max.into(),
                } {
                    // return Err(format!("Invalid arguments for {}, expected {} but found {}", f.name, f.arity, args.len()))
                    return Err(Error::ArgErr(Some((*f.name).clone()), f.arity.clone(), args.len() as u16))
                }
                match (f.func)(args) {
                    // Err(err) => Err(format!("{}\n\tat {}", err, f.name)),
                    Err(err) => Err(format!("{}\n\tat {}", err, f.name).into()),
                    Ok(x) => Ok(x)
                }
            },
            Value::Func{
                func, eval, env, ..
            } => {
                let ast = &(**func).ast;
                let binds = (&func.params, &func.opt_params, &func.has_kwargs, &func.rest_param);
                let local_env = EnvStruct::bind(Some(env.clone()), binds, args, *eval)?;
                return Ok(match eval(ast.clone(), local_env) {
                    Ok(val) => val,
                    Err(err) => return Err(format!("{}\n\tat {}", err, func.name).into())
                })
                // if let Value::List(l) = &(**func).params {
                //     let binds = (l.clone(), func.opt_params.clone(), func.has_kwargs.clone(), func.rest_param.clone());
                //     let local_env = EnvStruct::bind(Some(env.clone()), binds, args, *eval)?;
                //     return Ok(match eval(ast.clone(), local_env) {
                //         Ok(val) => val,
                //         Err(err) => return Err(format!("{}\n\tat {}", err, func.name))
                //     })
                // } else {
                //     return Err("Parameter list is not a list".to_string())
                // }
            },
            _ => Err(format!("Attempt to call non-function {}", self).into()),
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

    pub fn rest(&self) -> ValueResult {
        match self {
            Value::List(l) => {
                if l.len() == 0 {
                    Ok(Value::Nil)
                }else{
                    Ok(l[1..].to_vec().into())
                }
            },
            Value::Nil => Ok(Value::Nil),
            Value::Lazy{env, eval, tail, ..} => eval((**tail).clone(), env.clone()),
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

    pub fn from_sym(&self) -> Option<String> {
        match &self {
            Value::Sym(s) => Some(s.clone()),
            _ => None
        }
    }
}

/// Helper to make native functions
pub fn func(name: &str, arity: Arity, func: fn(ValueList) -> Result<Value, Error>) -> Value {
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
#[derive(Debug, Clone)]
pub struct EnvStruct {
    pub data: RefCell<HashMap<String, Value>>,
    pub access: Option<Env>,
}

impl EnvStruct {
    /// Creates a new environment optionaly with another environment to access
    pub fn new(access: Option<Env>) -> Env {
        Rc::new(EnvStruct{
            access, data: RefCell::new(HashMap::default())
        })
    }

    /// Creates a new environment and bind values acordingly to a lambda list
    pub fn bind(access: Option<Env>, binds: (&Vec<String>, &Vec<(String, Value)>, &bool, &Option<String>), exprs: ValueList, eval: fn(Value, Env) -> ValueResult) -> Result<Env, Error> {
        let env = EnvStruct::new(access);
        let (req, opt, keys, rest) = binds;
        let req_len = req.len();
        let opt_len = opt.len() * if *keys { 2 } else { 1 };

        if exprs.len() < req_len {
            return Err("not enough arguments passed to function".into())
            // return Error::ArgErr(None, )
        }
        for (i, b) in req.iter().enumerate() {
            env.set(b.clone(), exprs[i].clone());
        }
        let exprs = exprs[req_len..].to_vec();
        if *keys {
            if exprs.len() <= opt_len && exprs.len() % 2 != 0 {
                return Err("Keyword arguments aren't in pairs".into())
            }

            'next_key: for (_, b) in opt.iter().enumerate() {
                let key = &b.0;
                for i in (0..exprs.len()).step_by(2) {
                    if let Value::Keyword(kw) = &exprs[i] {
                        if key == kw {
                            env.set(key.clone(), exprs[i + 1].clone());
                            continue 'next_key;
                        }
                    }
                }
                env.set(key.clone(), eval(b.1.clone(), env.clone())?);
            }
        } else {
            for (i, b) in opt.iter().enumerate() {
                match exprs.get(i) {
                    Some(expr) => {
                        env.set(b.0.clone(), expr.clone());
                    }
                    None => {
                        env.set(b.0.clone(), eval(b.1.clone(), env.clone())?);
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

pub struct Printer {}

impl Printer {
    pub fn repr_(value: &Value, level: i32) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{:?}", s),
            Value::Char(s) => format!("#'{:?}'", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => if level == 0 {
                    format!("'{}", s)
                } else {
                    format!("{}", s)
                },
            Value::Keyword(s) => format!(":{}", s),
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_(x, level + 1)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_(expr, level + 1)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => format!("[Function {}]", func.name),
            Value::Box(val) => format!("(box {})", Printer::repr_(&(val.borrow()), level)),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {})", Printer::repr_(&*head, level), tail),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}:{} {}\n", (0..level+1).map(|_| "  ").collect::<String>(), k, Printer::repr_(v, level+1)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({} ", id));
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_(x, level + 1)))
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_(expr, level + 1)))
                }
                res.push_str("]");
                res
            }
        }
    }

    pub fn repr_color(value: &Value, level: i32) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("\x1b[93m#t\x1b[0m"),
            Value::False => format!("\x1b[93m#f\x1b[0m"),
            Value::Num(n) => format!("\x1b[93m{}\x1b[0m", n),
            Value::Str(s) => format!("\x1b[32m{:?}\x1b[0m", s),
            Value::Char(s) => format!("\x1b[93m#'{:?}'\x1b[0m", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => if level == 0 {
                    format!("'{}", s)
                } else {
                    format!("{}", s)
                },
            Value::Keyword(s) => format!("\x1b[32m:{}\x1b[0m", s),
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_color(x, level + 1)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_color(expr, level + 1)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(func) => format!("\x1b[36m[NativeFunction {}]\x1b[0m", func.name),
            Value::Func { func, .. } => format!("\x1b[36m[Function {}]\x1b[0m", func.name),
            Value::Box(val) => format!("(box {})", Printer::repr_color(&(val.borrow()), level)),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {})", Printer::repr_color(&*head, level), tail),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}:{} {}\n", (0..level+1).map(|_| "  ").collect::<String>(), k, Printer::repr_color(v, level+1)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({} ", id));
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_color(x, level + 1)))
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_color(expr, level + 1)))
                }
                res.push_str("]");
                res
            }
        }
    }

    // pub fn repr(value: &Value) -> String {
    //     Printer::repr_(value, 0)
    // }

    pub fn str_(value: &Value) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{}", s),
            Value::Char(s) => format!("{}", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => format!("{}", s),
            Value::Keyword(s) => format!(":{}", s),
            Value::List(list) => {
                let mut res = String::new();
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::str_(x)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::str_(expr)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => format!("[Function {}]", func.name),
            Value::Box(val) => format!("(box {})", Printer::str_(&(val.borrow()))),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {:?})", Printer::str_(&*head), tail),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[ ");
                for (k, v) in map.iter() {
                    res.push_str(&format!(":{} {} ", k, Printer::str_(v)));
                };
                res.push_str("]");
                res
            }
            Value::Struct(id, _) => format!("[Struct {}]", id)
        }
    }
}