use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

pub static mut USE_COLORS: bool = false;

/// Enum for the types used by vaterite
#[derive(Clone)]
pub enum Value {
    /// Nil value represents an empty list
    Nil,
    /// Number value is 64 bit float
    Num(f64),
    /// String value
    Str(String),
    /// Symbol value, used to bind a value to env
    Sym(String),
    /// Keyword value, like symbols that evaluate to themselves
    Keyword(String),
    /// List value, the basic container of vaterite
    List(Rc<Vec<Value>>),
    /// Native function are used to define functions in rust that can be used in vaterite
    NatFunc(fn(Vec<Value>) -> Result<Value, String>),
    /// Vaterite function, defined by vaterite code
    Func {
        eval: fn(Value, Env) -> Result<Value, String>,
        ast: Rc<Value>,
        env: Env,
        params: Rc<Value>,
        opt_params: Rc<Vec<(String, Value)>>,
        has_kwargs: bool,
        rest_param: Option<Rc<String>>,
        is_macro: bool,
    },
    /// Box refers to a vaterite value and is mutable
    Box(Rc<RefCell<Value>>),
    /// Lazy is used to implement lazy sequences
    Lazy{
        env: Env,
        eval: fn(Value, Env) -> Result<Value, String>,
        head: Rc<Value>,
        tail: Rc<Value>
    },
    Map(Rc<HashMap<String,Value>>)
}

pub type ValueList = Vec<Value>;
pub type ValueErr = Result<Value, String>;

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Value::Nil => write!(f, "()"),
            Value::Num(n) => write!(f, "{}", n),
            Value::Str(s) => write!(f, "{}", s),
            Value::Keyword(s) => write!(f, "{}", s),
            Value::Sym(s) => write!(f, "{}", s),
            Value::List(list) => {
                f.write_str("(")?;
                let mut it = list.iter();
                match it.next() {
                    Some(x) => write!(f, "{}", x)?,
                    None => {}
                }
                for expr in it {
                    write!(f," {}", expr)?;
                }
                f.write_str(")")?;
                Ok(())
            },
            Value::NatFunc(_) => write!(f, "[Function]"),
            Value::Func { .. } => write!(f, "[Lambda]"),
            Value::Box(val) => write!(f, "{}", val.borrow()),
            Value::Lazy{head, tail, ..} => write!(f, "(lazy-cons {} {})", head, tail),
            Value::Map(map) => {
                f.write_str("(")?;
                let mut it = map.iter();
                if let Some((k, v)) = it.next() {
                    write!(f, ":{} {}", k, v)?;
                }
                for (k, v) in it {
                    write!(f, ", :{} {}", k, v)?;
                };
                f.write_str(")")?;
                Ok(())
            }
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            match self {
                Value::Nil => write!(f, "()"),
                Value::Num(n) => if USE_COLORS { 
                    write!(f, "\x1b[93m{}\x1b[0m", n)
                }else{
                    write!(f, "{}", n)
                }
                Value::Str(s) => if USE_COLORS {
                    write!(f, "\x1b[32m\"{}\"\x1b[0m", s)
                } else {
                    write!(f, "\"{}\"", s)
                }
                Value::Sym(s) => write!(f, "{}", s),
                Value::Keyword(s) => if USE_COLORS {
                    write!(f, "\x1b[34m:{}\x1b[0m", s)
                } else {
                    write!(f, ":{}", s)
                }
                Value::List(list) => {
                    f.write_str("(")?;
                    let mut it = list.iter();
                    match it.next() {
                        Some(x) => write!(f, "{:?}", x)?,
                        None => {}
                    }
                    for expr in it {
                        write!(f," {:?}", expr)?;
                    }
                    f.write_str(")")?;
                    Ok(())
                },
                Value::NatFunc(_) => if USE_COLORS {
                    write!(f, "\x1b[36m[Function]\x1b[0m")
                } else {
                    write!(f, "[Function]")
                }
                Value::Func { .. } => if USE_COLORS {
                    write!(f, "\x1b[36m[Lambda]\x1b[0m")
                } else {
                    write!(f, "[Lambda]")
                }
                Value::Box(val) => write!(f, "(box {:?})", val.borrow()),
                Value::Lazy{head, tail, ..} => write!(f, "(lazy-cons {:?} {:?})", head, tail),
                Value::Map(map) => {
                    f.write_str("( ")?;
                    for (k, v) in map.iter() {
                        write!(f, ":{} {:?} ", k, v)?;
                    };
                    f.write_str(")")?;
                    Ok(())
                }
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Value) -> bool {
        use Value::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Num(ref a), Num(ref b)) => a == b,
            (Str(ref a), Str(ref b)) => a == b,
            (Sym(ref a), Sym(ref b)) => a == b,
            (Keyword(ref a), Keyword(ref b)) => a == b,
            (List(ref a), List(ref b)) => a == b,
            (Func { .. }, Func { .. }) => false,
            _ => false,
        }
    }
}

impl Value {
    /// Apply arguments to a vaterite expression that must be a function
    pub fn apply(&self, args: ValueList) -> ValueErr {
        match self {
            Value::NatFunc(f) => Ok(f(args)?),
            Value::Func{
                params, opt_params, has_kwargs, rest_param, 
                eval, ast, env, ..
            } => {
                let a = &**ast;
                if let Value::List(l) = &**params {
                    let binds = (l.clone(), opt_params.clone(), has_kwargs.clone(), rest_param.clone());
                    let local_env = EnvStruct::bind(Some(env.clone()), binds, args, *eval)?;
                    return Ok(eval(a.clone(), local_env)?)
                } else {
                    return Err("Parameter list is not a list".to_string())
                }
            },
            _ => Err("Attempt to call non-function".to_string()),
        }
    }

    pub fn is_nil(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::List(l) => l.len() == 0,
            _ => false,
        }
    }
}

/// Helper to make native functions
pub fn func(f: fn(ValueList) -> ValueErr) -> Value {
    Value::NatFunc(f)
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
    pub fn bind(access: Option<Env>, binds: (Rc<Vec<Value>>, Rc<Vec<(String, Value)>>, bool, Option<Rc<String>>), exprs: ValueList, eval: fn(Value, Env) -> ValueErr) -> Result<Env, String> {
        let env = EnvStruct::new(access);
        let (req, opt, keys, rest) = binds;
        let req_len = req.len();
        let opt_len = opt.len() * if keys { 2 } else { 1 };

        if exprs.len() < req_len {
            return Err("not enough arguments passed to function".to_string())
        }
        for (i, b) in req.iter().enumerate() {
            match b {
                Value::Sym(s) => {
                    env.set(s.clone(), exprs[i].clone());
                }
                _ => {
                    return Err("Can't bind to non symbol".to_string())
                }
            }
        }
        let exprs = exprs[req_len..].to_vec();
        if keys {
            if exprs.len() <= opt_len && exprs.len() % 2 != 0 {
                return Err("Keyword arguments aren't in pairs".to_string())
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
                env.set((*rest).clone(), list!(exprs[opt_len..].to_vec()));
            } else {
                env.set((*rest).clone(), Value::Nil);
            }
        };
        Ok(env)
    }

    /// Get an binding from the environment
    pub fn get(&self, key: String) -> ValueErr {
        match self.data.borrow().get(&key) {
            Some(e) => Ok(e.clone()),
            None => {
                if let Some(env) = &self.access {
                    match env.get(key) {
                        Ok(e) => Ok(e),
                        Err(err) => Err(err)
                    }
                }else {
                    Err(format!("'{}' not found", key))
                }
            }
        }
    }

    /// Set an binding to the environment
    pub fn set(&self, key: String, value: Value){
        self.data.borrow_mut().insert(key, value);
    }

    /// Set an binding to the environment
    pub fn assign(&self, key: String, value: Value) -> ValueErr{
        if self.data.borrow().contains_key(&key) {
            self.data.borrow_mut().insert(key, value).ok_or("Value assign before definition".to_string())
        } else {
            if let Some(env) = &self.access {
                env.assign(key, value)
            } else {
                Err(format!("'{}' not found", key))
            }
        }
    }
}

pub type Env = Rc<EnvStruct>;