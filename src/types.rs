use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

pub static mut USE_COLORS: bool = false;

/// Enum for the types used by vaterite
#[derive(Clone)]
pub enum Expr {
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
    List(Rc<Vec<Expr>>),
    /// Native function are used to define functions in rust that can be used in vaterite
    NatFunc(fn(Vec<Expr>) -> Result<Expr, String>),
    /// Vaterite function, defined by vaterite code
    Func {
        eval: fn(Expr, Env) -> Result<Expr, String>,
        ast: Rc<Expr>,
        env: Env,
        params: Rc<Expr>,
        opt_params: Rc<Vec<(String, Expr)>>,
        has_kwargs: bool,
        rest_param: Option<Rc<String>>,
        is_macro: bool,
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Str(s) => write!(f, "{}", s),
            Expr::Keyword(s) => write!(f, "{}", s),
            Expr::Sym(s) => write!(f, "{}", s),
            Expr::List(list) => {
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
            Expr::NatFunc(_) => write!(f, "[Function]"),
            Expr::Func { .. } => write!(f, "[Lambda]"),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        unsafe {
            match self {
                Expr::Nil => write!(f, "()"),
                Expr::Num(n) => if USE_COLORS { 
                    write!(f, "\x1b[93m{}\x1b[0m", n)
                }else{
                    write!(f, "{}", n)
                }
                Expr::Str(s) => if USE_COLORS {
                    write!(f, "\x1b[32m\"{}\"\x1b[0m", s)
                } else {
                    write!(f, "\"{}\"", s)
                }
                Expr::Sym(s) => write!(f, "{}", s),
                Expr::Keyword(s) => if USE_COLORS {
                    write!(f, "\x1b[34m:{}\x1b[0m", s)
                } else {
                    write!(f, ":{}", s)
                }
                Expr::List(list) => {
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
                Expr::NatFunc(_) => if USE_COLORS {
                    write!(f, "\x1b[36m[Function]\x1b[0m")
                } else {
                    write!(f, "[Function]")
                }
                Expr::Func { .. } => if USE_COLORS {
                    write!(f, "\x1b[36m[Lambda]\x1b[0m")
                } else {
                    write!(f, "[Lambda]")
                }
            }
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Expr) -> bool {
        use Expr::*;
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

impl Expr {
    /// Apply arguments to a vaterite expression that must be a function
    pub fn apply(&self, args: Vec<Expr>) -> Result<Expr, String>{
        match self {
            Expr::NatFunc(f) => Ok(f(args)?),
            Expr::Func{
                params, opt_params, has_kwargs, rest_param, 
                eval, ast, env, ..
            } => {
                let a = &**ast;
                if let Expr::List(l) = &**params {
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
            Expr::Nil => true,
            Expr::List(l) => l.len() == 0,
            _ => false,
        }
    }
}

/// Helper to make native functions
pub fn func(f: fn(Vec<Expr>) -> Result<Expr, String>) -> Expr {
    Expr::NatFunc(f)
}

/// An vaterite environment maps expressions to strings
#[derive(Debug, Clone)]
pub struct EnvStruct {
    data: RefCell<HashMap<String, Expr>>,
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
    pub fn bind(access: Option<Env>, binds: (Rc<Vec<Expr>>, Rc<Vec<(String, Expr)>>, bool, Option<Rc<String>>), exprs: Vec<Expr>, eval: fn(Expr, Env) -> ExprErr) -> Result<Env, String> {
        let env = EnvStruct::new(access);
        let (req, opt, keys, rest) = binds;
        let req_len = req.len();
        let opt_len = opt.len() * if keys { 2 } else { 1 };

        if exprs.len() < req_len {
            return Err("not enough arguments passed to function".to_string())
        }
        for (i, b) in req.iter().enumerate() {
            match b {
                Expr::Sym(s) => {
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
                    if let Expr::Keyword(kw) = &exprs[i] {
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
                env.set((*rest).clone(), Expr::Nil);
            }
        };
        Ok(env)
    }

    /// Get an binding from the environment
    pub fn get(&self, key: String) -> Result<Expr, String>{
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
    pub fn set(&self, key: String, value: Expr){
        self.data.borrow_mut().insert(key, value);
    }
}

pub type Env = Rc<EnvStruct>;
pub type ExprList = Vec<Expr>;
pub type ExprErr = Result<Expr, String>;