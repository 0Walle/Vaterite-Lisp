use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

#[derive(Clone)]
pub enum Expr {
    Nil,
    Num(f64),
    Str(String),
    Sym(String),
    List(Rc<Vec<Expr>>),
    NatFunc(fn(Vec<Expr>) -> Result<Expr, String>),
    Func {
        eval: fn(Expr, Env) -> Result<Expr, String>,
        ast: Rc<Expr>,
        env: Env,
        params: Rc<Expr>,
        is_macro: bool,
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Str(s) => write!(f, "{}", s),
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
        match self {
            Expr::Nil => write!(f, "()"),
            Expr::Num(n) => write!(f, "\x1b[93m{}\x1b[0m", n),
            Expr::Str(s) => write!(f, "\x1b[32m\"{}\"\x1b[0m", s),
            Expr::Sym(s) => write!(f, "{}", s),
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
            Expr::NatFunc(_) => write!(f, "\x1b[36m[Function]\x1b[0m"),
            Expr::Func { .. } => write!(f, "\x1b[36m[Lambda]\x1b[0m"),
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
            (List(ref a), List(ref b)) => a == b,
            (Func { .. }, Func { .. }) => false,
            _ => false,
        }
    }
}

impl Expr {
    pub fn apply(&self, args: Vec<Expr>) -> Result<Expr, String>{
        match self {
            Expr::NatFunc(f) => Ok(f(args)?),
            Expr::Func{
                eval, ast, params, env, ..
            } => {
                let a = &**ast;
                if let Expr::List(l) = &**params {
                    //l.clone()
                    let local_env = EnvStruct::bind(Some(env.clone()), l.clone(), args)?;
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

pub fn func(f: fn(Vec<Expr>) -> Result<Expr, String>) -> Expr {
    Expr::NatFunc(f)
}

#[derive(Debug, Clone)]
pub struct EnvStruct {
    data: RefCell<HashMap<String, Expr>>,
    pub access: Option<Env>,
}

impl EnvStruct {
    // fn find(&self, key: String) -> Option<Env> {
    //     match (self.data.borrow().contains_key(&key), self.access.clone()) {
    //         (true, _) => Some(self),
    //         (false, Some(o)) => env_find(&o, key),
    //         _ => None,
    //     }
    // }

    pub fn new(access: Option<Env>) -> Env {
        Rc::new(EnvStruct{
            access, data: RefCell::new(HashMap::default())
        })
    }

    pub fn bind(access: Option<Env>, binds: Rc<Vec<Expr>>, exprs: Vec<Expr>) -> Result<Env, String> {
        let env = EnvStruct::new(access);
        for (i, b) in binds.iter().enumerate() {
            match b {
                Expr::Sym(s) if s == "&rest" => {
                    if let Expr::Sym(bind) = binds[i + 1].clone() {
                        env.set(bind,  Expr::List(Rc::new(exprs[i..].to_vec())));
                        break;
                    }else{
                        return Err("Can't bind to non symbol".to_string())
                    }
                }
                Expr::Sym(s) => {
                    env.set(s.clone(), exprs[i].clone());
                }
                _ => {
                    return Err("Can't bind to non symbol".to_string())
                }
            }
        }
        Ok(env)
    }

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

    pub fn set(&self, key: String, value: Expr){
        self.data.borrow_mut().insert(key, value);
    }
}

pub type Env = Rc<EnvStruct>;