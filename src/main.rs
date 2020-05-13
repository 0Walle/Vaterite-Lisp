#[macro_use]
mod parser;
mod types;
use types::{Expr, Env};
use std::io;
use std::io::Write;
use std::rc::Rc;
use std::time::{Instant, SystemTime};

fn eval_ast(ast: &Expr, env: &Env) -> Result<Expr, String> {
    match ast {
        Expr::Sym(sym) => env.get(sym.clone()),
        Expr::List(v) =>  {
            let mut lst: Vec<Expr> = vec![];
            for expr in v.iter() {
                lst.push(eval(expr.clone(), env.clone())?)
            }
            Ok(Expr::List(Rc::new(lst)))
        }
        _ => Ok(ast.clone())
    }
}

fn eval(mut ast: Expr, mut env: Env) -> Result<Expr, String> {
    let ret: Result<Expr, String>;

    'tco: loop {
        ret = match ast.clone() {
            Expr::List(l) => {
                if l.len() == 0 {
                    return Ok(Expr::Nil);
                }
    
                let head = &l[0];
                match head {
                    Expr::Sym(sym) if sym == "quote" => 
                        if l.len() < 1 {
                            Err("quote form requires 1 argument".to_string())
                        }else{
                            Ok(l[1].clone())
                        }
                    Expr::Sym(sym) if sym == "bench" =>
                        if l.len() < 1 {
                            Err("bench form requires 1 argument".to_string())
                        }else{
                            let now = Instant::now();
                            let expr = eval(l[1].clone(), env.clone());
                            println!("{} μs", now.elapsed().as_micros());
                            expr
                        }
                    Expr::Sym(sym) if sym == "if" => 
                        if l.len() < 4 {
                            Err("if form requires 3 arguments".to_string())
                        }else if eval(l[1].clone(), env.clone())?.is_nil() {
                            //eval(l[3].clone(), env.clone())
                            ast = l[3].clone();
                            continue 'tco;
                        } else {
                            //eval(l[2].clone(), env.clone())
                            ast = l[2].clone();
                            continue 'tco;
                        }
                    Expr::Sym(sym) if sym == "cond" => {
                        let mut res: Result<Expr, String> = Ok(Expr::Nil);
                        for cond in l[1..].iter() {
                            res = if let Expr::List(l) = cond {
                                if l.len() < 2 {
                                    Err("condition must be a pair".to_string())
                                }else if !eval(l[0].clone(), env.clone())?.is_nil() {
                                    ast = l[1].clone();
                                    continue 'tco;
                                }else{
                                    Ok(Expr::Nil)
                                }
                            } else {
                                Err("condition must be a pair".to_string())
                            }
                        };
                        res
                    }
                    Expr::Sym(sym) if sym == "and" => {
                        let mut res: Result<Expr, String> = Ok(Expr::Sym("t".to_string()));
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if expr.is_nil() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                        //return Ok(Expr::Sym("t".to_string()))
                    }
                    Expr::Sym(sym) if sym == "or" => {
                        let mut res: Result<Expr, String> = Ok(Expr::Nil);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if !expr.is_nil() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                        //return Ok(Expr::Nil)
                    }
                    Expr::Sym(sym) if sym == "def" => 
                        if l.len() < 3 {
                            Err("def form requires 2 arguments".to_string())
                        } else if let Expr::Sym(s) = &l[1] {
                            env.set(s.clone(), eval(l[2].clone(), env.clone())?);
                            Ok(Expr::Nil)
                        } else {
                            Err("Binding name must be a symbol".to_string())
                        }
                    Expr::Sym(sym) if sym == "fun" =>
                        if l.len() < 3 {
                            Err("def form requires 3 arguments".to_string())
                        } else if let Expr::Sym(s) = &l[1] {
                            if let Expr::List(_) = &l[2] {
                                let func = Expr::Func{
                                    eval,
                                    is_macro: false,
                                    params: Rc::new(l[2].clone()),
                                    ast: Rc::new(l[3].clone()),
                                    env: env.clone()
                                };
                                env.set(s.clone(), func);
                                Ok(Expr::Nil)
                            } else {
                                Err("Lambda list must be a list".to_string())
                            }
                        } else {
                            Err("Binding name must be a symbol".to_string())
                        }
                    Expr::Sym(sym) if sym == "fn" => 
                        if l.len() < 3 {
                            Err("fn form requires 2 arguments".to_string())
                        } else if let Expr::List(_) = &l[1] {
                            Ok(Expr::Func{
                                eval,
                                is_macro: false,
                                params: Rc::new(l[1].clone()),
                                ast: Rc::new(l[2].clone()),
                                env: env.clone()
                            })
                        } else {
                            Err("Lambda list must be a list".to_string())
                        }
                    Expr::Sym(sym) if sym == "let" => {
                        if l.len() < 3 {
                            Err("let form requires 2 arguments".to_string())
                        } else if let Expr::List(binds) = &l[1] {
                            let local_env = types::EnvStruct::new(Some(env.clone()));
                            for pair in binds.iter() {
                                match pair {
                                    Expr::List(p) => {
                                        if p.len() < 2 {
                                            return Err("Binding must be a pair".to_string())
                                        }
                                        if let Expr::Sym(name) = &p[0] {
                                            local_env.set(name.clone(), eval(p[1].clone(), local_env.clone())?)
                                        }
                                    }
                                    _ => return Err("Binding must be a pair".to_string())
                                }
                            }
                            //return eval(l[2].clone(), local_env.clone())
                            ast = l[2].clone();
                            env = local_env.clone();
                            continue 'tco
                        } else {
                            Err("Binding list must be a list".to_string())
                        }
                    }
                    Expr::Sym(sym) if sym == "block" => {
                        let len = l.len();
                        for expr in l[1..len-1].iter() {
                            eval(expr.clone(), env.clone())?;
                        };
                        //eval(l[len].clone(), env.clone())
                        ast = l[len].clone();
                        continue 'tco;
                    }
                    _ => match eval_ast(&ast, &env)? {
                        Expr::List(ref list) => {
                            let ref func = list[0].clone();
                            let args = list[1..].to_vec();
                            match func {
                                Expr::NatFunc(f) => f(args),
                                Expr::Func{
                                    ast: fast, params, env: fenv, ..
                                } => {
                                    let a = &**fast;
                                    if let Expr::List(l) = &**params {
                                        //l.clone()
                                        let local_env = types::EnvStruct::bind(Some(fenv.clone()), l.clone(), args)?;
                                        //return Ok(eval(a.clone(), local_env)?)
                                        ast = a.clone();
                                        env = local_env.clone();
                                        continue 'tco;
                                    } else {
                                        Err("Parameter list is not a list".to_string())
                                    }
                                },
                                _ => Err("Attempt to call non-function".to_string()),
                            }
                            //return func.apply(list[1..].to_vec())
                        }
                        _ => Err("Expected a list".to_string()),
                    }
                }
            }
            _ => eval_ast(&ast, &env)
        };
        break;
    }
    ret
}

macro_rules! ord_op {
    ($op:tt, $v:expr) => {{
        let mut left = match &$v[0] {
            Expr::Num(n) => *n,
            _ => return Err("Invalid number arguments".to_string())
        };
        for e in $v[1..].iter() {
            if let Expr::Num(n) = e {
                if left $op *n {
                    left = *n;
                    continue
                }else{
                    return Ok(Expr::Nil)
                }
            }else{
                return Err("Invalid number arguments".to_string())
            }
        }
        return Ok(Expr::Sym("t".to_string()))
    }};
}

macro_rules! add_mul_op {
    ($op:tt, $init:expr, $args:expr) => {
        Ok(Expr::Num(
            $args.iter().fold(Ok($init), |acc, val| if let Expr::Num(n) = val {
                Ok(acc? $op *n)
            }else{
                return Err("Invalid number arguments".to_string())
            })?
        ))
    };
}

macro_rules! sub_div_op {
    ($op:tt, $none:expr, $one:expr, $args:expr) => {{
        if $args.len() == 0 {
            $none
        }else if let Expr::Num(first) = &$args[0]{
            if $args.len() > 1 {
                Ok(Expr::Num($args[1..].iter().fold(Ok(first.clone()), |acc, val| if let Expr::Num(n) = val {
                    Ok(acc? $op *n)
                }else{
                    return Err("Invalid number arguments".to_string())
                })?))
            }else{
                Ok(Expr::Num($one(*first)))
            }    
        }else{
            Err("Invalid number arguments".to_string())
        }
    }};
}

fn main() {
    let repl_env = types::EnvStruct::new(None);

    repl_env.set("+".to_string(), types::func(|v: Vec<Expr>| add_mul_op!(+, 0f64, v)));
    repl_env.set("*".to_string(), types::func(|v: Vec<Expr>| add_mul_op!(*, 1f64, v)));
    repl_env.set("-".to_string(), types::func(|v: Vec<Expr>| sub_div_op!(-, Ok(Expr::Num(0.)), |a: f64| -a, v)));
    repl_env.set("/".to_string(), types::func(|v: Vec<Expr>| sub_div_op!(/, Err("Invalid number argument".to_string()), |a: f64| 1./a, v)));
    repl_env.set("<".to_string(), types::func(|v: Vec<Expr>| ord_op!(<, v)));
    repl_env.set(">".to_string(), types::func(|v: Vec<Expr>| ord_op!(>, v)));
    repl_env.set("<=".to_string(), types::func(|v: Vec<Expr>| ord_op!(<=, v)));
    repl_env.set(">=".to_string(), types::func(|v: Vec<Expr>| ord_op!(>=, v)));
    repl_env.set("==".to_string(), types::func(|v: Vec<Expr>| {
        let left = &v[0]; 
        for e in v[1..].iter() {
            if left != e {
                return Ok(Expr::Nil)
            }
        }
        return Ok(Expr::Sym("t".to_string()))
    }));
    repl_env.set("!=".to_string(), types::func(|v: Vec<Expr>| {
        let left = &v[0]; 
        for e in v[1..].iter() {
            if left == e {
                return Ok(Expr::Nil)
            }
        }
        return Ok(Expr::Sym("t".to_string()))
    }));
    repl_env.set("..".to_string(), types::func(|v: Vec<Expr>| {
        let mut res = String::new();
        for e in v.iter() {
            res.push_str(&format!("{}", e))
        }
        return Ok(Expr::Str(res));
    }));
    repl_env.set("list".to_string(), types::func(|v: Vec<Expr>| Ok( list!(v))));
    repl_env.set("head".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        match &v[0] {
            Expr::List(l) => {
                if l.len() == 0 {
                    Ok(Expr::Nil)
                }else{
                    Ok(l[0].clone())
                }
            },
            Expr::Nil => Ok(Expr::Nil),
            _ => Err("Value is not a list".to_string()),
        }
    } else {
        Ok(Expr::Nil)
    }));
    repl_env.set("tail".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        match &v[0] {
            Expr::List(l) => {
                if l.len() == 0 {
                    Ok(Expr::Nil)
                }else{
                    Ok(list!(l[1..].to_vec()))
                }
            },
            Expr::Nil => Ok(Expr::Nil),
            _ => Err("Value is not a list".to_string()),
        }
    } else {
        Ok(Expr::Nil)
    }));
    repl_env.set("cons".to_string(), types::func(|v: Vec<Expr>| if v.len() == 2 {
        match &v[1] {
            Expr::List(l) => {
                if l.len() == 0 {
                    Ok(list![vec![v[0].clone()]])
                }else{
                    let mut new = vec![v[0].clone()];
                    new.extend_from_slice(&l);
                    Ok(list![new])
                }
            },
            Expr::Nil => Ok(list![vec![v[0].clone()]]),
            _ => Err("Can't cons to a non-list".to_string()),
        }
    } else {
        Err("cons require two arguments".to_string())
    }));
    repl_env.set("atom?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        if let Expr::List(_) = &v[0] {
            Ok(Expr::Nil)
        }else{
            Ok(Expr::Sym("t".to_string()))
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("list?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        match &v[0] {
            Expr::List(v) if v.len() != 0 => Ok(Expr::Sym("t".to_string())),
            Expr::Nil => Ok(Expr::Nil),
            _ => Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("nil?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        if v[0].is_nil() {
            Ok(Expr::Sym("t".to_string()))
        }else{
            Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Sym("t".to_string()))
    }));
    repl_env.set("number?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        if let Expr::Num(_) = &v[0] {
            Ok(Expr::Sym("t".to_string()))
        }else{
            Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("string?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        if let Expr::Str(_) = &v[0] {
            Ok(Expr::Sym("t".to_string()))
        }else{
            Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("symbol?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        if let Expr::Sym(_) = &v[0] {
            Ok(Expr::Sym("t".to_string()))
        }else{
            Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("function?".to_string(), types::func(|v: Vec<Expr>| if v.len() != 0 {
        match &v[0] {
            Expr::Func{ .. } => Ok(Expr::Sym("t".to_string())),
            Expr::NatFunc(_) => Ok(Expr::Sym("t".to_string())),
            _ => Ok(Expr::Nil)
        }
    }else{
        Ok(Expr::Nil)
    }));
    repl_env.set("apply".to_string(), types::func(|v: Vec<Expr>| {
        let len = v.len();
        if len < 2 {
            return Err("apply requires two or more arguments".to_string())
        }
        let mut args = v[1..len-1].to_vec();
        
        if let Expr::List(rest) = v[len-1].clone() {
            args.extend_from_slice(&rest);
            v[0].apply(args)
        }else{
            Err("last argument must be a list".to_string())
        }
    }));
    repl_env.set("map".to_string(), types::func(|v: Vec<Expr>| {
        let len = v.len();
        if len < 2 {
            return Err("map requires two arguments".to_string())
        }
        let func = &v[0];
        if let Expr::List(seq) = &v[1] {
            let mut result: Vec<Expr> = vec![];
            for expr in seq.iter(){
                result.push(func.apply(vec![expr.clone()])?)
            }
            return Ok(list!(result))
        }else{
            return Err("second argument must be list".to_string())
        }

    }));
    repl_env.set("append".to_string(), types::func(|v: Vec<Expr>| {
        let mut result: Vec<Expr> = vec![];
        for seq in v {
            if let Expr::List(l) = seq {
                result.extend_from_slice(&l);
            } else {
                return Err("arguments must be lists".to_string())
            }
        }
        Ok(list!(result))
    }));
    repl_env.set("time-ms".to_string(), types::func(|_v: Vec<Expr>| {
        Ok(Expr::Num(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64))
    }));
    repl_env.set("nil".to_string(), Expr::Nil);
    repl_env.set("t".to_string(), Expr::Sym("t".to_string()));
    

    println!("Vaterite Lisp - Walle - 2020");

    loop {
        print!(">> ");
        let _ = io::stdout().flush();
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Couldn't read input");
    
        let mut tk = parser::Tokenizer::new(input);
        let tok = tk.next_token().expect("Invalid Syntax");
        let val = tk.parse_expr(tok).expect("Invalid Syntax");
        match val {
            types::Expr::Sym(x) if x == "exit" => break,
            _ => {
                match eval(val, repl_env.clone()) {
                    Ok(e) => println!("{:?}", e),
                    Err(err) => println!("\x1b[91mError: {}\x1b[0m", err),
                }
            }
        }
    }

    println!("Done!");
}
