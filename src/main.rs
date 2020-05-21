#[macro_use]
mod parser;
mod types;
mod core;
use types::{Expr, Env};
use std::io;
use std::io::Write;
use std::rc::Rc;
use std::time::{Instant};
use std::fs::File;
use std::io::prelude::*;
use std::env;
use std::path::Path;

/// Quasiquote macro
fn quasiquote(ast: Expr) -> Expr {
    match ast {
        Expr::Sym(_) => list![Expr::Sym("quote".to_string()), ast],
        Expr::List(list) if list.len() > 0 => {
            let head = &list[0];
            match head {
                Expr::Sym(s) if s == "unquote" => return if list.len() > 1 { list[1].clone() } else { Expr::Nil },
                Expr::List(l) if l.len() > 0 => match &l[0] {
                    Expr::Sym(s) if s == "unquote-splicing" => return list![
                        Expr::Sym("append".to_string()),
                        
                        if  l.len() > 1 {
                            l[1].clone()
                        } else {
                            Expr::Nil
                        },
                        quasiquote(list!(list[1..].to_vec()))
                    ],
                    _ => {}
                }
                _ => {}
            };
            let car = quasiquote(head.clone());
            let cdr = quasiquote(list!(list[1..].to_vec()));
            if cdr.is_nil() {
                return list![Expr::Sym("list".to_string()), car]
            }
            
            if let Expr::List(l) = &cdr {
                return match &l[0] {
                    Expr::Sym(s) if s == "list" => {
                        let mut start = vec![Expr::Sym("list".to_string()), car];
                        start.extend_from_slice(&l[1..]);
                        list!(start)
                    }
                    _ => list![Expr::Sym("cons".to_string()), car, cdr]
                }
            }

            return list![Expr::Sym("cons".to_string()), car, cdr]
        }
        _ => ast
    }
}

/// Tests if a list is a macro call
fn is_macro_call(ast: Expr, env: Env) -> bool {
    match ast {
        Expr::List(l) if l.len() > 0 => if let Expr::Sym(sym) = &l[0] {
            match env.get(sym.clone()) {
                Ok(e) => match e {
                    Expr::Func{ is_macro, .. } => return is_macro,
                    _ => false
                }
                Err(_) => false
            }
        }else {
            false
        }
        _ => false
    }
}

/// Try to expand an ast as a macro call, if isn't return the original ast
fn macro_expand(mut ast: Expr, mut env: Env) -> (bool,Result<Expr, String>) {
    let mut was_expanded = false;
    while is_macro_call(ast.clone(), env.clone()) {
        if let Expr::List(l) = &ast { 
            if let Expr::Sym(s) = &l[0] {
                let makro = if let Ok(name) = env.get(s.clone()) {
                    name
                }else{
                    return (false, Err("Macro not defined".to_string()));
                };

                if let Expr::Func { 
                    ref opt_params, ref has_kwargs, ref rest_param, 
                    env: ref menv, ref params, ast: ref mast, ref eval, .. 
                } = makro {
                    let args = l[1..].to_vec();
                    let params = if let Expr::List(l) = &**params {
                        l
                    }else{
                        unreachable!();
                    };

                    let binds = (params.clone(), opt_params.clone(), has_kwargs.clone(), rest_param.clone());
                    let macro_scope = match types::EnvStruct::bind(Some(menv.clone()), binds, args, *eval){
                        Ok(scope) => scope,
                        Err(err) => return (false, Err(err))
                    };
                    let macro_ast = &**mast;
                    ast = match eval(macro_ast.clone(), macro_scope.clone()) {
                        Ok(ast) => ast,
                        Err(err) => return (false, Err(err))
                    };
                    env = macro_scope;
                }
            }
        }
        was_expanded = true;       
    };
    (was_expanded,Ok(ast))
}

/// Evaluates the ast
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

/// Produces an lambda list from an vaterite lambda list
fn from_lambda_list(list: Vec<Expr>) -> Result<(Expr, Vec<(String, Expr)>, bool, Option<String>), &'static str> {
    let mut req: Vec<Expr> = vec![];
    let mut opt: Vec<(String, Expr)> = vec![];
    let mut keys = false;
    let mut rest: Option<String> = None;

    let mut state = 0;

    for (i, expr) in list.iter().enumerate() {
        match state {
            0 => {
                match &expr {
                    Expr::Sym(s) if s == "&rest" => {
                        rest = Some(if let Expr::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol")
                        });
                        break;
                    },
                    Expr::Sym(s) if s == "&opt" => state = 1,
                    Expr::Sym(s) if s == "&key" => {state = 1; keys = true},
                    Expr::Sym(_) => req.push(expr.clone()),
                    _ => return Err("Required parameter is not a symbol")
                }
            }
            1 => {
                match &expr {
                    Expr::Sym(s) if s == "&rest" => {
                        rest = Some(if let Expr::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol")
                        });
                        break;
                    },
                    Expr::Sym(s) => opt.push((s.clone(), Expr::Nil)),
                    Expr::List(l) if l.len() == 2 => {
                        if let Expr::Sym(s) = &l[0] {
                            opt.push((s.clone(), l[1].clone()))
                        } else {
                            return Err("Parameter pair bind is not a symbol")
                        }
                    }
                    _ => return Err("Optional/keyword parameter is not a symbol or pair")
                }
            }
            _ => break
        }
    };
    Ok((list!(req), opt, keys, rest))
}

/// Evaluate an expression
fn eval(mut ast: Expr, mut env: Env) -> Result<Expr, String> {
    let ret: Result<Expr, String>;

    'tco: loop {
        ret = match ast.clone() {
            Expr::List(l) => {
                if l.len() == 0 {
                    return Ok(Expr::Nil);
                }

                match macro_expand(ast.clone(), env.clone()) {
                    (true, Ok(nast)) => {
                        ast = nast;
                        continue 'tco;
                    }
                    (_, Err(err)) => return Err(err),
                    _ => ()
                }
                
    
                let head = &l[0];
                match head {
                    Expr::Sym(sym) if sym == "quote" => 
                        if l.len() < 1 {
                            Err("quote form requires 1 argument".to_string())
                        }else{
                            Ok(l[1].clone())
                        }
                    Expr::Sym(sym) if sym == "quasiquote" => 
                        if l.len() < 1 {
                            Err("quasiquote form requires 1 argument".to_string())
                        }else{
                            ast = quasiquote(l[1].clone());
                            continue 'tco;
                        }
                    Expr::Sym(sym) if sym == "macro-expand" => macro_expand(l[1].clone(), env.clone()).1,
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
                            ast = l[3].clone();
                            continue 'tco;
                        } else {
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
                                    opt_params: Rc::new(vec![]),
                                    has_kwargs: false,
                                    ast: Rc::new(l[3].clone()),
                                    rest_param: None,
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
                    Expr::Sym(sym) if sym == "defmacro" =>
                        if l.len() < 3 {
                            Err("defmacro form requires 3 arguments".to_string())
                        } else if let Expr::Sym(s) = &l[1] {
                            if let Expr::List(_) = &l[2] {
                                let func = Expr::Func{
                                    eval,
                                    is_macro: true,
                                    params: Rc::new(l[2].clone()),
                                    opt_params: Rc::new(vec![]),
                                    has_kwargs: false,
                                    rest_param: None,
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
                        } else if let Expr::List(params) = &l[1] {
                            let mut body = vec![Expr::Sym("block".to_string())];
                            body.extend_from_slice(&l[2..]);
                            let (req, opt, key, rest) = from_lambda_list((&**params).clone())?;
                            Ok(Expr::Func{
                                eval,
                                is_macro: false,
                                params: Rc::new(req),
                                opt_params: Rc::new(opt),
                                has_kwargs: key,
                                rest_param: if let Some(s) = rest { Some(Rc::new(s)) } else { None },
                                ast: Rc::new(list!(body)),
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
                            ast = l[2].clone();
                            env = local_env.clone();
                            continue 'tco
                        } else {
                            Err("Binding list must be a list".to_string())
                        }
                    }
                    Expr::Sym(sym) if sym == "block" => {
                        let len = l.len();
                        if len == 1 {
                            return Ok(Expr::Nil);
                        }

                        for expr in l[1..len-1].iter() {
                            eval(expr.clone(), env.clone())?;
                        };
                        ast = l[len-1].clone();
                        continue 'tco;
                    }
                    Expr::Sym(ref sym) if sym == "eval" => {
                        ast = eval(l[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().access {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    _ => match ast {
                        Expr::List(v) =>  {
                            let mut list: Vec<Expr> = vec![];
                            for expr in v.iter() {
                                list.push(eval(expr.clone(), env.clone())?)
                            }
                            let ref func = list[0].clone();
                            let args = list[1..].to_vec();
                            match func {
                                Expr::NatFunc(f) => f(args),
                                Expr::Func{
                                    ast: fast, params, opt_params, has_kwargs, rest_param, env: fenv, eval, ..
                                } => {
                                    let a = &**fast;
                                    if let Expr::List(l) = &**params {
                                        let binds = (l.clone(), opt_params.clone(), has_kwargs.clone(), rest_param.clone());
                                        let local_env = types::EnvStruct::bind(Some(fenv.clone()), binds, args, *eval)?;
                                        ast = a.clone();
                                        env = local_env.clone();
                                        continue 'tco;
                                    } else {
                                        Err("Parameter list is not a list".to_string())
                                    }
                                },
                                _ => Err("Attempt to call non-function".to_string()),
                            }
                        }
                        _ => Err("Expected a list".to_string())
                    }
                }
            }
            _ => eval_ast(&ast, &env)
        };
        break;
    }
    ret
}

fn arg_parse(mut args: env::Args) -> (u8, Vec<String>) {
    // more flags can be added later
    let mut flags = 0b0001;
    let mut common_args = vec![];
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--color" => unsafe { types::USE_COLORS = true },
            "--print-last" => flags &= !1,
            _ => common_args.push(arg)
        }
    };
    (flags, common_args)
}

fn main() {
    let repl_env = types::EnvStruct::new(None);

    // add functions from core to the environment
    for (k, v) in core::ns() {
        repl_env.set(k.to_string(), v);
    }
    repl_env.set("*dir-name*".to_string(), Expr::Str(".".to_string()));
    repl_env.set("nil".to_string(), Expr::Nil);
    repl_env.set("t".to_string(), Expr::Sym("t".to_string()));
    
    let (flags, args) = arg_parse(env::args());

    match args.len() {
        1 => {
            println!("Vaterite Lisp - Walle - 2020");

            loop {
                print!(">> ");
                let _ = io::stdout().flush();
                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("Couldn't read input");
            
                let mut tk = parser::Tokenizer::new(input);
                let tok = match tk.next_token() {
                    Ok(tok) => tok,
                    Err(err) => {
                        println!("Error: {}", err);
                        continue;
                    },
                };

                if let parser::Token::Eof = tok {
                    continue;
                }

                let val = match tk.parse_expr(tok) {
                    Ok(val) => val,
                    Err(err) => {
                        println!("Error: {}", err);
                        continue;
                    },
                };
                match val {
                    types::Expr::Sym(x) if x == "exit" => break,
                    _ => {
                        match eval(val, repl_env.clone()) {
                            Ok(e) => println!("{:?}", e),
                            Err(err) => unsafe {
                                if types::USE_COLORS {
                                    println!("\x1b[91mError: {}\x1b[0m", err)
                                } else {
                                    println!("Error: {}", err)
                                }
                            }
                        }
                    }
                }
            }
            println!("Done!");
        }
        2 => {
            let filename = &args[1];

            repl_env.set("*dir-name*".to_string(), match Path::new(filename).parent() {
                Some(path) => if let Some(path) = path.to_str() {
                    Expr::Str(String::from(path))
                } else {
                    println!("File path is invalid, it may contain invalid utf-8 characters");
                    return;
                }
                None => {
                    println!("File path is invalid");
                    return;
                }
            });

            let file = File::open(filename);

            let mut contents = String::from("(block ");
            if let Ok(mut file) = file {
                match file.read_to_string(&mut contents) {
                    Ok(_) => {},
                    Err(_) => {
                        println!("Error reading file");
                        return;
                    }
                }
                contents.push_str(")");

                let mut tk = parser::Tokenizer::new(contents);
                let tok = match tk.next_token() {
                    Ok(tok) => tok,
                    Err(err) => {
                        println!("Error: {}", err);
                        return;
                    },
                };

                if let parser::Token::Eof = tok {
                    return;
                }

                let val = match tk.parse_expr(tok) {
                    Ok(val) => val,
                    Err(err) => {
                        println!("Error: {}", err);
                        return;
                    },
                };
                match eval(val, repl_env.clone()) {
                    Ok(e) => if flags & 1 != 0 {println!("{:?}", e)},
                    Err(err) => println!("Error: {}", err),
                }
            } else {
                println!("Couldn't open file");
            }
        }
        _ => println!("Invalid arguments")
    }
}
