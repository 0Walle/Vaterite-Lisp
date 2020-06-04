#[macro_use]
mod parser;
mod types;
mod core;
use types::{Value, Env, ValueList, ValueErr};
use std::io;
use std::io::Write;
use std::rc::Rc;
use std::time::{Instant};
use std::fs::File;
use std::io::prelude::*;
use std::env;
use std::path::Path;
use std::collections::HashMap;

/// Quasiquote macro
fn quasiquote(ast: Value) -> Value {
    match ast {
        Value::Sym(_) => list![Value::Sym("quote".to_string()), ast],
        Value::List(list) if list.len() > 0 => {
            let head = &list[0];
            match head {
                Value::Sym(s) if s == "unquote" => return if list.len() > 1 { list[1].clone() } else { Value::Nil },
                Value::List(l) if l.len() > 0 => match &l[0] {
                    Value::Sym(s) if s == "unquote-splicing" => return list![
                        Value::Sym("append".to_string()),
                        
                        if  l.len() > 1 {
                            l[1].clone()
                        } else {
                            Value::Nil
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
                return list![Value::Sym("list".to_string()), car]
            }
            
            if let Value::List(l) = &cdr {
                return match &l[0] {
                    Value::Sym(s) if s == "list" => {
                        let mut start = vec![Value::Sym("list".to_string()), car];
                        start.extend_from_slice(&l[1..]);
                        list!(start)
                    }
                    _ => list![Value::Sym("cons".to_string()), car, cdr]
                }
            }

            return list![Value::Sym("cons".to_string()), car, cdr]
        }
        _ => ast
    }
}

/// Tests if a list is a macro call
fn is_macro_call(ast: Value, env: Env) -> bool {
    match ast {
        Value::List(l) if l.len() > 0 => if let Value::Sym(sym) = &l[0] {
            match env.get(sym.clone()) {
                Ok(e) => match e {
                    Value::Func{ is_macro, .. } => return is_macro,
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
fn macro_expand(mut ast: Value, mut env: Env) -> (bool, ValueErr) {
    let mut was_expanded = false;
    while is_macro_call(ast.clone(), env.clone()) {
        if let Value::List(l) = &ast { 
            if let Value::Sym(s) = &l[0] {
                let makro = if let Ok(name) = env.get(s.clone()) {
                    name
                }else{
                    return (false, Err("Macro not defined".to_string()));
                };

                if let Value::Func { 
                    ref opt_params, ref has_kwargs, ref rest_param, 
                    env: ref menv, ref params, ast: ref mast, ref eval, .. 
                } = makro {
                    let args = l[1..].to_vec();
                    let params = if let Value::List(l) = &**params {
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
fn eval_ast(ast: &Value, env: &Env) -> ValueErr {
    match ast {
        Value::Sym(sym) => env.get(sym.clone()),
        Value::List(v) =>  {
            let mut lst: ValueList = vec![];
            for expr in v.iter() {
                lst.push(eval(expr.clone(), env.clone())?)
            }
            Ok(Value::List(Rc::new(lst)))
        }
        _ => Ok(ast.clone())
    }
}

/// Produces an lambda list from an vaterite lambda list
fn from_lambda_list(list: ValueList) -> Result<(Value, Vec<(String, Value)>, bool, Option<String>), &'static str> {
    let mut req: ValueList = vec![];
    let mut opt: Vec<(String, Value)> = vec![];
    let mut keys = false;
    let mut rest: Option<String> = None;

    let mut state = 0;

    for (i, expr) in list.iter().enumerate() {
        match state {
            0 => {
                match &expr {
                    Value::Sym(s) if s == "&rest" => {
                        rest = Some(if let Value::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol")
                        });
                        break;
                    },
                    Value::Sym(s) if s == "&opt" => state = 1,
                    Value::Sym(s) if s == "&key" => {state = 1; keys = true},
                    Value::Sym(_) => req.push(expr.clone()),
                    _ => return Err("Required parameter is not a symbol")
                }
            }
            1 => {
                match &expr {
                    Value::Sym(s) if s == "&rest" => {
                        rest = Some(if let Value::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol")
                        });
                        break;
                    },
                    Value::Sym(s) => opt.push((s.clone(), Value::Nil)),
                    Value::List(l) if l.len() == 2 => {
                        if let Value::Sym(s) = &l[0] {
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

fn match_pattern(pat: Value, expr: Value, env: Env) -> Option<Result<HashMap<String, Value>, String>> {
    match pat.clone() {
        Value::Sym(s) if s == "_" => {
            Some(Ok(HashMap::default()))
        }
        Value::Sym(s) if s == "nil" => {
            match expr {
                Value::List(l) if l.len() == 0 => Some(Ok(HashMap::default())),
                Value::Nil => Some(Ok(HashMap::default())),
                _ => None
            }
        }
        Value::Sym(s) => {
            let mut map = HashMap::default();
            map.insert(s, expr.clone());
            Some(Ok(map))
        }
        Value::List(l) if l.len() > 1 => {
            match &l[0] {
                Value::Sym(s) if s == "quote" => {
                    if l.get(1).unwrap_or(&Value::Nil).clone() == expr.clone() {
                        Some(Ok(HashMap::default()))
                    } else {
                        None
                    }
                }
                Value::Sym(s) if s == "list" => {
                    if let Value::List(expr) = expr {
                        if l.len() == expr.len() + 1 {
                            let mut all_binds = HashMap::default();
                            for (pat, expr) in l[1..].iter().zip(expr.iter()) {
                                match match_pattern(pat.clone(), expr.clone(), env.clone()) {
                                    Some(res) => match res {
                                        Ok(binds) => {
                                            for (k, v) in binds {
                                                all_binds.insert(k, v);
                                            }
                                        }
                                        Err(err) => return Some(Err(err))
                                    }
                                    None => return None
                                }
                            };
                            Some(Ok(all_binds))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Value::Sym(s) if s == "list-rest" => {
                    if let Value::List(expr) = expr {
                        let last = l.len();
                        if l.len() <= expr.len() + 2 {
                            let mut all_binds = HashMap::default();
                            for (pat, expr) in l[1..last-1].iter().zip(expr.iter()) {
                                match match_pattern(pat.clone(), expr.clone(), env.clone()) {
                                    Some(res) => match res {
                                        Ok(binds) => {
                                            for (k, v) in binds {
                                                all_binds.insert(k, v);
                                            }
                                        }
                                        Err(err) => return Some(Err(err))
                                    }
                                    None => return None
                                }
                            };
                            match match_pattern(l[last-1].clone(), list!(expr[last-2..].to_vec()), env.clone()) {
                                Some(res) => match res {
                                    Ok(binds) => {
                                        for (k, v) in binds {
                                            all_binds.insert(k, v);
                                        }
                                    }
                                    Err(err) => return Some(Err(err))
                                }
                                None => return None
                            }
                            Some(Ok(all_binds))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Value::Sym(s) if s == "list-last" => {
                    if let Value::List(expr) = expr {
                        let pat_len = l.len() - 1;
                        let expr_len = expr.len();
                        if pat_len <= expr.len() {
                            let mut all_binds = HashMap::default();
                            for i in 0..pat_len {
                                match match_pattern(l[pat_len-i].clone(), expr[expr_len-i-1].clone(), env.clone()) {
                                    Some(res) => match res {
                                        Ok(binds) => {
                                            for (k, v) in binds {
                                                all_binds.insert(k, v);
                                            }
                                        }
                                        Err(err) => return Some(Err(err))
                                    }
                                    None => return None
                                }
                            };
                            match match_pattern(l[1].clone(), list!(expr[0..expr_len-pat_len+1].to_vec()), env.clone()) {
                                Some(res) => match res {
                                    Ok(binds) => {
                                        for (k, v) in binds {
                                            all_binds.insert(k, v);
                                        }
                                    }
                                    Err(err) => return Some(Err(err))
                                }
                                None => return None
                            }
                            Some(Ok(all_binds))
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }
                Value::Sym(s) if s == "if" => {
                    let func = match eval(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone()) {
                        Ok(pred) => pred,
                        Err(err) => return Some(Err(err))
                    };

                    let mut all_binds = HashMap::default();
                    if match func.apply(vec![expr.clone()]) {
                        Ok(res) => !res.is_nil(),
                        Err(err) => return Some(Err(err))
                    } {
                        for pat in l[2..].iter() {
                            match match_pattern(pat.clone(), expr.clone(), env.clone()) {
                                Some(res) => match res {
                                    Ok(binds) => {
                                        for (k, v) in binds {
                                            all_binds.insert(k, v);
                                        }
                                    }
                                    Err(err) => return Some(Err(err))
                                }
                                None => return None
                            }
                        }
                        Some(Ok(all_binds))
                    } else {
                        None
                    }
                    
                }
                Value::Sym(s) if s == "and" => {
                    let mut all_binds = HashMap::default();
                    for pat in l[1..].iter() {
                        match match_pattern(pat.clone(), expr.clone(), env.clone()) {
                            Some(res) => match res {
                                Ok(binds) => {
                                    for (k, v) in binds {
                                        all_binds.insert(k, v);
                                    }
                                }
                                Err(err) => return Some(Err(err))
                            }
                            None => return None
                        }
                    }
                    Some(Ok(all_binds))
                }
                Value::Sym(s) if s == "or" => {
                    for pat in l[1..].iter() {
                        match match_pattern(pat.clone(), expr.clone(), env.clone()) {
                            Some(res) => match res {
                                Ok(binds) => {
                                    let mut all_binds = HashMap::default();
                                    for (k, v) in binds {
                                        all_binds.insert(k, v);
                                    }
                                    return Some(Ok(all_binds))
                                }
                                Err(err) => return Some(Err(err))
                            }
                            None => continue
                        }
                    }
                    None
                }
                Value::Sym(s) if s == "macro" => {
                    match macro_expand(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone()) {
                        (true, Ok(nast)) => {
                            match_pattern(nast, expr, env)
                        }
                        (_, Err(err)) => Some(Err(err)),
                        _ => Some(Err(format!("Failed expanding as macro")))
                    }
                }
                _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
            }
        }
        Value::Str(s) => if let Value::Str(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::Num(s) => if let Value::Num(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::Keyword(s) => if let Value::Keyword(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
    }
}

/// Evaluate an expression
fn eval(mut ast: Value, mut env: Env) -> ValueErr {
    let ret: ValueErr;

    'tco: loop {
        ret = match ast.clone() {
            Value::List(l) => {
                if l.len() == 0 {
                    return Ok(Value::Nil);
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
                    Value::Sym(sym) if sym == "quote" => 
                        if l.len() < 1 {
                            Err("quote form requires 1 argument".to_string())
                        }else{
                            Ok(l[1].clone())
                        }
                    Value::Sym(sym) if sym == "quasiquote" => 
                        if l.len() < 1 {
                            Err("quasiquote form requires 1 argument".to_string())
                        }else{
                            ast = quasiquote(l[1].clone());
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == "macro-expand" => macro_expand(l[1].clone(), env.clone()).1,
                    Value::Sym(sym) if sym == "bench" =>
                        if l.len() < 1 {
                            Err("bench form requires 1 argument".to_string())
                        }else{
                            let now = Instant::now();
                            let expr = eval(l[1].clone(), env.clone());
                            println!("{} μs", now.elapsed().as_micros());
                            expr
                        }
                    Value::Sym(sym) if sym == "if" => 
                        if l.len() < 4 {
                            Err("if form requires 3 arguments".to_string())
                        }else if eval(l[1].clone(), env.clone())?.is_nil() {
                            ast = l[3].clone();
                            continue 'tco;
                        } else {
                            ast = l[2].clone();
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == "cond" => {
                        let mut res: ValueErr = Ok(Value::Nil);
                        for cond in l[1..].iter() {
                            res = if let Value::List(l) = cond {
                                if l.len() < 2 {
                                    Err("condition must be a pair".to_string())
                                }else if !eval(l[0].clone(), env.clone())?.is_nil() {
                                    ast = l[1].clone();
                                    continue 'tco;
                                }else{
                                    Ok(Value::Nil)
                                }
                            } else {
                                Err("condition must be a pair".to_string())
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == "match" => {
                        let value = eval(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone())?;
                        for pair in l[2..].iter() {
                            match pair {
                                Value::List(l) if l.len() == 2 => {
                                    match match_pattern(l[0].clone(), value.clone(), env.clone()) {
                                        Some(res) => match res {
                                            Ok(binds) => {
                                                let local_env = types::EnvStruct::new(Some(env.clone()));
                                                for (k, v) in binds {
                                                    local_env.set(k, v);
                                                }
                                                ast = l[1].clone();
                                                env = local_env.clone();
                                                continue 'tco;
                                            }
                                            Err(err) => return Err(format!("Pattern Error: {}", err))
                                        }
                                        None => {}
                                    }
                                    // if !eval(l[0].clone(), env.clone())?.is_nil() {
                                    //     ast = l[1].clone();
                                    //     continue 'tco;
                                    // }else{
                                    //     Ok(Value::Nil)
                                    // }
                                }
                                _ => return Err("condition must be a pair".to_string())
                            }
                        };
                        Err("No matching pattern found".to_string())
                    }
                    Value::Sym(sym) if sym == "and" => {
                        let mut res: ValueErr = Ok(Value::Sym("t".to_string()));
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if expr.is_nil() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == "or" => {
                        let mut res: ValueErr = Ok(Value::Nil);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if !expr.is_nil() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == "def" => 
                        if l.len() < 3 {
                            Err("def form requires 2 arguments".to_string())
                        } else if let Value::Sym(s) = &l[1] {
                            env.set(s.clone(), eval(l[2].clone(), env.clone())?);
                            Ok(Value::Nil)
                        } else {
                            Err("Binding name must be a symbol".to_string())
                        }
                    Value::Sym(sym) if sym == "fun" =>
                        if l.len() < 3 {
                            Err("def form requires 3 arguments".to_string())
                        } else if let Value::Sym(s) = &l[1] {
                            if let Value::List(params) = &l[2] {
                                let (req, opt, key, rest) = from_lambda_list((&**params).clone())?;
                                let func = Value::Func{
                                    eval,
                                    is_macro: false,
                                    params: Rc::new(req),
                                    opt_params: Rc::new(opt),
                                    has_kwargs: key,
                                    rest_param: if let Some(s) = rest { Some(Rc::new(s)) } else { None },
                                    ast: Rc::new(l[3].clone()),
                                    env: env.clone()
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                Err("Lambda list must be a list".to_string())
                            }
                        } else {
                            Err("Binding name must be a symbol".to_string())
                        }
                    Value::Sym(sym) if sym == "defmacro" =>
                        if l.len() < 3 {
                            Err("defmacro form requires 3 arguments".to_string())
                        } else if let Value::Sym(s) = &l[1] {
                            if let Value::List(params) = &l[2] {
                                let (req, opt, key, rest) = from_lambda_list((&**params).clone())?;
                                let func = Value::Func{
                                    eval,
                                    is_macro: true,
                                    params: Rc::new(req),
                                    opt_params: Rc::new(opt),
                                    has_kwargs: key,
                                    rest_param: if let Some(s) = rest { Some(Rc::new(s)) } else { None },
                                    ast: Rc::new(l[3].clone()),
                                    env: env.clone()
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                Err("Lambda list must be a list".to_string())
                            }
                        } else {
                            Err("Binding name must be a symbol".to_string())
                        }
                    Value::Sym(sym) if sym == "fn" => 
                        if l.len() < 3 {
                            Err("fn form requires 2 arguments".to_string())
                        } else if let Value::List(params) = &l[1] {
                            let mut body = vec![Value::Sym("block".to_string())];
                            body.extend_from_slice(&l[2..]);
                            let (req, opt, key, rest) = from_lambda_list((&**params).clone())?;
                            Ok(Value::Func{
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
                    Value::Sym(sym) if sym == "let" => {
                        let len = l.len();
                        if len < 3 {
                            Err("let form requires 2 arguments".to_string())
                        } else if let Value::List(binds) = &l[1] {
                            let local_env = types::EnvStruct::new(Some(env.clone()));
                            for pair in binds.iter() {
                                match pair {
                                    Value::List(p) => {
                                        if p.len() < 2 {
                                            return Err("Binding must be a pair".to_string())
                                        }
                                        if let Value::Sym(name) = &p[0] {
                                            local_env.set(name.clone(), eval(p[1].clone(), local_env.clone())?)
                                        }
                                    }
                                    _ => return Err("Binding must be a pair".to_string())
                                }
                            }
                            for expr in l[2..len-1].iter() {
                                eval(expr.clone(), local_env.clone())?;
                            };
                            ast = l[len-1].clone();
                            //ast = l[2].clone();
                            env = local_env.clone();
                            continue 'tco
                        } else {
                            Err("Binding list must be a list".to_string())
                        }
                    }
                    Value::Sym(sym) if sym == "for" => {
                        let len = l.len();
                        if len == 1 {
                            return Ok(Value::Nil);
                        }
                        let mut result: ValueList = vec![];
                        let mut iters: HashMap<String, Value> = HashMap::default();
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for bind in l[1..len-1].iter() {
                            if let Value::List(bind) = bind {
                                if bind.len() != 2 {
                                    return Err("For binding must be a pair".to_string())
                                }

                                if let Value::Sym(name) = &bind[0] {
                                    iters.insert(name.clone(), eval(bind[1].clone(), env.clone())?);
                                }
                            }
                        };
                        
                        'end: loop {
                            for (name, iter) in iters.iter() {
                                let head = core::operator_head(vec![iter.clone()])?;
                                if let Value::Nil = head {
                                    break 'end Ok(list!(result))
                                }
                                local_env.set(name.clone(), head);
                            }
                            result.push(eval(l[len-1].clone(), local_env.clone())?);
                            let mut new_iters: HashMap<String, Value> = HashMap::default();
                            for (name, iter) in iters.iter() {
                                let tail = core::operator_tail(vec![iter.clone()])?;
                                new_iters.insert(name.clone(), tail);
                            }
                            iters = new_iters;
                        }
                    }
                    Value::Sym(sym) if sym == "module" => {
                        let len = l.len();
                        if len < 2 {
                            return Err("module form requires 1 argument".to_string())
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            return Err("module name must be an symbol".to_string())
                        };

                        let mut exports: ValueList = vec![];
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for expr in l[2..len].iter() {
                            match &expr {
                                Value::List(l) if l.len() > 0 => {
                                    match &l[0] {
                                        Value::Sym(s) if s == "exports" => exports = (&l[1..]).to_vec(),
                                        _ => {eval(expr.clone(), local_env.clone())?;}
                                    }
                                }
                                _ => {eval(expr.clone(), local_env.clone())?;}
                            }
                            
                        };
                        for expr in exports {
                            match &expr {
                                Value::Sym(s) => {
                                    env.set(format!("{}/{}", mod_name, s), local_env.get(s.clone())?)
                                }
                                _ => return Err("exported name is not an symbol".to_string())
                            }
                        }

                        return Ok(Value::Nil);
                    }
                    Value::Sym(sym) if sym == "exports" => Ok(Value::Nil),
                    Value::Sym(sym) if sym == "import" => {
                        let len = l.len();
                        if len < 3 {
                            return Err("import form requires 1 argument".to_string())
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            return Err("import namespace must be an symbol".to_string())
                        };

                        let mut modules: ValueList = vec![];
                        if match env.get("*modules*".to_string())? {
                            Value::List(l) => {
                                modules = (*l).clone();
                                l.iter().any(|i| match i {
                                    Value::Str(s) => s == mod_name,
                                    _ => false
                                })
                            },
                            Value::Nil => false,
                            _ => return Err("*modules* global not found".to_string())
                        } {
                            return Ok(Value::Nil)
                        }

                        let mut contents = String::new();
                        let file = File::open(match &l[2] {
                            Value::Str(s) => format!("./{}",s),
                            _ => return Err("Filename must be an string".to_string())
                        });
                        
                        if let Ok(mut file) = file {
                            match file.read_to_string(&mut contents) {
                                Ok(_) => (),
                                Err(err) => return Err(format!("Couldn't read file: {:?}", err))
                            };
                        } else {
                            return Err("Couldn't open file".to_string())
                        }

                        let mut tk = parser::Reader::new(format!("({})", contents));
                        let exprs = if let Ok(tok) = tk.next_token() {
                            match tk.parse_expr(tok)? {
                                Value::List(ct) => ct,
                                _ => unreachable!()
                            }
                        }else{
                            return Err("Invalid Syntax".to_string())
                        };

                        modules.push(Value::Str(mod_name.clone()));
                        env.assign("*modules*".to_string(), list!(modules))?;
                        

                        let mut exports: ValueList = vec![];
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for expr in exprs.iter() {
                            match &expr {
                                Value::List(l) if l.len() > 0 => {
                                    match &l[0] {
                                        Value::Sym(s) if s == "exports" => exports = (&l[1..]).to_vec(),
                                        _ => {eval(expr.clone(), local_env.clone())?;}
                                    }
                                }
                                _ => {eval(expr.clone(), local_env.clone())?;}
                            }
                            
                        };
                        for expr in exports {
                            match &expr {
                                Value::Sym(s) => {
                                    env.set(format!("{}/{}", mod_name, s), local_env.get(s.clone())?)
                                }
                                _ => return Err("exported name is not an symbol".to_string())
                            }
                        }
                        return Ok(Value::Nil);
                    }
                    Value::Sym(sym) if sym == "block" => {
                        let len = l.len();
                        if len == 1 {
                            return Ok(Value::Nil);
                        }

                        for expr in l[1..len-1].iter() {
                            eval(expr.clone(), env.clone())?;
                        };
                        ast = l[len-1].clone();
                        continue 'tco;
                    }
                    Value::Sym(ref sym) if sym == "eval" => {
                        ast = eval(l[1].clone(), env.clone())?;
                        while let Some(ref e) = env.clone().access {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    Value::Sym(ref sym) if sym == "throw" => {
                        Err(format!("{}", l.get(1).unwrap_or(&Value::Nil)))
                    }
                    Value::Sym(ref sym) if sym == "catch" => {
                        if l.len() == 3 {
                            match eval(l[1].clone(), env.clone()) {
                                Ok(val) => Ok(val),
                                Err(_) => {
                                    ast = l[2].clone();
                                    continue 'tco;
                                }
                            }
                        } else if l.len() == 4 {
                            match eval(l[1].clone(), env.clone()) {
                                Ok(val) => Ok(val),
                                Err(err) => {
                                    let local_env = types::EnvStruct::new(Some(env.clone()));
                                    if let Value::Sym(s) = &l[2] {
                                        local_env.set(s.clone(), Value::Str(err))
                                    } else {
                                        return Err("Error must be bound to a symbol".to_string())
                                    }
                                    env = local_env;
                                    ast = l[3].clone();
                                    continue 'tco;
                                }
                            }
                        } else {
                            return Err("catch requires 2 or 3 argument".to_string())
                        }
                    }
                    Value::Sym(ref sym) if sym == "lazy-cons" => {
                        if l.len() < 3 {
                            return Err("lazy-cons form requires 2 arguments".to_string())
                        }
                        let head = eval(l[1].clone(), env.clone())?;
                        Ok(Value::Lazy{
                            eval, env,
                            head: Rc::new(head),
                            tail: Rc::new(l[2].clone())
                        })
                    }
                    _ => match ast {
                        Value::List(v) =>  {
                            let mut list: Vec<Value> = vec![];
                            for expr in v.iter() {
                                list.push(eval(expr.clone(), env.clone())?)
                            }
                            let ref func = list[0].clone();
                            let args = list[1..].to_vec();
                            match func {
                                Value::NatFunc(f) => f(args),
                                Value::Func{
                                    ast: fast, params, opt_params, has_kwargs, rest_param, env: fenv, eval, ..
                                } => {
                                    let a = &**fast;
                                    if let Value::List(l) = &**params {
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
    repl_env.set("*dir-name*".to_string(), Value::Str(".".to_string()));
    repl_env.set("*modules*".to_string(), Value::Nil);
    repl_env.set("nil".to_string(), Value::Nil);
    repl_env.set("t".to_string(), Value::Sym("t".to_string()));
    
    let (flags, args) = arg_parse(env::args());

    match args.len() {
        1 => {
            println!("Vaterite Lisp - Walle - 2020");

            loop {
                print!(">> ");
                let _ = io::stdout().flush();
                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("Couldn't read input");
            
                let mut tk = parser::Reader::new(input);
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
                    Value::Sym(x) if x == "exit" => break,
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
                    Value::Str(String::from(path))
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
                contents.push_str("\n)");

                let mut tk = parser::Reader::new(contents);
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
