#[macro_use]
mod parser;
mod types;
mod core;
mod error;

use std::io;
use std::io::Write;
use std::rc::Rc;
// use std::time::{Instant};
use std::fs::File;
use std::io::prelude::*;
use std::env;
use std::path::Path;
use std::collections::HashMap;

//use std::mem;
use types::{Value, Env, ValueList, FuncData, Arity};
// use crate::error;

type ValueResult = Result<Value, error::Error>;
macro_rules! arg_err {
    ( $name:expr; $min:literal .. $max:literal; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Range($min,$max),$got as u16) };
    ( $name:expr; $ex:literal; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Exact($ex),$got as u16) };
    ( $name:expr; $min:literal .. ; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Min($min),$got as u16) };
}
macro_rules! type_err {
    ( $type:literal; $got:expr ) => { error::Error::TypeErr($type, Some($got)) };
}

/// Quasiquote macro
fn quasiquote(ast: Value) -> Value {
    match ast {
        // Value::Sym(_) => list![Value::Sym("quote".to_string()), ast],
        Value::Sym(_) => vater!{ (quote [ast]) },
        Value::List(list) if list.len() > 0 => {
            let head = &list[0];
            match head {
                Value::Sym(s) if s == "unquote" => return if list.len() > 1 { list[1].clone() } else { Value::Nil },
                Value::List(l) if l.len() > 0 => match &l[0] {
                    Value::Sym(s) if s == "unquote-splicing" => return vater!{
                        (append [if l.len() > 1 { l[1].clone() } else { Value::Nil}] [quasiquote(list[1..].to_vec().into())])
                    },
                    _ => {}
                }
                _ => {}
            };
            let car = quasiquote(head.clone());
            let cdr = quasiquote(list[1..].to_vec().into());
            if cdr.is_nil() {
                // return list![Value::Sym("list".to_string()), car]
                return vater!{ (list [car]) }
            }
            
            if let Value::List(l) = &cdr {
                return match &l[0] {
                    Value::Sym(s) if s == "list" => {
                        let mut start = vec![vater!(list), car];
                        start.extend_from_slice(&l[1..]);
                        start.into()
                    }
                    //_ => list![Value::Sym("cons".to_string()), car, cdr]
                    _ => vater!{ (cons [car] [cdr]) }
                }
            }

            //return list![Value::Sym("cons".to_string()), car, cdr]
            return vater!{ (cons [car] [cdr]) }
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
// fn macro_expand(mut ast: Value, mut env: Env) -> (bool, ValueErr) {
fn macro_expand(mut ast: Value, mut env: Env) -> (bool, ValueResult) {
    let mut was_expanded = false;
    while is_macro_call(ast.clone(), env.clone()) {
        if let Value::List(l) = &ast { 
            if let Value::Sym(s) = &l[0] {
                let makro = if let Ok(name) = env.get(s.clone()) {
                    name
                }else{
                    return (false, Err("Macro not defined".into()));
                };

                if let Value::Func { 
                    //ref opt_params, ref has_kwargs, ref rest_param, 
                    //env: ref menv, ref params, ast: ref mast, ref eval, .. 
                    env: menv, func, eval, ..
                } = makro {
                    let args = l[1..].to_vec();
                    // let params = if let Value::List(l) = &func.params {
                    //     l
                    // }else{
                    //     unreachable!();
                    // };

                    // let binds = (params.clone(), func.opt_params.clone(), func.has_kwargs.clone(), func.rest_param.clone());
                    // let binds = (&func.params, &func.opt_params, &func.has_kwargs, &func.rest_param);
                    let binds = &*func;
                    let macro_scope = match types::EnvStruct::bind(Some(menv.clone()), binds, args, eval){
                        Ok(scope) => scope,
                        Err(err) => return (false, Err(err))
                    };
                    let macro_ast = &func.ast;
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
fn eval_ast(ast: &Value, env: &Env) -> ValueResult {
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
fn from_lambda_list(list: ValueList) -> Result<(Vec<String>, Vec<(String, Value)>, bool, Option<String>, Arity), String> {
    let mut req: Vec<String> = vec![];
    let mut opt: Vec<(String, Value)> = vec![];
    let mut keys = false;
    let mut rest: Option<String> = None;

    let mut state = 0;

    for (i, expr) in list.iter().enumerate() {
        match state {
            0 => {
                match &expr {
                    Value::Keyword(s) if s == "rest" => {
                        rest = Some(if let Value::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol".into())
                        });
                        break;
                    },
                    Value::Keyword(s) if s == "opt" => state = 1,
                    Value::Keyword(s) if s == "key" => {state = 1; keys = true},
                    Value::Sym(s) => req.push(s.clone()),
                    x => return Err(format!("Required parameter is not a symbol, found {:?}", x))
                }
            }
            1 => {
                match &expr {
                    Value::Keyword(s) if s == "rest" => {
                        rest = Some(if let Value::Sym(rest_sym) = &list[i + 1] { 
                            rest_sym.clone()
                        }else {
                            return Err("Rest parameter is not a symbol".into())
                        });
                        break;
                    },
                    Value::Sym(s) => opt.push((s.clone(), Value::Nil)),
                    Value::List(l) if l.len() == 2 => {
                        if let Value::Sym(s) = &l[0] {
                            opt.push((s.clone(), l[1].clone()))
                        } else {
                            return Err("Parameter pair bind is not a symbol".into())
                        }
                    }
                    x => return Err(format!("Optional/keyword parameter is not a symbol or pair, found {:?}", x))
                }
            }
            _ => break
        }
    };
    let min = req.len() as u16;
    let arity = if rest.is_some() {
        Arity::Min(min)
    } else {
        if opt.len() != 0 {
            Arity::Range(min, opt.len() as u16 * if keys {2} else {1} + min)
        } else {
            Arity::Exact(min)
        }
    };
    
    Ok((req.into(), opt, keys, rest, arity))
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
                Value::Sym(s) if s == "cons" => {
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
                            match match_pattern(l[last-1].clone(), expr[last-2..].to_vec().into(), env.clone()) {
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
                Value::Sym(s) if s == "rev-cons" => {
                    if let Value::List(expr) = expr {
                        let pat_len = l.len() - 1;
                        let expr_len = expr.len();
                        if pat_len <= expr.len() + 1 {
                            let mut all_binds = HashMap::default();
                            for i in 0..pat_len-1 {
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
                            match match_pattern(l[1].clone(), expr[0..expr_len+1-pat_len].to_vec().into(), env.clone()) {
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
                        Err(err) => return Some(Err(err.to_string()))
                    };

                    let mut all_binds = HashMap::default();
                    if match func.apply(vec![expr.clone()]) {
                        Ok(res) => !res.is_false(),
                        Err(err) => return Some(Err(err.to_string()))
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
                Value::Sym(s) if s == "hash-map" => {
                    let map = match expr {
                        Value::Map(map) => map.clone(),
                        _ => return None
                    };

                    let mut all_binds = HashMap::default();
                    for i in (1..l.len()).step_by(2) {
                        let key = match &l[i] {
                              Value::Sym(s)
                            | Value::Str(s)
                            | Value::Keyword(s) => s,
                            x => return Some(Err(format!("Value {:?} cannot be used as key in hash-map pattern", x)))
                        };

                        if let Some(val) = map.get(key) {
                            match match_pattern(l[i+1].clone(), val.clone(), env.clone()) {
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
                        } else {
                            return None
                        }
                    }
                    Some(Ok(all_binds))
                }
                Value::Sym(s) if s == "match-struct" => {
                    if let Value::Struct(id, expr) = expr {
                        if ! match &l[1] {
                            Value::Sym(check_id) => *check_id == *id,
                            _ => return Some(Err(format!("Struct Id must be a symbol")))
                        } {
                            return None
                        }

                        if l.len() == expr.len() + 2 {
                            let mut all_binds = HashMap::default();
                            for (pat, expr) in l[2..].iter().zip(expr.iter()) {
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
                Value::Sym(s) if s == "from" => {
                    let num = if let Value::Num(expr) = expr {
                        expr
                    } else {
                        return None
                    };
                    if l.len() != 3 {
                        return Some(Err(format!("from pattern requires two arguments")))
                    }

                    match (&l[1], &l[2]) {
                        (Value::Num(start), Value::Num(end)) => {
                            if num >= *start && num <= *end {
                                Some(Ok(HashMap::default()))
                            } else {
                                None
                            }
                        }
                        _ => return Some(Err(format!("from pattern requires two number arguments")))
                    }                    
                }
                Value::Sym(s) if s == "macro" => {
                    match macro_expand(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone()) {
                        (true, Ok(nast)) => {
                            match_pattern(nast, expr, env)
                        }
                        (_, Err(err)) => Some(Err(err.to_string())),
                        _ => Some(Err(format!("Failed expanding pattern as macro")))
                    }
                }
                // _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
                _ => match macro_expand(pat.clone(), env.clone()) {
                    (true, Ok(nast)) => {
                        match_pattern(nast, expr, env)
                    }
                    (_, Err(err)) => Some(Err(err.to_string())),
                    _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
                }
            }
        }
        Value::Str(s) => if let Value::Str(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::Num(s) => if let Value::Num(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::False => if let Value::False = expr { Some(Ok(HashMap::default())) } else { None }
        Value::True => if let Value::True = expr { Some(Ok(HashMap::default())) } else { None }
        Value::Keyword(s) => if let Value::Keyword(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
    }
}

/// Evaluate an expression
// fn eval(mut ast: Value, mut env: Env) -> ValueErr {
fn eval(mut ast: Value, mut env: Env) -> ValueResult {
    // let ret: ValueErr;
    let ret: ValueResult;

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
                        if l.len() != 2 {
                            // Err(types::Error::ArgErr(Some("quote".to_string()), Arity::Exact(1), l.len() as u16))
                            Err(arg_err!("quote"; 1; l.len()))
                        }else{
                            Ok(l[1].clone())
                        }
                    Value::Sym(sym) if sym == "quasiquote" => 
                        if l.len() != 2 {
                            // Err("quasiquote form requires 1 argument".to_string())
                            Err(arg_err!("quasiquote"; 1; l.len()))
                        }else{
                            ast = quasiquote(l[1].clone());
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == "macro-expand" => macro_expand(l[1].clone(), env.clone()).1,
                    // Value::Sym(sym) if sym == "bench" =>
                    //     if l.len() < 1 {
                    //         Err("bench form requires 1 argument".to_string())
                    //     }else{
                    //         let now = Instant::now();
                    //         let expr = eval(l[1].clone(), env.clone());
                    //         println!("{} μs", now.elapsed().as_micros());
                    //         expr
                    //     }
                    Value::Sym(sym) if sym == "if" => 
                        if l.len() != 4 {
                            Err(arg_err!("if"; 3; l.len()))
                            // Err("if form requires 3 arguments".to_string())
                        }else if eval(l[1].clone(), env.clone())?.is_false() {
                            ast = l[3].clone();
                            continue 'tco;
                        } else {
                            ast = l[2].clone();
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == "cond" => {
                        let mut res: ValueResult = Ok(Value::Nil);
                        for cond in l[1..].iter() {
                            res = if let Some(l) = cond.to_vec() {
                                if l.len() < 2 {
                                    Err("condition must be a pair".into())
                                }else if !eval(l[0].clone(), env.clone())?.is_false() {
                                    ast = l[1].clone();
                                    continue 'tco;
                                }else{
                                    Ok(Value::Nil)
                                }
                            } else {
                                Err("condition must be a pair".into())
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
                                            Err(err) => return Err(format!("Pattern Error: {}", err).into())
                                        }
                                        None => {}
                                    }
                                }
                                _ => return Err("condition must be a pair".into())
                            }
                        };
                        Err("No matching pattern found".into())
                    }
                    Value::Sym(sym) if sym == "and" => {
                        let mut res: ValueResult = Ok(Value::True);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if expr.is_false() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == "or" => {
                        let mut res: ValueResult = Ok(Value::False);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone())?;
                            if !expr.is_false() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == "def" => 
                        if l.len() < 3 {
                            // Err("def form requires 2 arguments".to_string())
                            Err(arg_err!("def"; 2; l.len()))
                        } else if let Value::Sym(s) = &l[1] {
                            env.set(s.clone(), eval(l[2].clone(), env.clone())?);
                            Ok(Value::Nil)
                        } else {
                            // Err("Binding name must be a symbol".to_string())
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == "struct" =>
                        if l.len() < 3 {
                            // Err("struct form requires 3 arguments".to_string())
                            Err(arg_err!("struct"; 2; l.len()))
                        } else if let Value::Sym(name) = &l[1] {
                            if let Some(fields) = &l[2].to_vec() {
                                let mut ctor = vec![ vater!{(sym "make-struct")}, vater!{(quote (sym name))}];
                                ctor.extend_from_slice(&fields);
                                let fields = match l[2].to_vec() {
                                    Some(ls) => ls,
                                    _ => return Err(format!("Struct fields must be a list").into())
                                };
                                let fields: Vec<String> = fields.iter().map(|v| v.from_sym().ok_or(format!("Expected Symbol"))).collect::<Result<Vec<String>, String>>()?;
                                let ctor = Value::Func{
                                    env: env.clone(), eval, is_macro: false,
                                    func: Rc::new(FuncData {
                                        params: fields.clone(),
                                        opt_params: vec![], has_kwargs: false, rest_param: None,
                                        name: Some(name.clone()),
                                        arity: Arity::Exact(fields.len() as u16),
                                        ast: ctor.into()
                                    }),
                                };
                                env.set(name.clone(), ctor);

                                for (i, field) in fields.iter().enumerate() {
                                    // let field_name = match field {
                                    //     Value::Sym(name) => name,
                                    //     _ => return Err("Field name must be a symbol".into())
                                    // };
                                    let field_name = field;

                                    let acessor = Value::Func{
                                        env: env.clone(), eval, is_macro: false,
                                        func: Rc::new(FuncData {
                                            // params: vater!{(self)},
                                            params: vec!["self".into()],
                                            opt_params: vec![], has_kwargs: false, rest_param: None,
                                            name: Some(format!("{}-{}", name, field_name)),
                                            arity: Arity::Exact(1),
                                            ast: vater!{
                                                ((sym "index-struct") self (quote (sym name)) [Value::Num(i as f64)])
                                            },
                                        }),
                                    };
                                    env.set(format!("{}-{}", name, field_name), acessor);
                                }

                                let pred = Value::Func{
                                    env: env.clone(), eval, is_macro: false,
                                    func: Rc::new(FuncData {
                                        // params: vater!{(self)},
                                        params: vec!["self".into()],
                                        opt_params: vec![], has_kwargs: false, rest_param: None,
                                        name: Some(format!("{}?", name)),
                                        arity: Arity::Exact(1),
                                        ast: vater!{
                                            ((sym "assert-struct") self (quote (sym name)))
                                        },
                                    }),
                                };
                                env.set(format!("{}?", name), pred);

                                Ok(Value::Nil)
                            } else {
                                Err("Field list must be a list".into())
                            }
                        } else {
                            Err("Struct name must be a symbol".into())
                        }
                    Value::Sym(sym) if sym == "fun" =>
                        if l.len() < 4 {
                            // Err("fun form requires 2 arguments".to_string())
                            Err(arg_err!("fun"; 3; l.len()))
                        } else if let Value::Sym(s) = &l[1] {
                            if let Some(params) = &l[2].to_vec() {
                                let (req, opt, key, rest, arity) = from_lambda_list((&**params).clone())?;
                                let func = Value::Func{
                                    env: env.clone(),
                                    eval,
                                    func: Rc::new(FuncData {
                                        params: req,
                                        opt_params: opt,
                                        has_kwargs: key,
                                        rest_param: if let Some(s) = rest { Some(s) } else { None },
                                        ast: l[3].clone(),
                                        name: Some(s.clone()),
                                        arity,
                                    }),
                                    is_macro: false,
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                // Err(format!("Lambda list must be a list found {}", l[2]))
                                Err(type_err!("list"; l[2].clone()))
                            }
                        } else {
                            // Err("Binding name must be a symbol".to_string())
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == "defmacro" =>
                        if l.len() < 4 {
                            // Err("defmacro form requires 3 arguments".to_string())
                            Err(arg_err!("defmacro"; 3; l.len()))
                        } else if let Value::Sym(s) = &l[1] {
                            if let Some(params) = &l[2].to_vec() {
                                let (req, opt, key, rest, arity) = from_lambda_list((&**params).clone())?;
                                let func = Value::Func{
                                    env: env.clone(),
                                    eval,
                                    func: Rc::new(FuncData {
                                        params: req,
                                        opt_params: opt,
                                        has_kwargs: key,
                                        rest_param: if let Some(s) = rest { Some(s) } else { None },
                                        ast: l[3].clone(),
                                        name: Some(s.clone()),
                                        arity,
                                    }),
                                    is_macro: true,
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                // Err(format!("Lambda list must be a list found {}", l[2]))
                                Err(type_err!("list"; l[2].clone()))
                            }
                        } else {
                            // Err("Binding name must be a symbol".to_string())
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == "fn" => 
                        if l.len() < 3 {
                            // Err("fn form requires 2 arguments".to_string())
                            Err(arg_err!("fn"; 2..; l.len()))
                        } else if let Some(params) = l[1].to_vec() {
                            let mut body = vec![Value::Sym("block".to_string())];
                            body.extend_from_slice(&l[2..]);
                            let (req, opt, key, rest, arity) = from_lambda_list((&*params).clone())?;
                            Ok(Value::Func{
                                env: env.clone(),
                                eval,
                                func: Rc::new(FuncData {
                                    params: req,
                                    opt_params: opt,
                                    has_kwargs: key,
                                    rest_param: rest,
                                    ast: body.into(),
                                    name: None,
                                    arity,
                                }),
                                // params: Rc::new(req),
                                // opt_params: Rc::new(opt),
                                // has_kwargs: key,
                                // rest_param: if let Some(s) = rest { Some(Rc::new(s)) } else { None },
                                // ast: Rc::new(list!(body)),
                                // name: Rc::new(format!("Lambda"))
                                is_macro: false,
                            })
                        } else {
                            // Err(format!("Lambda list must be a list found {}", l[1]))
                            Err(type_err!("list"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == "let" => {
                        let len = l.len();
                        if len < 3 {
                            // Err("let form requires 2 arguments".to_string())
                            Err(arg_err!("let"; 2..; l.len()))
                        } else if let Some(binds) = &l[1].to_vec() {
                            let local_env = types::EnvStruct::new(Some(env.clone()));
                            for pair in binds.iter() {
                                match pair {
                                    Value::List(p) => {
                                        if p.len() < 2 {
                                            return Err("Binding must be a pair".into())
                                        }
                                        if let Value::Sym(name) = &p[0] {
                                            local_env.set(name.clone(), eval(p[1].clone(), local_env.clone())?)
                                        }
                                    }
                                    _ => return Err("Binding must be a pair".into())
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
                            // Err("Binding list must be a list".to_string())
                            Err(type_err!("list"; l[1].clone()))
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
                                    return Err("For binding must be a pair".into())
                                }

                                if let Value::Sym(name) = &bind[0] {
                                    iters.insert(name.clone(), eval(bind[1].clone(), env.clone())?);
                                }
                            }
                        };
                        
                        'end: loop {
                            for (name, iter) in iters.iter() {
                                let head = iter.first()?;
                                if let Value::Nil = head {
                                    break 'end Ok(result.into())
                                }
                                local_env.set(name.clone(), head);
                            }
                            result.push(eval(l[len-1].clone(), local_env.clone())?);
                            let mut new_iters: HashMap<String, Value> = HashMap::default();
                            for (name, iter) in iters.iter() {
                                let tail = iter.rest()?;
                                new_iters.insert(name.clone(), tail);
                            }
                            iters = new_iters;
                        }
                    }
                    Value::Sym(sym) if sym == "module" => {
                        let len = l.len();
                        if len < 2 {
                            // return Err("module form requires 1 argument".to_string())
                            return Err(arg_err!("module"; 2..; l.len()))
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            // return Err("module name must be an symbol".to_string())
                            return Err(type_err!("symbol"; l[1].clone()))
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
                                // _ => return Err("exported name is not an symbol".to_string())
                                x => return Err(type_err!("symbol"; x.clone()))
                            }
                        }

                        return Ok(Value::Nil);
                    }
                    Value::Sym(sym) if sym == "exports" => Ok(Value::Nil),
                    Value::Sym(sym) if sym == "import" => {
                        let len = l.len();
                        if len < 3 {
                            // return Err("import form requires 1 argument".to_string())
                            return Err(arg_err!("import"; 1..; l.len()))
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            // return Err("import namespace must be an symbol".to_string())
                            return Err(type_err!("symbol"; l[1].clone()))
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
                            _ => return Err("*modules* global not found".into())
                        } {
                            return Ok(Value::Nil)
                        }

                        let mut contents = String::new();
                        let file = File::open(match &l[2] {
                            Value::Str(s) => format!("./{}",s),
                            // _ => return Err("Filename must be an string".to_string())
                            x => return Err(type_err!("string"; x.clone()))
                        });
                        
                        if let Ok(mut file) = file {
                            match file.read_to_string(&mut contents) {
                                Ok(_) => (),
                                Err(err) => return Err(format!("Couldn't read file: {:?}", err).into())
                            };
                        } else {
                            return Err("Couldn't open file".to_string().into())
                        }

                        let mut tk = parser::Reader::new(format!("({})", contents));
                        let exprs = if let Ok(tok) = tk.next_token() {
                            match match tk.parse_expr(tok) {
                                parser::ParserResult::Expr(expr) => expr,
                                parser::ParserResult::EofErr => return Err(format!("Unexpected EOF").into()),
                                parser::ParserResult::TokenErr(err) => return Err(err.into()),
                            } {
                                Value::List(ct) => ct,
                                _ => unreachable!()
                            }
                        }else{
                            return Err("Invalid Syntax".into())
                        };

                        modules.push(Value::Str(mod_name.clone()));
                        env.assign("*modules*".to_string(), modules.into())?;
                        

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
                                // _ => return Err("exported name is not an symbol".to_string())
                                x => return Err(type_err!("symbol"; x.clone()))
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
                        // Err(format!("{}", eval(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone())?))
                        Err(error::Error::Throw(l.get(1).cloned()))
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
                                    if let Value::List(pair) = &l[2] {
                                        if pair.len() != 2 {
                                            return Err("Catch binding must be a pair".into())
                                        }
                                        let kind = pair[0].from_sym().ok_or(type_err!("symbol"; pair[0].clone()))?;
                                        let value = pair[1].from_sym().ok_or(type_err!("symbol"; pair[1].clone()))?;
                                        {
                                            use error::Error::*;
                                            let sym = match err {
                                                Throw(val) => {
                                                    local_env.set(value.clone(), val.unwrap_or(Value::Nil)); "ThrowError"
                                                }
                                                Reason(val) => {
                                                    local_env.set(value.clone(), Value::Str(val)); "ReasonError"
                                                }
                                                ArgErr(name, _arity, got) => {
                                                    let name = match name {
                                                        Some(name) => Value::Str(name),
                                                        None => Value::Nil
                                                    };
                                                    local_env.set(value.clone(), vater!{ ([name] [Value::Num(got as f64)]) }); "ArityError"
                                                }
                                                KwArgErr(name) => {
                                                    let name = match name {
                                                        Some(name) => Value::Str(name),
                                                        None => Value::Nil
                                                    };
                                                    local_env.set(value.clone(), vater!{ ([name]) }); "KwargsError"
                                                }
                                                TypeErr(ty, got) => {
                                                    local_env.set(value.clone(), vater!{ ([Value::Sym(ty.to_string())] [got.unwrap_or(Value::Nil)]) }); "TypeError"
                                                }
                                                BindErr(name) => {
                                                    local_env.set(value.clone(), Value::Str(name)); "NameError"
                                                }
                                            };
                                            local_env.set(kind.clone(), Value::Sym(sym.to_string()));
                                        }
                                    } else {
                                        // return Err("Error must be bound to a symbol".to_string())
                                        return Err(type_err!("symbol"; l[2].clone()))
                                    }
                                    env = local_env;
                                    ast = l[3].clone();
                                    continue 'tco;
                                }
                            }
                        } else {
                            // return Err("catch requires 2 or 3 argument".to_string())
                            return Err(arg_err!("catch"; 2..3; l.len()))
                        }
                    }
                    Value::Sym(ref sym) if sym == "lazy-cons" => {
                        if l.len() < 3 {
                            // return Err("lazy-cons form requires 2 arguments".to_string())
                            return Err(arg_err!("lazy"; 2; l.len()))
                        }
                        let head = eval(l[1].clone(), env.clone())?;
                        Ok(Value::Lazy{
                            eval, env,
                            head: Rc::new(head),
                            tail: Rc::new(l[2].clone())
                        })
                    }
                    Value::Keyword(key) => {
                        if l.len() < 2 {
                            return Ok(Value::Nil)
                        }
                        match eval(l[1].clone(), env.clone())? {
                            Value::Map(map) => {
                                match map.get(key) {
                                    Some(val) => Ok(val.clone()),
                                    None => {
                                        ast = match l.get(2) {
                                            Some(val) => val.clone(),
                                            None => return Ok(Value::Nil)
                                        };
                                        continue 'tco;
                                    }
                                }
                            }
                            // Err("Cannot get a key of non-map".to_string())
                            x => return Err(type_err!("map"; x))
                        }
                    }
                    _ => match ast {
                        Value::List(v) =>  {
                            let mut list: Vec<Value> = vec![];
                            for expr in v.iter() {
                                list.push(eval(expr.clone(), env.clone())?)
                            }
                            let ref func = list[0].clone();
                            let args = list[1..].to_vec();
                            // func.apply(args)
                            match func {
                                // Value::NatFunc(f) => f(args),
                                Value::NatFunc(f) => {
                                    // match f.arity {
                                    //     Arity::Exact(n) => if args.len() as u8 == n { Ok((f.func)(args)?) } else {
                                    //         Err(format!("Invalid arguments for {}, expected {} arguments but found {}", f.name, n, args.len()))
                                    //     },
                                    //     Arity::Min(n) => if args.len() as u8 >= n { Ok((f.func)(args)?) } else {
                                    //         Err(format!("Invalid arguments for {}, expected at least {} arguments but found {}", f.name, n, args.len()))
                                    //     },
                                    //     Arity::Range(min, max) => if min < args.len() as u8 && (args.len() as u8) < max { Ok((f.func)(args)?) } else {
                                    //         Err(format!("Invalid arguments for {}, expected {} to {} arguments but found {}", f.name, min, max, args.len()))
                                    //     },
                                    // }                  
                                    // match f.arity {
                                    //     Arity::Exact(n) => if !(args.len() == n.into()) { 
                                    //         return Err(format!("Invalid arguments for {}, expected {} arguments but found {}", f.name, n, args.len()))
                                    //     },
                                    //     Arity::Min(n) => if !(args.len() >= n.into()) { 
                                    //         return Err(format!("Invalid arguments for {}, expected at least {} arguments but found {}", f.name, n, args.len()))
                                    //     },
                                    //     Arity::Range(min, max) => if !(min as usize <= args.len() && args.len() <= max.into()) { 
                                    //         return Err(format!("Invalid arguments for {}, expected {} to {} arguments but found {}", f.name, min, max, args.len()))
                                    //     },
                                    // }
                                    // match (f.func)(args) {
                                    //     Err(err) => Err(format!("{}\n\tat {}", err, f.name)),
                                    //     Ok(x) => Ok(x)
                                    // }
                                    if !match f.arity {
                                        Arity::Exact(n) => args.len() == n.into(),
                                        Arity::Min(n) => args.len() >= n.into(),
                                        Arity::Range(min, max) => min as usize <= args.len() && args.len() <= max.into(),
                                    } {
                                        // return Err(format!("Invalid arguments for {}, expected {} but found {}", f.name, f.arity, args.len()))
                                        return Err(error::Error::ArgErr(Some((*f.name).clone()), f.arity.clone(), args.len() as u16))
                                    }
                                    match (f.func)(args) {
                                        // Err(err) => Err(format!("{}\n\tat {}", err, f.name)),
                                        Err(err) => Err(format!("{}\n\tat {}", err, f.name).into()),
                                        Ok(x) => Ok(x)
                                    }
                                },
                                Value::Func{
                                    func, eval, env: fenv, ..
                                } => {
                                    let a = &(func).ast;
                                    // let binds = (&func.params, &func.opt_params, &func.has_kwargs, &func.rest_param);
                                    let binds = &*func;
                                    let local_env = types::EnvStruct::bind(Some(fenv.clone()), binds, args, *eval)?;
                                    ast = a.clone();
                                    env = local_env.clone();
                                    continue 'tco;
                                    // if let Value::List(l) = &func.params {
                                    //     let binds = (l.clone(), func.opt_params.clone(), func.has_kwargs.clone(), func.rest_param.clone());
                                    //     let local_env = types::EnvStruct::bind(Some(fenv.clone()), binds, args, *eval)?;
                                    //     ast = a.clone();
                                    //     env = local_env.clone();
                                    //     continue 'tco;
                                    // } else {
                                    //     Err("Parameter list is not a list".to_string())
                                    // }
                                },
                                _ => Err(format!("Attempt to call non-function {}", func).into()),
                            }
                        }
                        // _ => Err("Expected a list".to_string())
                        _ => unreachable!()
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
            // "--color" => unsafe { types::USE_COLORS = true },
            "--nil-end" => flags &= !1,
            "--color" => flags &= !2,
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
    // repl_env.set("t".to_string(), Value::Sym("t".to_string()));

    eval(vater!{
        (fun enumerate (seq (: opt) (n 0f64))
            (if seq ((sym "lazy-cons") (list n (head seq)) (enumerate (tail seq) (inc n))) nil))
    }, repl_env.clone()).unwrap();

    eval(vater!{
        (defmacro loop (name binds body)
            (quasiquote (block
                (fun (unquote name) (unquote (for (bind binds) (first bind)))
                    (unquote body))
                ((unquote name) ((sym "unquote-splicing") (for (bind binds) (second bind)))))))
    }, repl_env.clone()).unwrap();

    eval(vater!{
        (defmacro printf (fmt (: rest) args)
            (quasiquote (println (format (unquote fmt) ((sym "unquote-splicing") args)))))
    }, repl_env.clone()).unwrap();

    eval(vater!{
        (defmacro run (name)
            (quasiquote (eval (read (str "(block\n" ((sym "read-file") (unquote name)) "\n)")))))
    }, repl_env.clone()).unwrap();
    
    let (flags, args) = arg_parse(env::args());

    match args.len() {
        1 => {
            println!("Vaterite Lisp - Walle - 2020");

            // println!("Sizeof Value = {}", mem::size_of::<Value>());
            
            let mut full_input = String::new();
            let mut done = true;

            loop {
                if done {
                    print!(">> ");
                } else {
                    print!("   ");
                }
                let _ = io::stdout().flush();
                
                let mut input = String::new();
                input.push_str(&full_input);
                io::stdin().read_line(&mut input).expect("Couldn't read input");
            
                let mut reader = parser::Reader::new(input.clone());
                let val = match reader.parse_form() {
                    (false, Err(err)) => {
                        done = true;
                        full_input = String::default();
                        println!("\x1b[31m{}\x1b[0m", err);
                        continue
                    }
                    (true, Err(_)) => {
                        full_input = input;
                        done = false;
                        continue
                    }
                    (false, Ok(_)) => continue,
                    (true, Ok(val)) => {
                        full_input = String::default();
                        done = true;
                        val
                    }
                };
                match val {
                    Value::Sym(x) if x == "exit" => break,
                    _ => match eval(val, repl_env.clone()) {
                        // Ok(e) => println!("{:?}", &e),
                        Ok(e) => println!("{}", types::Printer::repr_color(&e, 0)),
                        // Err(err) => println!("Error: {}", err)
                        Err(err) => println!("\x1b[31mError: {}\x1b[0m", err)
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
                        println!("Error: SyntaxError:{}: {}", err.line, err.err);
                        return;
                    },
                };

                if let parser::Token::Eof = tok {
                    return;
                }

                let val = match tk.parse_expr(tok) {
                    parser::ParserResult::Expr(val) => val,
                    parser::ParserResult::TokenErr(err) => {
                        println!("Error: {}", err);
                        return;
                    },
                    parser::ParserResult::EofErr => {
                        println!("Error: Unexpected end of file");
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
