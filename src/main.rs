#[macro_use]
mod parser;
mod types;
mod core;
mod error;
mod printer;
mod names;

use std::io;
use std::io::Write;
use std::rc::Rc;
use std::fs::File;
use std::io::prelude::*;
use std::env;
use std::path::Path;
use std::collections::HashMap;

// TODO: Name interning
// TODO: Remove chars
// TODO: Make "slice vector"
// TODO: Struct definition type
// TODO: Compile!!

use crate::types::{Value, Env, ValueList, FuncData, Arity};
use crate::printer::Printer;
use crate::names::{NamePool, Name};
use crate::names::builtin as stdname;

type ValueResult = Result<Value, error::Error>;
macro_rules! arg_err {
    ( $name:expr; $min:literal .. $max:literal; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Range($min,$max),$got as u16) };
    ( $name:expr; $ex:literal; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Exact($ex),$got as u16) };
    ( $name:expr; $min:literal .. ; $got:expr ) => { error::Error::ArgErr(Some($name.into()),Arity::Min($min),$got as u16) };
}
macro_rules! type_err {
    ( $type:literal; $got:expr ) => { error::Error::TypeErr($type, Some($got)) };
}
macro_rules! pair_err {
    ( $name:expr ) => { error::Error::PairErr(Some($name.into())) };
}

/// Quasiquote macro
fn quasiquote(ast: Value) -> Value {
    match ast {
        Value::Sym(_) => vater!{ (QUOTE [ast]) },
        Value::List(list) if list.len() > 0 => {
            let head = &list[0];
            match head {
                Value::Sym(s) if s == &stdname::UNQUOTE => return if list.len() > 1 { list[1].clone() } else { Value::Nil },
                Value::List(l) if l.len() > 0 => match &l[0] {
                    Value::Sym(s) if s == &stdname::UNQUOTE_SPLICING => return vater!{
                        (APPEND [if l.len() > 1 { l[1].clone() } else { Value::Nil}] [quasiquote(list[1..].to_vec().into())])
                    },
                    _ => {}
                }
                _ => {}
            };
            let car = quasiquote(head.clone());
            let cdr = quasiquote(list[1..].to_vec().into());
            if cdr.is_nil() {
                return vater!{ (LIST [car]) }
            }
            
            if let Value::List(l) = &cdr {
                return match &l[0] {
                    Value::Sym(s) if s == &stdname::LIST => {
                        let mut start = vec![vater!(LIST), car];
                        start.extend_from_slice(&l[1..]);
                        start.into()
                    }
                    _ => vater!{ (CONS [car] [cdr]) }
                }
            }

            return vater!{ (CONS [car] [cdr]) }
        }
        _ => ast
    }
}

/// Tests if a list is a macro call
fn is_macro_call(ast: Value, env: Env) -> bool {
    match ast {
        Value::List(l) if l.len() > 0 => if let Value::Sym(sym) = &l[0] {
            match env.get(*sym) {
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
fn macro_expand(mut ast: Value, mut env: Env, names: &NamePool) -> (bool, ValueResult) {
    let mut was_expanded = false;
    while is_macro_call(ast.clone(), env.clone()) {
        if let Value::List(l) = &ast { 
            if let Value::Sym(s) = &l[0] {
                let makro = if let Ok(name) = env.get(*s) {
                    name
                }else{
                    return (false, Err("Macro not defined".into()));
                };

                if let Value::Func { 
                    env: menv, func, eval, ..
                } = makro {
                    let args = l[1..].to_vec();
                    let binds = &*func;
                    let macro_scope = match types::EnvStruct::bind(Some(menv.clone()), binds, args, eval, names){
                        Ok(scope) => scope,
                        Err(err) => return (false, Err(err))
                    };
                    let macro_ast = &func.ast;
                    ast = match eval(macro_ast.clone(), macro_scope.clone(), names) {
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
fn eval_ast(ast: &Value, env: &Env, names: &NamePool) -> ValueResult {
    match ast {
        Value::Sym(sym) => env.get(sym.clone()),
        Value::List(v) =>  {
            let mut lst: ValueList = vec![];
            for expr in v.iter() {
                lst.push(eval(expr.clone(), env.clone(), names)?)
            }
            Ok(Value::List(Rc::new(lst)))
        }
        _ => Ok(ast.clone())
    }
}

/// Produces an lambda list from an vaterite lambda list
fn from_lambda_list(list: ValueList) -> Result<(Vec<Name>, Vec<(Name, Value)>, bool, Option<Name>, Arity), error::Error> {
    let mut req: Vec<Name> = vec![];
    let mut opt: Vec<(Name, Value)> = vec![];
    let mut keys = false;
    let mut rest: Option<Name> = None;

    let mut state = 0;

    for (i, expr) in list.iter().enumerate() {
        match state {
            0 => {
                match expr {
                    Value::Keyword(s) if *s == names::builtin::REST => {
                        rest = Some(match &list[i + 1] { 
                            Value::Sym(rest_sym) => *rest_sym,
                            x => return Err(type_err!("symbol"; x.clone()))
                        });
                        break;
                    },
                    Value::Keyword(s) if *s == names::builtin::OPT => {state = 1;},
                    Value::Keyword(s) if *s == names::builtin::KEY => {state = 1; keys = true},
                    Value::Sym(s) => req.push(*s),
                    x => return Err(type_err!("symbol"; x.clone()))
                }
            }
            1 => {
                match expr {
                    Value::Keyword(s) if *s == names::builtin::REST => {
                        rest = Some(match &list[i + 1] { 
                            Value::Sym(rest_sym) => *rest_sym,
                            x => return Err(type_err!("symbol"; x.clone()))
                        });
                        break;
                    },
                    Value::Sym(s) => opt.push((*s, Value::Nil)),
                    Value::List(l) if l.len() == 2 => {
                        match &l[0] {
                            Value::Sym(s) => opt.push((*s, l[1].clone())),
                            x => return Err(type_err!("symbol"; x.clone()))
                        }
                    }
                    x => return Err(type_err!("symbol, pair"; x.clone()))
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

fn match_pattern(pat: Value, expr: Value, env: Env, names: &NamePool) -> Option<Result<HashMap<Name, Value>, String>> {
    match pat.clone() {
        Value::Sym(s) if s == stdname::IT_ => {
            Some(Ok(HashMap::default()))
        }
        Value::Sym(s) if s == stdname::NIL => {
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
                Value::Sym(s) if s == &stdname::QUOTE => {
                    if l.get(1).unwrap_or(&Value::Nil).clone() == expr.clone() {
                        Some(Ok(HashMap::default()))
                    } else {
                        None
                    }
                }
                Value::Sym(s) if s == &stdname::LIST => {
                    if let Value::List(expr) = expr {
                        if l.len() == expr.len() + 1 {
                            let mut all_binds = HashMap::default();
                            for (pat, expr) in l[1..].iter().zip(expr.iter()) {
                                match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::CONS => {
                    if let Value::List(expr) = expr {
                        let last = l.len();
                        if l.len() <= expr.len() + 2 {
                            let mut all_binds = HashMap::default();
                            for (pat, expr) in l[1..last-1].iter().zip(expr.iter()) {
                                match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                            match match_pattern(l[last-1].clone(), expr[last-2..].to_vec().into(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::REV_CONS => {
                    if let Value::List(expr) = expr {
                        let pat_len = l.len() - 1;
                        let expr_len = expr.len();
                        if pat_len <= expr.len() + 1 {
                            let mut all_binds = HashMap::default();
                            for i in 0..pat_len-1 {
                                match match_pattern(l[pat_len-i].clone(), expr[expr_len-i-1].clone(), env.clone(), names) {
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
                            match match_pattern(l[1].clone(), expr[0..expr_len+1-pat_len].to_vec().into(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::IF => {
                    let func = match eval(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone(), names) {
                        Ok(pred) => pred,
                        Err(err) => return Some(Err(Printer::str_error(&err, names)))
                    };

                    let mut all_binds = HashMap::default();
                    if match func.apply(vec![expr.clone()], names) {
                        Ok(res) => !res.is_false(),
                        Err(err) => return Some(Err(Printer::str_error(&err, names)))
                    } {
                        for pat in l[2..].iter() {
                            match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::AND => {
                    let mut all_binds = HashMap::default();
                    for pat in l[1..].iter() {
                        match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::OR => {
                    for pat in l[1..].iter() {
                        match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                Value::Sym(s) if s == &stdname::HASH_MAP => {
                    let map = match expr {
                        Value::Map(map) => map.clone(),
                        _ => return None
                    };

                    let mut all_binds = HashMap::default();
                    for i in (1..l.len()).step_by(2) {
                        let key = match &l[i] {
                            Value::Keyword(n) | Value::Sym(n) => *n,
                            _ => return Some(Err(format!("Value cannot be used as key in hash-map pattern")))
                        };

                        if let Some(val) = map.get(&key) {
                            match match_pattern(l[i+1].clone(), val.clone(), env.clone(), names) {
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
                /*Value::Sym(s) if s == &stdname::STRUCT => {
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
                                match match_pattern(pat.clone(), expr.clone(), env.clone(), names) {
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
                }*/
                Value::Sym(s) if s == &stdname::FROM => {
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
                Value::Sym(s) if s == &stdname::MACRO_EXPAND => {
                    match macro_expand(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone(), names) {
                        (true, Ok(nast)) => {
                            match_pattern(nast, expr, env, names)
                        }
                        (_, Err(err)) => Some(Err(Printer::str_error(&err, names))),
                        _ => Some(Err(format!("Failed expanding pattern as macro")))
                    }
                }
                // _ => Some(Err(format!("Invalid pattern {:?}", pat.clone())))
                _ => match macro_expand(pat.clone(), env.clone(), names) {
                    (true, Ok(nast)) => {
                        match_pattern(nast, expr, env, names)
                    }
                    (_, Err(err)) => Some(Err(Printer::str_error(&err, names))),
                    _ => Some(Err(format!("Invalid pattern")))
                }
            }
        }
        Value::Str(s) => if let Value::Str(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::Num(s) => if let Value::Num(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::Char(c) => if let Value::Char(expr) = expr { if c == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        Value::False => if let Value::False = expr { Some(Ok(HashMap::default())) } else { None }
        Value::True => if let Value::True = expr { Some(Ok(HashMap::default())) } else { None }
        Value::Keyword(s) => if let Value::Keyword(expr) = expr { if s == expr { Some(Ok(HashMap::default())) } else { None } } else { None }
        _ => Some(Err(format!("Invalid pattern")))
    }
}

/// Evaluate an expression
fn eval(mut ast: Value, mut env: Env, names: &NamePool) -> ValueResult {
    let ret: ValueResult;

    'tco: loop {
        ret = match ast.clone() {
            Value::List(l) => {
                if l.len() == 0 {
                    return Ok(Value::Nil);
                }

                match macro_expand(ast.clone(), env.clone(), names) {
                    (true, Ok(nast)) => {
                        ast = nast;
                        continue 'tco;
                    }
                    (_, Err(err)) => return Err(err),
                    _ => ()
                }
                
    
                let head = &l[0];
                match head {
                    Value::Sym(sym) if sym == &stdname::QUOTE => 
                        if l.len() != 2 {
                            Err(arg_err!(stdname::QUOTE; 1; l.len()))
                        }else{
                            Ok(l[1].clone())
                        }
                    Value::Sym(sym) if sym == &stdname::QUASIQUOTE => 
                        if l.len() != 2 {
                            Err(arg_err!(stdname::QUASIQUOTE; 1; l.len()))
                        }else{
                            ast = quasiquote(l[1].clone());
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == &stdname::MACRO_EXPAND => macro_expand(l[1].clone(), env.clone(), names).1,
                    Value::Sym(sym) if sym == &stdname::IF => 
                        if l.len() != 4 {
                            Err(arg_err!(stdname::IF; 3; l.len()))
                        }else if eval(l[1].clone(), env.clone(), names)?.is_false() {
                            ast = l[3].clone();
                            continue 'tco;
                        } else {
                            ast = l[2].clone();
                            continue 'tco;
                        }
                    Value::Sym(sym) if sym == &stdname::COND => {
                        let mut res: ValueResult = Ok(Value::Nil);
                        for pair in l[1..].iter() {
                            res = if let Some((cond, expr)) = pair.to_pair() {
                                if !eval(cond.clone(), env.clone(), names)?.is_false() {
                                    ast = expr.clone();
                                    continue 'tco;
                                } else {
                                    Ok(Value::Nil)
                                }
                            } else {
                                Err(pair_err!("cond clause"))
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == &stdname::MATCH => {
                        let value = eval(l.get(1).unwrap_or(&Value::Nil).clone(), env.clone(), names)?;
                        for pair in l[2..].iter() {
                            match pair.to_pair() {
                                Some((pat, expr)) => {
                                    match match_pattern(pat.clone(), value.clone(), env.clone(), names) {
                                        Some(res) => match res {
                                            Ok(binds) => {
                                                let local_env = types::EnvStruct::new(Some(env.clone()));
                                                for (k, v) in binds {
                                                    local_env.set(k, v);
                                                }
                                                ast = expr.clone();
                                                env = local_env.clone();
                                                continue 'tco;
                                            }
                                            Err(err) => return Err(format!("Pattern Error: {}", err).into())
                                        }
                                        None => {}
                                    }
                                }
                                _ => return Err(pair_err!("match clause"))
                            }
                        };
                        Err("No matching pattern found".into())
                    }
                    Value::Sym(sym) if sym == &stdname::AND => {
                        let mut res: ValueResult = Ok(Value::True);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone(), names)?;
                            if expr.is_false() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == &stdname::OR => {
                        let mut res: ValueResult = Ok(Value::False);
                        for cond in l[1..].iter() {
                            let expr = eval(cond.clone(), env.clone(), names)?;
                            if !expr.is_false() {
                                res = Ok(expr);
                                break;
                            }
                        };
                        res
                    }
                    Value::Sym(sym) if sym == &stdname::DEF => 
                        if l.len() < 3 {
                            Err(arg_err!(stdname::DEF; 2; l.len()))
                        } else if let Value::Sym(s) = &l[1] {
                            env.set(*s, eval(l[2].clone(), env.clone(), names)?);
                            Ok(Value::Nil)
                        } else {
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    /*Value::Sym(sym) if sym == &stdname::STRUCT =>
                        if l.len() < 3 {
                            // Err("struct form requires 3 arguments".to_string())
                            Err(arg_err!("struct"; 2; l.len()))
                        } else if let Value::Sym(name) = &l[1] {
                            if let Some(fields) = &l[2].to_vec() {
                                let mut ctor = vec![ vater!{(sym "make-struct")}, vater!{(QUOTE (sym name))}];
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
                        }*/
                    Value::Sym(sym) if sym == &stdname::FUN =>
                        if l.len() < 4 {
                            Err(arg_err!(stdname::FUN; 3; l.len()))
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
                                        name: Some(*s),
                                        arity,
                                    }),
                                    is_macro: false,
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                Err(type_err!("list"; l[2].clone()))
                            }
                        } else {
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == &stdname::DEFMACRO =>
                        if l.len() < 4 {
                            Err(arg_err!(stdname::DEFMACRO; 3; l.len()))
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
                                        name: Some(*s),
                                        arity,
                                    }),
                                    is_macro: true,
                                };
                                env.set(s.clone(), func);
                                Ok(Value::Nil)
                            } else {
                                Err(type_err!("list"; l[2].clone()))
                            }
                        } else {
                            Err(type_err!("symbol"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == &stdname::FN => 
                        if l.len() < 3 {
                            Err(arg_err!(stdname::FN; 2..; l.len()))
                        } else if let Some(params) = l[1].to_vec() {
                            let mut body = vec![Value::Sym(stdname::BLOCK)];
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
                                is_macro: false,
                            })
                        } else {
                            Err(type_err!("list"; l[1].clone()))
                        }
                    Value::Sym(sym) if sym == &stdname::LET => {
                        let len = l.len();
                        if len < 3 {
                            Err(arg_err!(stdname::LET; 2..; l.len()))
                        } else if let Some(binds) = &l[1].to_vec() {
                            let local_env = types::EnvStruct::new(Some(env.clone()));
                            for pair in binds.iter() {
                                match pair.to_pair() {
                                    Some((name, value)) => {
                                        if let Value::Sym(name) = &name {
                                            local_env.set(name.clone(), eval(value.clone(), local_env.clone(), names)?)
                                        }
                                    }
                                    _ => return Err(pair_err!("let binding"))
                                }
                            }
                            for expr in l[2..len-1].iter() {
                                eval(expr.clone(), local_env.clone(), names)?;
                            };
                            ast = l[len-1].clone();
                            env = local_env.clone();
                            continue 'tco
                        } else {
                            Err(type_err!("list"; l[1].clone()))
                        }
                    }
                    Value::Sym(sym) if sym == &stdname::FOR => {
                        let len = l.len();
                        if len == 1 {
                            return Ok(Value::Nil);
                        }
                        let mut result: ValueList = vec![];
                        let mut iters: HashMap<Name, Value> = HashMap::default();
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for bind in l[1..len-1].iter() {
                            if let Some((name, value)) = bind.to_pair() {
                                if let Value::Sym(name) = &name {
                                    iters.insert(*name, eval(value.clone(), env.clone(), names)?);
                                }
                            } else {
                                return Err(pair_err!("for binding"))
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
                            result.push(eval(l[len-1].clone(), local_env.clone(), names)?);
                            let mut new_iters: HashMap<Name, Value> = HashMap::default();
                            for (name, iter) in iters.iter() {
                                let tail = iter.rest(names)?;
                                new_iters.insert(name.clone(), tail);
                            }
                            iters = new_iters;
                        }
                    }
                    Value::Sym(sym) if sym == &stdname::MODULE => {
                        let len = l.len();
                        if len < 2 {
                            return Err(arg_err!(stdname::MODULE; 2..; l.len()))
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            return Err(type_err!("symbol"; l[1].clone()))
                        };

                        let mut exports: ValueList = vec![];
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for expr in l[2..len].iter() {
                            match &expr {
                                Value::List(l) if l.len() > 0 => {
                                    match &l[0] {
                                        Value::Sym(s) if s == &stdname::EXPORTS => exports = (&l[1..]).to_vec(),
                                        _ => {eval(expr.clone(), local_env.clone(), names)?;}
                                    }
                                }
                                _ => {eval(expr.clone(), local_env.clone(), names)?;}
                            }
                            
                        };
                        for expr in exports {
                            match &expr {
                                Value::Sym(s) => {
                                    let exported = format!("{}/{}", names.get(*mod_name), names.get(*s));
                                    env.set(names.add(&exported), local_env.get(*s)?)
                                }
                                x => return Err(type_err!("symbol"; x.clone()))
                            }
                        }

                        return Ok(Value::Nil);
                    }
                    Value::Sym(sym) if sym == &stdname::EXPORTS => Ok(Value::Nil),
                    Value::Sym(sym) if sym == &stdname::IMPORT => {
                        let len = l.len();
                        if len < 3 {
                            return Err(arg_err!(stdname::IMPORT; 1..; l.len()))
                        }

                        let mod_name = if let Value::Sym(module) = &l[1] {
                            module
                        } else {
                            return Err(type_err!("symbol"; l[1].clone()))
                        };

                        let mut modules: ValueList = vec![];
                        if match env.get(stdname::SP_MODULES)? {
                            Value::List(l) => {
                                modules = (*l).clone();
                                l.iter().any(|i| match i {
                                    Value::Sym(s) => s == mod_name,
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

                        let file_content = format!("({})", contents);
                        let mut tk = parser::Reader::new(&file_content, names);
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

                        modules.push(Value::Sym(*mod_name));
                        env.assign(stdname::SP_MODULES, modules.into())?;
                        

                        let mut exports: ValueList = vec![];
                        let local_env = types::EnvStruct::new(Some(env.clone()));
                        for expr in exprs.iter() {
                            match &expr {
                                Value::List(l) if l.len() > 0 => {
                                    match &l[0] {
                                        Value::Sym(s) if s == &stdname::EXPORTS => exports = (&l[1..]).to_vec(),
                                        _ => {eval(expr.clone(), local_env.clone(), names)?;}
                                    }
                                }
                                _ => {eval(expr.clone(), local_env.clone(), names)?;}
                            }
                            
                        };
                        for expr in exports {
                            match &expr {
                                Value::Sym(s) => {
                                    let exported = format!("{}/{}", names.get(*mod_name), names.get(*s));
                                    env.set(names.add(&exported), local_env.get(*s)?)
                                }
                                x => return Err(type_err!("symbol"; x.clone()))
                            }
                        }
                        return Ok(Value::Nil);
                    }
                    Value::Sym(sym) if sym == &stdname::BLOCK => {
                        let len = l.len();
                        if len == 1 {
                            return Ok(Value::Nil);
                        }

                        for expr in l[1..len-1].iter() {
                            eval(expr.clone(), env.clone(), names)?;
                        };
                        ast = l[len-1].clone();
                        continue 'tco;
                    }
                    Value::Sym(ref sym) if sym == &stdname::EVAL => {
                        ast = eval(l[1].clone(), env.clone(), names)?;
                        while let Some(ref e) = env.clone().access {
                            env = e.clone();
                        }
                        continue 'tco;
                    }
                    Value::Sym(ref sym) if sym == &stdname::THROW => {
                        Err(error::Error::Throw(l.get(1).cloned()))
                    }
                    Value::Sym(ref sym) if sym == &stdname::CATCH => {
                        if l.len() == 3 {
                            match eval(l[1].clone(), env.clone(), names) {
                                Ok(val) => Ok(val),
                                Err(_) => {
                                    ast = l[2].clone();
                                    continue 'tco;
                                }
                            }
                        } else if l.len() == 4 {
                            match eval(l[1].clone(), env.clone(), names) {
                                Ok(val) => Ok(val),
                                Err(err) => {
                                    let local_env = types::EnvStruct::new(Some(env.clone()));
                                    if let Some((kind, value)) = &l[2].to_pair() {
                                        let kind = match kind {
                                            Value::Sym(n) => n,
                                            _ => return Err(type_err!("symbol"; kind.clone()))
                                        };
                                        let value = match value {
                                            Value::Sym(n) => n,
                                            _ => return Err(type_err!("symbol"; value.clone()))
                                        };
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
                                                    Some(name) => Value::Sym(name),
                                                    None => Value::Nil
                                                };
                                                local_env.set(value.clone(), vater!{ ([name] [Value::Num(got as f64)]) }); "ArityError"
                                            }
                                            KwArgErr(name) => {
                                                let name = match name {
                                                    Some(name) => Value::Sym(name),
                                                    None => Value::Nil
                                                };
                                                local_env.set(value.clone(), vater!{ ([name]) }); "KwargsError"
                                            }
                                            TypeErr(ty, got) => {
                                                local_env.set(value.clone(), vater!{ ([Value::Str(ty.to_string())] [got.unwrap_or(Value::Nil)]) }); "TypeError"
                                            }
                                            BindErr(name) => {
                                                local_env.set(value.clone(), Value::Sym(name)); "NameError"
                                            }
                                            PairErr(name) => {
                                                local_env.set(value.clone(), name.into()); "PairError"
                                            }
                                        };
                                        local_env.set(kind.clone(), Value::Str(sym.to_string()));
                                    } else {
                                        return Err(pair_err!("catch binding"))
                                    }
                                    env = local_env;
                                    ast = l[3].clone();
                                    continue 'tco;
                                }
                            }
                        } else {
                            return Err(arg_err!(stdname::CATCH; 2..3; l.len()))
                        }
                    }
                    Value::Sym(ref sym) if sym == &stdname::LAZY_CONS => {
                        if l.len() < 3 {
                            return Err(arg_err!(stdname::LAZY_CONS; 2; l.len()))
                        }
                        let head = eval(l[1].clone(), env.clone(), names)?;
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
                        match eval(l[1].clone(), env.clone(), names)? {
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
                            x => return Err(type_err!("map"; x))
                        }
                    }
                    _ => match ast {
                        Value::List(v) =>  {
                            let mut list: Vec<Value> = vec![];
                            for expr in v.iter() {
                                list.push(eval(expr.clone(), env.clone(), names)?)
                            }
                            let ref func = list[0].clone();
                            let args = list[1..].to_vec();
                            match func {
                                Value::NatFunc(f) => {
                                    if !match f.arity {
                                        Arity::Exact(n) => args.len() == n.into(),
                                        Arity::Min(n) => args.len() >= n.into(),
                                        Arity::Range(min, max) => min as usize <= args.len() && args.len() <= max.into(),
                                    } {
                                        return Err(error::Error::ArgErr(Some(f.name), f.arity.clone(), args.len() as u16))
                                    }
                                    match (f.func)(args, names) {
                                        Err(err) => Err(format!("{}\n\tat {}", Printer::str_error(&err, names), names.get(f.name)).into()),
                                        Ok(x) => Ok(x)
                                    }
                                },
                                Value::Func{
                                    func, eval, env: fenv, ..
                                } => {
                                    let a = &(func).ast;
                                    let binds = &*func;
                                    let local_env = types::EnvStruct::bind(Some(fenv.clone()), binds, args, *eval, names)?;
                                    ast = a.clone();
                                    env = local_env.clone();
                                    continue 'tco;
                                },
                                _ => Err(format!("Attempt to call non-function {}", Printer::repr_name(func, names)).into()),
                            }
                        }
                        _ => unreachable!()
                    }
                }
            }
            _ => eval_ast(&ast, &env, names)
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
            "--nil-end" => flags &= !1,
            "--color" => flags &= !2,
            _ => common_args.push(arg)
        }
    };
    (flags, common_args)
}

fn main() {
    let repl_env = types::EnvStruct::new(None);
    let pool = NamePool::new();

    // add functions from core to the environment
    for (name, arity, func) in core::ns() {
        let name = pool.add(name);
        repl_env.set(name, types::func(name, arity, func));
    }
    repl_env.set(stdname::SP_DIR_NAME, ".".into());
    repl_env.set(stdname::SP_MODULES, Value::Nil);
    repl_env.set(stdname::NIL, Value::Nil);

    {
        let name_printf = pool.add("printf");
        let name_run = pool.add("run");

        let name_println = pool.add("println");
        let name_eval = pool.add("eval");
        let name_read = pool.add("read");
        let name_read_file = pool.add("read-file");

        let name_a = pool.add("#a");
        let name_b = pool.add("#b");
        let name_c = pool.add("#c");
        let name_d = pool.add("#d");
        
        eval(vater!{
            (DEFMACRO LOOP ([name_a] [name_b] [name_c])
                (QUASIQUOTE (BLOCK
                    (FUN (UNQUOTE [name_a]) (UNQUOTE (FOR ([name_d] [name_b]) (FIRST [name_d])))
                        (UNQUOTE [name_c]))
                    ((UNQUOTE [name_a]) (UNQUOTE_SPLICING (FOR ([name_d] [name_b]) (SECOND [name_d])))))))
        }, repl_env.clone(), &pool).ok().unwrap();
    
        eval(vater!{
            (DEFMACRO [name_printf] ([name_a] (: REST) [name_b])
                (QUASIQUOTE ([name_println] (FORMAT (UNQUOTE [name_a]) (UNQUOTE_SPLICING [name_b])))))
        }, repl_env.clone(), &pool).ok().unwrap();
    
        eval(vater!{
            (DEFMACRO [name_run] ([name_a])
                (QUASIQUOTE ([name_eval] ([name_read] (STR "(block\n" ([name_read_file] (UNQUOTE [name_a])) "\n)")))))
        }, repl_env.clone(), &pool).ok().unwrap();
    }
    
    let (flags, args) = arg_parse(env::args());

    match args.len() {
        1 => {
            println!("Vaterite Lisp - Walle - 2020");

            println!("Sizeof Value = {}", std::mem::size_of::<crate::types::NatFunc>());
            
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
            
                let mut reader = parser::Reader::new(&input, &pool);
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
                    Value::Sym(x) if x == stdname::EXIT => break,
                    _ => match eval(val, repl_env.clone(), &pool) {
                        Ok(e) => println!("{}", Printer::repr_color(&e, 0, &pool)),
                        Err(err) => println!("\x1b[31mError: {}\x1b[0m", Printer::str_error(&err, &pool))
                    }
                }
            }
            println!("Done!");
        }
        2 => {
            let filename = &args[1];

            repl_env.set(stdname::SP_DIR_NAME, match Path::new(filename).parent() {
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

                let mut tk = parser::Reader::new(&contents, &pool);
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
                match eval(val, repl_env.clone(), &pool) {
                    Ok(e) => if flags & 1 != 0 {println!("{}", Printer::repr_name(&e, &pool))},
                    Err(err) => println!("Error: {}", Printer::str_error(&err, &pool)),
                }
            } else {
                println!("Couldn't open file");
            }
        }
        _ => println!("Invalid arguments")
    }
}
