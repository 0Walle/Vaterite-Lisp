use crate::types;
use crate::parser;
use types::{Expr, ExprErr, ExprList};
use std::rc::Rc;
use std::time::{SystemTime};
use std::fs::File;
use std::io::Read;

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

macro_rules! n_args {
    { $args:expr; $($len:pat => $action:expr),*} => {
        match $args.len() {
            $($len => $action),*
        }
    };
}

macro_rules! predicate_op {
    { $args:expr; $($len:pat => $action:expr),*; $fail:expr} => {
        if $args.len() != 0 {
            match &$args[0] {
                $($len => $action),*,
                _ => $fail
            }
        } else {
            Ok(Expr::Nil)
        }
    };
}

fn operator_eq(v: ExprList) -> ExprErr {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left != e {
            return Ok(Expr::Nil)
        }
    }
    return Ok(Expr::Sym("t".to_string()))
}

fn operator_ne(v: ExprList) -> ExprErr {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left == e {
            return Ok(Expr::Nil)
        }
    }
    return Ok(Expr::Sym("t".to_string()))
}

fn operator_str(v: ExprList) -> ExprErr {
    let mut res = String::new();
    for e in v.iter() {
        res.push_str(&format!("{}", e))
    }
    return Ok(Expr::Str(res));
}

fn operator_head(v: ExprList) -> ExprErr {
    n_args! { v;
        0 => Ok(Expr::Nil),
        1 => {
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
        },
        _ => Err("head requires one argument".to_string())
    }
}

fn operator_nth(v: ExprList) -> ExprErr {
    n_args! { v;
        2 => {
            let n = match &v[1] {
                Expr::Num(n) => *n as usize,
                _ => return Err("Value is not a number".to_string()),
            };

            match &v[0] {
                Expr::List(l) => {
                    if l.len() == 0 {
                        Ok(Expr::Nil)
                    }else{
                        Ok(match l.get(n) {
                            Some(expr) => expr.clone(),
                            None => Expr::Nil
                        })
                    }
                },
                Expr::Nil => Ok(Expr::Nil),
                _ => Err("Value is not a list".to_string()),
            }
        },
        _ => Err("nth requires two arguments".to_string())
    }
}

fn operator_tail(v: ExprList) -> ExprErr {
    n_args! { v;
        0 => Ok(Expr::Nil),
        1 => {
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
        },
        _ => Err("tail requires one argument".to_string())
    }
}

fn operator_cons(v: ExprList) -> ExprErr {
    n_args! { v;
        2 => {
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
        },
        _ => Err("cons requires two arguments".to_string())
    }
}

fn pred_atom(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::List(l) => if l.len() > 0 { Ok(Expr::Nil) } else { Ok(Expr::Sym("t".to_string())) };
        Ok(Expr::Sym("t".to_string()))
    }
}

fn pred_list(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::List(l) => if l.len() > 0 { Ok(Expr::Sym("t".to_string())) } else { Ok(Expr::Nil) };
        Ok(Expr::Nil)
    }
}

fn pred_nil(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::List(l) => if l.len() > 0 { Ok(Expr::Nil) } else { Ok(Expr::Sym("t".to_string())) },
        Expr::Nil => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn pred_number(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::Num(_) => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn pred_string(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::Str(_) => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn pred_symbol(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::Sym(_) => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn pred_keyword(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::Keyword(_) => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn pred_function(v: ExprList) -> ExprErr {
    predicate_op! {v;
        Expr::Func{ .. } => Ok(Expr::Sym("t".to_string())),
        Expr::NatFunc(_) => Ok(Expr::Sym("t".to_string()));
        Ok(Expr::Nil)
    }
}

fn core_apply(v: ExprList) -> ExprErr {
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
}

fn core_map(v: ExprList) -> ExprErr {
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
}

fn core_append(v: ExprList) -> ExprErr {
    let mut result: Vec<Expr> = vec![];
    for seq in v {
        if let Expr::List(l) = seq {
            result.extend_from_slice(&l);
        } else {
            return Err("arguments must be lists".to_string())
        }
    }
    Ok(list!(result))
}

fn core_time_ms(_v: ExprList) -> ExprErr {
    Ok(Expr::Num(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64))
}

fn core_println(v: ExprList) -> ExprErr {
    for expr in v {
        println!("{}", expr);
    }
    Ok(Expr::Nil)
}

fn core_print(v: ExprList) -> ExprErr {
    for expr in v {
        print!("{}", expr);
    }
    Ok(Expr::Nil)
}

fn operator_len(v: ExprList) -> ExprErr {
    n_args! { v;
        0 => Ok(Expr::Nil),
        1 => {
            match &v[0] {
                Expr::List(l) => Ok(Expr::Num(l.len() as f64)),
                Expr::Str(s) => Ok(Expr::Num(s.len() as f64)),
                Expr::Nil => Ok(Expr::Num(0f64)),
                _ => Err("This value is not countable".to_string()),
            }
        },
        _ => Err("len requires one argument".to_string())
    }
}

fn core_read(v: ExprList) -> ExprErr {
    if v.len() == 0 {
        return Err("read requires one argument".to_string())
    }

    if let Expr::Str(input) = v[0].clone(){
        let mut tk = parser::Tokenizer::new(input);
        if let Ok(tok) = tk.next_token() {
            tk.parse_expr(tok)
        }else{
            Err("Invalid Syntax".to_string())
        }
    }else{
        Err("Argument is not a string".to_string())
    }
}

fn core_read_file(v: ExprList) -> ExprErr {
    let file = File::open(match &v[0] {
        Expr::Str(s) => s,
        _ => return Err("Filename must be an string".to_string())
    });
    let mut contents = String::new();
    if let Ok(mut file) = file {
        match file.read_to_string(&mut contents) {
            Ok(_) => Ok(Expr::Str(contents)),
            Err(err) => Err(format!("Couldn't read file: {:?}", err))
        }
        
    } else {
        Err("Couldn't open file".to_string())
    }
}

fn operator_inc(v: ExprList) -> ExprErr {
    n_args! { v;
        1 => {
            match &v[0] {
                Expr::Num(n) => Ok(Expr::Num(*n + 1f64)),
                _ => Err("Value is not a number".to_string()),
            }
        },
        _ => Err("inc requires one argument".to_string())
    }
}

fn operator_dec(v: ExprList) -> ExprErr {
    n_args! { v;
        1 => {
            match &v[0] {
                Expr::Num(n) => Ok(Expr::Num(*n - 1f64)),
                _ => Err("Value is not a number".to_string()),
            }
        },
        _ => Err("inc requires one argument".to_string())
    }
}

pub fn ns() -> Vec<(&'static str, Expr)>{
    vec![
        ("+", types::func(|v: Vec<Expr>| add_mul_op!(+, 0f64, v))),
        ("*", types::func(|v: Vec<Expr>| add_mul_op!(*, 1f64, v))),
        ("-", types::func(|v: Vec<Expr>| sub_div_op!(-, Ok(Expr::Num(0.)), |a: f64| -a, v))),
        ("/", types::func(|v: Vec<Expr>| sub_div_op!(/, Err("Invalid number argument".to_string()), |a: f64| 1./a, v))),
        ("<", types::func(|v: Vec<Expr>| ord_op!(<, v))),
        (">", types::func(|v: Vec<Expr>| ord_op!(>, v))),
        ("<=", types::func(|v: Vec<Expr>| ord_op!(<=, v))),
        (">=", types::func(|v: Vec<Expr>| ord_op!(>=, v))),
        ("==", types::func(operator_eq)),
        ("!=", types::func(operator_ne)),
        ("str", types::func(operator_str)),
        ("list", types::func(|v: Vec<Expr>| Ok(list!(v)))),
        ("first", types::func(operator_head)),
        ("second", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(1f64)]))),
        ("third", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(2f64)]))),
        ("fouth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(3f64)]))),
        ("fifth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(4f64)]))),
        ("sixth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(5f64)]))),
        ("seventh", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(6f64)]))),
        ("eigth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(6f64)]))),
        ("nineth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(6f64)]))),
        ("tenth", types::func(|v: Vec<Expr>| operator_nth(vec![v.get(0).unwrap_or(&Expr::Nil).clone(), Expr::Num(6f64)]))),
        ("nth", types::func(operator_nth)),
        ("head", types::func(operator_head)),
        ("tail", types::func(operator_tail)),
        ("cons", types::func(operator_cons)),
        ("atom?", types::func(pred_atom)),
        ("list?", types::func(pred_list)),
        ("nil?", types::func(pred_nil)),
        ("number?", types::func(pred_number)),
        ("string?", types::func(pred_string)),
        ("symbol?", types::func(pred_symbol)),
        ("function?", types::func(pred_function)),
        ("keyword?", types::func(pred_keyword)),
        ("apply", types::func(core_apply)),
        ("map", types::func(core_map)),
        ("append", types::func(core_append)),
        ("time-ms", types::func(core_time_ms)),
        ("println", types::func(core_println)),
        ("print", types::func(core_print)),
        ("len", types::func(operator_len)),
        ("read", types::func(core_read)),
        ("read-file", types::func(core_read_file)),
        ("inc", types::func(operator_inc)),
        ("dec", types::func(operator_dec)),
    ]
}