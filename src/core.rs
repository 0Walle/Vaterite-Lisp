use crate::types;
use crate::parser;
use types::{Value, ValueErr, ValueList};
use std::rc::Rc;
use std::cell::RefCell;
use std::time::{SystemTime};
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;

macro_rules! ord_op {
    ($op:tt, $v:expr) => {{
        let mut left = match &$v[0] {
            Value::Num(n) => *n,
            _ => return Err("Invalid number arguments".to_string())
        };
        for e in $v[1..].iter() {
            if let Value::Num(n) = e {
                if left $op *n {
                    left = *n;
                    continue
                }else{
                    return Ok(Value::Nil)
                }
            }else{
                return Err("Invalid number arguments".to_string())
            }
        }
        return Ok(Value::Sym("t".to_string()))
    }};
}

macro_rules! add_mul_op {
    ($op:tt, $init:expr, $args:expr) => {
        Ok(Value::Num(
            $args.iter().fold(Ok($init), |acc, val| if let Value::Num(n) = val {
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
        }else if let Value::Num(first) = &$args[0]{
            if $args.len() > 1 {
                Ok(Value::Num($args[1..].iter().fold(Ok(first.clone()), |acc, val| if let Value::Num(n) = val {
                    Ok(acc? $op *n)
                }else{
                    return Err("Invalid number arguments".to_string())
                })?))
            }else{
                Ok(Value::Num($one(*first)))
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
            Ok(Value::Nil)
        }
    };
}

fn operator_eq(v: ValueList) -> ValueErr {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left != e {
            return Ok(Value::Nil)
        }
    }
    return Ok(Value::Sym("t".to_string()))
}

fn operator_ne(v: ValueList) -> ValueErr {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left == e {
            return Ok(Value::Nil)
        }
    }
    return Ok(Value::Sym("t".to_string()))
}

fn operator_str(v: ValueList) -> ValueErr {
    let mut res = String::new();
    for e in v.iter() {
        res.push_str(&format!("{}", e))
    }
    return Ok(Value::Str(res));
}

pub fn operator_head(v: ValueList) -> ValueErr {
    n_args! { v;
        0 => Ok(Value::Nil),
        1 => {
            match &v[0] {
                Value::List(l) => {
                    if l.len() == 0 {
                        Ok(Value::Nil)
                    }else{
                        Ok(l[0].clone())
                    }
                },
                Value::Nil => Ok(Value::Nil),
                Value::Lazy{head, ..} => Ok((**head).clone()),
                _ => Err("Value is not a list".to_string()),
            }
        },
        _ => Err("head requires one argument".to_string())
    }
}

fn operator_nth(v: ValueList) -> ValueErr {
    n_args! { v;
        2 => {
            let n = match &v[1] {
                Value::Num(n) => *n as usize,
                _ => return Err("Value is not a number".to_string()),
            };

            match &v[0] {
                Value::List(l) => {
                    if l.len() == 0 {
                        Ok(Value::Nil)
                    }else{
                        Ok(match l.get(n) {
                            Some(expr) => expr.clone(),
                            None => Value::Nil
                        })
                    }
                },
                Value::Nil => Ok(Value::Nil),
                Value::Lazy{env, eval, tail, head} => {
                    if n == 0 {
                        return Ok((&**head).clone());
                    }

                    let mut count = n;
                    let mut nth = (**tail).clone();
                    let mut env = env.clone();
                    loop {
                        count -= 1;
                        match eval(nth, env.clone())? {
                            Value::Lazy{env: tenv, tail: ttail, head, ..} => {
                                if count == 0 {
                                    break Ok((*head).clone())
                                }else{
                                    nth = (*ttail).clone();
                                    env = tenv;
                                }
                            }
                            _ => break Ok(Value::Nil)
                        }
                    }
                },
                _ => Err("Value is not a list".to_string()),
            }
        },
        _ => Err("nth requires two arguments".to_string())
    }
}

pub fn operator_tail(v: ValueList) -> ValueErr {
    n_args! { v;
        0 => Ok(Value::Nil),
        1 => {
            match &v[0] {
                Value::List(l) => {
                    if l.len() == 0 {
                        Ok(Value::Nil)
                    }else{
                        Ok(list!(l[1..].to_vec()))
                    }
                },
                Value::Nil => Ok(Value::Nil),
                Value::Lazy{env, eval, tail, ..} => eval((**tail).clone(), env.clone()),
                _ => Err("Value is not a list".to_string()),
            }
        },
        _ => Err("tail requires one argument".to_string())
    }
}

fn operator_cons(v: ValueList) -> ValueErr {
    n_args! { v;
        2 => {
            match &v[1] {
                Value::List(l) => {
                    if l.len() == 0 {
                        Ok(list![vec![v[0].clone()]])
                    }else{
                        let mut new = vec![v[0].clone()];
                        new.extend_from_slice(&l);
                        Ok(list![new])
                    }
                },
                Value::Nil => Ok(list![vec![v[0].clone()]]),
                _ => Err("Can't cons to a non-list".to_string()),
            }
        },
        _ => Err("cons requires two arguments".to_string())
    }
}

fn core_hashmap(v: ValueList) -> ValueErr {
    if v.len() % 2 != 0 {
        return Err("Arguments are not in pairs".to_string());
    }

    let mut map: HashMap<String, Value> = HashMap::default();

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Str(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Sym(s) => map.insert(s.clone(), v[i+1].clone()),
            _ => return Err("Value can't be used as key".to_string()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_assoc(v: ValueList) -> ValueErr {
    if v.len() < 1 {
        return Err("assoc requires a map as first argument".to_string());
    }
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return Err("assoc requires a map as first argument".to_string());
    };

    let v = &v[1..];

    if v.len() % 2 != 0 {
        return Err("Arguments are not in pairs".to_string());
    }

    //let mut map: HashMap<String, Value> = HashMap::default();

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Str(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Sym(s) => map.insert(s.clone(), v[i+1].clone()),
            _ => return Err("Value can't be used as key".to_string()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_dissoc(v: ValueList) -> ValueErr {
    if v.len() < 1 {
        return Err("dissoc requires a map as first argument".to_string());
    }
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return Err("dissoc requires a map as first argument".to_string());
    };

    let v = &v[1..];

    for key in v {
        match key {
            Value::Keyword(s) => map.remove(s),
            Value::Str(s) => map.remove(s),
            Value::Sym(s) => map.remove(s),
            _ => return Err("Value can't be used as key".to_string()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_map_get(v: ValueList) -> ValueErr {
    if v.len() < 2 {
        return Err("get-key requires a map as first argument".to_string());
    }
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return Err("get-key requires a map as first argument".to_string());
    };

    match &v[1] {
        Value::Keyword(s) | Value::Str(s) | Value::Sym(s) => match map.get(s){
            Some(v) => Ok(v.clone()),
            None => Err(format!("Key {} is not present in map", s))
        },
        _ => return Err("Value can't be used as key".to_string()),
    }
}

fn operator_has_key(v: ValueList) -> ValueErr {
    if v.len() < 2 {
        return Err("has-key? requires a map as first argument".to_string());
    }
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return Err("has-key? requires a map as first argument".to_string());
    };

    if match &v[1] {
        Value::Keyword(s) | Value::Str(s) | Value::Sym(s) => map.contains_key(s),
        _ => return Err("Value can't be used as key".to_string()),
    } {
        return Ok(Value::Sym("t".to_string()));
    };
    Ok(Value::Nil)
}

fn core_map_keys(v: ValueList) -> ValueErr {
    if v.len() < 1 {
        return Err("map-keys requires a map as first argument".to_string());
    }
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return Err("map-keys requires a map as first argument".to_string());
    };

    let mut keys: ValueList = vec![];
    for (k, _) in map {
        keys.push(Value::Str(k))
    }
    Ok(list!(keys))
}

fn pred_atom(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::List(l) => if l.len() > 0 { Ok(Value::Nil) } else { Ok(Value::Sym("t".to_string())) };
        Ok(Value::Sym("t".to_string()))
    }
}

fn pred_list(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::List(l) => if l.len() > 0 { Ok(Value::Sym("t".to_string())) } else { Ok(Value::Nil) };
        Ok(Value::Nil)
    }
}

fn pred_hashmap(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Map(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_nil(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::List(l) => if l.len() > 0 { Ok(Value::Nil) } else { Ok(Value::Sym("t".to_string())) },
        Value::Nil => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_number(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Num(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_string(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Str(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_symbol(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Sym(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_keyword(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Keyword(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn pred_function(v: ValueList) -> ValueErr {
    predicate_op! {v;
        Value::Func{ .. } => Ok(Value::Sym("t".to_string())),
        Value::NatFunc(_) => Ok(Value::Sym("t".to_string()));
        Ok(Value::Nil)
    }
}

fn core_apply(v: ValueList) -> ValueErr {
    let len = v.len();
    if len < 2 {
        return Err("apply requires two or more arguments".to_string())
    }
    let mut args = v[1..len-1].to_vec();
    
    if let Value::List(rest) = v[len-1].clone() {
        args.extend_from_slice(&rest);
        v[0].apply(args)
    }else{
        Err("last argument must be a list".to_string())
    }
}

fn core_map(v: ValueList) -> ValueErr {
    let len = v.len();
    if len < 2 {
        return Err("map requires two arguments".to_string())
    }
    let func = &v[0];
    if let Value::List(seq) = &v[1] {
        let mut result: Vec<Value> = vec![];
        for expr in seq.iter(){
            result.push(func.apply(vec![expr.clone()])?)
        }
        return Ok(list!(result))
    }else{
        return Err("second argument must be list".to_string())
    }
}

fn core_append(v: ValueList) -> ValueErr {
    let mut result: Vec<Value> = vec![];
    for seq in v {
        if let Value::List(l) = seq {
            result.extend_from_slice(&l);
        } else if let Value::Nil = seq {
        } else {
            return Err(format!("arguments must be lists, found {:?}", seq))
        }
    }
    Ok(list!(result))
}

fn core_time_ms(_v: ValueList) -> ValueErr {
    Ok(Value::Num(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64))
}

fn core_println(v: ValueList) -> ValueErr {
    for expr in v {
        println!("{}", expr);
    }
    Ok(Value::Nil)
}

fn core_print(v: ValueList) -> ValueErr {
    for expr in v {
        print!("{}", expr);
    }
    Ok(Value::Nil)
}

fn core_repr(v: ValueList) -> ValueErr {
    Ok(Value::Str(format!("{:?}", v.get(0).unwrap_or(&Value::Nil))))
}

fn operator_len(v: ValueList) -> ValueErr {
    n_args! { v;
        0 => Ok(Value::Nil),
        1 => {
            match &v[0] {
                Value::List(l) => Ok(Value::Num(l.len() as f64)),
                Value::Str(s) => Ok(Value::Num(s.len() as f64)),
                Value::Nil => Ok(Value::Num(0f64)),
                _ => Err("This value is not countable".to_string()),
            }
        },
        _ => Err("len requires one argument".to_string())
    }
}

fn core_read(v: ValueList) -> ValueErr {
    if v.len() == 0 {
        return Err("read requires one argument".to_string())
    }

    if let Value::Str(input) = v[0].clone(){
        let mut tk = parser::Reader::new(input);
        if let Ok(tok) = tk.next_token() {
            tk.parse_expr(tok)
        }else{
            Err("Invalid Syntax".to_string())
        }
    }else{
        Err("Argument is not a string".to_string())
    }
}

fn core_read_file(v: ValueList) -> ValueErr {
    let file = File::open(match &v[0] {
        Value::Str(s) => s,
        _ => return Err("Filename must be an string".to_string())
    });
    let mut contents = String::new();
    if let Ok(mut file) = file {
        match file.read_to_string(&mut contents) {
            Ok(_) => Ok(Value::Str(contents)),
            Err(err) => Err(format!("Couldn't read file: {:?}", err))
        }
        
    } else {
        Err("Couldn't open file".to_string())
    }
}

fn operator_inc(v: ValueList) -> ValueErr {
    n_args! { v;
        1 => {
            match &v[0] {
                Value::Num(n) => Ok(Value::Num(*n + 1f64)),
                _ => Err("Value is not a number".to_string()),
            }
        },
        _ => Err("inc requires one argument".to_string())
    }
}

fn operator_dec(v: ValueList) -> ValueErr {
    n_args! { v;
        1 => {
            match &v[0] {
                Value::Num(n) => Ok(Value::Num(*n - 1f64)),
                _ => Err("Value is not a number".to_string()),
            }
        },
        _ => Err("inc requires one argument".to_string())
    }
}

fn core_collect(v: ValueList) -> ValueErr {
    n_args! { v;
        1 => {
            match &v[0] {
                Value::List(_) => Ok(v[0].clone()),
                Value::Nil => Ok(Value::Nil),
                Value::Lazy{env, eval, tail, head} => {
                    let mut collect: ValueList = vec![(**head).clone()];
                    
                    let mut nth = (**tail).clone();
                    let mut env = env.clone();
                    loop {
                        match eval(nth, env.clone())? {
                            Value::Lazy{env: tenv, tail: ttail, head, ..} => {
                                collect.push((*head).clone());
                                nth = (*ttail).clone();
                                env = tenv;
                            }
                            _ => break Ok(list!(collect))
                        }
                    }
                }
                _ => Err("Value is not a list".to_string()),
            }
        },
        _ => Err("collect requires one argument".to_string())
    }
}

fn core_format(v: ValueList) -> ValueErr {
    if v.len() == 0 {
        return Err("format requires a format string argument".to_string());
    };
    if let Value::Str(format) = &v[0] {
        let mut iter = format.chars().peekable();
        let mut result = String::new();
        let mut current = 1;
        loop {
            match iter.next() {
                Some(ch) => match ch {
                    '{' => match iter.next() {
                        None => break Err("Invalid syntax in format string".to_string()),
                        Some(mut ch) => {
                            let mut debug = false;
                            if ch == '?' {
                                debug = true;
                                ch = match iter.next() {
                                    Some(ch) => ch,
                                    None => break Err("Invalid syntax in format string".to_string()),
                                }
                            }
                            match ch {
                                '}' => {
                                    match v.get(current) {
                                        Some(e) => result.push_str(&format!("{}", e)),
                                        None => break Err("Value expected to format string not found".to_string()),
                                    };
                                    current += 1;
                                }
                                '@' => {
                                    let mut sep: Option<char> = None;
                                    match iter.peek() {
                                        Some(ch) if *ch == '}' => {},
                                        Some(ch) => {sep = Some(*ch);iter.next();},
                                        _ => break Err("Invalid syntax in format string".to_string()),
                                    }
                                    match v.get(current) {
                                        Some(e) => {
                                            if let Value::List(l) = e {
                                                let mut it = l.iter();
                                                if let Some(expr) = it.next() {
                                                    if debug {
                                                        result.push_str(&format!("{:?}", expr))
                                                    }else{
                                                        result.push_str(&format!("{}", expr))
                                                    } 
                                                };
                                                for expr in it {
                                                    if let Some(sep) = sep {
                                                        result.push_str(& if debug {format!("{}{:?}", sep, expr)} else {format!("{}{}", sep, expr)})
                                                    }else{
                                                        result.push_str(& if debug {format!("{:?}", expr)} else {format!("{}", expr)})
                                                    }
                                                }
                                            } else {
                                                break Err("Value expected to slice in format string must be a list".to_string())
                                            }
                                        },
                                        None => break Err("Value expected to format string not found".to_string()),
                                    };
                                    current += 1;
                                    match iter.next() {
                                        Some(ch) if ch == '}' => (),
                                        _ => break Err("Invalid syntax in format string".to_string()),
                                    }
                                }
                                '{' => {
                                    result.push('{')
                                }
                                _ => break Err("Invalid syntax in format string".to_string()),
                            }
                        }
                    }
                    '}' => match iter.next() {
                        Some(ch) if ch == '}' => result.push('}'),
                        _ => break Err("Invalid syntax in format string".to_string()),
                    }
                    _ => result.push(ch)
                }
                None => break Ok(Value::Str(result))
            }
        }
    } else {
        return Err("format requires a format string argument".to_string());
    }
}

fn core_join(v: ValueList) -> ValueErr {
    if v.len() < 2 {
        return Err("join requires 2 arguments".to_string());
    }

    let sep = if let Value::Str(sep) = &v[0] {
        sep
    } else {
        return Err("join requires a string separator".to_string());
    };

    let mut result = String::new();
    if let Value::List(list) = &v[1] {
        let mut it = list.iter();
        if let Some(x) = it.next() {
            result.push_str(&format!("{}", x))
        }
        for expr in it {
            result.push_str(&format!("{}{}", sep, expr))
        }
        Ok(Value::Str(result))
    } else if let Value::Nil = &v[1] {
        Ok(Value::Str("".to_string()))
    } else {
        Err("join second argument must be  a list".to_string())
    }
}

pub fn ns() -> Vec<(&'static str, Value)>{
    vec![
        ("+", types::func(|v: Vec<Value>| add_mul_op!(+, 0f64, v))),
        ("*", types::func(|v: Vec<Value>| add_mul_op!(*, 1f64, v))),
        ("-", types::func(|v: Vec<Value>| sub_div_op!(-, Ok(Value::Num(0.)), |a: f64| -a, v))),
        ("/", types::func(|v: Vec<Value>| sub_div_op!(/, Err("Invalid number argument".to_string()), |a: f64| 1./a, v))),
        ("<", types::func(|v: Vec<Value>| ord_op!(<, v))),
        (">", types::func(|v: Vec<Value>| ord_op!(>, v))),
        ("<=", types::func(|v: Vec<Value>| ord_op!(<=, v))),
        (">=", types::func(|v: Vec<Value>| ord_op!(>=, v))),
        ("==", types::func(operator_eq)),
        ("!=", types::func(operator_ne)),
        ("str", types::func(operator_str)),
        ("list", types::func(|v: Vec<Value>| Ok(list!(v)))),
        ("first", types::func(operator_head)),
        ("second", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(1f64)]))),
        ("third", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(2f64)]))),
        ("fouth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(3f64)]))),
        ("fifth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(4f64)]))),
        ("sixth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(5f64)]))),
        ("seventh", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(6f64)]))),
        ("eigth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(6f64)]))),
        ("nineth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(6f64)]))),
        ("tenth", types::func(|v: Vec<Value>| operator_nth(vec![v.get(0).unwrap_or(&Value::Nil).clone(), Value::Num(6f64)]))),
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
        ("hash-map?", types::func(pred_hashmap)),
        ("apply", types::func(core_apply)),
        ("map", types::func(core_map)),
        ("append", types::func(core_append)),
        ("time-ms", types::func(core_time_ms)),
        ("println", types::func(core_println)),
        ("print", types::func(core_print)),
        ("repr", types::func(core_repr)),
        ("len", types::func(operator_len)),
        ("read", types::func(core_read)),
        ("read-file", types::func(core_read_file)),
        ("inc", types::func(operator_inc)),
        ("dec", types::func(operator_dec)),
        ("collect", types::func(core_collect)),
        ("format", types::func(core_format)),
        ("join", types::func(core_join)),
        ("hash-map", types::func(core_hashmap)),
        ("assoc", types::func(operator_assoc)),
        ("dissoc", types::func(operator_dissoc)),
        ("get-key", types::func(operator_map_get)),
        ("has-key?", types::func(operator_has_key)),
        ("map-keys", types::func(core_map_keys)),
        ("box", types::func(|v: Vec<Value>| Ok(Value::Box(Rc::new(RefCell::new(v.get(0).unwrap_or(&Value::Nil).clone())))))),
        ("set-box", types::func(|v: Vec<Value>| n_args! { v;
            2 => {
                match &v[0] {
                    Value::Box(data) => {*data.borrow_mut() = v[1].clone(); Ok(v[1].clone())},
                    _ => Err("Value is not a box".to_string()),
                }
            },
            _ => Err("set-box requires 2 arguments".to_string())
        })),
        ("swap-box", types::func(|v: Vec<Value>| 
            if v.len() >= 2 {
                match &v[0] {
                    Value::Box(data) => {
                        let mut args = vec![data.borrow().clone()];
                        args.extend_from_slice(&v[2..]);
                        let new_value = v[1].apply(args)?;
                        *data.borrow_mut() = new_value;
                        Ok(v[1].clone())
                    },
                    _ => Err("Value is not a box".to_string()),
                }
            } else {
                Err("swap requires at least 2 arguments".to_string())
            }
        )),
        ("deref", types::func(|v: Vec<Value>| match v.get(0).unwrap_or(&Value::Nil) {
            Value::Box(data) => Ok(data.borrow().clone()),
            _ => Err("Can't deref non box".to_string())
        })),        
    ]
}