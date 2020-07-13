use crate::types;
use crate::error;
use crate::parser;
use types::{Value, ValueList, Arity};
use std::rc::Rc;
use std::cell::RefCell;
use std::time::{SystemTime};
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;

type ValueResult = Result<Value, error::Error>;

macro_rules! type_err {
    ($t:expr, $v:expr) => (Err(error::Error::TypeErr($t, Some($v.clone()))));
}

macro_rules! ord_op {
    ($op:tt, $v:expr) => {{
        let mut left = match &$v[0] {
            Value::Num(n) => *n,
            x => return type_err!("number", x)
        };
        for e in $v[1..].iter() {
            if let Value::Num(n) = e {
                if left $op *n {
                    left = *n;
                    continue
                }else{
                    return Ok(Value::False)
                }
            }else{
                return type_err!("number", e)
            }
        }
        return Ok(Value::True)
    }};
}

macro_rules! add_mul_op {
    ($op:tt, $init:expr, $args:expr) => {
        Ok(Value::Num(
            $args.iter().fold(Ok($init), |acc, val| if let Value::Num(n) = val {
                Ok(acc? $op *n)
            }else{
                return type_err!("number", val)
            })?
        ))
    };
}

macro_rules! sub_div_op {
    ($op:tt, $none:expr, $one:expr, $args:expr) => {{
        if $args.len() == 0 {
            return $none
        }
        match &$args[0] {
            Value::Num(first) => if $args.len() > 1 {
                Ok(Value::Num($args[1..].iter().fold(Ok(first.clone()), |acc, val| if let Value::Num(n) = val {
                    Ok(acc? $op *n)
                }else{
                    return type_err!("number", val)
                })?))
            }else{
                Ok(Value::Num($one(*first)))
            }
            x => type_err!("number", x)
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
        match &$args[0] {
            $($len => $action),*,
            _ => $fail
        }
    };
}

fn operator_eq(v: ValueList) -> ValueResult {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left != e {
            return Ok(Value::False)
        }
    }
    return Ok(Value::True)
}

fn operator_ne(v: ValueList) -> ValueResult {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left == e {
            return Ok(Value::False)
        }
    }
    return Ok(Value::True)
}

fn operator_str(v: ValueList) -> ValueResult {
    let mut res = String::new();
    for e in v.iter() {
        res.push_str(&format!("{}", e))
    }
    return Ok(Value::Str(res));
}

pub fn operator_head(v: ValueList) -> ValueResult {
    v[0].first().map_err(From::from)
}

fn operator_nth(v: ValueList) -> ValueResult {
    // n_args! { v;
    //     2 => {
            let n = match &v[1] {
                Value::Num(n) => *n as usize,
                x => return type_err!("number", x),
            };

            match &v[0] {
                Value::List(l) => {
                    if l.len() == 0 {
                        Ok(Value::Nil)
                    }else{
                        Ok(l.get(n).cloned().into())
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
                x => type_err!("collection", x),
            }
        // },
    //     _ => Err("nth requires two arguments".to_string())
    // }
}

pub fn operator_tail(v: ValueList) -> ValueResult {
    v[0].rest().map_err(From::from)
}

fn operator_cons(v: ValueList) -> ValueResult {
    match &v[1] {
        Value::List(l) => {
            if l.len() == 0 {
                Ok(vec![v[0].clone()].into())
            }else{
                let mut new = vec![v[0].clone()];
                new.reserve(l.len());
                new.extend_from_slice(&l);
                Ok(new.into())
            }
        },
        Value::Nil => Ok(vec![v[0].clone()].into()),
        x => Err(format!("Can't cons to a non-list {}", x).into()),
    }
}

fn operator_rev_cons(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::List(l) => {
            if l.len() == 0 {
                Ok(vec![v[1].clone()].into())
            }else{
                let mut new = vec![];
                new.reserve(l.len() + 1);
                new.extend_from_slice(&l);
                new.push(v[1].clone());
                Ok(new.into())
            }
        },
        Value::Nil => Ok(vec![v[1].clone()].into()),
        x => Err(format!("Can't rev-cons to a non-list {}", x).into()),
    }
}


fn core_hashmap(v: ValueList) -> ValueResult {
    if v.len() % 2 != 0 {
        return Err(error::Error::KwArgErr(Some("hash-map".to_string())));
    }

    let mut map: HashMap<String, Value> = HashMap::default();

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Str(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Sym(s) => map.insert(s.clone(), v[i+1].clone()),
            x => return Err(format!("Value {} can't be used as key", x).into()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_assoc(v: ValueList) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let v = &v[1..];

    if v.len() % 2 != 0 {
        return Err(error::Error::KwArgErr(Some("assoc".to_string())));
    }

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Str(s) => map.insert(s.clone(), v[i+1].clone()),
            Value::Sym(s) => map.insert(s.clone(), v[i+1].clone()),
            x => return Err(format!("Value {} can't be used as key", x).into()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_map_update(v: ValueList) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let (old, key) = match &v[1] {
        Value::Str(s) | Value::Sym(s) => match map.get(s){
            Some(v) => (v.clone(), s),
            None => (Value::Nil, s)
        },
        x => return Err(format!("Value {} can't be used as key", x).into()),
    };
    let mut args = vec![old];
    args.extend_from_slice(&v[3..]);
    let new = v[2].apply(args)?;
    map.insert(key.clone(), new);
    Ok(Value::Map(Rc::new(map)))
}

fn operator_dissoc(v: ValueList) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let v = &v[1..];

    for key in v {
        match key {
            Value::Keyword(s) => map.remove(s),
            Value::Str(s) => map.remove(s),
            Value::Sym(s) => map.remove(s),
            x => return Err(format!("Value {} can't be used as key", x).into()),
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_map_get(v: ValueList) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    match &v[1] {
        Value::Keyword(s) | Value::Str(s) | Value::Sym(s) => match map.get(s){
            Some(v) => Ok(v.clone()),
            None => Err(format!("Key {} is not present in map", s).into())
        },
        x => return Err(format!("Value {} can't be used as key", x).into()),
    }
}

fn operator_has_key(v: ValueList) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    if match &v[1] {
        Value::Keyword(s) | Value::Str(s) | Value::Sym(s) => map.contains_key(s),
        x => return Err(format!("Value {} can't be used as key", x).into()),
    } {
        return Ok(Value::True);
    };
    Ok(Value::False)
}

fn core_map_keys(v: ValueList) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let mut keys: ValueList = vec![];
    for (k, _) in map {
        keys.push(Value::Str(k))
    }
    Ok(keys.into())
}

fn pred_atom(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() == 0).into());
        Ok(Value::True)
    }
}

fn pred_list(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() > 0).into());
        Ok(Value::False)
    }
}

fn pred_hashmap(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Map(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_nil(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() == 0).into()),
        Value::Nil => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_number(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Num(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_string(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Str(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_symbol(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Sym(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_keyword(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Keyword(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_function(v: ValueList) -> ValueResult {
    predicate_op! {v;
        Value::Func{ .. } => Ok(Value::True),
        Value::NatFunc(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn core_apply(v: ValueList) -> ValueResult {
    let len = v.len();
    
    let mut args = v[1..len-1].to_vec();
    
    match &v[len-1] {
        Value::List(rest) => {
            args.extend_from_slice(&rest);
            v[0].apply(args).map_err(From::from)
        }
        Value::Nil => v[0].apply(args).map_err(From::from),
        x => type_err!("list", x)
    }

    // if let Value::List(rest) = v[len-1].clone() {
    //     args.extend_from_slice(&rest);
    //     v[0].apply(args).map_err(From::from)
    // }else if let Value::Nil = v[len-1].clone() {
    //     v[0].apply(args).map_err(From::from)
    // }else {
    //     type_err!("list", v[len-1]);
    // }
}

fn core_map(v: ValueList) -> ValueResult {
    let func = &v[0];
    if let Value::List(seq) = &v[1] {
        let mut result: Vec<Value> = vec![];
        for expr in seq.iter(){
            result.push(func.apply(vec![expr.clone()])?)
        }
        return Ok(result.into())
    }else{
        return type_err!("list", v[1])
    }
}

fn core_append(v: ValueList) -> ValueResult {
    let mut result: Vec<Value> = vec![];
    for seq in v {
        if let Value::List(l) = seq {
            result.extend_from_slice(&l);
        } else if let Value::Nil = seq {
        } else {
            return type_err!("list", seq)
        }
    }
    Ok(result.into())
}

fn core_time_ms(_v: ValueList) -> ValueResult {
    Ok(Value::Num(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64))
}

fn core_println(v: ValueList) -> ValueResult {
    for expr in v {
        println!("{}", expr);
    }
    Ok(Value::Nil)
}

fn core_input(_v: ValueList) -> ValueResult {
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::Str(input)),
        Err(err) => Err(format!("IoError: {}", err).into())
    }
}

fn core_print(v: ValueList) -> ValueResult {
    for expr in v {
        print!("{}", expr);
    }
    Ok(Value::Nil)
}

fn core_repr(v: ValueList) -> ValueResult {
    Ok(Value::Str(format!("{:?}", v.get(0).unwrap_or(&Value::Nil))))
}

fn operator_len(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::List(l) => Ok(Value::Num(l.len() as f64)),
        Value::Chars(chs) => Ok(Value::Num(chs.len() as f64)),
        Value::Str(s) => Ok(Value::Num(s.len() as f64)),
        Value::Nil => Ok(Value::Num(0f64)),
        x => type_err!("list, chars or string", x),
    }
}

fn core_read(v: ValueList) -> ValueResult {
    if let Value::Str(input) = v[0].clone(){
        let mut tk = parser::Reader::new(&input);
        if let Ok(tok) = tk.next_token() {
            match tk.parse_expr(tok) {
                parser::ParserResult::Expr(expr) => Ok(expr),
                parser::ParserResult::TokenErr(err) => Err(err.into()),
                parser::ParserResult::EofErr => Err(format!("Error: Unexpected EOF").into()),
            }
        }else{
            Err("Invalid Syntax".into())
        }
    }else{
        type_err!("string", v[0])
    }
}

fn core_read_file(v: ValueList) -> ValueResult {
    let file = File::open(match &v[0] {
        Value::Str(s) => s,
        _ => return type_err!("string", v[0])
    });
    let mut contents = String::new();
    if let Ok(mut file) = file {
        match file.read_to_string(&mut contents) {
            Ok(_) => Ok(Value::Str(contents)),
            Err(err) => Err(format!("Couldn't read file: {:?}", err).into())
        }
        
    } else {
        Err("Couldn't open file".into())
    }
}

fn operator_inc(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::Num(n) => Ok(Value::Num(*n + 1f64)),
        x => type_err!("number", x),
    }
}

fn operator_dec(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::Num(n) => Ok(Value::Num(*n - 1f64)),
        x => type_err!("number", x),
    }
}

fn core_collect(v: ValueList) -> ValueResult {
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
                    _ => break Ok(collect.into())
                }
            }
        }
        x => type_err!("list", x),
    }
}

fn core_format(v: ValueList) -> ValueResult {
    // if v.len() == 0 {
    //     return Err("format requires a format string argument".to_string());
    // };
    if let Value::Str(format) = &v[0] {
        let mut iter = format.chars().peekable();
        let mut result = String::new();
        let mut current = 1;
        loop {
            match iter.next() {
                Some(ch) => match ch {
                    '{' => match iter.next() {
                        None => break Err("Invalid syntax in format string".into()),
                        Some(mut ch) => {
                            let mut debug = false;
                            if ch == '?' {
                                debug = true;
                                ch = match iter.next() {
                                    Some(ch) => ch,
                                    None => break Err("Invalid syntax in format string".into()),
                                }
                            }
                            match ch {
                                '}' => {
                                    match v.get(current) {
                                        Some(e) => if debug {
                                            result.push_str(&format!("{:?}", e))
                                        }else{
                                            result.push_str(&format!("{}", e))
                                        },
                                        None => break Err("Value expected to format string not found".into()),
                                    };
                                    current += 1;
                                }
                                '@' => {
                                    let mut sep: Option<String> = None;
                                    match iter.peek() {
                                        Some(ch) if *ch == '}' => {},
                                        Some(_) => {
                                            let mut sep_ = String::new();
                                            loop {
                                                match iter.next() {
                                                    Some(ch) if ch == '}' => {
                                                        sep = Some(sep_.clone());
                                                        break
                                                    }
                                                    Some(ch) => sep_.push(ch),
                                                    None => return Err("Invalid syntax in format string expected closing }".into()),
                                                }
                                            }
                                        },
                                        _ => break Err("Invalid syntax in format string".into()),
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
                                                    if let Some(sep) = sep.clone() {
                                                        result.push_str(& if debug {format!("{}{:?}", sep, expr)} else {format!("{}{}", sep, expr)})
                                                    }else{
                                                        result.push_str(& if debug {format!("{:?}", expr)} else {format!("{}", expr)})
                                                    }
                                                }
                                            } else {
                                                break Err("Value expected to slice in format string must be a list".into())
                                            }
                                        },
                                        None => break Err("Value expected to format string not found".into()),
                                    };
                                    current += 1;
                                    // match iter.next() {
                                    //     Some(ch) if ch == '}' => (),
                                    //     _ => break Err("Invalid syntax in format string".to_string()),
                                    // }
                                }
                                '{' => {
                                    result.push('{')
                                }
                                _ => break Err("Invalid syntax in format string".into()),
                            }
                        }
                    }
                    '}' => match iter.next() {
                        Some(ch) if ch == '}' => result.push('}'),
                        _ => break Err("Invalid syntax in format string".into()),
                    }
                    _ => result.push(ch)
                }
                None => break Ok(Value::Str(result))
            }
        }
    } else {
        return Err("format requires a format string argument".into());
    }
}

fn core_join(v: ValueList) -> ValueResult {
    let sep = if let Value::Str(sep) = &v[0] {
        sep
    } else {
        return type_err!("string", v[0]);
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
        type_err!("list", v[1])
    }
}

fn core_symbol(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::Str(s) => Ok(Value::Sym(s.clone())),
        x => type_err!("string", x),
    }
}

fn core_assert(v: ValueList) -> ValueResult {
    n_args! { v;
        1 => {
            if v[0].is_nil() {
                return Err("AssertError".into())
            }else{
                return Ok(v[0].clone())
            }
        },
        2 => {
            if v[0] != v[1] {
                return Err("AssertError".into())
            }else{
                return Ok(v[0].clone())
            }
        },
        3 => {
            if v[0] != v[1] {
                return Err(format!("{}", v[2]).into())
            }else{
                return Ok(v[0].clone())
            }
        },
        x => Err(error::Error::ArgErr(Some("assert".into()), Arity::Range(1,3), x as u16))
    }
}

fn core_make_struct(v: ValueList) -> ValueResult {
    let mut data: ValueList = vec![];

    let struct_id = match &v[0] {
        Value::Sym(s) => s,
        _ => return Err("Struct id must be a symbol".into())
    };

    for i in &v[1..] {
        data.push(i.clone());
    };
    Ok(Value::Struct(Rc::new(struct_id.clone()),Rc::new(data)))
}

fn core_member_struct(v: ValueList) -> ValueResult {
    let (struct_id, struct_data) = match &v[0] {
        Value::Struct(id, data) => (id, data),
        x => return type_err!("struct", x)
    };

    let check_id = match &v[1] {
        Value::Sym(s) => s,
        x => return type_err!("symbol", x)
    };

    if (**struct_id).ne(check_id) {
        return Err(format!("Expected {} struct but found {}", check_id, struct_id).into())
    }

    let index = match &v[2] {
        Value::Num(n) => n,
        x => return type_err!("number", x)
    };

    let value = match struct_data.get(*index as usize) {
        Some(val) => val.clone(),
        None => return Err(format!("Invalid access to struct {}, index {} not found", struct_id, index).into())
    };

    Ok(value)
}

fn core_assert_struct(v: ValueList) -> ValueResult {
    let struct_id = match &v[0] {
        Value::Struct(id, _) => id,
        _ => return Err(format!("Expected Struct got {:?}", &v[0]).into())
    };

    let check_id = match &v[1] {
        Value::Sym(s) => s,
        x => return type_err!("symbol", x)
    };

    Ok((**struct_id).eq(check_id).into())
}

pub fn core_string_to_chars(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::Str(s) => {
            let chars: Vec<char> = s.chars().collect();
            Ok(Value::Chars(chars.into_boxed_slice()))
        },
        Value::Chars(_) => Ok(v[0].clone()),
        x => type_err!("string", x),
    }
}

pub fn core_string_append_char(v: ValueList) -> ValueResult {
    match (&v[0], &v[1]) {
        (Value::Str(s), Value::Char(chr)) => {
            Ok(Value::Str(format!("{}{}", s, *chr)))
        },
        x => type_err!("string", x.0),
    }
}

pub fn core_char_to_string(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::Char(c) => {
            Ok(Value::Str(c.to_string()))
        },
        x => type_err!("char", x),
    }
}

pub fn core_char_list_to_string(v: ValueList) -> ValueResult {
    match &v[0] {
        Value::List(chrs) => {
            let mut res = String::new();
            for chr in chrs.iter() {
                match chr {
                    // Value::Num(n) => match std::char::from_u32(*n as u32) {
                    //     Some(ch) => res.push(ch),
                    //     None => return Err(format!("Invalid char {}", n).into())
                    // }
                    Value::Char(ch) => res.push(*ch),
                    x => return type_err!("char", x),
                }
            };
            Ok(Value::Str(res))
        },
        x => type_err!("list", x),
    }
}

pub fn core_string_starts_with(v: ValueList) -> ValueResult {
    match (&v[0], &v[1]) {
        (Value::Str(s), Value::Str(check)) => Ok(s.starts_with(check).into()),
        (Value::Chars(chrs), Value::Str(check)) => Ok(chrs.starts_with(check.chars().collect::<Vec<char>>().as_slice()).into()),
        (Value::Nil, Value::Str(_)) => Ok(Value::False),
        _ => Err(format!("Invalid arguments, found ({:?}, {:?})", v[0], v[1]).into()),
    }
}

pub fn core_chars_slice(v: ValueList) -> ValueResult {
    n_args! { v;
        2 => match (&v[0], &v[1]) {
            (Value::Chars(chars), Value::Num(start)) => {
                Ok(Value::Chars( Box::from(&chars[*start as usize ..]) ))
            },
            _ => Err("arguments are invalid".into())
        },
        3 => match (&v[0], &v[1], &v[2]) {
            (Value::Chars(chars), Value::Num(start), Value::Num(end)) => {
                let slice = &chars[*start as usize .. *end as usize];
                if slice.len() == 0 {
                    Ok(Value::Nil)
                } else {
                    Ok(Value::Chars(Box::from(slice)))
                }
            },
            _ => Err("arguments are invalid".into())
        },
        _ => Err("arguments are invalid".into())
    }
}

pub fn ns() -> Vec<(&'static str, Value)>{
    vec![
        ("+", types::func("+", Arity::Min(0), |v: Vec<Value>| add_mul_op!(+, 0f64, v))),
        ("*", types::func("*", Arity::Min(0), |v: Vec<Value>| add_mul_op!(*, 1f64, v))),
        ("-", types::func("-", Arity::Min(0), |v: Vec<Value>| sub_div_op!(-, Ok(Value::Num(0.)), |a: f64| -a, v))),
        ("/", types::func("/", Arity::Min(0), |v: Vec<Value>| sub_div_op!(/, Err("Invalid number argument".into()), |a: f64| 1./a, v))),
        ("<", types::func("<", Arity::Min(0), |v: Vec<Value>| ord_op!(<, v))),
        (">", types::func(">", Arity::Min(0), |v: Vec<Value>| ord_op!(>, v))),
        ("<=", types::func("<=", Arity::Min(0), |v: Vec<Value>| ord_op!(<=, v))),
        (">=", types::func(">=", Arity::Min(0), |v: Vec<Value>| ord_op!(>=, v))),
        ("==", types::func("==", Arity::Min(1), operator_eq)),
        ("!=", types::func("!=", Arity::Min(1), operator_ne)),
        ("str", types::func("str", Arity::Min(0), operator_str)),
        // ("list", types::func("list", Arity::Min(0), |v: Vec<Value>| if v.len() == 0 {Ok(Value::Nil)} else {Ok(list!(v))})),
        ("list", types::func("list", Arity::Min(0), |v: Vec<Value>| Ok(v.into()))),
        ("first", types::func("first", Arity::Exact(1), operator_head)),
        ("second", types::func("second", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(1f64)]))),
        ("third", types::func("third", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(2f64)]))),
        ("fourth", types::func("fourth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(3f64)]))),
        ("fifth", types::func("fifth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(4f64)]))),
        ("sixth", types::func("sixth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(5f64)]))),
        ("seventh", types::func("seventh", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(6f64)]))),
        ("eigth", types::func("eigth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(6f64)]))),
        ("nineth", types::func("nineth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(6f64)]))),
        ("tenth", types::func("tenth", Arity::Exact(1), |v: Vec<Value>| operator_nth(vec![v[0].clone(), Value::Num(6f64)]))),
        ("nth", types::func("th", Arity::Exact(2), operator_nth)),
        // ("head", types::func("head", Arity::Exact(1), operator_head)),
        // ("tail", types::func("tail", Arity::Exact(1), operator_tail)),
        ("rest", types::func("rest", Arity::Exact(1), operator_tail)),
        ("cons", types::func("cons", Arity::Exact(2), operator_cons)),
        ("rev-cons", types::func("rev-cons", Arity::Exact(2), operator_rev_cons)),
        ("atom?", types::func("atom?", Arity::Exact(1), pred_atom)),
        ("list?", types::func("list?", Arity::Exact(1), pred_list)),
        ("nil?", types::func("nil?", Arity::Exact(1), pred_nil)),
        ("number?", types::func("number?", Arity::Exact(1), pred_number)),
        ("string?", types::func("string?", Arity::Exact(1), pred_string)),
        ("symbol?", types::func("symbol?", Arity::Exact(1), pred_symbol)),
        ("function?", types::func("function?", Arity::Exact(1), pred_function)),
        ("keyword?", types::func("keyword?", Arity::Exact(1), pred_keyword)),
        ("hash-map?", types::func("hash-map?", Arity::Exact(1), pred_hashmap)),
        ("apply", types::func("apply", Arity::Min(2), core_apply)),
        ("map", types::func("map", Arity::Exact(2), core_map)),
        ("append", types::func("append", Arity::Min(0), core_append)),
        ("time-ms", types::func("time-ms", Arity::Exact(0), core_time_ms)),
        ("println", types::func("println", Arity::Min(0), core_println)),
        ("print", types::func("print", Arity::Min(0), core_print)),
        ("input", types::func("input", Arity::Exact(0), core_input)),
        ("repr", types::func("repr", Arity::Min(0), core_repr)),
        ("len", types::func("len", Arity::Exact(1), operator_len)),
        ("read", types::func("read", Arity::Exact(1), core_read)),
        ("read-file", types::func("read-file", Arity::Exact(1), core_read_file)),
        ("inc", types::func("inc", Arity::Exact(1), operator_inc)),
        ("dec", types::func("dec", Arity::Exact(1), operator_dec)),
        ("collect", types::func("collect", Arity::Exact(1), core_collect)),
        ("format", types::func("format", Arity::Min(1), core_format)),
        ("join", types::func("join", Arity::Min(2), core_join)),
        ("hash-map", types::func("hash-map", Arity::Min(0), core_hashmap)),
        ("assoc", types::func("assoc", Arity::Min(1), operator_assoc)),
        ("dissoc", types::func("dissoc", Arity::Min(1), operator_dissoc)),
        ("get-key", types::func("get-key", Arity::Exact(2), operator_map_get)),
        ("update", types::func("update", Arity::Min(3), operator_map_update)),
        ("has-key?", types::func("has-key?", Arity::Exact(2), operator_has_key)),
        ("map-keys", types::func("map-keys", Arity::Exact(1), core_map_keys)),
        ("symbol", types::func("symbol", Arity::Exact(1), core_symbol)),
        ("make-struct", types::func("make-struct", Arity::Min(1), core_make_struct)),
        ("index-struct", types::func("index-struct", Arity::Exact(3), core_member_struct)),
        ("assert-struct", types::func("assert-struct", Arity::Exact(2), core_assert_struct)),
        ("assert", types::func("assert", Arity::Range(1,3),core_assert)),
        ("box", types::func("box", Arity::Exact(1),|v: Vec<Value>| Ok(Value::Box(Rc::new(RefCell::new(v[0].clone())))))),
        ("set-box", types::func("set-box", Arity::Exact(2), |v: Vec<Value>| 
            match &v[0] {
                Value::Box(data) => {*data.borrow_mut() = v[1].clone(); Ok(v[1].clone())},
                _ => Err("Value is not a box".into()),
        })),
        ("swap-box", types::func("swap-box", Arity::Min(2), |v: Vec<Value>| 
            match &v[0] {
                Value::Box(data) => {
                    let mut args = vec![data.borrow().clone()];
                    args.extend_from_slice(&v[2..]);
                    let new_value = v[1].apply(args)?;
                    *data.borrow_mut() = new_value;
                    Ok(v[1].clone())
                },
                _ => Err("Value is not a box".into()),
            }
        )),
        ("deref", types::func("deref", Arity::Exact(1), |v: Vec<Value>| match &v[0] {
            Value::Box(data) => Ok(data.borrow().clone()),
            _ => Err("Can't deref non box".into())
        })),
        ("reverse", types::func("reverse", Arity::Exact(1), |v: Vec<Value>| match &v[0] {
            Value::List(data) => Ok(data.iter().rev().map(|v| v.clone()).collect::<ValueList>().into()),
            _ => Err("Can't reverse a non list".into())
        })),
        // ("chars/head", types::func(core_string_head)),
        // ("chars/tail", types::func(core_string_tail)),
        ("string->chars", types::func("string->chars", Arity::Exact(1), core_string_to_chars)),
        ("string/starts-with", types::func("string/starts-with", Arity::Exact(2), core_string_starts_with)),
        ("string/append-char", types::func("string/append-char", Arity::Exact(2), core_string_append_char)),
        ("char->string", types::func("char->string", Arity::Exact(1), core_char_to_string)),
        ("char-list->string", types::func("char-list->string", Arity::Exact(1), core_char_list_to_string)),
        ("chars/slice", types::func("chars/slice", Arity::Range(2, 3), core_chars_slice)),
    ]
}
