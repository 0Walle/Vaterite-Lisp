use std::rc::Rc;
use std::cell::RefCell;
use std::time::{SystemTime};
use std::fs::File;
use std::io::Read;
use std::collections::HashMap;

use crate::error;
use crate::parser;
use crate::types::{Value, ValueList, Arity, LazyData};
use crate::names::{NamePool, Name};
use crate::printer::Printer;

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

fn operator_eq(v: ValueList, _names: &NamePool) -> ValueResult {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left != e {
            return Ok(Value::False)
        }
    }
    return Ok(Value::True)
}

fn operator_ne(v: ValueList, _names: &NamePool) -> ValueResult {
    let left = &v[0]; 
    for e in v[1..].iter() {
        if left == e {
            return Ok(Value::False)
        }
    }
    return Ok(Value::True)
}

fn operator_str(v: ValueList, names: &NamePool) -> ValueResult {
    let mut res = String::new();
    for e in v.iter() {
        res.push_str(&format!("{}", Printer::str_name(e, names)))
    }
    return Ok(Value::Str(res.into()));
}

pub fn operator_head(v: ValueList, _names: &NamePool) -> ValueResult {
    v[0].first().map_err(From::from)
}

fn operator_nth(v: ValueList, names: &NamePool) -> ValueResult {
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
        Value::Lazy{env, eval, data} => {
            if n == 0 {
                return Ok(data.head.clone());
            }

            let mut count = n;
            let mut nth = data.tail.clone();
            let mut env = env.clone();
            loop {
                count -= 1;
                match eval(nth, env.clone(), names)? {
                    Value::Lazy{env: tenv, data, ..} => {
                        if count == 0 {
                            break Ok((data.head).clone())
                        }else{
                            nth = (data.tail).clone();
                            env = tenv;
                        }
                    }
                    _ => break Ok(Value::Nil)
                }
            }
        },
        x => type_err!("collection", x),
    }
}

pub fn operator_tail(v: ValueList, names: &NamePool) -> ValueResult {
    v[0].rest(names).map_err(From::from)
}

fn operator_cons(v: ValueList, names: &NamePool) -> ValueResult {
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
        x => Err(format!("Can't cons to a non-list {}", Printer::str_name(x, names)).into()),
    }
}

fn operator_rev_cons(v: ValueList, names: &NamePool) -> ValueResult {
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
        x => Err(format!("Can't rev-cons to a non-list {}", Printer::str_name(x, names)).into()),
    }
}


fn core_hashmap(v: ValueList, _names: &NamePool) -> ValueResult {
    if v.len() % 2 != 0 {
        return Err(error::Error::KwArgErr(Some(crate::names::builtin::HASH_MAP)));
    }

    let mut map: HashMap<Name, Value> = HashMap::default();

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) | Value::Sym(s) => map.insert(*s, v[i+1].clone()),
            x => return type_err!("keyword", x.clone())
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_assoc(v: ValueList, _names: &NamePool) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let v = &v[1..];

    if v.len() % 2 != 0 {
        return Err(error::Error::KwArgErr(Some(crate::names::builtin::ASSOC)));
    }

    for i in (0..v.len()).step_by(2) {
        match &v[i] {
            Value::Keyword(s) | Value::Sym(s) => map.insert(*s, v[i+1].clone()),
            x => return type_err!("keyword", x.clone())
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_map_update(v: ValueList, names: &NamePool) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let (old, key) = match &v[1] {
        Value::Keyword(n) | Value::Sym(n) => match map.get(n){
            Some(v) => (v.clone(), *n),
            None => (Value::Nil, *n)
        },
        x => return type_err!("keyword", x.clone())
    };
    let mut args = vec![old];
    args.extend_from_slice(&v[3..]);
    let new = v[2].apply(args, names)?;
    map.insert(key.clone(), new);
    Ok(Value::Map(Rc::new(map)))
}

fn operator_dissoc(v: ValueList, _names: &NamePool) -> ValueResult {
    let mut map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let v = &v[1..];

    for key in v {
        match key {
            Value::Keyword(s) | Value::Sym(s) => map.remove(s),
            x => return type_err!("keyword", x.clone())
        };
    };
    Ok(Value::Map(Rc::new(map)))
}

fn operator_map_get(v: ValueList, _names: &NamePool) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    match &v[1] {
        Value::Keyword(s) | Value::Sym(s) => match map.get(s){
            Some(v) => Ok(v.clone()),
            None => Err(format!("Key {} is not present in map", s.0).into())
        },
        x => return type_err!("keyword", x.clone())
    }
}

fn operator_has_key(v: ValueList, _names: &NamePool) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    if match &v[1] {
        Value::Keyword(s) | Value::Sym(s) => map.contains_key(s),
        x => return type_err!("keyword", x.clone())
    } {
        return Ok(Value::True);
    };
    Ok(Value::False)
}

fn core_map_keys(v: ValueList, _names: &NamePool) -> ValueResult {
    let map = if let Value::Map(hashmap) = &v[0] {
        (**hashmap).clone()
    } else {
        return type_err!("map", v[0]);
    };

    let mut keys: ValueList = vec![];
    for (k, _) in map {
        keys.push(Value::Keyword(k))
    }
    Ok(keys.into())
}

fn pred_atom(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() == 0).into());
        Ok(Value::True)
    }
}

fn pred_list(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() > 0).into());
        Ok(Value::False)
    }
}

fn pred_hashmap(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Map(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_nil(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::List(l) => Ok((l.len() == 0).into()),
        Value::Nil => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_number(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Num(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_string(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Str(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_symbol(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Sym(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_keyword(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Keyword(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn pred_function(v: ValueList, _names: &NamePool) -> ValueResult {
    predicate_op! {v;
        Value::Func{ .. } => Ok(Value::True),
        Value::NatFunc(_) => Ok(Value::True);
        Ok(Value::False)
    }
}

fn core_apply(v: ValueList, names: &NamePool) -> ValueResult {
    let len = v.len();
    
    let mut args = v[1..len-1].to_vec();
    
    match &v[len-1] {
        Value::List(rest) => {
            args.extend_from_slice(&rest);
            v[0].apply(args, names).map_err(From::from)
        }
        Value::Nil => v[0].apply(args, names).map_err(From::from),
        x => type_err!("list", x)
    }
}

fn core_flatmap(v: ValueList, names: &NamePool) -> ValueResult {
    let func = &v[0];
    if let Value::List(seq) = &v[1] {
        let mut result: Vec<Value> = vec![];
        for expr in seq.iter(){
            match func.apply(vec![expr.clone()], names)? {
                Value::List(ls) => result.extend(ls.iter().cloned()),
                Value::Nil => {}
                x => return type_err!("list", x.clone())
            }
        }
        return Ok(result.into())
    }else{
        return type_err!("list", v[1])
    }
}

fn core_map(v: ValueList, names: &NamePool) -> ValueResult {
    let func = &v[0];
    match &v[1] {
        Value::List(seq) => {
            let mut result: Vec<Value> = Vec::with_capacity(seq.len());
            for expr in seq.iter(){
                result.push(func.apply(vec![expr.clone()], names)?)
            }
            return Ok(result.into())
        },
        Value::Lazy{
            data, env, eval
        } => Ok(Value::Lazy {
            eval: *eval, env: env.clone(), 
            data: Rc::new(LazyData {
                head: func.apply(vec![data.head.clone()], names)?,
                tail: vater!{ (MAP [func.clone()] [data.tail.clone()]) }
            })
        }),
        x => return type_err!("sequence", x.clone())
    }
}

fn core_filter(v: ValueList, names: &NamePool) -> ValueResult {
    let func = &v[0];
    let mut seq = v[1].clone();
    loop {
        match seq {
            Value::List(seq) => {
                let mut result: Vec<Value> = Vec::with_capacity(seq.len());
                for expr in seq.iter(){
                    if !func.apply(vec![expr.clone()], names)?.is_false() {
                        result.push(expr.clone())
                    }
                }
                return Ok(result.into())
            },
            Value::Lazy{
                data, env, eval
            } => {
                if func.apply(vec![data.head.clone()], names)?.is_false() {
                    seq = eval(data.tail.clone(), env.clone(), names)?
                } else {
                    return Ok(Value::Lazy {
                        eval, env: env.clone(),
                        data: Rc::new(LazyData {
                            head: data.head.clone(),
                            tail: vater!{ (FILTER [func.clone()] [data.tail.clone()]) }
                        })
                    })
                }
            },
            Value::Nil => return Ok(Value::Nil),
            x => return type_err!("sequence", x.clone())
        }
    }
}

fn core_append(v: ValueList, _names: &NamePool) -> ValueResult {
    let mut result: Vec<Value> = vec![];
    for seq in v {
        match seq {
            Value::List(l) => {
                result.extend_from_slice(&l);
            }
            Value::Nil => {}
            x => return type_err!("sequence", x.clone())
        }
    }
    Ok(result.into())
}

fn core_time_ms(_v: ValueList, _names: &NamePool) -> ValueResult {
    Ok(Value::Num(SystemTime::now().duration_since(SystemTime::UNIX_EPOCH).unwrap().as_millis() as f64))
}

fn core_println(v: ValueList, names: &NamePool) -> ValueResult {
    let mut it = v.iter();
    if let Some(val) = it.next() {
        print!("{}", Printer::str_name(val, names));
    }
    for expr in it {
        print!(" {}", Printer::str_name(expr, names));
    }
    println!();
    Ok(Value::Nil)
}

fn core_input(_v: ValueList, _names: &NamePool) -> ValueResult {
    let mut input = String::new();
    match std::io::stdin().read_line(&mut input) {
        Ok(_) => Ok(Value::Str(input.into())),
        Err(err) => Err(format!("IoError: {}", err).into())
    }
}

fn core_print(v: ValueList, names: &NamePool) -> ValueResult {
    let mut it = v.iter();
    if let Some(val) = it.next() {
        print!("{}", Printer::str_name(val, names));
    }
    for expr in it {
        print!(" {}", Printer::str_name(expr, names));
    }
    Ok(Value::Nil)
}

fn core_repr(v: ValueList, names: &NamePool) -> ValueResult {
    Ok(Value::Str(format!("{}", Printer::repr_name(&v[0], names)).into()))
}

fn operator_len(v: ValueList, names: &NamePool) -> ValueResult {
    let mut len = 0;
    let mut val = v[0].clone();
    loop {
        match val {
            Value::List(l) => return Ok(Value::Num(l.len() as f64)),
            Value::Str(s) => return Ok(Value::Num(s.len() as f64)),
            Value::Lazy{data, env, eval} => {
                len += 1;
                val = eval(data.tail.clone(), env.clone(), names)?;
            }
            Value::Nil => break,
            x => return type_err!("sequence", x.clone()),
        }
    }
    Ok(Value::Num(len as f64))
}

fn core_read(v: ValueList, names: &NamePool) -> ValueResult {
    if let Value::Str(input) = v[0].clone(){
        let input = input.inner().to_owned();
        let mut tk = parser::Reader::new(&input, names);
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

fn core_read_file(v: ValueList, _names: &NamePool) -> ValueResult {
    let file = File::open(match &v[0] {
        Value::Str(s) => s.inner().to_owned(),
        _ => return type_err!("string", v[0])
    });
    let mut contents = String::new();
    if let Ok(mut file) = file {
        match file.read_to_string(&mut contents) {
            Ok(_) => Ok(Value::Str(contents.into())),
            Err(err) => Err(format!("Couldn't read file: {:?}", err).into())
        }
        
    } else {
        Err("Couldn't open file".into())
    }
}

fn operator_inc(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Num(n) => Ok(Value::Num(*n + 1f64)),
        x => type_err!("number", x),
    }
}

fn operator_dec(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Num(n) => Ok(Value::Num(*n - 1f64)),
        x => type_err!("number", x),
    }
}

fn core_collect(v: ValueList, names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::List(_) => Ok(v[0].clone()),
        Value::Nil => Ok(Value::Nil),
        Value::Lazy{env, eval, data} => {
            let mut collect: ValueList = vec![data.head.clone()];
            
            let mut nth = data.tail.clone();
            let mut env = env.clone();
            loop {
                match eval(nth, env.clone(), names)? {
                    Value::Lazy{env: tenv, data, ..} => {
                        collect.push(data.head.clone());
                        nth = data.tail.clone();
                        env = tenv;
                    }
                    Value::Nil => break Ok(collect.into()),
                    Value::List(ls) => {
                        collect.extend(ls.iter().cloned());
                        break Ok(collect.into())
                    }
                    x => {
                        collect.push(x);
                        break Ok(collect.into())
                    }
                }
            }
        }
        x => type_err!("list", x),
    }
}

fn core_format(v: ValueList, names: &NamePool) -> ValueResult {
    if let Value::Str(format) = &v[0] {
        let mut iter = format.inner().chars().peekable();
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
                                            result.push_str(&format!("{}", Printer::repr_name(e, names)))
                                        }else{
                                            result.push_str(&format!("{}", Printer::str_name(e, names)))
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
                                                        result.push_str(&format!("{}", Printer::repr_name(expr, names)))
                                                    }else{
                                                        result.push_str(&format!("{}", Printer::str_name(expr, names)))
                                                    }
                                                };
                                                for expr in it {
                                                    if let Some(sep) = sep.clone() {
                                                        result.push_str(& if debug {format!("{}{:?}", sep, Printer::repr_name(expr, names))} else {format!("{}{}", sep, Printer::str_name(expr, names))})
                                                    }else{
                                                        result.push_str(& if debug {format!("{:?}", Printer::repr_name(expr, names))} else {format!("{}", Printer::str_name(expr, names))})
                                                    }
                                                }
                                            } else {
                                                break Err("Value expected to slice in format string must be a list".into())
                                            }
                                        },
                                        None => break Err("Value expected to format string not found".into()),
                                    };
                                    current += 1;
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
                None => break Ok(Value::Str(result.into()))
            }
        }
    } else {
        return Err("format requires a format string argument".into());
    }
}

fn core_join(v: ValueList, names: &NamePool) -> ValueResult {
    let sep = if let Value::Str(sep) = &v[0] {
        sep
    } else {
        return type_err!("string", v[0]);
    };

    let mut result = String::new();
    if let Value::List(list) = &v[1] {
        let mut it = list.iter();
        if let Some(x) = it.next() {
            result.push_str(&format!("{}", Printer::str_name(x, names)))
        }
        for expr in it {
            result.push_str(&format!("{}{}", sep.inner(), Printer::str_name(expr, names)))
        }
        Ok(Value::Str(result.into()))
    } else if let Value::Nil = &v[1] {
        Ok(Value::Str("".into()))
    } else {
        type_err!("list", v[1])
    }
}

fn core_symbol(v: ValueList, names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Str(s) => Ok(Value::Sym(names.add(s.inner()))),
        Value::Sym(_) => Ok(v[0].clone()),
        x => type_err!("string", x),
    }
}

fn core_assert(v: ValueList, names: &NamePool) -> ValueResult {
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
                return Err(format!("{}", Printer::str_name(&v[2], names)).into())
            }else{
                return Ok(v[0].clone())
            }
        },
        x => Err(error::Error::ArgErr(Some(crate::names::builtin::ASSERT), Arity::Range(1,3), x as u16))
    }
}

fn core_make_struct(v: ValueList, names: &NamePool) -> ValueResult {
    let mut data: ValueList = vec![];

    let struct_id = match &v[0] {
        Value::Sym(s) => names.get(*s),
        _ => return Err("Struct id must be a symbol".into())
    };

    for i in &v[1..] {
        data.push(i.clone());
    };
    Ok(Value::Struct(Rc::new(struct_id.clone()),Rc::new(data)))
}

fn core_member_struct(v: ValueList, names: &NamePool) -> ValueResult {
    let (struct_id, struct_data) = match &v[0] {
        Value::Struct(id, data) => (id, data),
        x => return type_err!("struct", x)
    };

    let check_id = match &v[1] {
        Value::Sym(s) => names.get(*s),
        x => return type_err!("symbol", x)
    };

    if (**struct_id).ne(&check_id) {
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

fn core_assert_struct(v: ValueList, names: &NamePool) -> ValueResult {
    let struct_id = match &v[0] {
        Value::Struct(id, _) => id,
        _ => return Err(format!("Expected Struct got {:?}", Printer::repr_name(&v[0], names)).into())
    };

    let check_id = match &v[1] {
        Value::Sym(s) => names.get(*s),
        x => return type_err!("symbol", x)
    };

    Ok((**struct_id).eq(&check_id).into())
}

pub fn core_string_append_char(v: ValueList, _names: &NamePool) -> ValueResult {
    match (&v[0], &v[1]) {
        (Value::Str(s), Value::Char(chr)) => {
            Ok(Value::Str(format!("{}{}", s.inner(), *chr).into()))
        },
        x => type_err!("string", x.0),
    }
}

pub fn core_char_to_string(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Char(c) => {
            Ok(Value::Str(c.to_string().into()))
        },
        x => type_err!("char", x),
    }
}

pub fn core_char_list_to_string(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::List(chrs) => {
            let mut res = String::new();
            for chr in chrs.iter() {
                match chr {
                    Value::Char(ch) => res.push(*ch),
                    x => return type_err!("char", x),
                }
            };
            Ok(Value::Str(res.into()))
        },
        x => type_err!("list", x),
    }
}

pub fn core_string_starts_with(v: ValueList, _names: &NamePool) -> ValueResult {
    match (&v[0], &v[1]) {
        (Value::Str(s), Value::Str(check)) => Ok(s.inner().starts_with(check.inner()).into()),
        (Value::Nil, Value::Str(_)) => Ok(Value::False),
        (x, y) => type_err!("(string string)", Value::from(vec![x.clone(), y.clone()])),
    }
}

pub fn core_chars_slice(v: ValueList, _names: &NamePool) -> ValueResult {
    n_args! { v;
        2 => match (&v[0], &v[1]) {
            (Value::Str(chars), Value::Num(start)) => {
                Ok(chars.slice(*start as usize).into())
            },
            _ => Err("arguments are invalid".into())
        },
        // 3 => match (&v[0], &v[1], &v[2]) {
        //     (Value::Chars(chars), Value::Num(start), Value::Num(end)) => {
        //         let slice = &chars[*start as usize .. *end as usize];
        //         if slice.len() == 0 {
        //             Ok(Value::Nil)
        //         } else {
        //             Ok(Value::Chars(Box::from(slice)))
        //         }
        //     },
        //     _ => Err("arguments are invalid".into())
        // },
        _ => Err("arguments are invalid".into())
    }
}

pub fn core_keyword(v: ValueList, names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Keyword(_) => Ok(v[0].clone()),
        Value::Str(s) => Ok(Value::Keyword(names.add(s.inner()))),
        x => type_err!("string, keyword", x.clone())
    }
}

pub fn core_keyword_intern_number(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Keyword(n) => Ok(Value::Num(n.0 as f64)),
        Value::Sym(n) => Ok(Value::Num(n.0 as f64)),
        x => type_err!("keyword, symbol", x.clone())
    }
}

pub fn core_name_from_intern_number(v: ValueList, _names: &NamePool) -> ValueResult {
    match &v[0] {
        Value::Num(n) => Ok(Value::Sym(Name(*n as i32))),
        x => type_err!("number", x.clone())
    }
}

pub fn core_name_interned_count(_v: ValueList, names: &NamePool) -> ValueResult {
    Ok(Value::Num(names.name_vec_size() as f64))
}

pub fn ns() -> Vec<(&'static str, Arity, fn(ValueList, &NamePool) -> ValueResult)>{
    vec![
        ("+", Arity::Min(0), |v: Vec<Value>, _| add_mul_op!(+, 0f64, v)),
        ("*", Arity::Min(0), |v: Vec<Value>, _| add_mul_op!(*, 1f64, v)),
        ("-", Arity::Min(0), |v: Vec<Value>, _| sub_div_op!(-, Ok(Value::Num(0.)), |a: f64| -a, v)),
        ("/", Arity::Min(0), |v: Vec<Value>, _| sub_div_op!(/, Err("Invalid number argument".into()), |a: f64| 1./a, v)),
        ("mod", Arity::Exact(2), |v: Vec<Value>, _| match (&v[0], &v[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a.rem_euclid(*b))),
            (a, b) => type_err!("number", Value::from(vec![a.clone(), b.clone()]))
        }),
        ("rem", Arity::Exact(2), |v: Vec<Value>, _| match (&v[0], &v[1]) {
            (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a % b)),
            (a, b) => type_err!("number", Value::from(vec![a.clone(), b.clone()]))
        }),
        ("<", Arity::Min(0), |v: Vec<Value>, _| ord_op!(<, v)),
        (">", Arity::Min(0), |v: Vec<Value>, _| ord_op!(>, v)),
        ("<=", Arity::Min(0), |v: Vec<Value>, _| ord_op!(<=, v)),
        (">=", Arity::Min(0), |v: Vec<Value>, _| ord_op!(>=, v)),
        ("==", Arity::Min(1), operator_eq),
        ("!=", Arity::Min(1), operator_ne),
        ("str", Arity::Min(0), operator_str),
        ("list", Arity::Min(0), |v: Vec<Value>, _| Ok(v.into())),
        ("first", Arity::Exact(1), operator_head),
        ("second", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(1f64)], n)),
        ("third", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(2f64)], n)),
        ("fourth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(3f64)], n)),
        ("fifth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(4f64)], n)),
        ("sixth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(5f64)], n)),
        ("seventh", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(6f64)], n)),
        ("eigth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(6f64)], n)),
        ("nineth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(6f64)], n)),
        ("tenth", Arity::Exact(1), |v: Vec<Value>, n| operator_nth(vec![v[0].clone(), Value::Num(6f64)], n)),
        ("nth", Arity::Exact(2), operator_nth),
        ("rest", Arity::Exact(1), operator_tail),
        ("cons", Arity::Exact(2), operator_cons),
        ("rev-cons", Arity::Exact(2), operator_rev_cons),
        ("atom?", Arity::Exact(1), pred_atom),
        ("list?", Arity::Exact(1), pred_list),
        ("nil?", Arity::Exact(1), pred_nil),
        ("number?", Arity::Exact(1), pred_number),
        ("string?", Arity::Exact(1), pred_string),
        ("symbol?", Arity::Exact(1), pred_symbol),
        ("function?", Arity::Exact(1), pred_function),
        ("keyword?", Arity::Exact(1), pred_keyword),
        ("hash-map?", Arity::Exact(1), pred_hashmap),
        ("apply", Arity::Min(2), core_apply),
        ("map", Arity::Exact(2), core_map),
        ("filter", Arity::Exact(2), core_filter),
        ("flatmap", Arity::Exact(2), core_flatmap),
        ("append", Arity::Min(0), core_append),
        ("time-ms", Arity::Exact(0), core_time_ms),
        ("println", Arity::Min(0), core_println),
        ("print", Arity::Min(0), core_print),
        ("input", Arity::Exact(0), core_input),
        ("repr", Arity::Min(0), core_repr),
        ("len", Arity::Exact(1), operator_len),
        ("read", Arity::Exact(1), core_read),
        ("read-file", Arity::Exact(1), core_read_file),
        ("inc", Arity::Exact(1), operator_inc),
        ("dec", Arity::Exact(1), operator_dec),
        ("collect", Arity::Exact(1), core_collect),
        ("format", Arity::Min(1), core_format),
        ("join", Arity::Min(2), core_join),
        ("hash-map", Arity::Min(0), core_hashmap),
        ("assoc", Arity::Min(1), operator_assoc),
        ("dissoc", Arity::Min(1), operator_dissoc),
        ("get-key", Arity::Exact(2), operator_map_get),
        ("update", Arity::Min(3), operator_map_update),
        ("has-key?", Arity::Exact(2), operator_has_key),
        ("map-keys", Arity::Exact(1), core_map_keys),
        ("symbol", Arity::Exact(1), core_symbol),
        ("make-struct", Arity::Min(1), core_make_struct),
        ("index-struct", Arity::Exact(3), core_member_struct),
        ("assert-struct", Arity::Exact(2), core_assert_struct),
        ("assert", Arity::Range(1,3),core_assert),
        ("keyword", Arity::Exact(1), core_keyword),
        ("name-intern-number", Arity::Exact(1), core_keyword_intern_number),
        ("symbol-from-intern-number", Arity::Exact(1), core_name_from_intern_number),
        ("interned-name-count", Arity::Exact(0), core_name_interned_count),
        ("box", Arity::Exact(1),|v: Vec<Value>, _| Ok(Value::Box(Rc::new(RefCell::new(v[0].clone()))))),
        ("set-box", Arity::Exact(2), |v: Vec<Value>, _| 
            match &v[0] {
                Value::Box(data) => {*data.borrow_mut() = v[1].clone(); Ok(v[1].clone())},
                _ => Err("Value is not a box".into()),
        }),
        ("swap-box", Arity::Min(2), |v: Vec<Value>, names| 
            match &v[0] {
                Value::Box(data) => {
                    let mut args = vec![data.borrow().clone()];
                    args.extend_from_slice(&v[2..]);
                    let new_value = v[1].apply(args, names)?;
                    *data.borrow_mut() = new_value;
                    Ok(v[1].clone())
                },
                _ => Err("Value is not a box".into()),
            }
        ),
        ("deref", Arity::Exact(1), |v: Vec<Value>, _| match &v[0] {
            Value::Box(data) => Ok(data.borrow().clone()),
            _ => Err("Can't deref non box".into())
        }),
        ("reverse", Arity::Exact(1), |v: Vec<Value>, _| match &v[0] {
            Value::List(data) => Ok(data.iter().rev().map(|v| v.clone()).collect::<ValueList>().into()),
            _ => Err("Can't reverse a non list".into())
        }),
        ("id", Arity::Exact(1), |v: Vec<Value>, _| return Ok(v[0].clone())),
        ("string/starts-with", Arity::Exact(2), core_string_starts_with),
        ("string/append-char", Arity::Exact(2), core_string_append_char),
        ("string/slice", Arity::Range(2, 3), core_chars_slice),
        ("char->string", Arity::Exact(1), core_char_to_string),
        ("char-list->string", Arity::Exact(1), core_char_list_to_string),
    ]
}
