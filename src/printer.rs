use crate::types::{Value};
use crate::names::{NamePool};
use crate::error::Error;

pub struct Printer {}

impl Printer {
    pub fn repr_name(value: &Value, names: &NamePool) -> String {
        Printer::repr_name_(value, 0, names)
    }

    fn repr_name_(value: &Value, level: i32, names: &NamePool) -> String {
        match value {
            Value::Keyword(s) => format!(":{}", names.get(*s)),
            Value::Sym(s) => if level == 0 {
                format!("'{}", names.get(*s))
            } else {
                format!("{}", names.get(*s))
            },
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_name_(x, level + 1, names)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_name_(expr, level + 1, names)));
                }
                res.push_str(")");
                res
            },
            Value::Box(val) => format!("(box {})", Printer::repr_name_(&(val.borrow()), level, names)),
            Value::Lazy{data, ..} => format!("(cons* {} {})", Printer::repr_name_(&data.head, level, names), Printer::repr_name_(&data.tail, level, names)),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}:{} {}\n", (0..level+1).map(|_| "  ").collect::<String>(), names.get(*k), Printer::repr_name_(v, level+1, names)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({}", names.get(id.name)));
                for expr in list.iter() {
                    res.push_str(&format!(" {}", Printer::repr_name_(expr, level + 1, names)))
                }
                res.push_str(")");
                res
            }
            Value::StructDef(id) => format!("[Struct {}]", names.get(id.name)),
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{:?}", s.inner()),
            Value::Char(s) => format!("#{:?}", s),
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => if let Some(name) = &func.name {
                format!("[Function {}]", names.get(*name))
            } else {
                format!("[Function]")
            },
        }
    }

    pub fn repr_color(value: &Value, level: i32, names: &NamePool) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("\x1b[95m#t\x1b[0m"),
            Value::False => format!("\x1b[95m#f\x1b[0m"),
            Value::Num(n) => format!("\x1b[93m{}\x1b[0m", n),
            Value::Str(s) => format!("\x1b[32m{:?}\x1b[0m", s.inner()),
            Value::Char(s) => format!("\x1b[93m#{:?}\x1b[0m", s),
            Value::Sym(s) => if level == 0 {
                    format!("'{}", names.get(*s))
                } else {
                    if *s == crate::names::builtin::NIL {
                        format!("\x1b[90mnil\x1b[0m")
                    } else {
                        format!("{}", names.get(*s))
                    }
                },
            Value::Keyword(s) => format!("\x1b[32m:{}\x1b[0m", names.get(*s)),
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_color(x, level + 1, names)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_color(expr, level + 1, names)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(func) => format!("\x1b[36m[NativeFunction {}]\x1b[0m", names.get(func.name)),
            Value::Func { func, .. } => if let Some(name) = &func.name {
                format!("\x1b[36m[Function {}]\x1b[0m", names.get(*name))
            } else {
                format!("\x1b[36m[Function]\x1b[0m")
            },
            Value::Box(val) => format!("(box {})", Printer::repr_color(&(val.borrow()), level, names)),
            Value::Lazy{data, ..} => format!("(cons* {} {})", Printer::repr_color(&data.head, level, names), Printer::repr_color(&data.tail, level, names)),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}:{} {}\n", (0..level+1).map(|_| "  ").collect::<String>(), names.get(*k), Printer::repr_color(v, level+1, names)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({}", names.get(id.name)));
                for expr in list.iter() {
                    res.push_str(&format!(" {}", Printer::repr_color(expr, level + 1, names)))
                }
                res.push_str(")");
                res
            }
            Value::StructDef(id) => format!("[Struct {}]", names.get(id.name)),
        }
    }

    pub fn str_name(value: &Value, names: &NamePool) -> String {
        match value {
            Value::Keyword(s) => format!(":{}", names.get(*s)),
            Value::Sym(s) => format!("{}", names.get(*s)),
            Value::List(list) => {
                let mut res = String::new();
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::str_name(x, names)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::str_name(expr, names)));
                }
                res.push_str(")");
                res
            },
            Value::Box(val) => format!("(box {})", Printer::repr_name(&(val.borrow()), names)),
            Value::Lazy{data, ..} => format!("(cons* {} {})", Printer::repr_name(&data.head, names), Printer::repr_name(&data.tail, names)),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[ ");
                for (k, v) in map.iter() {
                    res.push_str(&format!(":{} {}", names.get(*k), Printer::str_name(v, names)));
                };
                res.push_str("]");
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({}", names.get(id.name)));
                for expr in list.iter() {
                    res.push_str(&format!(" {}", Printer::str_name(expr, names)))
                }
                res.push_str(")");
                res
            }
            Value::StructDef(id) => format!("[Struct {}]", names.get(id.name)),
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{}", s.inner()),
            Value::Char(s) => format!("{}", s),
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => if let Some(name) = &func.name {
                format!("[Function {}]", names.get(*name))
            } else {
                format!("[Function]")
            },
        }
    }

    pub fn str_error(value: &Error, names: &NamePool) -> String {
        match value {
            Error::Reason(s) => format!("{}", s),
            Error::BindErr(s) => format!("Name {} not found", names.get(*s)),
            Error::KwArgErr(name) => if let Some(name) = name {
                format!("Keyword arguments are not in pairs calling {}", names.get(*name))
            } else {
                format!("Keyword arguments are not in pairs")
            }
            Error::ArgErr(name, arity, got) => if let Some(name) = name {
                format!("{} expected {} but got {}", names.get(*name), arity, got)
            } else {
                format!("Expected {} but got {}", arity, got)
            }
            Error::TypeErr(expected, got) => if let Some(v) = got {
                format!("Expected value of type '{}' but got {}", expected, Printer::repr_name(&v, names))
            } else {
                format!("Expected value of type '{}'", expected)
            },
            Error::PairErr(name) => if let Some(v) = name {
                format!("{} must be a pair", v)
            } else {
                format!("Expected a pair")
            },
            Error::Throw(v) => if let Some(v) = v {
                format!("Thrown value '{}'", Printer::repr_name(&v, names))
            } else {
                format!("Thrown error")
            },
        }
    }
}