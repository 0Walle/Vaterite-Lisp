use crate::types::{Value};
use crate::names::{NamePool};

pub struct Printer {}

impl Printer {
    pub fn repr_(value: &Value, level: i32) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{:?}", s),
            Value::Char(s) => format!("#{:?}", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => if level == 0 {
                    format!("'[Symbol {}]", s.0)
                } else {
                    format!("[Symbol {}]", s.0)
                },
            Value::Keyword(s) => format!("[Keyword {}]", s.0),
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_(x, level + 1)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_(expr, level + 1)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => if let Some(_) = &func.name {
                format!("[Function]")
            } else {
                format!("[Function]")
            },
            Value::Box(val) => format!("(box {})", Printer::repr_(&(val.borrow()), level)),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {})", Printer::repr_(&*head, level), Printer::repr_(&*tail, level)),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}[Keyword {}] {}\n", (0..level+1).map(|_| "  ").collect::<String>(), k.0, Printer::repr_(v, level+1)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({} ", id));
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_(x, level + 1)))
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_(expr, level + 1)))
                }
                res.push_str("]");
                res
            }
        }
    }

    pub fn repr_name(value: &Value, level: i32, names: &NamePool) -> String {
        match value {
            Value::Keyword(s) => format!(":{}", names.get(*s)),
            Value::List(list) => {
                let mut res = String::new();
                if level == 0 {
                    res.push('\'');
                };
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_name(x, level + 1, names)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_name(expr, level + 1, names)));
                }
                res.push_str(")");
                res
            },
            Value::Box(val) => format!("(box {})", Printer::repr_name(&(val.borrow()), level, names)),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {})", Printer::repr_name(&*head, level, names), Printer::repr_name(&*tail, level, names)),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[\n");
                for (k, v) in map.iter() {
                    res.push_str(&format!("{}:{} {}\n", (0..level+1).map(|_| "  ").collect::<String>(), names.get(*k), Printer::repr_name(v, level+1, names)));
                };
                res.push_str(&format!("{}]", (0..level).map(|_| "  ").collect::<String>()));
                res
            }
            Value::Struct(id, list) => {
                let mut res = String::new();
                res.push_str(&format!("({} ", id));
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_name(x, level + 1, names)))
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_name(expr, level + 1, names)))
                }
                res.push_str("]");
                res
            }
            _ => Printer::repr_(value, level)
        }
    }

    pub fn repr_color(value: &Value, level: i32, names: &NamePool) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("\x1b[95m#t\x1b[0m"),
            Value::False => format!("\x1b[95m#f\x1b[0m"),
            Value::Num(n) => format!("\x1b[93m{}\x1b[0m", n),
            Value::Str(s) => format!("\x1b[32m{:?}\x1b[0m", s),
            Value::Char(s) => format!("\x1b[93m#{:?}\x1b[0m", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => if level == 0 {
                    format!("'{}", names.get(*s))
                } else {
                    format!("{}", names.get(*s))
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
            Value::NatFunc(func) => format!("\x1b[36m[NativeFunction {}]\x1b[0m", func.name),
            Value::Func { func, .. } => if let Some(name) = &func.name {
                format!("\x1b[36m[Function {}]\x1b[0m", names.get(*name))
            } else {
                format!("\x1b[36m[Function]\x1b[0m")
            },
            Value::Box(val) => format!("(box {})", Printer::repr_color(&(val.borrow()), level, names)),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {})", Printer::repr_color(&*head, level, names), Printer::repr_color(&*tail, level, names)),
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
                res.push_str(&format!("({} ", id));
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::repr_color(x, level + 1, names)))
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::repr_color(expr, level + 1, names)))
                }
                res.push_str("]");
                res
            }
        }
    }

    pub fn str_(value: &Value) -> String {
        match value {
            Value::Nil => format!("()"),
            Value::True => format!("#t"),
            Value::False => format!("#f"),
            Value::Num(n) => format!("{}", n),
            Value::Str(s) => format!("{}", s),
            Value::Char(s) => format!("{}", s),
            Value::Chars(s) => format!("{:?}", s),
            Value::Sym(s) => format!("[Symbol {}]", s.0),
            Value::Keyword(s) => format!("[Keyword {}]", s.0),
            Value::List(list) => {
                let mut res = String::new();
                res.push('(');
                let mut it = list.iter();
                if let Some(x) = it.next() {
                    res.push_str(&format!("{}", Printer::str_(x)));
                }
                for expr in it {
                    res.push_str(&format!(" {}", Printer::str_(expr)));
                }
                res.push_str(")");
                res
            },
            Value::NatFunc(_) => format!("[NativeFunction]"),
            Value::Func { func, .. } => if let Some(_) = &func.name {
                format!("[Function]")
            } else {
                format!("[Function]")
            },
            Value::Box(val) => format!("(box {})", Printer::str_(&(val.borrow()))),
            Value::Lazy{head, tail, ..} => format!("(lazy-cons {} {:?})", Printer::str_(&*head), tail),
            Value::Map(map) => {
                let mut res = String::new();
                res.push_str("#[ ");
                for (k, v) in map.iter() {
                    res.push_str(&format!("[Keyword {}] {} ", k.0, Printer::str_(v)));
                };
                res.push_str("]");
                res
            }
            Value::Struct(id, _) => format!("[Struct {}]", id)
        }
    }

    pub fn str_name(value: &Value, names: &NamePool) -> String {
        match value {
            Value::Keyword(s) => format!(":{}", names.get(*s)),
            _ => Printer::str_(value)
        }
    }
}