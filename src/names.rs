use std::cell::RefCell;

/// Interned name id
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub i32);

// impl std::fmt::Display for Name {
//     fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

/// Stores the interned names (aka hospital)
pub struct NamePool {
    names: RefCell<Vec<String>>
}

impl NamePool {
    pub fn new() -> Self {
        NamePool {
            names: RefCell::new(vec![])
        }
    }

    pub fn add(&self, name: &str) -> Name {
        let mut names = self.names.borrow_mut();
        if let Some(name) = get_builtin_name(name) {
            name
        } else if let Some(pos) = names.iter().position(|n| n == name) {
            Name(pos as i32)
        } else {
            let n = names.len();
            names.push(name.to_owned());
            Name(n as i32)
        }
    }

    pub fn get(&self, name: Name) -> String {
        if let Some(s) = get_builtin(name) {
            return s.to_string()
        }
        self.names.borrow().get(name.0 as usize).unwrap_or(&"[invalid name]".to_string()).clone()
    }
}


macro_rules! buitin_names {
    ( $($s:expr => $name:ident = $value:expr,)+ ) => {
        #[allow(dead_code)]
        pub mod builtin {
            use super::Name;
            $( pub const $name: Name = Name($value); )+
        }

        pub fn get_builtin_name(name: &str) -> Option<Name> {
            match name {
                $( $s => Some(Name($value)), )+
                _ => None
            }
        }
        
        pub fn get_builtin(name: Name) -> Option<&'static str> {
            match name.0 {
                $( $value => Some($s), )+
                _ => None
            }
        }
    };
}

buitin_names! {
    "rest" => REST = -1,
    "opt" => OPT = -2,
    "key" => KEY = -3,
    "quote" => QUOTE = -4,
    "quasiquote" => QUASIQUOTE = -5,
    "unquote" => UNQUOTE = -6,
    "unquote-splicing" => UNQUOTE_SPLICING = -7,
    "macro-expand" => MACRO_EXPAND = -8,
    "if" => IF = -9,
    "cond" => COND = -10,
    "match" => MATCH = -11,
    "and" => AND = -12,
    "or" => OR = -13,
    "def" => DEF = -14,
    "struct" => STRUCT = -15,
    "fun" => FUN = -16,
    "defmacro" => DEFMACRO = -17,
    "fn" => FN = -18,
    "let" => LET = -19,
    "for" => FOR = -20,
    "module" => MODULE = -21,
    "exports" => EXPORTS = -22,
    "import" => IMPORT = -23,
    "block" => BLOCK = -24,
    "eval" => EVAL = -25,
    "throw" => THROW = -26,
    "catch" => CATCH = -27,
    "lazy-cons" => LAZY_CONS = -28,
    "_" => IT_ = -29,
    "nil" => NIL = -30,
    "list" => LIST = -31,
    "hash-map" => HASH_MAP = -32,
    "cons" => CONS = -33,
    "rev-cons" => REV_CONS = -34,
    "first" => FIRST = -35,
    "second" => SECOND = -36,
    "third" => THIRD = -37,
    "fourth" => FOURTH = -38,
    "fifth" => FIFTH = -39,
    "sixth" => SIXTH = -40,
    "seventh" => SEVENTH = -41,
    "eigth" => EIGTH = -42,
    "nineth" => NINETH = -43,
    "tenth" => TENTH = -44,
    "nth" => NTH = -45,
    "str" => STR = -46,
    "repr" => REPR = -47,
    "append" => APPEND = -48,
    "deref" => DEREF = -49,
    "from" => FROM = -50,
    "exit" => EXIT = -51,
    "." => DOT_ = -52,
    "assert" => ASSERT = -53,
}
