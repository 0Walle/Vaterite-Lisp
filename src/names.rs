use std::cell::RefCell;

/// Interned name id
#[derive(Copy, Clone, PartialEq, Eq, Debug, Hash)]
pub struct Name(pub i32);

/// Stores the interned names (aka hospital)
pub struct NamePool {
    names: RefCell<[Vec<String>; 7]>
}

impl NamePool {
    pub fn new() -> Self {
        NamePool {
            names: RefCell::new([vec![], vec![], vec![], vec![], vec![], vec![], vec![]])
        }
    }

    pub fn add(&self, name: &str) -> Name {
        let mut names = self.names.borrow_mut();
        if let Some(name) = get_builtin_name(name) {
            return name
        }
        let (index, level) = match name.len() {
            0..=3 => unsafe {
                let mut bytes: [u8; 4] = [0, 0, 0, 0];
                bytes[0] = name.as_bytes().get(0usize).cloned().unwrap_or(0);
                bytes[1] = name.as_bytes().get(1usize).cloned().unwrap_or(0);
                bytes[2] = name.as_bytes().get(2usize).cloned().unwrap_or(0);
                let index = std::mem::transmute::<[u8; 4], u32>(bytes);
                (index as i32, 0)
            },
            4..=5 => (NamePool::search_vec(&mut names[0], name), 1),
            6..=7 => (NamePool::search_vec(&mut names[1], name), 2),
            8..=11 => (NamePool::search_vec(&mut names[2], name), 3),
            12..=15 => (NamePool::search_vec(&mut names[3], name), 4),
            16..=23 => (NamePool::search_vec(&mut names[4], name), 5),
            24..=31 => (NamePool::search_vec(&mut names[5], name), 6),
            _ => (NamePool::search_vec(&mut names[6], name), 7),
        };
        let name = index << 3 | level;
        Name(name)
    }

    pub fn get(&self, name: Name) -> String {
        if let Some(s) = get_builtin(name) {
            return s.to_string()
        }
        let level = name.0 & 0b111;
        let index = name.0 >> 3;
        let names = self.names.borrow();
        if level == 0 {
            unsafe {
                let bytes: [u8; 3] = [(index & 0xff) as u8, ((index & 0x00ff00) >> 8) as u8, ((index & 0xff0000) >> 16) as u8];
                return String::from_utf8_unchecked(bytes.to_vec())
            }
        }
        let level = level - 1;
        names[level as usize].get(index as usize).unwrap_or(&"[Invalid Name]".to_string()).clone()
    }

    fn search_vec(names: &mut Vec<String>, name: &str) -> i32 {
        if let Some(pos) = names.iter().position(|n| n == name) {
            pos as i32
        } else {
            let n = names.len();
            names.push(name.to_owned());
            n as i32
        }
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
    "cons*" => LAZY_CONS = -28,
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
    "*modules*" => SP_MODULES = -54,
    "*dir-name*" => SP_DIR_NAME = -55,
    "loop" => LOOP = -56,
    "format" => FORMAT = -57,
    "assoc" => ASSOC = -58,
    "map" => MAP = -59,
    "filter" => FILTER = -60,
    "number" => NUMBER = -61,
    "char" => CHAR = -62,
    "string" => STRING = -63,
    "symbol" => SYMBOL = -64,
    "keyword" => KEYWORD = -65,
    "function" => FUNCTION = -66,
    "box" => BOX = -67,
    "sequence" => SEQUENCE = -68,
    "=>" => PIPE_ = -69,
    "=>>" => PIPEPE_ = -70,
    "p>" => PARTIALR_ = -71,
    "apply" => APPLY = -72,
}
