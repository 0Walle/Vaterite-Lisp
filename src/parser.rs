use crate::types::{Value, ValueList};
use std::iter::Peekable;
use std::vec::IntoIter;
use std::rc::Rc;

#[macro_export]
macro_rules! list {
    // ( $arg:expr ) => {
    //     Value::List(Rc::new($arg))
    // };
    [ $($args:expr),* ] => {{
        let vec: Vec<Value> = vec![$($args),*];
        Value::List(Rc::new(vec))
    }}
}

macro_rules! vater_args {
    ( $($rest:tt)* ) => { vater!( $($rest)* ) };
}

#[macro_export]
macro_rules! vater {
    { (sym $id:expr) } => { Value::Sym($id.to_string()) };
    { (: $id:ident) } => { Value::Keyword(stringify!($id).to_string()) };
    { [ $val:expr] } => { $val };
    { [list $ls:expr ] } => { Value::List(Rc::new($ls)) };
    { ( $($exprs:tt)* ) } => { Value::List(Rc::new(vec![ $(vater_args!($exprs)),* ])) };
    { nil } => { Value::Nil };
    { $id:ident } => { Value::Sym( stringify!($id).to_string() ) };
    { $val:expr } => { Value::from($val) };
}

/// Vaterite tokens
#[derive(Debug)]
pub enum Token {
    Eof,
    Lparen,
    Rparen,
    Quote,
    Backquote,
    Comma,
    CommaAt,
    Tilde,
    Lbrack,
    Rbrack,
    HashBrack,
    HashComment,
    True,
    False,
    Number(f64),
    Symbol(String),
    Keyword(String),
    String(String),
    Macro(String),
}

/// The tokenizer reads an input and produces expressions
pub struct Reader {
    chars: Peekable<IntoIter<char>>,
    current_line: i32,
}

pub struct ReaderError {
    pub err: String,
    pub line: i32,
}

pub enum ParserResult {
    EofErr,
    TokenErr(String),
    Expr(Value)
}

macro_rules! reader_err {
    ($err:expr, $line:expr) => {Err(ReaderError {err: $err.to_string(), line: $line})};
}

macro_rules! token_try {
    ($reader:expr) => {
        match $reader.next_token() {
            Ok(tok) => tok,
            Err(err) => return ParserResult::TokenErr(format!("Invalid Syntax (line {}): {}", err.line, err.err))
        };
    };
}

impl Reader {
    /// Create a new Tokenizer from a source string
    pub fn new(source: String) -> Self {
        Reader{
            chars: source.chars().collect::<Vec<char>>().into_iter().peekable(),
            current_line: 1,
        }
    }

    /// Read the next Token in the stream
    pub fn next_token(&mut self) -> Result<Token, ReaderError> {
        loop{
            let ch = self.chars.next();
            match ch {
                Some(chr) => match chr {
                    ' ' | '\t' | '\r' => continue,
                    '\n' => self.current_line += 1,
                    ';' => loop {
                        match self.chars.next() {
                            Some('\n') => {
                                self.current_line += 1;
                                break
                            },
                            Some(_) => continue,
                            None => return Ok(Token::Eof),
                        }
                    }
                    '(' => return Ok(Token::Lparen),
                    ')' => return Ok(Token::Rparen),
                    '[' => return Ok(Token::Lbrack),
                    ']' => return Ok(Token::Rbrack),
                    '\'' => return Ok(Token::Quote),
                    ',' => match self.chars.peek() {
                        Some(c) => {
                            if *c == '@' {
                                self.chars.next();
                                return Ok(Token::CommaAt)
                            }else{
                                return Ok(Token::Comma)
                            }
                        }
                        None => return reader_err!("Unexpected end of input reading unquote", self.current_line),
                        // None => return Err(ReaderError{err: "Unexpected end of input reading unquote".to_string(), line: self.current_line}),
                    }
                    '`' => return Ok(Token::Backquote),
                    '~' => return Ok(Token::Tilde),
                    '-' => match self.chars.peek() {
                        Some(c) => match *c {
                            '0' ..= '9' => match self.read_number(chr) {
                                Ok(num) => return Ok(Token::Number(num)),
                                Err(err) => return reader_err!(err, self.current_line)
                            }
                            _ => match self.read_symbol('-') {
                                Ok(sym) => return Ok(Token::Symbol(sym)),
                                Err(err) => return reader_err!(err, self.current_line)
                            }
                        }
                        None => return Ok(Token::Symbol("-".to_string())),
                    },
                    '0' ..= '9' => {
                        match self.read_number(chr) {
                            Ok(num) => return Ok(Token::Number(num)),
                            Err(err) => return reader_err!(err, self.current_line)
                        }
                    }
                    '"' => {
                        let mut res: String = String::new();
                        loop {
                            match self.chars.next() {
                                Some(c) => match c {
                                    '"' => break,
                                    '\\' => match self.chars.next() {
                                        Some(c) => match c {
                                            '"' => res.push('"'),
                                            '\\' => res.push('\\'),
                                            'n' => res.push('\n'),
                                            't' => res.push('\t'),
                                            'r' => res.push('\r'),
                                            '\n' => {self.current_line += 1;},
                                            _ => res.push(c),
                                        }
                                        None => return reader_err!("Unexpected end of input reading string", self.current_line),
                                    }
                                    // '\n' => return reader_err!("Unexpected line break reading string", self.current_line),
                                    c => res.push(c)
                                }
                                None => return reader_err!("Unexpected end of input reading string", self.current_line),
                            }
                        }
                        return Ok(Token::String(res));
                    }
                    ':' => {
                        let chr = match self.chars.next() {
                            Some(c) => c,
                            None => return reader_err!("Unexpected end of input reading keyword", self.current_line)
                        };
                        match self.read_symbol(chr) {
                            Ok(sym) => return Ok(Token::Keyword(sym)),
                            Err(err) => return reader_err!(err, self.current_line)
                        }
                    }
                    '#' => match self.chars.peek() {
                        Some(c) => {
                            match *c {
                                '[' => {
                                    self.chars.next();
                                    return Ok(Token::HashBrack)
                                }
                                '#' => {
                                    self.chars.next();
                                    let chr = match self.chars.next() {
                                        Some(c) => c,
                                        None => return reader_err!("Unexpected end of input reading reader macro", self.current_line)
                                    };
                                    match self.read_symbol(chr) {
                                        Ok(sym) => return Ok(Token::Macro(sym)),
                                        Err(err) => return reader_err!(err, self.current_line)
                                    }
                                }
                                '\'' => {
                                    self.chars.next();
                                    let chr = match self.chars.next() {
                                        Some(ch) => match ch {
                                            '\\' => match self.chars.next() {
                                                Some(ch) => Token::Number(match ch {
                                                    '\'' => '\'',
                                                    '\\' => '\\',
                                                    'n' => '\n',
                                                    't' => '\t',
                                                    'r' => '\r',
                                                    c => c,
                                                } as i32 as f64),
                                                None => return reader_err!("Unexpected end of input reading character literal", self.current_line)
                                            }
                                            ch => Token::Number(ch as i32 as f64),
                                        }
                                        None => return reader_err!("Unexpected end of input reading character literal", self.current_line)
                                    };
                                    return match self.chars.next() {
                                        Some(ch) if ch == '\'' => Ok(chr),
                                        _ => return reader_err!("Unexpected end of input reading character literal", self.current_line)
                                    }
                                }
                                't' => {
                                    self.chars.next();
                                    return Ok(Token::True)
                                }
                                'f' => {
                                    self.chars.next();
                                    return Ok(Token::False)
                                }
                                ';' => {
                                    self.chars.next();
                                    return Ok(Token::HashComment)
                                }
                                _ => return Ok(Token::Macro("macro-expand".to_string()))
                            }
                            // if *c == '[' {
                            //     self.chars.next();
                            //     return Ok(Token::HashBrack)
                            // }else if *c == '#' {
                            //     self.chars.next();
                            //     let chr = match self.chars.next() {
                            //         Some(c) => c,
                            //         None => return reader_err!("Unexpected end of input reading reader macro", self.current_line)
                            //     };
                            //     match self.read_symbol(chr) {
                            //         Ok(sym) => return Ok(Token::Macro(sym)),
                            //         Err(err) => return reader_err!(err, self.current_line)
                            //     }
                            //     //self.chars.next();
                            //     //return Ok(Token::HashBrack)
                            // }else{
                            //     return Ok(Token::Quote)
                            // }
                        }
                        None => return reader_err!("Unexpected end of input reading hash syntax", self.current_line),
                    }
                    c => {
                        match self.read_symbol(c) {
                            Ok(sym) => return Ok(Token::Symbol(sym)),
                            Err(err) => return reader_err!(err, self.current_line)
                        }
                    }
                }
                None => return Ok(Token::Eof),
            }
        }
    }

    fn read_number(&mut self, chr: char) -> Result<f64, &'static str> {
        let mut res: String = String::new();
        let mut next_char = chr;
        loop {
            res.push(next_char);
            match self.chars.peek() {
                Some(c) => match *c {
                    '0' ..= '9' => {
                        next_char = *c;
                        self.chars.next();
                    }
                    '.' => {
                        next_char = *c;
                        self.chars.next();
                    }
                    '\n' | ' ' | '\t' | '\r'
                    | ',' | ';' | '"' | '`' | '@' | '~' | '\''
                    | '(' | ')' | '{' | '}' | '[' | ']' => break,
                    _ => return Err("Unexpected character on number token"),
                }
                None => return Err("Unexpected end of input reading number"),
            }
        }
        return Ok(res.parse::<f64>().unwrap());
    }

    fn read_symbol(&mut self, chr: char) -> Result<String, &'static str> {
        let mut res: String = String::new();
        res.push(chr);
        loop {
            if let Some(c) = self.chars.peek() {
                match *c {
                     '\n' | ' ' | '\t' | '\r'
                    | ',' | ';' | '"' | '`' | '@' | '~' | '\''
                    | '(' | ')' | '{' | '}' | '[' | ']' => break,
                    _ => {res.push(*c);self.chars.next();}
                }
            } else {
                break
            }
        } 
        return Ok(res);
    }

    /// Parse an expression from a starting token
    pub fn parse_expr(&mut self, look: Token) -> ParserResult {
        match look {
            Token::Lparen => {
                let mut list_val: ValueList = vec![];
                loop{
                    let tok = token_try!(self);
                    match tok {
                        Token::Rparen => return ParserResult::Expr(list_val.into()),
                        Token::Eof => return ParserResult::EofErr,
                        Token::HashComment => {
                            let tok = token_try!(self);
                            match self.parse_expr(tok) {
                                ParserResult::Expr(expr) => expr,
                                err => return err
                            };
                        },
                        // Token::Symbol(s) if s == "->" => {
                        //     let mut nlist: ValueList = vec![];
                        //     nlist.push(Value::Sym("arrow".to_string()));
                        //     nlist.push(list!(list_val.clone()));
                        //     list_val = nlist;
                        // }
                        Token::Symbol(s) if s == "." => {
                            if list_val.len() == 0 {
                                list_val.push(Value::Sym(".".to_string()));
                            } else {
                                let mut nlist: ValueList = vec![];
                                nlist.push(list_val.pop().unwrap());
                                let tok = token_try!(self);
                                nlist.push(match self.parse_expr(tok) {
                                    ParserResult::Expr(expr) => expr,
                                    err => return err
                                });
                                list_val.push(nlist.into());
                            }
                        }
                        // Token::Symbol(s) if s == "..." => {
                        //     if list_val.len() == 0 {
                        //         list_val.push(Value::Sym("...".to_string()));
                        //     } else {
                        //         let mut nlist: ValueList = vec![];
                        //         nlist.push(Value::Sym("ellipsis".to_string()));
                        //         nlist.push(list_val.pop().unwrap());
                        //         list_val.push(list!(nlist));
                        //     }
                        // }
                        tk => list_val.push(match self.parse_expr(tk) {
                            ParserResult::Expr(expr) => expr,
                            err => return err
                        })
                    }
                }
            },
            Token::Lbrack => {
                let mut list_val: ValueList = vec![Value::Sym("list".to_string())];
                loop{
                    let tok = token_try!(self);
                    match tok {
                        Token::Rbrack => return ParserResult::Expr(list_val.into()),
                        Token::Eof => return ParserResult::EofErr,
                        Token::HashComment => {
                            let tok = token_try!(self);
                            match self.parse_expr(tok) {
                                ParserResult::Expr(expr) => expr,
                                err => return err
                            };
                        },
                        tk => list_val.push(match self.parse_expr(tk) {
                            ParserResult::Expr(expr) => expr,
                            err => return err
                        })
                    }
                }
            },
            Token::HashBrack => {
                let mut list_val: ValueList = vec![Value::Sym("hash-map".to_string())];
                loop{
                    let tok = token_try!(self);
                    match tok {
                        Token::Rbrack => return ParserResult::Expr(list_val.into()),
                        Token::Eof => return ParserResult::EofErr,
                        Token::HashComment => {
                            let tok = token_try!(self);
                            match self.parse_expr(tok) {
                                ParserResult::Expr(expr) => expr,
                                err => return err
                            };
                        },
                        tk => list_val.push(match self.parse_expr(tk) {
                            ParserResult::Expr(expr) => expr,
                            err => return err
                        })
                    }
                }
            },
            Token::Quote => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                //Ok(list![Value::Sym("quote".to_string()), val])
                ParserResult::Expr(vater!{ (quote [val]) })
            },
            Token::Backquote => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                // Ok(list![Value::Sym("quasiquote".to_string()), val])
                ParserResult::Expr(vater!{ (quasiquote [val]) })
            },
            Token::Comma => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                // Ok(list![Value::Sym("unquote".to_string()), val])
                ParserResult::Expr(vater!{ (unquote [val]) })
            }
            Token::CommaAt => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                // Ok(list![Value::Sym("unquote-splicing".to_string()), val])
                ParserResult::Expr(vater!{ ((sym "unquote-splicing") [val]) })
            },
            Token::Tilde => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                // Ok(list![Value::Sym("deref".to_string()), val])
                ParserResult::Expr(vater!{ (deref [val]) })
            },
            Token::Macro(s) => {
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                // Ok(list![Value::Sym(s), val])
                ParserResult::Expr(vater!{ ((sym s) [val]) })
            },
            Token::HashComment => {
                let tok = token_try!(self);
                match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                let tok = token_try!(self);
                let val = match self.parse_expr(tok) {
                    ParserResult::Expr(expr) => expr,
                    err => return err
                };
                ParserResult::Expr(val)
            },
            Token::True => ParserResult::Expr(Value::True),
            Token::False => ParserResult::Expr(Value::False),
            Token::Number(n) => ParserResult::Expr(Value::Num(n)),
            Token::String(s) => ParserResult::Expr(Value::Str(s)),
            Token::Symbol(s) => ParserResult::Expr(Value::Sym(s)),
            Token::Keyword(s) => ParserResult::Expr(Value::Keyword(s)),
            _ => ParserResult::TokenErr("Invalid Syntax: Unexpected Token".to_string())
        }
    }

    pub fn parse_form(&mut self) -> (bool, Result<Value, String>) {
        let tok = match self.next_token() {
            Ok(Token::Eof) => return (false, Ok(Value::Nil)),
            Ok(tok) => tok,
            Err(err) => return (false, Err(format!("Error: SyntaxError:{}: {}", err.line, err.err)))
        };
        match self.parse_expr(tok) {
            ParserResult::Expr(val) => (true, Ok(val)),
            ParserResult::TokenErr(err) => (false, Err(format!("Error: {}", err))),
            ParserResult::EofErr => (true, Err(format!("Error: Unexpected EOF")))
        }
    }
}
