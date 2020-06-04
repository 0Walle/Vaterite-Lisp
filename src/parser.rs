use crate::types::{Value, ValueList, ValueErr};
use std::iter::Peekable;
use std::vec::IntoIter;
use std::rc::Rc;

#[macro_export]
macro_rules! list {
    ( $arg:expr ) => {
        Value::List(Rc::new($arg))
    };
    [ $($args:expr),* ] => {{
        let vec: Vec<Value> = vec![$($args),*];
        Value::List(Rc::new(vec))
    }}
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
    Number(f64),
    Symbol(String),
    Keyword(String),
    String(String),
}

/// The tokenizer reads an input and produces expressions
pub struct Reader {
    chars: Peekable<IntoIter<char>>,
    current_line: i32,
}

// TODO
// struct ReaderError {
//     err: String,
//     line: i32,
// }

impl Reader {
    /// Create a new Tokenizer from a source string
    pub fn new(source: String) -> Self {
        Reader{
            chars: source.chars().collect::<Vec<char>>().into_iter().peekable(),
            current_line: 1,
        }
    }

    /// Read the next Token in the stream
    pub fn next_token(&mut self) -> Result<Token, &str> {
        loop{
            let ch = self.chars.next();
            match ch {
                Some(chr) => match chr {
                    ' ' | '\t' | '\r' => continue,
                    '\n' => self.current_line += 1,
                    ';' => loop {
                        match self.chars.next() {
                            Some('\n') => break,
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
                        None => return Err("Unexpected end of input reading unquote"),
                    }
                    '`' => return Ok(Token::Backquote),
                    '~' => return Ok(Token::Tilde),
                    '-' => match self.chars.peek() {
                        Some(c) => match *c {
                            '0' ..= '9' => match self.read_number(chr) {
                                Ok(num) => return Ok(Token::Number(num)),
                                Err(err) => return Err(err)
                            }
                            _ => match self.read_symbol('-') {
                                Ok(sym) => return Ok(Token::Symbol(sym)),
                                Err(err) => return Err(err)
                            }
                        }
                        None => return Ok(Token::Symbol("-".to_string())),
                    },
                    '0' ..= '9' => {
                        match self.read_number(chr) {
                            Ok(num) => return Ok(Token::Number(num)),
                            Err(err) => return Err(err)
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
                                            '\n' => (),
                                            _ => res.push(c),
                                        }
                                        None => return Err("Unexpected end of input reading string"),
                                    }
                                    '\n' => return Err("Unexpected line break reading string"),
                                    c => res.push(c)
                                }
                                None => return Err("Unexpected end of input reading string"),
                            }
                        }
                        return Ok(Token::String(res));
                    }
                    ':' => {
                        let chr = match self.chars.next() {
                            Some(c) => c,
                            None => return Err("Unexpected end of input reading keyword")
                        };
                        match self.read_symbol(chr) {
                            Ok(sym) => return Ok(Token::Keyword(sym)),
                            Err(err) => return Err(err)
                        }
                    }
                    c => {
                        match self.read_symbol(c) {
                            Ok(sym) => return Ok(Token::Symbol(sym)),
                            Err(err) => return Err(err)
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
    pub fn parse_expr(&mut self, look: Token) -> ValueErr {
        match look {
            Token::Lparen => {
                let mut list_val: ValueList = vec![];
                loop{
                    let tok = match self.next_token() {
                        Ok(tok) => tok,
                        Err(err) => return Err(format!("Invalid Syntax: {}", err))
                    };
                    match tok {
                        Token::Rparen => return Ok(Value::List(Rc::new(list_val))),
                        Token::Eof => return Err("Invalid Syntax: Unexpected end of input reading list".to_string()),
                        tk => list_val.push(self.parse_expr(tk)?)
                    }
                }
            },
            Token::Lbrack => {
                let mut list_val: ValueList = vec![Value::Sym("list".to_string())];
                loop{
                    let tok = match self.next_token() {
                        Ok(tok) => tok,
                        Err(err) => return Err(format!("Invalid Syntax: {}", err))
                    };
                    match tok {
                        Token::Rbrack => return Ok(Value::List(Rc::new(list_val))),
                        Token::Eof => return Err("Invalid Syntax: Unexpected end of input reading list".to_string()),
                        tk => list_val.push(self.parse_expr(tk)?)
                    }
                }
            },
            Token::Quote => {
                let tok = match self.next_token() {
                    Ok(tok) => tok,
                    Err(err) => return Err(format!("Invalid Syntax: {}", err))
                };
                let val = self.parse_expr(tok)?;
                Ok(list![Value::Sym("quote".to_string()), val])
            },
            Token::Backquote => {
                let tok = match self.next_token() {
                    Ok(tok) => tok,
                    Err(err) => return Err(format!("Invalid Syntax: {}", err))
                };
                let val = self.parse_expr(tok)?;
                Ok(list![Value::Sym("quasiquote".to_string()), val])
            },
            Token::Comma => {
                let tok = match self.next_token() {
                    Ok(tok) => tok,
                    Err(err) => return Err(format!("Invalid Syntax: {}", err))
                };
                let val = self.parse_expr(tok)?;
                Ok(list![Value::Sym("unquote".to_string()), val])
            }
            Token::CommaAt => {
                let tok = match self.next_token() {
                    Ok(tok) => tok,
                    Err(err) => return Err(format!("Invalid Syntax: {}", err))
                };
                let val = self.parse_expr(tok)?;
                Ok(list![Value::Sym("unquote-splicing".to_string()), val])
            },
            Token::Tilde => {
                let tok = match self.next_token() {
                    Ok(tok) => tok,
                    Err(err) => return Err(format!("Invalid Syntax: {}", err))
                };
                let val = self.parse_expr(tok)?;
                Ok(list![Value::Sym("deref".to_string()), val])
            },
            Token::Number(n) => Ok(Value::Num(n)),
            Token::String(s) => Ok(Value::Str(s)),
            Token::Symbol(s) => Ok(Value::Sym(s)),
            Token::Keyword(s) => Ok(Value::Keyword(s)),
            _ => Err("Invalid Syntax: Unexpected Token".to_string())
        }
    }
}