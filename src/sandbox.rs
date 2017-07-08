struct Node<'a> {
    token: Token<'a>,
    node: &'a Node<'a>
}

#[derive(Clone, Copy)]
struct Token<'a> {
    literal: &'a [u8],
}

struct LexerCache {
    strings: Vec<Vec<u8>>
}

impl LexerCache {
    fn push_string(&mut self, s: Vec<u8>) -> &Self {
        self.strings.push(s);
        self
    }
    fn last_string(& self) -> &Vec<u8> {
        self.strings.last().unwrap()
    }
}

struct Lexer<'b> {
    data: Vec<u8>,
    cache: LexerCache,
    cur_pos: usize,
    cur_token: Option<Token<'b>>
}


impl <'b>Lexer<'b> {
    fn new(mut cache: LexerCache, data: String) -> Lexer<'b> {
        let d = data.into_bytes();
        Lexer{data: d, cache: cache, cur_pos: 0, cur_token: None}
    }

    pub fn next_token(&'b mut self, len: usize) -> Option<Token<'b>> {
        self.cur_pos += 1;
        let data = self.data[self.cur_pos..self.cur_pos+len].to_owned();
        self.cache.push_string(data);
        let s = self.cache.last_string();
        let cur_token = self.cur_token;
        self.cur_token = Some(Token{literal: s});
        cur_token
    }
}

#[cfg(test)]
mod tests{
    use super::*;
    #[test]
    fn test_lexer() {
        let mut cache = LexerCache{strings: vec![]};
        let mut l = Lexer::new(cache, String::from("asdflkjsdfasdf"));
        let mut tokens: Vec<Token> = vec![];
        l.next_token(1).map(|t| tokens.push(t));



    }

}
