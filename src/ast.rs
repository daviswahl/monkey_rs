use token;
trait Node {
    fn token_literal(&self) -> String;
}

trait Statement : Node {}
trait Expression : Node {}

pub struct Program<'a> {
    pub statements: Vec<&'a Statement>,
}

impl <'a>Node for Program<'a> {
    fn token_literal(&self) -> String {
        if self.statements.len() > 1 {
            return self.statements[0].token_literal()
        }
        String::from("")
    }
}

struct Identifier {
    token: token::Token,
    value: String,
}
impl Expression for Identifier {}

impl Node for Identifier {
    fn token_literal(&self) ->  String {
        self.token.literal.clone()
    }
}

struct LetStatement {
    token: token::Token,
    name: Identifier,
    value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {}

#[cfg(tests)]
mod tests {
    use super::*;
}
