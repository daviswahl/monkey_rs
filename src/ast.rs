use token;
pub trait Node {
    fn token_literal(&self) -> String;
}

pub trait Statement : Node {}
pub trait Expression : Node {}

pub struct Program {
    pub statements: Vec<Box<Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() > 1 {
            return self.statements[0].token_literal()
        }
        String::from("")
    }
}

pub struct Identifier {
    pub token: token::Token,
    pub value: String,
}
impl Expression for Identifier {}

impl Node for Identifier {
    fn token_literal(&self) ->  String {
        self.token.literal.clone()
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Box<Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {}


pub struct Unit {}
impl Node for Unit {
    fn token_literal(&self) -> String {
        String::from("()")
    }
}
impl Expression for Unit {}

#[cfg(tests)]
mod tests {
    use super::*;
}
