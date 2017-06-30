use token;
pub trait Node {
    fn token_literal(&self) -> String;
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match *self {
            Statement::Let(ref stmt) => stmt.token_literal(),
            Statement::Return(ref stmt) => stmt.token_literal(),
        }
    }
}

pub enum Expression {
    Unit(UnitExpression),
}

pub struct Program {
    pub statements: Vec<Statement>,
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


impl Node for Identifier {
    fn token_literal(&self) ->  String {
        self.token.literal.clone()
    }
}

impl Identifier {
    pub fn new(t: token::Token) -> Identifier {
        let lit = t.literal.clone();
        Identifier{token: t, value: lit}
    }
}

pub struct LetStatement {
    pub token: token::Token,
    pub name: Identifier,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}


pub struct ReturnStatement {
    pub token: token::Token,
    pub expression: Expression,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}


pub struct UnitExpression ();
impl Node for UnitExpression {
    fn token_literal(&self) -> String {
        String::from("()")
    }
}

#[cfg(tests)]
mod tests {
    use super::*;
}
