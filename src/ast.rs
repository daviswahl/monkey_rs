use token;
use std::fmt;

pub trait Node : fmt::Display {
    fn token_literal(&self) -> String;
}

pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match *self {
            Statement::Let(ref stmt) => stmt.token_literal(),
            Statement::Return(ref stmt) => stmt.token_literal(),
            Statement::Expression(ref stmt) => stmt.token_literal(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Let(ref stmt) => stmt.fmt(f),
            Statement::Return(ref stmt) => stmt.fmt(f),
            Statement::Expression(ref stmt) => stmt.fmt(f),
        }
    }
}

// Expression
#[derive(Debug, Clone)]
pub enum Expression {
    Unit(UnitExpression),
    Identifier(IdentifierExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match *self {
            Expression::Unit(ref stmt) => stmt.token_literal(),
            Expression::Identifier(ref stmt) => stmt.token_literal(),
            Expression::Integer(ref stmt) => stmt.token_literal(),
            Expression::Prefix(ref exp) => exp.token_literal(),
            Expression::Infix(ref exp) => exp.token_literal(),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Expression::Unit(ref stmt) => stmt.fmt(f),
            Expression::Identifier(ref stmt) => stmt.fmt(f),
            Expression::Integer(ref stmt) => stmt.fmt(f),
            Expression::Prefix(ref exp) => exp.fmt(f),
            Expression::Infix(ref exp) => exp.fmt(f),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() >= 1 {
            return self.statements[0].token_literal()
        }
        String::from("")
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt);
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct IdentifierExpression {
    pub token: token::Token,
    pub value: String,
}


impl Node for IdentifierExpression {
    fn token_literal(&self) ->  String {
        self.token.literal.clone()
    }
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl IdentifierExpression {
    pub fn new(t: token::Token) -> IdentifierExpression {
        let lit = t.literal.clone();
        IdentifierExpression{token: t, value: lit}
    }
}

// Let Statement
pub struct LetStatement {
    pub token: token::Token,
    pub name: IdentifierExpression,
    pub value: Expression,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} {} = {};", self.token_literal(), self.name, self.value)
    }
}

// Return Statement
pub struct ReturnStatement {
    pub token: token::Token,
    pub value: Expression,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: token::Token,
    pub value: Expression,
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone)]
pub struct UnitExpression ();

impl Node for UnitExpression {
    fn token_literal(&self) -> String {
        String::from("()")
    }
}

impl fmt::Display for UnitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String{
        self.value.to_string()
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: token::Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl Node for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[cfg(tests)]
mod tests {
    use super::*;
}
