use token;
use std::fmt;

#[derive(Debug, PartialEq, Clone)]
pub enum Node {
    Statement(Statement),
    Expression(Expression),
    Program(Program),
}

impl HasToken for Node {
    fn token_literal(&self) -> String {
        match *self {
            Node::Statement(ref node) => node.token_literal(),
            Node::Expression(ref node) => node.token_literal(),
            Node::Program(ref node) => node.token_literal(),
        }
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Node::Program(ref node) => node.fmt(f),
            Node::Expression(ref node) => node.fmt(f),
            Node::Statement(ref node) => node.fmt(f),
        }
    }
}
pub trait HasToken: fmt::Display {
    fn token_literal(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl HasToken for Statement {
    fn token_literal(&self) -> String {
        match *self {
            Statement::Let(ref stmt) => stmt.token_literal(),
            Statement::Return(ref stmt) => stmt.token_literal(),
            Statement::Expression(ref stmt) => stmt.token_literal(),
            Statement::Block(ref stmt) => stmt.token_literal(),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Let(ref stmt) => stmt.fmt(f),
            Statement::Return(ref stmt) => stmt.fmt(f),
            Statement::Expression(ref stmt) => stmt.fmt(f),
            Statement::Block(ref stmt) => stmt.fmt(f),
        }
    }
}

// Expression
#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Unit(UnitExpression),
    Identifier(IdentifierExpression),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(BooleanExpression),
    If(IfExpression),
    Function(FunctionLiteral),
    Call(CallExpression),
    String(StringLiteral),
    Builtin(token::Token),
    Array(ArrayLiteral)
}

impl HasToken for Expression {
    fn token_literal(&self) -> String {
        match *self {
            Expression::Unit(ref stmt) => stmt.token_literal(),
            Expression::Identifier(ref stmt) => stmt.token_literal(),
            Expression::Integer(ref stmt) => stmt.token_literal(),
            Expression::Prefix(ref exp) => exp.token_literal(),
            Expression::Infix(ref exp) => exp.token_literal(),
            Expression::Boolean(ref exp) => exp.token_literal(),
            Expression::If(ref exp) => exp.token_literal(),
            Expression::Function(ref exp) => exp.token_literal(),
            Expression::Call(ref exp) => exp.token_literal(),
            Expression::String(ref exp) => exp.token_literal(),
            Expression::Builtin(ref exp) => exp.literal(),
            Expression::Array(ref exp) => exp.token_literal(),
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
            Expression::Boolean(ref exp) => exp.fmt(f),
            Expression::If(ref exp) => exp.fmt(f),
            Expression::Function(ref exp) => exp.fmt(f),
            Expression::Call(ref exp) => exp.fmt(f),
            Expression::String(ref exp) => exp.fmt(f),
            Expression::Builtin(ref exp) => write!(f, "{}", exp.literal()),
            Expression::Array(ref exp) => exp.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Program {
    pub statements: Vec<Node>,
}

impl HasToken for Program {
    fn token_literal(&self) -> String {
        if self.statements.len() >= 1 {
            return self.statements[0].token_literal();
        }
        "".to_string()
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IdentifierExpression {
    pub token: token::Token,
    pub value: String,
}


impl HasToken for IdentifierExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for IdentifierExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl IdentifierExpression {
    pub fn new(t: token::Token) -> IdentifierExpression {
        let lit = t.literal();
        IdentifierExpression {
            token: t,
            value: lit,
        }
    }
}

// Let Statement
#[derive(Debug, PartialEq, Clone)]
pub struct LetStatement {
    pub token: token::Token,
    pub name: IdentifierExpression,
    pub value: Box<Expression>,
}

impl HasToken for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token_literal(),
            self.name,
            self.value
        )
    }
}

// Return Statement
#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub token: token::Token,
    pub value: Box<Expression>,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl HasToken for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: token::Token,
    pub value: Expression,
}

impl HasToken for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct UnitExpression();

impl HasToken for UnitExpression {
    fn token_literal(&self) -> String {
        "()".to_string()
    }
}

impl fmt::Display for UnitExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: token::Token,
    pub value: i64,
}

impl HasToken for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct PrefixExpression {
    pub token: token::Token,
    pub operator: String,
    pub right: Box<Expression>,
}

impl HasToken for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct InfixExpression {
    pub token: token::Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}

impl HasToken for InfixExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct StringLiteral {
    pub token: token::Token,
}

impl HasToken for StringLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for StringLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct BooleanExpression {
    pub token: token::Token,
    pub value: bool,
}

impl HasToken for BooleanExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for BooleanExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct IfExpression {
    pub token: token::Token,
    pub condition: Box<Expression>,
    pub consequence: Box<Statement>,
    pub alternative: Option<Box<Statement>>,
}

impl HasToken for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if{} {}", self.condition, self.consequence)?;
        self.alternative.as_ref().map(
            |alt| write!(f, "else {}", &alt),
        );

        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BlockStatement {
    pub token: token::Token,
    pub statements: Vec<Node>,
}

impl HasToken for BlockStatement {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}
impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stmt in &self.statements {
            write!(f, "{}", stmt)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinFunction {
    pub token: token::Token,
}

impl HasToken for BuiltinFunction {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token_literal())
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub token: token::Token,
    pub parameters: Vec<Expression>,
    pub body: Box<Statement>,
}

impl HasToken for FunctionLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: String = self.parameters
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(",");
        write!(f, "{}({}) {}", self.token_literal(), params, *self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallExpression {
    pub token: token::Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl HasToken for CallExpression {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let params: String = self.arguments
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "{}({})", self.function, params)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayLiteral {
    pub token: token::Token,
    pub elements: Vec<Expression>,
}

impl HasToken for ArrayLiteral {
    fn token_literal(&self) -> String {
        self.token.literal()
    }
}

impl fmt::Display for ArrayLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let elements: String = self.elements
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(", ");
        write!(f, "[{}]", elements)
    }
}

#[cfg(tests)]
mod tests {
    use super::*;
}
