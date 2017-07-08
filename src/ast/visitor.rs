use ast::*;
pub trait Visitor<T>{
    fn visit_statement(&mut self, n: &Node) -> T;
    fn visit_expr(&mut self, n: &Node) -> T;
    fn visit_program(&mut self, n: &Program) -> T;
}