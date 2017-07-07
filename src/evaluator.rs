use ast;
use ast::AstNode;
use object;
use parser;
use lexer;

fn eval<'a, 'b>(node: &'a ast::Node) -> Result<object::Object, &'b str> {
    match node {
        &ast::Node::Program(ref prog) => eval_statements(&prog.statements),
        _ => Err("fuck"),
    }
}

fn eval_statements<'a, 'b>(stmts: &'a Vec<ast::Node>) -> Result<object::Object, &'b str> {
    if stmts.len() == 0 { return Err("woops") }
    match &stmts[0] {
        &ast::Node::Statement(ref s) => eval_statement(s),
        _ => return Err("Expected ast::Node::Statement"),
    }
}

fn eval_statement<'a, 'b>(stmt: &'a ast::Statement) -> Result<object::Object, &'b str> {
    match stmt {
        &ast::Statement::Expression(ref exp) => eval_expression(&exp.value),
        &ast::Statement::Let(ref stmt) => Err("unimplemented"),
        &ast::Statement::Return(ref stmt) => Err("unimplemented"),
        _ => Err("unimplemented"),
    }
}

fn eval_expression<'a, 'b>(exp: &'a ast::Expression) -> Result<object::Object, &'b str> {
    use ast::Expression::*;
    match exp {
        &Integer(ref i) => Ok(object::Object::Integer(object::Integer{value: i.value})),
        _ => Err("woops"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_eval_int() {
        let tests = vec![("5", 5), ("10", 10)];

        for test in tests {
            let evaluated = assert_eval(test.0);
            assert_integer_obj(evaluated, test.1);
        }
    }


    fn assert_eval(s: &str) -> object::Object {
        eval(&parser::parse(s)).unwrap()
    }

    fn assert_integer_obj(obj: object::Object, expected: i64) {
        match obj {
            object::Object::Integer(integer) => assert_eq!(integer.value, expected),
            x => assert!(false, format!("Expected integer object, got {:?}", x)),
        }
    }
}
