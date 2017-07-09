use ast;
use ast::AstNode;
use ast::visitor::Visitor;
use object;
use object::Object;
use parser;
use lexer;
use std::collections::HashMap;


struct Environment<'a, 'b> {
    outer: Option<&'a Environment<'a, 'a>>,
    env: HashMap<&'b str, object::Object>,
}
struct Evaluator {}

type ObjectResult<'a> = Result<object::Object, &'a str>;

fn eval_bang_prefix_op_exp<'a>(obj: &Object) -> ObjectResult<'a> {
    let result = match *obj {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    };
    Ok(result)
}

fn eval_infix_expression<'a>(op: &str, left: &Object, right: &Object) -> ObjectResult<'a> {
    let result = match (left, right) {
        (&Object::Integer(l), &Object::Integer(r)) => {
            match op {
                "/" => Object::Integer(l / r),
                "*" => Object::Integer(l * r),
                "-" => Object::Integer(l - r),
                "+" => Object::Integer(l + r),

                ">" => Object::Boolean(l > r),
                "<" => Object::Boolean(l < r),
                "==" => Object::Boolean(l == r),
                "!=" => Object::Boolean(l != r),
                _ => Object::Null,
            }
        }
        (&Object::Boolean(l), &Object::Boolean(r)) => {
            match op {
                "==" => Object::Boolean(l == r),
                "!=" => Object::Boolean(l != r),
                _ => Object::Null,
            }
        }
        _ => Object::Null,
    };

    Ok(result)
}

fn eval_minus_prefix_op_exp<'a>(obj: &Object) -> ObjectResult<'a> {
    let result = match *obj {
        Object::Integer(int) => Object::Integer(-int),
        _ => Object::Null,
    };
    Ok(result)
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        &Object::Boolean(true) => true,
        _ => false,
    }
}

impl<'a> Evaluator {
    fn visit_expr(&mut self, expr: &ast::Expression, env: &Environment) -> ObjectResult<'a> {
        use ast::Expression::*;
        match *expr {
            Integer(ref int) => Ok(Object::Integer(int.value)),
            Boolean(ref b) => Ok(Object::Boolean(b.value)),

            Prefix(ref prefix) => {
                let right = self.visit_expr(&*prefix.right, env)?;
                self.visit_prefix_expression(prefix.operator.as_str(), &right, env)
            }

            Infix(ref infix) => {
                let left = self.visit_expr(&*infix.left, env)?;
                let right = self.visit_expr(&*infix.right, env)?;
                eval_infix_expression(infix.operator.as_str(), &left, &right)
            }

            If(ref ifexp) => self.visit_cond_expression(ifexp, env),

            _ => Err("unimpl"),
        }
    }

    fn visit_cond_expression(
        &mut self,
        ifexp: &ast::IfExpression,
        env: &Environment,
    ) -> ObjectResult<'a> {
        let cond = self.visit_expr(&*ifexp.condition, env)?;

        if is_truthy(&cond) {
            self.visit_statement(&*ifexp.consequence, env)
        } else if let Some(ref alt) = ifexp.alternative {
            self.visit_statement(&**alt, env)
        } else {
            Ok(Object::Null)
        }
    }

    fn visit_prefix_expression(
        &mut self,
        op: &str,
        right: &Object,
        env: &Environment,
    ) -> ObjectResult<'a> {
        match op {
            "!" => eval_bang_prefix_op_exp(right),
            "-" => eval_minus_prefix_op_exp(right),
            _ => Err("Unimplemented"),
        }
    }

    fn visit_program(&mut self, n: &ast::Program) -> ObjectResult<'a> {
        let env = Environment {
            outer: None,
            env: HashMap::new(),
        };
        let mut result = Object::Null;
        for stmt in n.statements.iter() {
            if let &ast::Node::Statement(ref s) = stmt {
                result = self.visit_statement(s, &env)?;
            }
        }

        Ok(result)
    }


    fn visit_block_statement(
        &mut self,
        block: &ast::BlockStatement,
        env: &Environment,
    ) -> ObjectResult<'a> {
        let mut result = Object::Null;
        for stmt in block.statements.iter() {
            if let &ast::Node::Statement(ref s) = stmt {
                result = self.visit_statement(&s, &env)?;
            }
        }
        Ok(result)
    }

    fn visit_statement(&mut self, stmt: &ast::Statement, env: &Environment) -> ObjectResult<'a> {
        use ast::Statement::*;
        match *stmt {
            Expression(ref expr) => self.visit_expr(&expr.value, env),
            Block(ref block) => self.visit_block_statement(block, env),

            ref x => {
                println!("{:?} is unimplemented", x);
                Err("unimplemented")
            }
        }
    }
}

pub fn eval<'a, 'b>(node: &'a ast::Node) -> ObjectResult<'b> {
    use ast::Node::*;
    let mut visitor = Evaluator {};
    match *node {
        Program(ref program) => visitor.visit_program(program),
        _ => Err("Expected Program"),
    }
}
#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_if_else_exp() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1 == 1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];

        for t in tests {
            let evaluated = assert_eval(t.0);
            assert_object(evaluated, t.1);
        }
    }

    #[test]
    fn test_eval_int() {
        let tests = vec![
            ("5", 5),
            ("10", 10),
            ("-5", -5),
            ("-10", -10),
            ("5 + 5 + 5 + 5 - 10", 10),
            ("2 * 2 * 2 * 2 * 2", 32),
            ("-50 + 100 + -50", 0),
            ("5 * 2 + 10", 20),
            ("50 / 2 * 2 + 10", 60),
            ("2 * (5 + 10)", 30),
            ("3 * 3 * 3 + 10", 37),
            ("3 * (3 * 3) + 10", 37),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50),
        ];

        for test in tests {
            let evaluated = assert_eval(test.0);
            assert_integer_obj(evaluated, test.1);
        }
    }

    #[test]
    fn test_eval_bool() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),

            ("true == true", true),
            ("false == false", true),
            ("true != false", true),
            ("true == false", false),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for t in tests {
            let evaluated = assert_eval(t.0);
            assert_bool(evaluated, t.1);
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];
        for t in tests {
            let evaluated = assert_eval(t.0);
            assert_bool(evaluated, t.1)
        }
    }

    fn assert_object(obj: Object, expect: Object) {
        match (obj, expect) {
            (Object::Boolean(l), Object::Boolean(r)) => assert_eq!(l, r),
            (Object::Integer(l), Object::Integer(r)) => assert_eq!(l, r),
            (Object::Null, Object::Null) => assert!(true),
            _ => assert!(false),
        }
    }

    fn assert_bool(obj: object::Object, b: bool) {
        match obj {
            object::Object::Boolean(o) => assert_eq!(o, b),
            x => assert!(false, format!("Expected boolean object, got {:?}", x)),
        }
    }

    fn assert_eval(s: &str) -> object::Object {
        match eval(&parser::parse(s)) {
            Ok(e) => e,
            Err(e) => {
                assert!(false, format!("Expected program, got {:?}", e));
                object::Object::Null
            }
        }
    }

    fn assert_integer_obj(obj: object::Object, expected: i64) {
        match obj {
            object::Object::Integer(integer) => assert_eq!(integer, expected),
            x => assert!(false, format!("Expected integer object, got {:?}", x)),
        }
    }
}
