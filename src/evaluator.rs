use ast;
use ast::AstNode;
use ast::visitor::Visitor;
use object;
use object::Object;
use parser;
use lexer;
use environment::Environment;


struct Evaluator {}

type ObjectResult<'a> = Result<object::Object<'a>, String>;

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
                x => return Err(format!("unknown operator: BOOLEAN {} BOOLEAN", x))
            }
        }
        (l, r) => return Err(format!("type mismatch: {} {} {}", l, op, r))
    };

    Ok(result)
}

fn eval_minus_prefix_op_exp<'a>(obj: &Object) -> ObjectResult<'a> {
    let result = match *obj {
        Object::Integer(int) => Object::Integer(-int),
        ref t => return Err(format!("unknown operator: -{}", t))
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
    fn visit_expr(&mut self, expr: &ast::Expression, env: &mut Environment) -> ObjectResult<'a> {
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

            Identifier(ref ident) => self.visit_identifier(ident, env),
            ref expr => Err(format!("Unimplemented: {}", expr)),
        }
    }

    fn visit_identifier(&mut self, expr: &ast::IdentifierExpression, env: &mut Environment) -> ObjectResult<'a> {
        env.get(expr.value.clone()).cloned().ok_or(format!("unknown identifier: {}", expr.value.clone()))
    }

    fn visit_cond_expression(
        &mut self,
        ifexp: &ast::IfExpression,
        env: &mut Environment,
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
        env: &mut Environment,
    ) -> ObjectResult<'a> {
        match op {
            "!" => eval_bang_prefix_op_exp(right),
            "-" => eval_minus_prefix_op_exp(right),
            _ => Err(String::from("Unimplemented")),
        }
    }

    fn visit_program(&mut self, n: &ast::Program) -> ObjectResult<'a> {
        let mut env = Environment::new();

        let result = self.visit_statements(&n.statements, &mut env);
        if let Ok(Object::Return(ret)) = result {
            Ok(*ret)
        } else {
           result
        }
    }

    fn visit_statements(&mut self, stmts: &Vec<ast::Node>, env: &mut Environment) -> ObjectResult<'a> {

        let mut result = Object::Null;
        for (i, stmt) in stmts.iter().enumerate() {
            if let &ast::Node::Statement(ref s) = stmt {
                result = self.visit_statement(s, env)?;
                if let Object::Return(_) = result {
                    return Ok(result)
                }
            }
        }

        Ok(result)
    }

    fn visit_let_statement(&mut self, stmt: &ast::LetStatement, env: &mut Environment) -> ObjectResult<'a> {
        let ident = stmt.name.value.to_owned();
        let value = self.visit_expr(&*stmt.value, env)?;
        env.set(ident, value);
        Ok(Object::Null)
    }

    fn visit_block_statement(
        &mut self,
        block: &ast::BlockStatement,
        env: &mut Environment,
    ) -> ObjectResult<'a> {
        self.visit_statements(&block.statements, env)
    }

    fn visit_statement(&mut self, stmt: &ast::Statement, env: &mut Environment) -> ObjectResult<'a> {
        use ast::Statement::*;
        match *stmt {

            Expression(ref expr) => self.visit_expr(&expr.value, env),

            Block(ref block) => self.visit_block_statement(block, env),

            Return(ref ret) => {
                let result = self.visit_expr(&ret.value, env)?;
                let ret = Object::Return(Box::new(result));
                Ok(ret)
            },

            Let(ref stmt) => {
                self.visit_let_statement(stmt, env)
            },
        }
    }
}

pub fn eval<'a>(node: &'a ast::Node) -> ObjectResult<'a> {
    use ast::Node::*;
    let mut visitor = Evaluator {};
    match *node {
        Program(ref program) => visitor.visit_program(program),
        _ => Err(String::from("Expected Program")),
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for t in tests {
            let evaluated = assert_eval(t.0);
            assert_integer_obj(evaluated, t.1);
        }
    }

    #[test]
    fn test_errors() {
        let tests = vec![
            ( "5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ( "5 + true; 5", "type mismatch: INTEGER + BOOLEAN"),
            ( "-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { if (10 > 1) { return true + false; }; return 1; }", "unknown operator: BOOLEAN + BOOLEAN")
        ];

        for t in tests {
            let evaluated = eval(&parser::parse(t.0));
            assert_error(evaluated, t.1);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", 10),
            ("return 10; 9;", 10),
            ("return 2 * 5; 9;", 10),
            ("9; return 2 * 5; 9;", 10),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10),
        ];

        for t in tests {
            let evaluated = assert_eval(t.0);
            assert_integer_obj(evaluated, t.1);
        }
    }

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

    fn assert_error<'a>(result: ObjectResult<'a>, err: &str) {
        match result {
            Ok(error) => assert!(false, "Expected error {}, got: {:?}", err, error),
            Err(error) => assert_eq!(err, error)
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
