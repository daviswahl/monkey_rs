use ast;
use ast::AstNode;
use ast::visitor::Visitor;
use object;
use object::Object;
use parser;
use lexer;
use environment::Environment;
use std::rc::Rc;
use std::rc;

struct Evaluator {}

type ObjectResult<'a> = Result<Rc<object::Object<'a>>, String>;

fn eval_bang_prefix_op_exp<'a>(obj: &Object) -> ObjectResult<'a> {
    let result = match *obj {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    };
    Ok(Rc::new(result))
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
                x => return Err(format!("fell through to {:?}", x)),
            }
        }
        (&Object::Boolean(l), &Object::Boolean(r)) => {
            match op {
                "==" => Object::Boolean(l == r),
                "!=" => Object::Boolean(l != r),
                x => return Err(format!("unknown operator: BOOLEAN {} BOOLEAN", x)),
            }
        }
        (l, r) => return Err(format!("type mismatch: {} {} {}", l, op, r)),
    };
    Ok(Rc::new(result))
}

fn eval_minus_prefix_op_exp<'a>(obj: &Object) -> ObjectResult<'a> {
    let result = match *obj {
        Object::Integer(int) => Object::Integer(-int),
        ref t => return Err(format!("unknown operator: -{}", t)),
    };
    Ok(Rc::new(result))
}

fn is_truthy(obj: &Object) -> bool {
    match obj {
        &Object::Boolean(true) => true,
        _ => false,
    }
}

impl<'a> Evaluator {
    fn visit_expr(&self, expr: &ast::Expression, env: &mut Environment<'a>) -> ObjectResult<'a> {
        use ast::Expression::*;
        match *expr {
            Integer(ref int) => Ok(Rc::new(Object::Integer(int.value))),
            Boolean(ref b) => Ok(Rc::new(Object::Boolean(b.value))),

            Prefix(ref prefix) => {
                let right = self.visit_expr(&*prefix.right, env)?;
                self.visit_prefix_expression(prefix.operator.as_str(), &*right, env)
            }

            Infix(ref infix) => {
                let left = self.visit_expr(&*infix.left, env)?;
                let right = self.visit_expr(&*infix.right, env)?;
                eval_infix_expression(infix.operator.as_str(), &left, &right)
            }

            If(ref ifexp) => self.visit_cond_expression(ifexp, env),

            Identifier(ref ident) => self.visit_identifier(ident, env),

            Function(ref exp) => self.visit_function_expression(exp, env),
            ref expr => Err(format!("Unimplemented: {}", expr)),
        }
    }

    fn visit_function_expression(
        &self,
        expr: &ast::FunctionLiteral,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        let mut identifiers: Vec<ast::IdentifierExpression> = vec![];
        for param in expr.parameters.clone().into_iter() {
            match *param {
                ast::Expression::Identifier(exp) => identifiers.push(exp),
                x => return Err(format!("Expected identifier exp, got {}", x)),
            }
        }

        let function = Object::Function(identifiers, expr.body.clone(), Rc::new(env.clone()));
        Ok(Rc::new(function))
    }

    fn visit_identifier(
        &self,
        expr: &ast::IdentifierExpression,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        env.get(expr.value.clone()).ok_or(format!(
            "unknown identifier: {}",
            expr.value.clone()
        ))
    }

    fn visit_cond_expression(
        &self,
        ifexp: &ast::IfExpression,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        let cond = self.visit_expr(&*ifexp.condition, env)?;
        if is_truthy(&cond) {
            self.visit_statement(&*ifexp.consequence, env)
        } else if let Some(ref alt) = ifexp.alternative {
            self.visit_statement(&**alt, env)
        } else {
            Ok(Rc::new(Object::Null))
        }
    }

    fn visit_prefix_expression(
        &self,
        op: &str,
        right: &Object,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        match op {
            "!" => eval_bang_prefix_op_exp(right),
            "-" => eval_minus_prefix_op_exp(right),
            _ => Err(String::from("Unimplemented")),
        }
    }

    fn visit_program(&self, n: &ast::Program, env: &mut Environment<'a>) -> ObjectResult<'a> {
        let result = self.visit_statements(&n.statements, env)?;
        if let Object::Return(ref ret) = *result {
            Ok(ret.clone())
        } else {
            Ok(result.clone())
        }
    }

    fn visit_statements(
        &self,
        stmts: &Vec<ast::Node>,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        let mut result = Rc::new(Object::Null);
        for stmt in stmts.iter() {
            if let &ast::Node::Statement(ref s) = stmt {
                result = self.visit_statement(s, env)?;
                if let Object::Return(_) = *result {
                    return Ok(result);
                }
            }
        }

        Ok(result)
    }

    fn visit_let_statement(
        &self,
        stmt: &ast::LetStatement,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        let ident = stmt.name.value.to_owned();
        let value = self.visit_expr(&*stmt.value, env)?;
        env.set(ident, value);
        Ok(Rc::new(Object::Null))
    }

    fn visit_block_statement(
        &self,
        block: &ast::BlockStatement,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        self.visit_statements(&block.statements, env)
    }

    fn visit_statement(
        &self,
        stmt: &ast::Statement,
        env: &mut Environment<'a>,
    ) -> ObjectResult<'a> {
        use ast::Statement::*;
        match *stmt {

            Expression(ref expr) => self.visit_expr(&expr.value, env),

            Block(ref block) => self.visit_block_statement(block, env),

            Return(ref ret) => {
                let result = self.visit_expr(&ret.value, env)?;
                Ok(Rc::new(Object::Return(result.clone())))
            }

            Let(ref stmt) => self.visit_let_statement(stmt, env),
        }
    }
}

pub fn eval<'a>(node: &ast::Node, env: &'a mut Environment<'a>) -> ObjectResult<'a> {
    use ast::Node::*;
    let mut visitor = Evaluator {};
    match *node {
        Program(ref program) => visitor.visit_program(program, env),
        _ => Err(String::from("Expected Program")),
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5),
            ("let identity = fn(x) { return x; }; identity(5);", 5),
            ("let double = fn(x) { x * 2; }; double(5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20),
            ("fn(x) { x; }(5)", 5),
        ];

        for t in tests {
            let mut env = Environment::new();
            let evaluated = assert_eval(s, &mut env);
            assert_integer_obj(evaluated, t.1)
        }

    }
    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let mut env = Environment::new();
        let evaluated = assert_eval(input, &mut env);
        let func = assert_function_obj(evaluated.as_ref(), vec!["x"], "(x + 2)");
    }

    #[test]
    fn test_let() {
        let tests = vec![
            ("let a = 5; a;", 5),
            ("let a = 5 * 5; a;", 25),
            ("let a = 5; let b = a; b;", 5),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15),
        ];

        for t in tests {
            let mut env = Environment::new();
            let evaluated = assert_eval(t.0, &mut env);
            assert_integer_obj(evaluated.as_ref(), t.1);
        }
    }

    #[test]
    fn test_errors() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: BOOLEAN + BOOLEAN"
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; }; return 1; }",
                "unknown operator: BOOLEAN + BOOLEAN"
            ),
        ];

        for t in tests {
            let mut env = Environment::new();
            let node = parser::parse(t.0);
            let evaluated = eval(&node, &mut env);
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
            let mut env = Environment::new();
            let evaluated = assert_eval(t.0, &mut env);
            assert_integer_obj(evaluated.as_ref(), t.1);
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
            let mut env = Environment::new();
            let evaluated = assert_eval(t.0, &mut env);
            assert_object(evaluated.as_ref(), &t.1);
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
            let mut env = Environment::new();
            let evaluated = assert_eval(test.0, &mut env);
            assert_integer_obj(evaluated.as_ref(), test.1);
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
            let mut env = Environment::new();
            let evaluated = assert_eval(t.0, &mut env);
            assert_bool(evaluated.as_ref(), t.1);
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
            let mut env = Environment::new();
            let evaluated = assert_eval(t.0, &mut env);
            assert_bool(evaluated.as_ref(), t.1)
        }
    }

    fn assert_function_obj(obj: &Object, expect_params: Vec<&str>, expect_body: &str) {
        match obj {
            &Object::Function(ref params, ref body, _) => {
                for (i, param) in params.iter().enumerate() {
                    assert_eq!(param.value.as_str(), expect_params[i]);
                    assert_eq!(format!("{}", body), expect_body)
                }
            }
            x => assert!(false, "Expected function object, got {:?}", x),
        }
    }

    fn assert_object(obj: &Object, expect: &Object) {
        match (obj, expect) {
            (&Object::Boolean(l), &Object::Boolean(r)) => assert_eq!(l, r),
            (&Object::Integer(l), &Object::Integer(r)) => assert_eq!(l, r),
            (&Object::Null, &Object::Null) => assert!(true),
            _ => assert!(false),
        }
    }

    fn assert_error<'a>(result: ObjectResult<'a>, err: &str) {
        match result {
            Ok(error) => assert!(false, "Expected error {}, got: {:?}", err, error),
            Err(error) => assert_eq!(err, error),
        }
    }

    fn assert_bool(obj: &object::Object, b: bool) {
        match obj {
            &object::Object::Boolean(o) => assert_eq!(o, b),
            x => assert!(false, format!("Expected boolean object, got {:?}", x)),
        }
    }

    fn assert_eval<'a>(s: &str, env: &'a mut Environment<'a>) -> Rc<object::Object<'a>> {
        let node = parser::parse(s);
        match eval(&node, env) {
            Ok(e) => e.clone(),
            Err(e) => {
                assert!(false, format!("Expected program, got {:?}", e));
                Rc::new(object::Object::Null)
            }
        }
    }

    fn assert_integer_obj(obj: &object::Object, expected: i64) {
        match obj {
            &object::Object::Integer(integer) => assert_eq!(integer, expected),
            x => assert!(false, format!("Expected integer object, got {:?}", x)),
        }
    }
}
