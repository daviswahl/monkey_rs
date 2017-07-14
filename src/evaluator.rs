use ast;
use object::{Object, ObjectRcResult,ObjectsResult};
use environment::Environment;
use std::rc::Rc;
use std::cell::RefCell;
use runtime;

use builtin;

struct Evaluator {
    runtime: runtime::Runtime,
}

type Env = Rc<RefCell<Environment>>;
fn is_truthy(obj: &Object) -> bool {
    match obj {
        &Object::Boolean(true) => true,
        _ => false,
    }
}


fn extend_function_env(
    parameters: &Vec<ast::IdentifierExpression>,
    env: Env,
    args: Vec<Rc<Object>>,
) -> Result<Environment, String> {
    let mut extended = Environment::extend(env);

    if parameters.len() != args.len() {
        return Err(format!(
            "function expected {} arguments, got {}",
            parameters.len(),
            args.len()
        ));
    }

    for (i, param) in parameters.into_iter().enumerate() {
        extended.set(param.value.clone(), args[i].clone())
    }

    Ok(extended)
}

impl Evaluator {
    fn visit_expr(&self, expr: ast::Expression, env: Env) -> ObjectRcResult {
        use ast::Expression::*;
        use ast::HasToken;
        match expr {
            Integer(ref int) => Ok(Rc::new(Object::Integer(int.value))),
            Boolean(b) => Ok(self.runtime.bool(b.value)),
            String(ref s) => Ok(Rc::new(Object::StringLiteral(s.token_literal()))),

            Prefix(prefix) => {
                let op = prefix.operator;
                self.visit_expr(*prefix.right, env).and_then(|right| {
                    self.visit_prefix_expression(op.as_str(), &*right)
                })
            }

            Infix(infix) => {
                let left = infix.left;
                let op = infix.operator;
                let right = self.visit_expr(*infix.right, env.clone())?;

                if op == "=" {
                    self.visit_update_assignment(*left, right, env)
                } else {

                    self.visit_expr(*left, env).and_then(|left| {
                        self.eval_infix_expression(op.as_str(), &left, &right)
                    })
                }
            }

            If(ifexp) => self.visit_cond_expression(ifexp, env),

            Identifier(ident) => self.visit_identifier(ident, env),

            Function(exp) => self.visit_function_expression(exp, env),

            Call(exp) => self.visit_call_expression(exp, env),

            Builtin(builtin) => Ok(Rc::new(Object::BuiltinFunction(builtin.clone()))),

            Array(exp) => self.visit_array_expression(exp, env),

            Index(exp) => self.visit_index_expression(exp, env),
            ref expr => Err(format!("Unimplemented: {:?}", expr)),
        }
    }

    fn visit_update_assignment(
        &self,
        expr: ast::Expression,
        obj: Rc<Object>,
        env: Env,
    ) -> ObjectRcResult {

        match expr {
            ast::Expression::Identifier(exp) => {
                env.as_ref().borrow_mut().update(exp.value, obj).map(|_| {
                    self.runtime.NULL()
                })
            }
            _ => Err(format!("expected identifier expression, got: {}", expr)),
        }
    }

    fn eval_bang_prefix_op_exp(&self, obj: &Object) -> ObjectRcResult {
        let result = match *obj {
            Object::Boolean(true) => self.runtime.bool(false),
            Object::Boolean(false) => self.runtime.bool(true),
            Object::Null => self.runtime.bool(true),
            _ => self.runtime.bool(false),
        };
        Ok(result)
    }

    fn eval_infix_expression(&self, op: &str, left: &Object, right: &Object) -> ObjectRcResult {
        let result = match (left, right) {
            (&Object::Integer(l), &Object::Integer(r)) => {
                match op {
                    "/" => Rc::new(Object::Integer(l / r)),
                    "*" => Rc::new(Object::Integer(l * r)),
                    "-" => Rc::new(Object::Integer(l - r)),
                    "+" => Rc::new(Object::Integer(l + r)),

                    ">" => self.runtime.bool(l > r),
                    "<" => self.runtime.bool(l < r),
                    "==" => self.runtime.bool(l == r),
                    "!=" => self.runtime.bool(l != r),
                    x => return Err(format!("unknown operator: {} {} {}", left, x, right)),
                }
            }
            (&Object::Boolean(l), &Object::Boolean(r)) => {
                match op {
                    "==" => self.runtime.bool(l == r),
                    "!=" => self.runtime.bool(l != r),
                    x => return Err(format!("unknown operator: {} {} {}", left, x, right)),
                }
            }
            (&Object::StringLiteral(ref l), &Object::StringLiteral(ref r)) => {
                match op {
                    "++" => {
                        let mut new = l.clone();
                        new.push_str(r);
                        Rc::new(Object::StringLiteral(new.to_string()))
                    }
                    x => return Err(format!("unknown operator: {} {} {}", left, x, right)),
                }
            }
            (l, r) => return Err(format!("type mismatch: {} {} {}", l, op, r)),
        };
        Ok(result)
    }

    fn eval_minus_prefix_op_exp(&self, obj: &Object) -> ObjectRcResult {
        let result = match *obj {
            Object::Integer(int) => Object::Integer(-int),
            ref t => return Err(format!("unknown operator: -{}", t)),
        };
        Ok(Rc::new(result))
    }

    fn visit_index_expression(&self, exp: ast::IndexExpression, env: Env) -> ObjectRcResult {
        let l = exp.left;
        let i = exp.index;
        self.visit_expr(*l, env.clone()).and_then(move |left| {
            if let Object::ArrayLiteral(ref array) = *left {
                self.visit_expr(*i, env).and_then(|index| {
                    if let Object::Integer(i) = *index {
                        if i <= array.len() as i64 {
                            Ok(array[i as usize].clone())
                        } else {
                            Err(format!(
                                "error, indexed out of range: {}, length: {}",
                                i,
                                array.len()
                            ))
                        }
                    } else {
                        Err(format!("cannot index out of array with type: {}", index))
                    }
                })
            } else {
                return Err(format!("Expected array literal, got: {}", left));
            }
        })
    }

    fn visit_array_expression(&self, exp: ast::ArrayLiteral, env: Env) -> ObjectRcResult {

        self.visit_expressions(exp.elements, env).map(|elements| {
            Rc::new(Object::ArrayLiteral(elements))
        })
    }

    fn apply_function(&self, func: Rc<Object>, env: Env, args: Vec<Rc<Object>>) -> ObjectRcResult {
        match *func {
            Object::Function(ref parameters, ref body, ref func_env) => {
                let env = extend_function_env(parameters, func_env.clone(), args)?;
                self.visit_statement(*body.clone(), Rc::new(RefCell::new(env)))
            }
            Object::BuiltinFunction(ref tok) => builtin::call(tok, env, &self.runtime, args),
            ref x => Err(format!("expected function object, got {}", x)),
        }
    }

    fn visit_expressions(&self, exprs: Vec<ast::Expression>, env: Env) -> ObjectsResult {
        let mut results: Vec<Rc<Object>> = vec![];
        for expr in exprs.into_iter() {
            results.push(self.visit_expr(expr, env.clone())?);
        }
        Ok(results)
    }

    fn visit_call_expression(&self, expr: ast::CallExpression, env: Env) -> ObjectRcResult {
        let function = self.visit_expr(*expr.function, env.clone())?;
        let args = self.visit_expressions(expr.arguments, env.clone())?;
        let block = expr.block.map(|block| self.visit_statement(*block, env.clone()).ok());
        self.apply_function(function, env, args)
    }

    fn visit_function_expression(&self, expr: ast::FunctionLiteral, env: Env) -> ObjectRcResult {
        let mut identifiers: Vec<ast::IdentifierExpression> = vec![];
        for param in expr.parameters.into_iter() {
            match param {
                ast::Expression::Identifier(exp) => identifiers.push(exp),
                x => return Err(format!("Expected identifier, got {}", x)),
            }
        }

        let function = Object::Function(identifiers, expr.body.clone(), env);
        Ok(Rc::new(function))
    }

    fn visit_identifier(&self, expr: ast::IdentifierExpression, env: Env) -> ObjectRcResult {
        env.as_ref().borrow().get(expr.value.clone()).ok_or(
            format!(
                "unknown identifier: {}",
                expr.value
                    .clone()
            ),
        )
    }

    fn visit_cond_expression(&self, ifexp: ast::IfExpression, env: Env) -> ObjectRcResult {
        let cond = self.visit_expr(*ifexp.condition, env.clone())?;
        if is_truthy(&cond) {
            self.visit_statement(*ifexp.consequence, env)
        } else if let Some(alt) = ifexp.alternative {
            self.visit_statement(*alt, env)
        } else {
            Ok(self.runtime.NULL())
        }
    }

    fn visit_prefix_expression(&self, op: &str, right: &Object) -> ObjectRcResult {
        match op {
            "!" => self.eval_bang_prefix_op_exp(right),
            "-" => self.eval_minus_prefix_op_exp(right),
            _ => return Err(format!("unknown operator: {}{}", op, right)),
        }
    }

    fn visit_program(&self, n: ast::Program, env: Env) -> ObjectRcResult {
        let result = self.visit_statements(n.statements, env)?;
        if let Object::Return(ref ret) = *result {
            Ok(ret.clone())
        } else {
            Ok(result.clone())
        }
    }

    fn visit_statements(&self, stmts: Vec<ast::Node>, env: Env) -> ObjectRcResult {
        let mut result = self.runtime.NULL();
        for stmt in stmts.into_iter() {
            if let ast::Node::Statement(s) = stmt {
                result = self.visit_statement(s, env.clone())?;
                if let Object::Return(_) = *result {
                    return Ok(result);
                }
            }
        }

        Ok(result)
    }

    fn visit_let_statement(&self, stmt: ast::LetStatement, env: Env) -> ObjectRcResult {
        let ident = stmt.name.value.to_owned();
        let value = self.visit_expr(*stmt.value, env.clone())?;
        env.as_ref().borrow_mut().set(ident, value);
        Ok(self.runtime.NULL())
    }

    fn visit_block_statement(&self, block: ast::BlockStatement, env: Env) -> ObjectRcResult {
        self.visit_statements(block.statements, env)
    }

    fn visit_statement(&self, stmt: ast::Statement, env: Env) -> ObjectRcResult {
        use ast::Statement::*;
        match stmt {

            Expression(expr) => self.visit_expr(expr.value, env),

            Block(block) => self.visit_block_statement(block, env),

            Return(ret) => {
                let result = self.visit_expr(*ret.value, env)?;
                Ok(Rc::new(Object::Return(result.clone())))
            }

            Let(stmt) => self.visit_let_statement(stmt, env),

            BlockArgument(block) => unimplemented!(),
        }
    }
}

pub fn eval(node: ast::Node, env: Env) -> ObjectRcResult {
    use ast::Node::*;
    let visitor = Evaluator { runtime: runtime::new() };
    match node {
        Program(program) => visitor.visit_program(program, env),
        ref x => Err(format!("expected program, got {}", x)),
    }
}

#[cfg(test)]
mod tests {
    use parser;
    use object;
    use super::*;

    #[test]
    fn big_test() {
        let input = "
let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)));
    }
  };

  iter(arr, initial);
};

let sum = fn(arr) {
  reduce(arr, 0, fn(initial, el) { initial + el });
};

sum([1,2,3,4,5]);
";
        let env = Environment::new();
        assert_integer(assert_eval(input, env).as_ref(), 15);

    }
    #[test]
    fn test_block_arguments() {
        let input= ("let f = fn() {}; f() { |a| print(\"in block\"); };");
        let env = Environment::new();
        assert_eval(input, env);
        assert!(false)
    }
    #[test]
    fn test_update_assignment() {
        let tests = vec![
            ("let i = 3; i = 2; i", Object::Integer(2)),
            ("let i = 3; i = i + 1; i", Object::Integer(4)),
            (
                "let i = 2; fn () { i = i + 1; stats(); }(); i;",
                Object::Integer(3)
            ),
            ("let i = 2; fn () { i + 1; }(); i;", Object::Integer(2)),
            ("let i = 0; let k = 10; let f = fn(){ if (i < k) { i = i + 1; k = k - 1;
f() } }; f(); i;", Object::Integer(5)),
        ];

        for t in tests {
            let env = Environment::new();
            println!("Testing: {}", t.0);
            assert_object(assert_eval(t.0, env).as_ref(), &t.1);
        }
    }

    #[test]
    fn test_array_indexing() {
        let tests = vec![
            ("[1,2,3][0]", 1),
            ("let arr = [1,2,3]; arr[0]", 1),
            ("let arr = [1,2,3]; arr[2]", 3),
            ("let arr = [1,2,3]; let i = 0; arr[i];", 1),
        ];

        for t in tests {
            let env = Environment::new();
            assert_integer(assert_eval(t.0, env).as_ref(), t.1)
        }
    }
    #[test]
    fn test_array_literals() {
        let tests = vec![
            (
                "[1,2,3]",
                vec![Object::Integer(1), Object::Integer(2), Object::Integer(3)]
            ),
            (
                "[true,false,true]",
                vec![
                    Object::Boolean(true),
                    Object::Boolean(false),
                    Object::Boolean(true),
                ]
            ),
            ("[\"foo\"]", vec![Object::StringLiteral("foo".to_string())]),
            ("[]", vec![]),
        ];

        for t in tests {
            let env = Environment::new();
            assert_array(assert_eval(t.0, env).as_ref(), t.1)
        }
    }

    #[test]
    fn test_builtins() {
        let input = "len(\"foobar\");";
        let env = Environment::new();
        assert_integer(assert_eval(input, env).as_ref(), 6);
    }

    #[test]
    fn test_builtins_2() {
        let tests = vec![
            ("len(1);", "len: unsupported type 1"),
            ("len(1,2);", "expected 1 argument, got 2"),
        ];

        for t in tests {
            let env = Environment::new();
            assert_error(eval(parser::parse(t.0).unwrap(), env), t.1)
        }
    }
    #[test]
    fn test_builtin_eval() {
        let env = Environment::new();
        let input = "eval(\"let add = fn(a,b) { a + b; };\"); add(2,3);";
        assert_integer(assert_eval(input, env).as_ref(), 5);
    }

    #[test]
    fn test_closures() {
        let input = "
let newAdder = fn(x) {
    fn(y) { x + y };
};
let addTwo = newAdder(2);
addTwo(2);";

        let env = Environment::new();
        assert_integer(assert_eval(input, env).as_ref(), 4)
    }

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
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
            assert_integer(evaluated.as_ref(), t.1)
        }

    }
    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let env = Environment::new();
        let evaluated = assert_eval(input, env);
        assert_function_obj(evaluated.as_ref(), vec!["x"], "(x + 2)");
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
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
            assert_integer(evaluated.as_ref(), t.1);
        }
    }

    #[test]
    fn test_errors() {
        let tests = vec![
            ("5 + true;", "type mismatch: 5 + true"),
            ("5 ++ true;", "type mismatch: 5 ++ true"),
            ("5 + true; 5", "type mismatch: 5 + true"),
            ("-true", "unknown operator: -true"),
            ("let s = \"foo\"; -s", "unknown operator: -foo"),
            ("true + false;", "unknown operator: true + false"),
            ("5; true + false; 5", "unknown operator: true + false"),
            (
                "if (10 > 1) { true + false; }",
                "unknown operator: true + false"
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; }; return 1; }",
                "unknown operator: true + false"
            ),
        ];

        for t in tests {
            let env = Environment::new();
            let node = parser::parse(t.0).unwrap();
            let evaluated = eval(node, env);
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
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
            assert_integer(evaluated.as_ref(), t.1);
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
            (
                "if (1 < 2) { \"foobar\" } else { 20 }",
                Object::StringLiteral("foobar".to_string())
            ),
        ];

        for t in tests {
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
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
            let env = Environment::new();
            let evaluated = assert_eval(test.0, env);
            assert_integer(evaluated.as_ref(), test.1);
        }
    }

    #[test]
    fn test_eval_string() {
        let tests = vec![("\"foobar\"", "foobar"), ("\"foo\" ++ \"bar\"", "foobar")];
        for t in tests {
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
            assert_string(evaluated.as_ref(), t.1);
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
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
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
            let env = Environment::new();
            let evaluated = assert_eval(t.0, env);
            assert_bool(evaluated.as_ref(), t.1)
        }
    }

    fn assert_array(obj: &Object, expect: Vec<Object>) {
        match obj {
            &Object::ArrayLiteral(ref array) => {
                for (i, elem) in array.clone().iter().enumerate() {
                    assert_eq!(**elem, expect[i])
                }
            }
            x => assert!(false, "Expected string object, got {}", x),
        }
    }
    fn assert_string(obj: &Object, expect: &str) {
        match obj {
            &Object::StringLiteral(ref s) => {
                assert_eq!(s, expect);
            }
            x => assert!(false, "Expected string object, got {:?}", x),
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
            (&Object::StringLiteral(ref l), &Object::StringLiteral(ref r)) => assert_eq!(l, r),
            (&Object::Null, &Object::Null) => assert!(true),
            _ => assert!(false),
        }
    }

    fn assert_error(result: ObjectRcResult, err: &str) {
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

    fn assert_eval(s: &str, env: Env) -> Rc<object::Object> {
        let node = parser::parse(s).unwrap();
        match eval(node, env) {
            Ok(e) => e.clone(),
            Err(e) => {
                assert!(false, format!("Expected program, got {:?}", e));
                Rc::new(object::Object::Null)
            }
        }
    }

    fn assert_integer(obj: &object::Object, expected: i64) {
        match obj {
            &object::Object::Integer(integer) => assert_eq!(integer, expected),
            x => assert!(false, format!("Expected integer object, got {:?}", x)),
        }
    }
}
