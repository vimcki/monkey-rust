use std::{cell::RefCell, rc::Rc};

use crate::{
    ast::{Expression, Identifier, Infix, Literal, Prefix, Program, Statement},
    environment::Environment,
    object::Object,
};

pub struct Evaluator {
    env: Rc<RefCell<Environment>>,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Rc::new(RefCell::new(Environment::new())),
        }
    }
    pub fn eval_program(&mut self, program: Program) -> Object {
        let mut result = Object::Null;
        for statement in program.statements {
            result = self.eval_statement(&statement);
            match result {
                Object::ReturnValue(x) => return x.as_ref().clone(),
                Object::Error(_) => return result,
                _ => {}
            }
        }
        return result;
    }

    pub fn eval_expression(&mut self, expression: &Expression) -> Object {
        match expression {
            Expression::LiteralExpression(i) => self.eval_literal(i),
            Expression::PrefixExpression(prefix, expression) => {
                let obj = self.eval_expression(expression);
                self.eval_prefix_expression(prefix, obj)
            }
            Expression::InfixExpression(left, op, right) => {
                let left = self.eval_expression(left);
                let right = self.eval_expression(right);
                self.eval_infix_expression(op, left, right)
            }
            Expression::IfExpression {
                condition,
                consequence,
                alternative,
            } => {
                let condition = self.eval_expression(condition);
                if self.is_truthy(condition) {
                    return self.eval_statement(consequence);
                } else if let Some(alt) = alternative {
                    return self.eval_statement(alt);
                } else {
                    return Object::Null;
                }
            }
            Expression::IdentifierExpression(ident) => {
                let obj = self.env.borrow().get(&ident.name);
                match obj {
                    Some(o) => o,
                    None => Object::Error(format!("identifier not found: {}", ident.name)),
                }
            }

            Expression::FunctionExpression { parameters, body } => {
                let statements = match body.as_ref() {
                    Statement::BlockStatement(statements) => statements,
                    _ => panic!("function body must be block statement"),
                };
                Object::Function(
                    parameters.clone(),
                    Program {
                        statements: statements.clone(),
                    },
                    Rc::clone(&self.env),
                )
            }
            Expression::CallExpression {
                function,
                arguments,
            } => self.eval_call(function.as_ref().clone(), arguments.clone()),
        }
    }

    fn eval_block_statement(&mut self, statements: &Vec<Statement>) -> Object {
        let mut evaluated = Object::Null;
        for statement in statements {
            evaluated = self.eval_statement(&statement);
            match evaluated {
                Object::ReturnValue(_) => return evaluated,
                Object::Error(_) => return evaluated,
                _ => {}
            }
        }
        return evaluated;
    }

    fn eval_statement(&mut self, statement: &Statement) -> Object {
        match statement {
            Statement::ExpressionStatement(expression) => {
                return self.eval_expression(expression);
            }
            Statement::BlockStatement(statements) => {
                return self.eval_block_statement(statements);
            }
            Statement::ReturnStatement(expression) => {
                let obj = self.eval_expression(expression);
                return Object::ReturnValue(Box::new(obj));
            }
            Statement::LetStatement(ident, expression) => {
                let obj = self.eval_expression(expression);
                self.env.borrow_mut().set(ident.name.clone(), obj.clone());
                return obj;
            }
        }
    }

    fn eval_literal(&self, l: &Literal) -> Object {
        match l {
            Literal::IntegerLiteral(i) => Object::Integer(*i),
            Literal::BooleanLiteral(b) => Object::Boolean(*b),
            _ => {
                todo!();
            }
        }
    }

    fn eval_prefix_expression(&self, prefix: &Prefix, obj: Object) -> Object {
        match prefix {
            Prefix::Bang => match obj {
                Object::Boolean(b) => {
                    if b {
                        return Object::Boolean(false);
                    } else {
                        return Object::Boolean(true);
                    }
                }
                Object::Null => return Object::Boolean(true),
                _ => return Object::Boolean(false),
            },
            Prefix::Minus => match obj {
                Object::Integer(i) => return Object::Integer(-i),
                _ => return Object::Error(format!("unknown operator: -{}", obj.typee())),
            },
        }
    }

    fn eval_infix_expression(&self, op: &Infix, left: Object, right: Object) -> Object {
        match op {
            Infix::Plus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Integer(l + r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} + {}", x.typee(), y.typee()))
                }
            },
            Infix::Minus => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Integer(l - r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} - {}", x.typee(), y.typee()))
                }
            },
            Infix::Asterisk => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Integer(l * r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} * {}", x.typee(), y.typee()))
                }
            },
            Infix::Slash => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Integer(l / r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} / {}", x.typee(), y.typee()))
                }
            },
            Infix::Lt => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Boolean(l < r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} < {}", x.typee(), y.typee()))
                }
            },
            Infix::Gt => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Boolean(l > r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} > {}", x.typee(), y.typee()))
                }
            },
            Infix::Eq => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Boolean(l == r),
                (Object::Boolean(l), Object::Boolean(r)) => return Object::Boolean(l == r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} == {}", x.typee(), y.typee()))
                }
            },
            Infix::NotEq => match (left, right) {
                (Object::Integer(l), Object::Integer(r)) => return Object::Boolean(l != r),
                (Object::Boolean(l), Object::Boolean(r)) => return Object::Boolean(l != r),
                (x, y) => {
                    return Object::Error(format!("type mismatch: {} != {}", x.typee(), y.typee()))
                }
            },
        }
    }

    fn is_truthy(&self, condition: Object) -> bool {
        match condition {
            Object::Null => false,
            Object::Boolean(b) => b,
            _ => true,
        }
    }
    pub fn eval_call(&mut self, fn_expr: Expression, args_expr: Vec<Expression>) -> Object {
        let fn_object = self.eval_expression(&fn_expr);
        match fn_object {
            Object::Function(params, body, f_env) => {
                self.eval_fn_call(args_expr, params, body, &f_env)
            }
            _ => Object::Error(format!("not a function: {}", fn_object.typee())),
        }
    }

    fn eval_fn_call(
        &mut self,
        args_expr: Vec<Expression>,
        params: Vec<Identifier>,
        program: Program,
        f_env: &Rc<RefCell<Environment>>,
    ) -> Object {
        if args_expr.len() != params.len() {
            Object::Error(format!(
                "wrong number of arguments: {} expected but {} given",
                params.len(),
                args_expr.len()
            ))
        } else {
            let args = args_expr
                .into_iter()
                .map(|e| self.eval_expression(&e))
                .collect::<Vec<_>>();
            let old_env = Rc::clone(&self.env);
            let mut new_env = Environment::new_with_outer(Rc::clone(f_env));
            let zipped = params.into_iter().zip(args);
            for (_, (Identifier { name }, o)) in zipped.enumerate() {
                new_env.set(name, o);
            }
            self.env = Rc::new(RefCell::new(new_env));
            let object = self.eval_block_statement(&program.statements);
            self.env = old_env;
            return object;
        }
    }
}

#[cfg(test)]
mod tests {
    use std::{cell::RefCell, rc::Rc};

    use crate::{
        ast::{Expression, Identifier, Infix, Literal, Program, Statement},
        environment::Environment,
        lexer::lexer::Lexer,
        object::Object,
        parser::Parser,
    };

    use super::Evaluator;

    fn compare(input: &str, expected: &Object) {
        let lexer = Lexer::new(input.as_bytes().to_vec());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let program = program.unwrap();
        let evaluated = Evaluator::new().eval_program(program);
        println!("input: {:?}", input);
        assert_eq!(evaluated, *expected);
    }

    #[test]
    fn test_programs() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("!true", Object::Boolean(false)),
            ("!false", Object::Boolean(true)),
            ("!!true", Object::Boolean(true)),
            ("!!false", Object::Boolean(false)),
            ("!!5", Object::Boolean(true)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_integer_expressions() {
        let tests = vec![
            ("5", Object::Integer(5)),
            ("10", Object::Integer(10)),
            ("-5", Object::Integer(-5)),
            ("-10", Object::Integer(-10)),
            ("5 + 5 + 5 + 5 - 10", Object::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Integer(32)),
            ("-50 + 100 + -50", Object::Integer(0)),
            ("5 * 2 + 10", Object::Integer(20)),
            ("5 + 2 * 10", Object::Integer(25)),
            ("20 + 2 * -10", Object::Integer(0)),
            ("50 / 2 * 2 + 10", Object::Integer(60)),
            ("2 * (5 + 10)", Object::Integer(30)),
            ("3 * 3 * 3 + 10", Object::Integer(37)),
            ("3 * (3 * 3) + 10", Object::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_bool_expression() {
        let tests = vec![
            ("true", Object::Boolean(true)),
            ("false", Object::Boolean(false)),
            ("1 < 2", Object::Boolean(true)),
            ("1 > 2", Object::Boolean(false)),
            ("1 < 1", Object::Boolean(false)),
            ("1 > 1", Object::Boolean(false)),
            ("1 == 1", Object::Boolean(true)),
            ("1 != 1", Object::Boolean(false)),
            ("1 == 2", Object::Boolean(false)),
            ("1 != 2", Object::Boolean(true)),
            ("true == true", Object::Boolean(true)),
            ("false == false", Object::Boolean(true)),
            ("true == false", Object::Boolean(false)),
            ("true != false", Object::Boolean(true)),
            ("false != true", Object::Boolean(true)),
            ("(1 < 2) == true", Object::Boolean(true)),
            ("(1 < 2) == false", Object::Boolean(false)),
            ("(1 > 2) == true", Object::Boolean(false)),
            ("(1 > 2) == false", Object::Boolean(true)),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_if_expressions() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10)),
            ("if (1 < 2) { 10 }", Object::Integer(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10)),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_return_statements() {
        let tests = vec![
            ("return 10;", Object::Integer(10)),
            ("return 10; 9;", Object::Integer(10)),
            ("return 2 * 5; 9;", Object::Integer(10)),
            ("9; return 2 * 5; 9;", Object::Integer(10)),
            (
                "if (10 > 1) { if (10 > 1) { return 10; } return 1; }",
                Object::Integer(10),
            ),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true;",
                Object::Error("type mismatch: INTEGER + BOOLEAN".to_string()),
            ),
            (
                "5 + true; 5;",
                Object::Error("type mismatch: INTEGER + BOOLEAN".to_string()),
            ),
            (
                "-true",
                Object::Error("unknown operator: -BOOLEAN".to_string()),
            ),
            (
                "true + false;",
                Object::Error("type mismatch: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "5; true + false; 5",
                Object::Error("type mismatch: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "if (10 > 1) { true + false; }",
                Object::Error("type mismatch: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }",
                Object::Error("type mismatch: BOOLEAN + BOOLEAN".to_string()),
            ),
            (
                "foobar",
                Object::Error("identifier not found: foobar".to_string()),
            ),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_let_statement() {
        let tests = vec![
            ("let a = 5; a;", Object::Integer(5)),
            ("let a = 5 * 5; a;", Object::Integer(25)),
            ("let a = 5; let b = a; b;", Object::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Integer(15),
            ),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        compare(
            &String::from(input),
            &Object::Function(
                vec![Identifier {
                    name: String::from("x"),
                }],
                Program {
                    statements: vec![Statement::ExpressionStatement(Expression::InfixExpression(
                        Box::new(Expression::IdentifierExpression(Identifier {
                            name: String::from("x"),
                        })),
                        Infix::Plus,
                        Box::new(Expression::LiteralExpression(Literal::IntegerLiteral(2))),
                    ))],
                },
                Rc::new(RefCell::new(Environment::new())),
            ),
        );
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            (
                "let identity = fn(x) { x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Integer(5),
            ),
            (
                "let double = fn(x) { x * 2; }; double(5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5, 5);",
                Object::Integer(10),
            ),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Integer(20),
            ),
            ("fn(x) { x; }(5)", Object::Integer(5)),
        ];
        for (input, expected) in tests {
            compare(&String::from(input), &expected);
        }
    }
}
