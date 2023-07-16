use crate::{
    ast::{Expression, Infix, Literal, Prefix, Program, Statement},
    environment::Environment,
    object::Object,
};

pub struct Evaluator {
    env: Environment,
}

impl Evaluator {
    pub fn new() -> Evaluator {
        Evaluator {
            env: Environment::new(),
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
                let obj = self.env.get(&ident.name);
                match obj {
                    Some(o) => o,
                    None => Object::Error(format!("identifier not found: {}", ident.name)),
                }
            }
            _ => {
                todo!();
            }
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
                self.env.set(ident.name.clone(), obj.clone());
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
}

#[cfg(test)]
mod tests {
    use crate::{lexer::lexer::Lexer, object::Object, parser::Parser};

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
}
