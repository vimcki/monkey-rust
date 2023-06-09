use crate::{
    ast::Node,
    object::{Null, Object},
};

pub fn eval(node: Box<dyn Node>) -> Box<dyn Object> {
    return Box::new(Null {});
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::lexer::Lexer,
        object::{Integer, Object},
        parser::Parser,
    };

    #[test]
    fn test_evel_integer_expression() {
        let tests = vec![("5", 5), ("10", 10)];
        for t in tests.iter() {
            let evaluated = test_eval(&t.0.to_string());
            test_integer_object(evaluated, t.1);
        }
    }

    fn test_eval(input: &String) -> Box<dyn Object> {
        let l = Lexer::new(input.as_bytes().to_vec());
        let mut p = Parser::new(l);
        let program = p.parse_program().unwrap();
        return super::eval(Box::new(program));
    }

    fn test_integer_object(obj: Box<dyn Object>, expected: i64) {
        let obj_int = obj.as_any().downcast_ref::<Integer>().unwrap();
        assert_eq!(obj_int.value, expected);
    }
}
