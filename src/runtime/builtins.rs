use std::io::{self, Write};

macro_rules! builtins {
    (

        ($interpreter:ident)

        $(
            [$name:ident] $fn_name:ident(
                $(...$spread:ident)?
                $($arg:ident),*
            ) $code:block
        )*
    ) => {
        use crate::runtime::interpreter::{ValueKey, ScopeKey};
        use crate::Interpreter;
        use crate::runtime::value::Value;
        use crate::runtime::error::RuntimeError;
        use crate::parsing::ast::ExprNode;
        use crate::sources::CodeSpan;

        #[derive(Debug, Clone, Copy, PartialEq)]
        pub enum Builtin {
            $(
                $name,
            )*
        }

        impl Builtin {
            pub fn run(
                &self,
                args: &Vec<ExprNode>,
                span: CodeSpan,
                scope: ScopeKey,
                $interpreter: &mut Interpreter
            ) -> Result<Value, RuntimeError> {

                #[derive(Debug, Clone)]
                struct Param<'a> {
                    pub value: &'a Value,
                    pub key: ValueKey,
                    pub span: CodeSpan,
                }

                match self {
                    $(
                        #[allow(unused_assignments)]
                        Builtin::$name => {

                            #[allow(unused_variables)]
                            let skip_check = false;
                            $(
                                stringify!($spread);
                                let skip_check = true;
                            )?

                            if !skip_check {

                                #[allow(unused_mut)]
                                let mut arg_amount = 0;
                                $(
                                    stringify!($arg);
                                    arg_amount += 1;
                                )*
                                if arg_amount != args.len() {
                                    return Err(RuntimeError::ArgumentAmount {
                                        expected: arg_amount,
                                        found: args.len(),
                                        span,
                                    })
                                }
                            }


                            let mut keys = vec![];
                            for arg in args {
                                keys.push($interpreter.execute_expr(arg, scope)?);
                            }

                            #[allow(unused_mut, unused_variables)]
                            let mut arg = 0;
                            $(
                                let $arg = Param {
                                    value: &$interpreter.memory[keys[arg]],
                                    key: keys[arg],
                                    span: args[arg].span,
                                };
                                arg += 1;
                            )*

                            $(
                                let mut $spread = vec![];
                                for (k, node) in keys.iter().zip(args) {
                                    let arg = Param {
                                        value: &$interpreter.memory[*k],
                                        key: *k,
                                        span: node.span,
                                    };
                                    $spread.push(arg);
                                }
                            )?

                            Ok(
                                $code
                            )
                        }
                    )*
                }
            }
            pub fn name(&self) -> String {
                match self {
                    $(
                        Builtin::$name => stringify!($fn_name).into(),
                    )*
                }
            }
            pub fn populate_scope(interpreter: &mut Interpreter, scope: ScopeKey) {
                let scope = &mut interpreter.scopes[scope];

                $(
                    scope.vars.insert(
                        interpreter.interner.get_or_intern(stringify!($fn_name)),
                        interpreter.memory.insert(Value::Builtin(Builtin::$name)),
                    );
                )*
            }
        }
    };
}

impl Builtin {
    pub fn print_str(&self) -> String {
        format!("<builtin: {}>", self.name())
    }
}

macro_rules! math_helper {
    ($e:expr, $fn_name:ident) => {
        match $e.value {
            Value::Int(n) => Value::Float((*n as f64).$fn_name()),
            Value::Float(n) => Value::Float(n.$fn_name()),
            v => {
                return Err(RuntimeError::MismatchedType {
                    found: v.to_type(),
                    expected: "int or float".into(),
                    span: $e.span,
                })
            }
        }
    };
}

builtins! {

    (interpreter)

    [Print] print(...args) {
        let mut out = String::new();
        for i in args {
            out += &interpreter.value_str(i.key);
        }
        print!("{}", out);
        Value::unit()
    }
    [PrintLn] println(...args) {
        let mut out = String::new();
        for i in args {
            out += &interpreter.value_str(i.key);
        }
        println!("{}", out);
        Value::unit()
    }
    [Input] input(...args) {
        let mut out = String::new();
        for i in args {
            out += &interpreter.value_str(i.key);
        }
        print!("{}", out);

        io::stdout().flush().unwrap();
        let mut input_str = String::new();
        io::stdin()
            .read_line(&mut input_str)
            .expect("Failed to read line");
        Value::String(
            input_str
                .replace('\r', "")
                .replace('\n', "")
        )
    }

    [Sqrt] sqrt(x) { math_helper!(x, sqrt) }
    [Cbrt] sqrt(x) { math_helper!(x, cbrt) }

    [Sin] sin(x) { math_helper!(x, sin) }
    [Cos] cos(x) { math_helper!(x, cos) }
    [Tan] tan(x) { math_helper!(x, tan) }

    [SinH] sinh(x) { math_helper!(x, sinh) }
    [CosH] cosh(x) { math_helper!(x, cosh) }
    [TanH] tanh(x) { math_helper!(x, tanh) }

    [ASin] asin(x) { math_helper!(x, asin) }
    [ACos] acos(x) { math_helper!(x, acos) }
    [ATan] atan(x) { math_helper!(x, atan) }

    [ASinH] asinh(x) { math_helper!(x, asinh) }
    [ACosH] acosh(x) { math_helper!(x, acosh) }
    [ATanH] atanh(x) { math_helper!(x, atanh) }

    [Ln] ln(x) { math_helper!(x, ln) }


}
