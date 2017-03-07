enum ExprC {
    NumC {n: i32},
    BoolC {b: bool},
    IdC {s: String},
    IfC {c: Box<ExprC>, t: Box<ExprC>, f: Box<ExprC>},
    LamC {args: Vec<String>, body: Box<ExprC>},
    BinopC {s: String, l: Box<ExprC>, r: Box<ExprC>},
    //AppC {fun: Box<ExprC>, arg: Vec<ExprC>}
}
enum Value{
    NumV {n: i32},
    BoolV {b: bool},
    ClosV {args: Vec<String>, body: Box<ExprC>}
}

fn binop_Value(s: String, l: Box<ExprC>, r: Box<ExprC>) -> Value{
  let left = interp(*l);
  let right = interp(*r);
  match left {
    Value::NumV { n: l } => match right {
    Value::NumV { n: r} => match s.as_ref() {
                                    "+" => Value::NumV {n : l + r},
                                    "-" => Value::NumV {n : l - r},
                                    "*" => Value::NumV {n : l * r},
                                    "/" => Value::NumV {n : l / r},
                                    "<=" => Value::BoolV {b : l <= r},
                                    "eq?" => Value::BoolV {b : l == r},
                                    _ => panic!("interp: Binop Error"),
                                    },
    _ => panic!("Type mismatch!"),
  },
    Value::BoolV { b: l } => match right {
    Value::BoolV { b: r} => match s.as_ref() {
                                      "eq?" => Value::BoolV {b : l == r},
                                      _ => panic!("interp: Binop Error"),
                                      },
    _ => panic!("Type mismatch!"),
  },
  _ => Value::BoolV {b : false},
}}
fn interp(e: ExprC)-> Value {
  match e {
    ExprC::IdC { s: s } => panic!("interp: IdC Error"),
    ExprC::NumC { n: n } => Value::NumV {n : n},
    ExprC::BoolC { b: b} => Value::BoolV {b : b},
    ExprC::LamC {args: args, body: body} => Value::ClosV{args: args, body: body},
    ExprC::BinopC { s: s, l: l, r: r } => binop_Value(s, l, r),
    ExprC::IfC { c, t, f } =>  {let exp = interp(*c);
                                match exp {
                                Value::BoolV {b: boo} => if boo {interp(*t)} else {interp(*f)},
                                _ => panic! ("interp: BoolC Error"),
                                }}
    //ExprC::appC {fun: fun, arg: arg} => interp_app(fun, arg),
    }
}
fn serialize(v: Value) -> String {
   match v {
      Value::NumV {n: n } => format!("{}", n),
      Value::BoolV {b: b } => if b {"True".to_string()} else  {"False".to_string()},
      Value::ClosV {args: args, body: body} => "#<procedure>".to_string(),
   }
}
fn top_eval(e : ExprC) -> String {
  serialize(interp(e))
}

fn main() {

//Primitive tests
  assert_eq!(top_eval(ExprC::NumC {n : 9}), "9");
  assert_eq!(top_eval(ExprC::BoolC {b : false}), "False");
  assert_eq!(top_eval(ExprC::LamC {args : vec!["x".to_string(), "y".to_string()],
                                  body: Box::new(ExprC::BoolC {b : true})}), "#<procedure>");
//Binop tests
  assert_eq!(top_eval(ExprC::BinopC {s : "<=".to_string(), l : Box::new(ExprC::NumC { n : 2}), r : Box::new(ExprC::NumC {n : 3})}), "True");
  assert_eq!(top_eval(ExprC::BinopC {s : "/".to_string(), l : Box::new(ExprC::NumC { n : 25}), r : Box::new(ExprC::NumC {n : 5})}), "5");
  assert_eq!(top_eval(ExprC::BinopC {s : "*".to_string(), l : Box::new(ExprC::NumC { n : 8}), r : Box::new(ExprC::NumC {n : 4})}), "32");
  assert_eq!(top_eval(ExprC::BinopC {s : "-".to_string(), l : Box::new(ExprC::NumC { n : 5}), r : Box::new(ExprC::NumC {n : 3})}), "2");
  assert_eq!(top_eval(ExprC::BinopC {s : "+".to_string(), l : Box::new(ExprC::NumC { n : 4}), r : Box::new(ExprC::NumC {n : 8})}), "12");
  assert_eq!(top_eval(ExprC::BinopC {s : "eq?".to_string(), l : Box::new(ExprC::NumC { n : 5}), r : Box::new(ExprC::NumC {n : 8})}), "False");
  assert_eq!(top_eval(ExprC::BinopC {s : "eq?".to_string(), l : Box::new(ExprC::NumC { n : 5}), r : Box::new(ExprC::NumC {n : 5})}), "True");
  //assert_eq!(top_eval(ExprC::BinopC {s : "eq?".to_string(), l : Box::new(ExprC::NumC { n : 5}), r : Box::new(ExprC::BoolC {b : true})}), "Type mismatch!");
  assert_eq!(top_eval(ExprC::BinopC {s : "eq?".to_string(), l : Box::new(ExprC::BoolC { b : true}), r : Box::new(ExprC::BoolC {b : false})}), "False");
  assert_eq!(top_eval(ExprC::BinopC {s : "eq?".to_string(), l : Box::new(ExprC::BoolC { b : true}), r : Box::new(ExprC::BoolC {b : true})}), "True");

// if tests
  assert_eq!(top_eval(ExprC::IfC {c : Box::new(ExprC::BoolC {b : true}), t : Box::new(ExprC::NumC {n : 1}), f : Box::new(ExprC::NumC {n : 0})}), "1");
  assert_eq!(top_eval(ExprC::IfC {c : Box::new(ExprC::BoolC {b : false}), t : Box::new(ExprC::NumC {n : 1}), f : Box::new(ExprC::NumC {n : 0})}), "0");
  assert_eq!(top_eval(
        ExprC::IfC {c : Box::new(ExprC::BinopC {s : "<=".to_string(), l : Box::new(ExprC::NumC { n : 5}), r : Box::new(ExprC::NumC {n : 5})}),
                    t : Box::new(ExprC::NumC {n:5}),
                    f : Box::new(ExprC::BoolC {b: false})}), "5");
  assert_eq!(top_eval(
        ExprC::IfC {c : Box::new(ExprC::BinopC {s : "<=".to_string(), l : Box::new(ExprC::NumC { n : 6}), r : Box::new(ExprC::NumC {n : 5})}),
                    t : Box::new(ExprC::NumC {n:5}),
                    f : Box::new(ExprC::BoolC {b: false})}), "False");
  assert_eq!(top_eval(
        ExprC::IfC {c : Box::new(ExprC::BinopC {s : "<=".to_string(), l : Box::new(ExprC::NumC { n : 4}), r : Box::new(ExprC::NumC {n : 5})}),
                    t : Box::new(ExprC::IfC {c : Box::new(ExprC::BinopC {s : "eq?".to_string(),
                                                                            l : Box::new(ExprC::NumC { n : 2}),
                                                                            r : Box::new(ExprC::NumC {n : 2})}),
                                                t : Box::new(ExprC::BoolC {b: true}),
                                                f : Box::new(ExprC::BoolC {b: false})}),
                    f : Box::new(ExprC::NumC {n:3})}
                    ), "True");
  assert_eq!(top_eval(
        ExprC::IfC {c : Box::new(ExprC::BinopC {s : "<=".to_string(), l : Box::new(ExprC::NumC { n : 7}), r : Box::new(ExprC::NumC {n : 5})}),
                    t : Box::new(ExprC::IfC {c : Box::new(ExprC::BinopC {s : "eq?".to_string(),
                                                                            l : Box::new(ExprC::NumC { n : 2}),
                                                                            r : Box::new(ExprC::NumC {n : 2})}),
                                                t : Box::new(ExprC::BoolC {b: true}),
                                                f : Box::new(ExprC::BoolC {b: false})}),
                    f : Box::new(ExprC::NumC {n:3})}
                    ), "3");
}
