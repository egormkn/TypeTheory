open Hw2_unify;;
open Test;;

let rec string_of_term x =
  match x with
  | Var v -> v
  | Fun (f, t) -> f ^ "(" ^ (String.concat ", " (List.map string_of_term t)) ^ ")";;

let rec string_of_equation (l, r) =
  string_of_term l ^ " = " ^ string_of_term r;;

let term_of_string input =
  let stream = Stream.of_string (input ^ "=") in
  let tokens = Genlex.make_lexer [","; "("; ")"; "="] stream in
  let next() = Stream.next tokens in
  let peek() = Stream.peek tokens in
  let check c err value = if (next() <> Genlex.Kwd c) then failwith err else value in
  let check_parenthesis = check ")" "Parenthesis not closed" in

  let rec parse_term() =
    match next() with
    | Genlex.Ident name -> check_fun name
    | Genlex.Kwd "(" -> let t = parse_term() in check_parenthesis t
    | _ -> failwith "Unexpected symbol"

  and check_fun name =
    match peek() with
    | Some (Genlex.Kwd "(") -> let _ = next() in parse_fun name []
    | Some (Genlex.Kwd _) -> Var name
    | _ -> failwith "Unexpected end of string"

  and parse_fun name args =
    let args = (parse_term()) :: args in
    match next() with
    | Genlex.Kwd "," -> parse_fun name args
    | Genlex.Kwd ")" -> Fun (name, List.rev args)
    | _ -> failwith "Unexpected symbol"
  in parse_term();;

test "string_of_term"
  ~tester: string_of_term
  ~string_of: (fun x -> x)
  (Stream.of_list [
      { input = Fun ("f", [Var "x"; Var "y"; Fun ("z", [Var "abc"; Var "def"]); Var "x"]); output = "f(x, y, z(abc, def), x)" };
    ]);;

test "term_of_string"
  ~tester: term_of_string
  ~string_of: string_of_term
  (Stream.of_list [
      { input = "f(x,(y),z(abc,def),x)"; output = Fun ("f", [Var "x"; Var "y"; Fun ("z", [Var "abc"; Var "def"]); Var "x"]) };
    ]);;

let sym1 = [
  (Fun ("x", [Var "p1"]), Fun ("x", [Var "p2"]));
  (Fun ("y", [Var "p2"]), Fun ("y", [Var "p4"]));
  (Fun ("z", [Var "p5"]), Fun ("z", [Var "p6"]));
];;

let sym2 = [
  (Fun("x", [Var "p1"]), Fun("x", [Var "p2"]));
  (Fun("m", [Var "p1"]), Fun("y", [Var "p4"]));
  (Fun("z", [Var "p5"]), Fun("z", [Var "p6"]));
];;

let sym3 = [
  (Fun("x", [Var "p1"]), Fun("x", [Var "p2"]));
  (Fun("y", [Var "p1"]), Fun("y", [Var "p4"]));
  (Fun("z", [Var "p1"]), Fun("z", [Var "p6"]));
];;

let sym4 = [
  (Fun("a", [Var "tx"; Fun("a", [Var "ty"; Fun("a", [Var "tz";Var "t2"])])]), Fun("a", [Fun("a", [Var "ta"; Fun("a", [Var "tb"; Var "ta"])]); Var "t1"]));
  (Var("ty"), Fun("a", [Var "tz"; Var "t4"]));
  (Var("tx"), Fun("a", [Var "tz"; Var "t3"]));
  (Var("t3"), Fun("a", [Var "t4"; Var "t2"]));
];;

let substitution = [
  ("p1", Var "s1");
  ("p2", Var "s2");
  ("p3", Var "s3");
];;

(* let system_to_equation_compare (exp1, exp2) (res1, res2) =
  match (exp1, exp2, res1, res2) with
  | (Fun (exp_n1, exp_a1), Fun (exp_n1, exp_a1), res1, res2)
  | _ -> expr2 *)

test "system_to_equation"
  ~tester: system_to_equation
  ~string_of: string_of_equation
  (Stream.of_list [
      { input = sym1; output = (term_of_string "temp_0(x(p1), y(p2), z(p5))", term_of_string "temp_0(x(p2), y(p4), z(p6))") };
      { input = sym2; output = (term_of_string "", term_of_string "") };
      { input = sym3; output = (Var "x", Var "y") };
      { input = sym4; output = (Var "x", Var "y") };
    ]);;



let rec print_substitution sub =
  match sub with
  | [] -> print_string "\n"
  | (h::t) -> print_string ((fst h) ^ " = " ^ (string_of_term (snd h)) ^ "\n"); print_substitution t;;

let print_ans ans =
  match ans with
  | Some s -> print_substitution s
  | None -> print_string "Not substitution!\n";;

let maj_eqt = system_to_equation(sym1);;

let eql = apply_substitution substitution (fst maj_eqt);;
let eqr = apply_substitution substitution (snd maj_eqt);;

print_string ((string_of_term eql) ^ "\n");;
print_string ((string_of_term eqr) ^ "\n");;

let check_subt = check_solution substitution sym1;;
print_string ((string_of_bool check_subt) ^ "\n");;

let ans_sym1 = solve_system sym1;;
let ans_sym2 = solve_system sym2;;
let ans_sym3 = solve_system sym3;;
let ans_sym4 = solve_system sym4;;

print_endline "System 1";;
print_ans ans_sym1;;

print_endline "System 2";;
print_ans ans_sym2;;
print_endline "System 3";;
print_ans ans_sym3;;
print_endline "System 4";;
print_ans ans_sym4;;
