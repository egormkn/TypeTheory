(*
 * OCaml implementation of Peano arithmetics
 * https://wiki.haskell.org/Peano_numbers
 *)

type peano =
  | Z
  | S of peano;;

(* Basic peano <-> string conversion *)
let rec peano_of_int i =
  match i with
  | 0 -> Z
  | i -> S (peano_of_int (i - 1));;

let rec int_of_peano x =
  match x with
  | Z -> 0
  | S x -> 1 + int_of_peano x;;

(* Peano arithmetic *)
let inc x =
  S x;;

let dec x =
  match x with
  | Z -> Z
  | S x -> x;;

let rec add x y =
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> y
  | (x, S y) -> S (add x y);;

let rec sub x y =
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> Z
  | (S x, S y) -> sub x y;;

let rec mul x y =
  match (x, y) with
  | (x, Z) -> Z
  | (Z, y) -> Z
  | (x, S y) -> add x (mul x y);;

let rec power x y =
  match (x, y) with
  | (Z, Z) -> failwith "0^0 is undefined"
  | (x, Z) -> S Z
  | (Z, y) -> Z
  | (x, S y) -> mul x (power x y);;

(* Recursive implementation of list operations *)
let rev x =
  let rec rev_acc acc = function
    | [] -> acc
    | head::tail -> rev_acc (head::acc) tail
  in rev_acc [] x;;

let rec merge x y =
  match (x, y) with
  | ([], y) -> y
  | (x, []) -> x
  | (x::xs, y::ys) -> if x < y
    then x :: (merge xs (y::ys))
    else y :: (merge (x::xs) ys);;

let rec split x = match x with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x::y::xs -> let (l, r) = split xs in (x::l, y::r);;

let rec merge_sort x =
  match x with
  | [] -> []
  | [x] -> [x]
  | x -> let (l, r) = split x in
    merge (merge_sort l) (merge_sort r);;

(*
 * Working with lambda expressions
 * https://en.wikipedia.org/wiki/Lambda_calculus
 *)

type lambda =
  | Var of string
  | Abs of string * lambda
  | App of lambda * lambda;;

(* Converting from lambda to string *)
let rec string_of_lambda x =
  let str = string_of_lambda in
  match x with
  | Var x -> x
  | Abs (x, y) -> "\\" ^ x ^ "." ^ str y
  | App (Abs (_, _) as abs, Var y) -> "(" ^ str abs ^ ") " ^ y
  | App (Abs (_, _) as abs, y) -> "(" ^ str abs ^ ") (" ^ str y ^ ")"
  | App (x, Var y) -> str x ^ " " ^ y
  | App (x, y) -> str x ^ " (" ^ str y ^ ")";;

(*
 * Lambda parser grammar:
 * <lambda> -> <expr> ' ' <abs> | <expr> | <abs>
 * <abs> -> \<var>.<lambda>
 * <expr> -> { <var> | (<lambda>) }+{' '}
 *)
let lambda_of_string input =
  let stream = Stream.of_string (input ^ ";") in
  let tokens = Genlex.make_lexer ["\\"; "."; "("; ")"; ";"] stream in
  let next() = Stream.next tokens in
  let peek() = Stream.peek tokens in
  let check c err = if (next() <> Genlex.Kwd c) then failwith err in
  let check_parenthesis() = check ")" "Parenthesis not closed" in
  let check_fullstop() = check "." "No full stop symbol found" in

  let rec parse_lambda() =
    match next() with
    | Genlex.Kwd "("  -> parse_parentheses()
    | Genlex.Kwd "\\" -> parse_abs()
    | Genlex.Ident v  -> parse_var v
    | _ -> failwith "Unexpected symbol"

  and parse_parentheses() =
    let lambda = parse_lambda() in
    check_parenthesis();
    check_app lambda;

  and parse_abs() =
    match next() with
    | Genlex.Ident v ->
      check_fullstop();
      let lambda = parse_lambda() in
      check_app (Abs (v, lambda));
    | _ -> failwith "Unexpected symbol"

  and parse_var v =
    check_app (Var v);

  and parse_app lambda token =
    match token with
    | Genlex.Kwd ")"  -> lambda
    | Genlex.Kwd ";"  -> lambda
    | Genlex.Kwd "\\" -> App(lambda, parse_lambda())
    | Genlex.Kwd "("  -> let _ = next() and arg = parse_lambda() in
      check_parenthesis();
      check_app (App (lambda, arg));
    | Genlex.Ident v  -> let _ = next() in check_app (App (lambda, Var v))
    | _ -> failwith "Unexpected symbol"

  and check_app lambda =
    match peek() with
    | None       -> failwith "Unexpected end of string"
    | Some token -> parse_app lambda token
  in parse_lambda();;
