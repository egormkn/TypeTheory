(* OCaml implementation of Peano arithmetics *)
(* https://wiki.haskell.org/Peano_numbers    *)

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

let rec split i x = match (i, x) with
  | (0, x) -> ([], x)
  | (i, []) -> (x, [])
  | (i, head::tail) ->
    let res = (split (i - 1) tail) in
    match res with
    | (l, r) -> (head::l, r);;

let rec merge_sort x =
  match x with
  | [] -> []
  | [x] -> [x]
  | x -> let (l, r) = split (List.length x / 2) x in
    merge (merge_sort l) (merge_sort r);;

(* Working with lambda expressions               *)
(* https://en.wikipedia.org/wiki/Lambda_calculus *)

type lambda =
  | Var of string
  | Abs of string * lambda
  | App of lambda * lambda;;

(* Converting from lambda to string *)
let rec string_of_lambda x =
  match x with
  | Var x -> x
  | Abs (x, y) -> "\\" ^ x ^ "." ^ string_of_lambda y
  | App (Abs (abs1, abs2), Var y) -> "(" ^ string_of_lambda (Abs (abs1, abs2)) ^ ") " ^ y
  | App (Abs (abs1, abs2), y) -> "(" ^ string_of_lambda (Abs (abs1, abs2)) ^ ") (" ^ string_of_lambda y ^ ")"
  | App (x, Var y) -> string_of_lambda x ^ " " ^ y
  | App (x, y) -> string_of_lambda x ^ " (" ^ string_of_lambda y ^ ")";;

(* 
 * Lambda parser grammar:
 * <lambda> -> <expr> ' ' <abs> | <expr> | <abs>
 * <abs> -> \<var>.<lambda>
 * <expr> -> { <var> | (<lambda>) }+{' '}
 *)
let lambda_of_string input =
  let input = input ^ ";" in
  let tokens = Genlex.make_lexer ["\\"; "."; "("; ")"; ";"] (Stream.of_string input) in
  let next() = Stream.next tokens in
  let peek() = Stream.peek tokens in
  let check_parenthesis() = if (next() <> Genlex.Kwd ")") then failwith "Parenthesis not closed" in
  let check_fullstop() = if (next() <> Genlex.Kwd ".") then failwith "No full stop symbol found" in

  let rec parse_lambda() =
    match (next()) with
    | Genlex.Kwd "("  -> parse_parentheses()
    | Genlex.Kwd "\\" -> parse_abs()
    | Genlex.Ident s  -> parse_var s
    | _ -> failwith "Unexpected symbol"

  and parse_parentheses() =
    let lambda = parse_lambda() in
    check_parenthesis();
    check_app lambda;

  and parse_abs() =
    match (next()) with
    | Genlex.Ident s ->
      check_fullstop();
      let lambda = parse_lambda() in
      check_app (Abs (s, lambda));
    | _ -> failwith "Unexpected symbol"

  and parse_var s =
    check_app (Var s);

  and parse_app lambda next_token = match next_token with
    | Genlex.Kwd ")"  -> lambda
    | Genlex.Kwd ";"  -> lambda
    | Genlex.Kwd "\\" -> App(lambda, parse_lambda())
    | Genlex.Kwd "("  -> let _ = next() and arg = parse_lambda() in (check_parenthesis(); check_app (App (lambda, arg)))
    | Genlex.Ident s  -> let _ = next() in check_app (App (lambda, Var s))
    | _ -> failwith "Unexpected symbol"

  and check_app lambda =
    match (peek()) with
    | None   -> failwith "Unexpected end of string"
    | Some k -> parse_app lambda k
  in parse_lambda();;
