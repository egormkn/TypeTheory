(** Peano numbers are a simple way of representing the natural numbers
    using only a zero value and a successor function.

    In OCaml it is easy to create a type of Peano number values,
    but since unary representation is inefficient, they are more
    often used to do type arithmetic due to their simplicity.
*)

type peano =
  | Z
  | S of peano;;

(** Converts int to peano number *)
let rec peano_of_int i =
  match i with
  | 0 -> Z
  | i -> S (peano_of_int (i - 1));;

(** Converts peano number to int *)
let rec int_of_peano x =
  match x with
  | Z -> 0
  | S x -> 1 + int_of_peano x;;

(** Increment peano number *)
let inc x =
  S x;;

(** Decrement peano number *)
let dec x =
  match x with
  | Z -> Z
  | S x -> x;;

(** Add one peano to another *)
let rec add x y =
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> y
  | (x, S y) -> S (add x y);;

(** Subtract one peano from another *)
let rec sub x y =
  match (x, y) with
  | (x, Z) -> x
  | (Z, y) -> Z
  | (S x, S y) -> sub x y;;

(** Multiply two peano numbers *)
let rec mul x y =
  match (x, y) with
  | (x, Z) -> Z
  | (Z, y) -> Z
  | (x, S y) -> add x (mul x y);;

(** Raise peano number to the power *)
let rec power x y =
  match (x, y) with
  | (Z, Z) -> failwith "0^0 is undefined"
  | (x, Z) -> S Z
  | (Z, y) -> Z
  | (x, S y) -> mul x (power x y);;

(** Reverse list *)
let rev x =
  let rec rev_acc acc = function
    | [] -> acc
    | head :: tail -> rev_acc (head :: acc) tail
  in rev_acc [] x;;

(** Merge two sorted lists *)
let rec merge x y =
  match (x, y) with
  | ([], y) -> y
  | (x, []) -> x
  | (x :: xs, y :: ys) -> if x < y
    then x :: (merge xs (y :: ys))
    else y :: (merge (x :: xs) ys);;

(** Split list to two lists of equal length *)
let rec split x = match x with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | x :: y :: xs -> let (l, r) = split xs in (x :: l, y :: r);;

(** Sort list using mergesort algorithm *)
let rec merge_sort x =
  match x with
  | [] -> []
  | [x] -> [x]
  | x -> let (l, r) = split x in merge (merge_sort l) (merge_sort r);;




type lambda =
  | Var of string
  | App of lambda * lambda
  | Abs of string * lambda;;

(** Converts lambda expression to string *)
let rec string_of_lambda x =
  let str = string_of_lambda in
  match x with
  | Var x -> x
  | Abs (x, y) -> "\\" ^ x ^ "." ^ str y
  | App (Abs (_, _) as abs, Var y) -> "(" ^ str abs ^ ") " ^ y
  | App (Abs (_, _) as abs, y) -> "(" ^ str abs ^ ") (" ^ str y ^ ")"
  | App (x, Var y) -> str x ^ " " ^ y
  | App (x, y) -> str x ^ " (" ^ str y ^ ")";;

(** Converts string to lambda expression *)
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
