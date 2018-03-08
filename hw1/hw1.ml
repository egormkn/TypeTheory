(* OCaml implementation of Peano arithmetics *)
(* https://wiki.haskell.org/Peano_numbers    *)

(* Define types used in *.mli interface *)
type peano = 
    | Z 
	| S of peano;;

type lambda =
    | Var of string
    | Abs of string * lambda
    | App of lambda * lambda;;

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

(* Working with lambda expressions *)
let rec string_of_lambda x = 
    match x with
	| Var x -> x
	| Abs (x, y) -> "\\" ^ x ^ "." ^ string_of_lambda y
	| App (Abs (abs1, abs2), Var y) -> "(" ^ string_of_lambda (Abs (abs1, abs2)) ^ ") " ^ y
	| App (Abs (abs1, abs2), y) -> "(" ^ string_of_lambda (Abs (abs1, abs2)) ^ ") (" ^ string_of_lambda y ^ ")"
	| App (x, Var y) -> string_of_lambda x ^ " " ^ y
	| App (x, y) -> string_of_lambda x ^ " (" ^ string_of_lambda y ^ ")";;

open Genlex;;
let end_sym = ";";;
let end_token_sym = Kwd ";";; (* No use *)
let lexer_lambda = make_lexer ["\\";".";"(";")";";"];;
let str_with_end_sym str = str^end_sym;;

let parse_lambda_of_tokens str_tokens =
    let tn() = Stream.next str_tokens in
    let pk() = Stream.peek str_tokens in
    let cb() = if (tn() <> Kwd ")") then failwith "Unexpected symbol" in
    let comm() = if (tn() <> Kwd ".") then failwith "Unexpected symbol" in
    
    let rec parse_lambda() =
    match (tn()) with
        | Kwd "("  -> 
                let pl = parse_lambda() in
                cb(); 
                maybe_app pl;

        | Kwd "\\" ->
                let pa = parse_abs() in
            (let abs = maybe_app pa in abs)

        | Ident s  ->
                let v = Var s in
            maybe_app v;
                   
        | _ -> failwith "Unexpected symbol"

    and parse_abs() = 
        match (tn()) with 
        | Ident s -> 
                comm();
                Abs(s, parse_lambda());
            
        | _ -> failwith "Unexpected symbol" 

    and maybe_app l_app = 
        match (pk()) with 
        | None   -> failwith "Unexpected error";
        | Some k -> 
                (match k with 
                 | Kwd ")"  -> l_app
                 | Kwd ";"  -> l_app
                 | Kwd "\\" -> App(l_app, parse_lambda())
                 | Kwd "("  -> let _ = tn() and newp = parse_lambda() in (cb(); maybe_app (App(l_app, newp)))
                 | Ident s  -> let _ = tn() in maybe_app (App(l_app, Var(s))) 
                 | _ -> failwith "Unexpected symbol") in 

    parse_lambda();; 
 
let lambda_of_string x = parse_lambda_of_tokens (lexer_lambda(Stream.of_string (str_with_end_sym x)));;

