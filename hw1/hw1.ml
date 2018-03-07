(* OCaml implementation of Peano arithmetics and lambda expressions *)

(* Define types used in *.mli interface *)
type peano = 
        | Z 
		| S of peano;;
		
type lambda = 
        | Var of string 
		| Abs of string * lambda 
		| App of lambda * lambda;;

(* Basic peano <-> string conversion *)
let rec peano_of_int x = match x with
    | 0 -> Z
	| x -> S (peano_of_int (x - 1));;

let rec int_of_peano p = match p with
  | Z -> 0
  | S x -> 1 + int_of_peano x;;

let inc x = failwith "Not implemented";;
let add x = failwith "Not implemented";;
let sub x = failwith "Not implemented";;
let mul x = failwith "Not implemented";;
let power x = failwith "Not implemented";;

let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;

let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;