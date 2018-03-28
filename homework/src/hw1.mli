(** Peano numbers are a simple way of representing the natural numbers
    using only a zero value and a successor function.

    In OCaml it is easy to create a type of Peano number values,
    but since unary representation is inefficient, they are more
    often used to do type arithmetic due to their simplicity.
*)

(** Peano number is Zero or Succ(peano) *)
type peano = Z             (* Zero *)
           | S of peano    (* Succ *)

(** Converts int to peano number *)
val peano_of_int: int -> peano

(** Converts peano number to int *)
val int_of_peano: peano -> int

val inc: peano -> peano               (** Increment peano number          *)
val add: peano -> peano -> peano      (** Add one peano to another        *)
val sub: peano -> peano -> peano      (** Subtract one peano from another *)
val mul: peano -> peano -> peano      (** Multiply two peano numbers      *)
val power: peano -> peano -> peano    (** Raise peano number to the power *)

(** Reverse list *)
val rev: 'a list -> 'a list

(** Sort list using mergesort algorithm *)
val merge_sort: 'a list -> 'a list

(** Lambda is a variable, application or abstraction *)
type lambda = Var of string             (* Variable: x       *)
            | App of lambda * lambda    (* Application: f x  *)
            | Abs of string * lambda    (* Abstraction: λx.f *)

(* Lambda expression grammar:
   <lambda> -> <expr> ' ' <abs> | <expr> | <abs>
   <abs> -> \<var>.<lambda>
   <expr> -> { <var> | (<lambda>) }+{' '}
*)

(** Converts lambda expression to string *)
val string_of_lambda: lambda -> string

(** Converts string to lambda expression *)
val lambda_of_string: string -> lambda
