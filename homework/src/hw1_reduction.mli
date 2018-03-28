(** A beta reduction (also written β reduction) is the process of
    calculating a result from the application of a function to an expression *)

open Hw1

(** Returns True, if first lambda is free for substitution
    to the second instead of given variable *)
val free_to_subst: lambda -> lambda -> string -> bool

(** Returns a list of free variables in lambda expression *)
val free_vars: lambda -> string list

(** Returns True, lambda expression is in normal form *)
val is_normal_form: lambda -> bool

(** Returns True, if lambda expressions are alpha-equivalent *)
val is_alpha_equivalent: lambda -> lambda -> bool

(** Does one step of beta-reduction using normal-order reduction *)
val normal_beta_reduction: lambda -> lambda

(** Effectively reduces lambda expression to its normal form *)
val reduce_to_normal_form: lambda -> lambda
