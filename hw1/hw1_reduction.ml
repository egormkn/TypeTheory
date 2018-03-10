(* Functions for working with lambda expressions from hw1 *)

open Hw1;;

module StringSet = Set.Make(String);;
module StringMap = Map.Make(String);;

(* Returns a set of free variables in `lambda` *)
let rec free_var_set lambda =
  match lambda with
  | Var v -> StringSet.singleton v
  | App (x, y) -> StringSet.union (free_var_set x) (free_var_set y);
  | Abs (x, y) -> StringSet.remove x (free_var_set y);;

(* Returns a list of free variables in `lambda` *)
let free_vars lambda = StringSet.elements (free_var_set lambda);;

(* Returns true if `key` is free in `lambda` *)
let rec has_free key lambda =
  match lambda with
  | Var v -> v = key
  | App (x, y) -> (has_free key x) || (has_free key y);
  | Abs (x, y) -> if (x = key) then false else has_free key y;;

(* Returns `dest`, where all free `key`s replaced by `src`   *)
(* or throws an error, if `src` is not free for substitution *)
let substitute src dest key =
  let src_free_vars = free_var_set src in
  let not_bounding x = not (StringSet.mem x src_free_vars) in
  let rec substitute_rec dest =
    match dest with
    | Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) ->
      if not (has_free key dest) then dest
      else if not_bounding x then Abs (x, substitute_rec y)
      else failwith ("'" ^ (string_of_lambda src) ^ "' is not free for substitution in '" ^ (string_of_lambda dest) ^ "' instead of '" ^ key ^ "'")
  in substitute_rec dest;;

(* Returns true if `src` is free for substitution *)
(* in `dest` instead of `key`, false otherwise    *)
let free_to_subst src dest key =
  try
    let _ = substitute src dest key in true
  with _ -> false;;

(* Returns true if `lambda` expression is in normal form *)
let rec is_normal_form lambda =
  match lambda with
  | Var _ -> true
  | Abs (x, y) -> is_normal_form y
  | App (Abs (_, _), _) -> false
  | App (x, y) -> is_normal_form x && is_normal_form y;;

(* Returns true if `lambda1` is alpha-equivalent to `lambda2` *)
let is_alpha_equivalent lambda1 lambda2 =
  let int_generator = Stream.from (fun i -> Some i) in
  let temp() = "temp" ^ string_of_int (Stream.next int_generator) in
  let rec is_alpha_eq lambda1 lambda2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2) -> v1 = v2
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_eq x1 x2 && is_alpha_eq y1 y2)
    | (Abs (x1, y1), Abs (x2, y2)) -> let t = Var (temp()) in is_alpha_eq (substitute t y1 x1) (substitute t y2 x2)
    | _ -> false
  in is_alpha_eq lambda1 lambda2;;


(*
Выполнить один шаг бета-редукции, используя нормальный порядок
val normal_beta_reduction: lambda -> lambda

Свести выражение к нормальной форме с использованием нормального
порядка редукции; реализация должна быть эффективной: использовать
мемоизацию
val reduce_to_normal_form: lambda -> lambda
*)

let normal_beta_reduction x = failwith "Not implemented";;

let reduce_to_normal_form x = failwith "Not implemented";;