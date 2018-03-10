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
  let no_key_to_replace x = not (has_free key x) in
  let not_bounding x = not (StringSet.mem x src_free_vars) in
  let error() = "'" ^ (string_of_lambda src) ^ "' is not free for substitution in '" ^ (string_of_lambda dest) ^ "' instead of '" ^ key ^ "'" in
  let rec substitute_rec dest =
    match dest with
    | Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) ->
      if no_key_to_replace dest then dest
      else if not_bounding x then Abs (x, substitute_rec y)
      else failwith (error())
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
  | App (Abs (x, y), t) -> not (free_to_subst t y x)
  | App (x, y) -> is_normal_form x && is_normal_form y;;

(* Returns true if `lambda1` is alpha-equivalent to `lambda2` *)
let is_alpha_equivalent lambda1 lambda2 =
  let unique_name = Stream.from (fun i -> Some ("temp_" ^ string_of_int i)) in
  let rec is_alpha_eq lambda1 lambda2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2) -> v1 = v2
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_eq x1 x2 && is_alpha_eq y1 y2)
    | (Abs (x1, y1), Abs (x2, y2)) -> let t = Var (Stream.next unique_name) in is_alpha_eq (substitute t y1 x1) (substitute t y2 x2)
    | _ -> false
  in is_alpha_eq lambda1 lambda2;;

(* Returns true if `lambda1` is alpha-equivalent to `lambda2` *)
(* Another variant without restrictions on variable names     *)
let is_alpha_equivalent lambda1 lambda2 =
  let check_vars v1 v2 map1 map2 =
    if (StringMap.mem v1 map1) && (StringMap.mem v2 map2) && (StringMap.find v1 map1 = v2) && (StringMap.find v2 map2 = v1) then true
    else if (not (StringMap.mem v1 map1)) && (not (StringMap.mem v2 map2)) && v1 = v2 then true
    else false
  in
  let rec is_alpha_eq lambda1 lambda2 map1 map2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2) -> check_vars v1 v2 map1 map2
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_eq x1 x2 map1 map2 && is_alpha_eq y1 y2 map1 map2)
    | (Abs (x1, y1), Abs (x2, y2)) -> is_alpha_eq y1 y2 (StringMap.add x1 x2 map1) (StringMap.add x2 x1 map2)
    | _ -> false
  in is_alpha_eq lambda1 lambda2 StringMap.empty StringMap.empty;;




(* Lambda reference type and conversion functions *)
type lambda_ref = Var_ref of string
                | Abs_ref of (string * lambda_ref ref)
                | App_ref of (lambda_ref ref * lambda_ref ref);;

let rec ref_of_lambda lambda =
  match lambda with
  | Var v -> ref (Var_ref v)
  | App (x, y) -> ref (App_ref (ref_of_lambda x, ref_of_lambda y))
  | Abs (x, y) -> ref (Abs_ref (x, ref_of_lambda y));;

let rec lambda_of_ref lambda_ref =
  match !lambda_ref with
  | Var_ref v -> Var v
  | App_ref (x, y) -> App (lambda_of_ref x, lambda_of_ref y)
  | Abs_ref (x, y) -> Abs (x, lambda_of_ref y);;

(* Generator of unique variable names *)
let unique_name = Stream.from (fun i -> Some ("temp_" ^ string_of_int i));;

(* Rename all abstractions in `lambda` to unique ones *)
let rec to_alpha_eq lambda map =
  match lambda with
  | Var v -> if StringMap.mem v map then Var (StringMap.find v map) else lambda
  | App (x, y) -> App(to_alpha_eq x map, to_alpha_eq y map)
  | Abs (x, y) ->
    let temp = Stream.next unique_name in
    Abs(temp, to_alpha_eq y (StringMap.add x temp map));;

(* TODO *)
let _normal_beta_reduction_impl lr =
  let rec try_to_subst term lr var =
    match !lr with
    | Var_ref a -> if a = var then lr := !term
    | App_ref (a, b) -> try_to_subst term a var; try_to_subst term b var
    | Abs_ref (a, b) -> if a <> var then try_to_subst term b var
  in
  let rec reduction_impl lr =
    let app_case a b =
      match !a with
      | Abs_ref (x, y) ->
        let fresh_var = Stream.next unique_name in
        lr := !(ref_of_lambda (to_alpha_eq (lambda_of_ref y) (StringMap.singleton x fresh_var)));
        try_to_subst b lr fresh_var;
        Some lr
      | _ ->
        match reduction_impl a with
        | Some x -> Some lr
        | None ->
          match reduction_impl b with
          | Some x -> Some lr
          | None -> None
    in
    match !lr with
    | Var_ref a -> None
    | App_ref (a, b) -> app_case a b
    | Abs_ref (a, b) ->
      match reduction_impl b with
      | Some x -> Some lr
      | None -> None
  in reduction_impl lr;;

(* Do one beta-reduction step for `lambda` using normal reduction order *)
let normal_beta_reduction lambda =
  let lr = ref_of_lambda (to_alpha_eq lambda StringMap.empty) in
  let new_lr = _normal_beta_reduction_impl lr in
  match new_lr with
  | Some x -> lambda_of_ref x
  | None -> lambda;;

(* Beta-reduce `lambda` to its normal form using normal reduction order *)
(* Implementation should be effective and use memoization *)
let reduce_to_normal_form lambda =
  let lr = ref_of_lambda (to_alpha_eq lambda StringMap.empty) in
  let rec do_step r =
    let new_ref = _normal_beta_reduction_impl r in
    match new_ref with
    | Some x -> do_step r
    | None -> r
  in
  lambda_of_ref (do_step lr);;
