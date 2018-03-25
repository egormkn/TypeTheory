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
  | App (x, y) -> (has_free key x) || (has_free key y)
  | Abs (x, y) when x = key -> false
  | Abs (x, y) -> has_free key y;;

(* Returns `dest`, where all free `key`s replaced by `src`   *)
(* or throws an error, if `src` is not free for substitution *)
let substitute src dest key =
  let src_free_vars = free_var_set src in
  let no_key_to_replace x = not (has_free key x) in
  let not_bounding x = not (StringSet.mem x src_free_vars) in
  let str = string_of_lambda in
  let error() = "'" ^ (str src) ^ "' is not free for substitution in '" ^
                (str dest) ^ "' instead of '" ^ key ^ "'" in
  let rec substitute_rec dest =
    match dest with
    | Var v -> if (v = key) then src else dest
    | App (x, y) -> App(substitute_rec x, substitute_rec y);
    | Abs (x, y) when no_key_to_replace dest -> dest
    | Abs (x, y) when not_bounding x -> Abs (x, substitute_rec y)
    | _ -> failwith (error())
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
    | (Abs (x1, y1), Abs (x2, y2)) ->
      let t = Var (Stream.next unique_name) in
      is_alpha_eq (substitute t y1 x1) (substitute t y2 x2)
    | _ -> false
  in is_alpha_eq lambda1 lambda2;;

(* Returns true if `lambda1` is alpha-equivalent to `lambda2` *)
(* Another variant without restrictions on variable names     *)
let is_alpha_equivalent lambda1 lambda2 =
  let mem = StringMap.mem in
  let find = StringMap.find in
  let add = StringMap.add in
  let check_vars v1 v2 map1 map2 =
    if (mem v1 map1) && (mem v2 map2) && (find v1 map1 = v2) && (find v2 map2 = v1) then true
    else if (not (mem v1 map1)) && (not (mem v2 map2)) && v1 = v2 then true
    else false
  in
  let rec is_alpha_eq lambda1 lambda2 map1 map2 =
    match (lambda1, lambda2) with
    | (Var v1, Var v2) -> check_vars v1 v2 map1 map2
    | (App (x1, y1), App (x2, y2)) -> (is_alpha_eq x1 x2 map1 map2 && is_alpha_eq y1 y2 map1 map2)
    | (Abs (x1, y1), Abs (x2, y2)) -> is_alpha_eq y1 y2 (add x1 x2 map1) (add x2 x1 map2)
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

(* Do one step of normal beta-reduction for `lambda_ref` *)
let rec reduction_step lambda_ref =
  let mem = StringMap.mem in
  let find = StringMap.find in
  let add = StringMap.add in
  let rec to_alpha_eq lambda_ref map =
    match !lambda_ref with
    | Var_ref v -> if mem v map then ref (Var_ref (find v map)) else lambda_ref
    | App_ref (x, y) -> ref (App_ref (to_alpha_eq x map, to_alpha_eq y map))
    | Abs_ref (x, y) ->
      let temp = Stream.next unique_name in
      ref (Abs_ref (temp, to_alpha_eq y (add x temp map)))
  in
  let rec try_to_subst src dest key =
    match !dest with
    | Var_ref a -> if a = key then dest := !src
    | Abs_ref (a, b) -> if a <> key then try_to_subst src b key
    | App_ref (a, b) ->
      try_to_subst src a key;
      try_to_subst src b key
  in
  let reduction_app a b =
    match !a with
    | Abs_ref (x, y) ->
      let temp = Stream.next unique_name in
      lambda_ref := !(to_alpha_eq y (StringMap.singleton x temp));
      try_to_subst b lambda_ref temp;
      Some lambda_ref
    | _ ->
      match reduction_step a with
      | Some _ -> Some lambda_ref
      | None ->
        match reduction_step b with
        | Some _ -> Some lambda_ref
        | None -> None
  in
  match !lambda_ref with
  | Var_ref a -> None
  | App_ref (a, b) -> reduction_app a b
  | Abs_ref (a, b) ->
    match reduction_step b with
    | Some _ -> Some lambda_ref
    | None -> None;;

(* Do one step of normal beta-reduction for `lambda` *)
let normal_beta_reduction lambda =
  match reduction_step (ref_of_lambda lambda) with
  | Some x -> lambda_of_ref x
  | None -> lambda;;

(* Beta-reduce `lambda` to its normal form using normal reduction order *)
(* Implementation should be effective and use memoization *)
let reduce_to_normal_form lambda =
  let rec reduction lambda_ref =
    match reduction_step lambda_ref with
    | Some x -> reduction x
    | None -> lambda_ref
  in
  let result = reduction (ref_of_lambda lambda) in
  lambda_of_ref result;;
