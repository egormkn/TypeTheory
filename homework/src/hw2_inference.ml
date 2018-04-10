(** Algorithm W is the original algorithm for infering types in
    the Damas-Hindley-Milner type system. It supports polymorphic types,
    such as `forall[a] a -> a`, let generalization, and infers principal
    (most general) types. Although it is formally described using explicit
    substitutions, it permits an efficient implemenation using updatable
    references, achieving close to linear time complexity
    (in terms of the size of the expression being type-infered). *)

open Hw1;;
open Hw1_reduction;;
open Hw2_unify;;

(** Simple type is a type name or arrow operator *)
type simp_type =
  | S_Elem of string                    (** Type: a       *)
  | S_Arrow of simp_type * simp_type    (** Arrow: a -> a *)

(** Hindley-Milner lambda is a variable, abstraction, application or let *)
type hm_lambda =
  | HM_Var of string                            (** Variable: x          *)
  | HM_Abs of string * hm_lambda                (** Abstraction: λx.f    *)
  | HM_App of hm_lambda * hm_lambda             (** Application: f x     *)
  | HM_Let of string * hm_lambda * hm_lambda    (** Let: let x = t in t' *)

(** Hindley-Milner type is a type name, arrow operator or forall quantifier *)
type hm_type =
  | HM_Elem of string                (** Type: a          *)
  | HM_Arrow of hm_type * hm_type    (** Arrow: a -> a    *)
  | HM_ForAll of string * hm_type    (** Quantifier: ∀t.t *)

module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

(** Unique type name generator *)
let unique_type = Stream.from (fun i -> Some ("type" ^ string_of_int i));;

(** Unique variable name generator *)
let unique_var = Stream.from (fun i -> Some ("var" ^ string_of_int i));;

(** Converts simple type to algebraic term using `->` as a function *)
let rec term_of_simp_type t =
  let to_term = term_of_simp_type in
  match t with
  | S_Elem v -> Var v
  | S_Arrow (a, b) -> Fun("->", [ (to_term a); (to_term b) ]);;

(** Converts algebraic term with `->` functions to simple type *)
let rec simp_type_of_term t =
  let to_type = simp_type_of_term in
  match t with
  | Var v -> S_Elem v
  | Fun(f, [ l; r ]) when f = "->" -> S_Arrow (to_type l, to_type r)
  | _ -> failwith "Term is not representing a simple type";;

(** Converts pair of types to algebraic equation *)
let equation_of_types (l, r) = (term_of_simp_type l, term_of_simp_type r);;

(** Returns a list of variable types and a type of simple lambda expression *)
let infer_simp_type lambda =
  let new_type() = S_Elem (Stream.next unique_type) in
  let add_type_to_map map t = StringMap.add t (new_type()) map in
  let rec get_system lambda types =
    match (lambda : lambda) with
    | Var v -> ([], StringMap.find v types)
    | App (lambda1, lambda2) ->
      let (system1, t1) = get_system lambda1 types in
      let (system2, t2) = get_system lambda2 types in
      let new_t = new_type() in
      (system1 @ system2 @ [(t1, S_Arrow(t2, new_t))], new_t)
    | Abs (v, l) ->
      let new_map = add_type_to_map types v in
      let (system1, t1) = get_system l new_map in
      (system1, S_Arrow(StringMap.find v new_map, t1))
  in
  let free = free_vars lambda in
  let types = List.fold_left add_type_to_map StringMap.empty free in
  let (system, t) = get_system lambda types in
  match solve_system (List.map equation_of_types system) with
  | None -> None
  | Some solution ->
    let lambda_type_term = apply_substitution solution (term_of_simp_type t) in
    let to_type_list = List.map (fun (a, b) -> (a, simp_type_of_term b)) in
    Some (to_type_list solution, simp_type_of_term lambda_type_term);;




(** Converts Hindley-Milner type to algebraic term using `->` as a function *)
let rec term_of_hm_type hm_type =
  let to_term = term_of_hm_type in
  match hm_type with
  | HM_Elem a  -> Var a
  | HM_Arrow (a, b) -> Fun ("->", [ (to_term a); (to_term b) ])
  | _ -> failwith "Forall quantifier cannot be represented as a term";;

(** Converts algebraic term with `->` functions to Hindley-Milner type *)
let rec hm_type_of_term term =
  let to_type = hm_type_of_term in
  match term with
  | Var a  -> HM_Elem a
  | Fun (f, [l;r]) when f = "->" -> HM_Arrow(to_type l, to_type r)
  | _ -> failwith "Term is not representing a simple type";;

(** Returns a set of free variables in Hindley-Milner lambda *)
let rec free_vars hm_lambda =
  match hm_lambda with
  | HM_Var a -> StringSet.singleton a
  | HM_App (a, b) -> StringSet.union (free_vars a) (free_vars b)
  | HM_Abs (a, b) -> StringSet.remove a (free_vars b)
  | HM_Let (a, b, c) ->
    let free_vars_c = StringSet.remove a (free_vars c) in
    StringSet.union (free_vars b) free_vars_c;;

(** Returns a set of free types in Hindley-Milner lambda *)
let rec free_types hm_type =
  match hm_type with
  | HM_Elem a  -> StringSet.singleton a
  | HM_Arrow (a, b) -> StringSet.union (free_types a) (free_types b)
  | HM_ForAll (a, b) -> StringSet.remove a (free_types b);;

exception NoSolution of string;;

(* TODO: implement algorithm W *)

let rec do_subst subst hm_type set =
  match hm_type with
  | HM_Elem(a) -> if StringSet.mem a set then hm_type else
    if StringMap.mem a subst then StringMap.find a subst else hm_type
  | HM_Arrow(a, b) -> HM_Arrow(do_subst subst a set, do_subst subst b set)
  | HM_ForAll(a, b) -> HM_ForAll(a, do_subst subst b (StringSet.add a set));;

let subst_to_subst subst1 subst2 =
  StringMap.fold (fun a b map -> if StringMap.mem a map then map else StringMap.add a b map) subst1
    (StringMap.fold (fun a b map -> StringMap.add a (do_subst subst1 b StringSet.empty) map)
       subst2 StringMap.empty);;

let do_subst_types subst types =
  StringMap.fold (fun a b map -> StringMap.add a (do_subst subst b StringSet.empty) map) types StringMap.empty;;

let add_union hm_type types =
  let availble_types = StringMap.fold (fun a b set -> StringSet.union (free_types b) set) types StringSet.empty in
  StringSet.fold (fun a b -> HM_ForAll(a, b)) (StringSet.fold (fun a b -> if StringSet.mem a availble_types then b else StringSet.add a b) (free_types hm_type) StringSet.empty) hm_type;;

let rec rm_union hm_type =
  match hm_type with
  | HM_ForAll(a, b) -> do_subst (StringMap.add a (HM_Elem(Stream.next unique_var)) StringMap.empty) (rm_union b) StringSet.empty
  | _ -> hm_type;;

let algorithm_w hm_lambda =
  let rec impl hm_lambda types =
    match hm_lambda with
    | HM_Var(a) -> if StringMap.mem a types then (rm_union (StringMap.find a types), StringMap.empty) else raise (NoSolution "Free variable encountered")
    | HM_App(a, b) ->
      (let (hmt1, t1) = impl a types in
       let (hmt2, t2) = impl b (do_subst_types t1 types) in
       let new_type = HM_Elem (Stream.next unique_var) in
       match solve_system ([((term_of_hm_type (do_subst t2 hmt1 StringSet.empty)), (term_of_hm_type (HM_Arrow(hmt2, new_type))))]) with
       | None -> raise (NoSolution "Couldn't solve the system")
       | Some ans -> let ans_types = subst_to_subst
                         (List.fold_left (fun map (str, term) -> StringMap.add str (hm_type_of_term term) map) StringMap.empty ans) (subst_to_subst t2 t1) in
         (do_subst ans_types new_type StringSet.empty, ans_types))
    | HM_Abs(a, b) -> let new_type = HM_Elem (Stream.next unique_var) in
      let (hmt1, t1) = impl b (StringMap.add a new_type (StringMap.remove a types)) in
      (HM_Arrow(do_subst t1 new_type StringSet.empty, hmt1), t1)
    | HM_Let(a, b, c) -> let (hmt1, t1) = impl b types in
      let new_types = do_subst_types t1 types in
      let (hmt2, t2) = impl c (StringMap.add a (add_union hmt1 new_types) (StringMap.remove a new_types)) in
      (hmt2, subst_to_subst t2 t1) in
  let types = StringSet.fold
      (fun a map -> StringMap.add a (HM_Elem (Stream.next unique_var)) map)
      (free_vars hm_lambda) StringMap.empty
  in
  try
    let (tp, map) = impl hm_lambda types in
    Some (StringMap.bindings map, tp)
  with (NoSolution e) -> None;;
