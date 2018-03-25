(* Unification algorithm *)

type algebraic_term =
  | Var of string
  | Fun of string * (algebraic_term list);;

type substitution = (string * algebraic_term) list
type equation = algebraic_term * algebraic_term
type system_of_equations = equation list

(* Unique function name generator *)
let unique_name = Stream.from (fun i -> Some ("temp_" ^ string_of_int i));;

(* Returns one equation build from a list of equations *)
let system_to_equation system =
  let new_name = Stream.next unique_name in
  let (l, r) = List.split system in
  (Fun (new_name, l), Fun (new_name, r));;

(* Applies substitution to algebraic term *)
let apply_substitution substs_list term =
  let rec apply (key, subst) term =
    match term with
    | Var v -> if (key = v) then subst else term
    | Fun (f, t) -> Fun (f, List.map (apply (key, subst)) t)
  in
  List.fold_right apply substs_list term;;

let check_solution substs_list system =
  let (l, r) = system_to_equation system in
  let substitute = apply_substitution substs_list in
  substitute l = substitute r;;

let solve_system x = failwith "Not implemented";;


exception NoSolution of string;;
module StringSet = Set.Make (String);;

let rec contains str at msk =
  match at with
    (Var a) -> if a = str then msk lor 1 else msk
  | (Fun (f, l)) -> (contains_l str l msk) lor (if str = f then 2 else 0)
and contains_l str l msk =
  match l with
    [] -> msk
  | (h::t) -> (contains str h msk) lor (contains_l str t msk);;

let rec algebraic_term_to_string (at : algebraic_term) =
  let rec impl a =
    match a with
    | Var x -> x
    | Fun(f, l) -> f ^ "(" ^ impl_for_list l ^ ")"

  and impl_for_list lt =
    match lt with
    | [] -> ""
    | (h::[]) -> impl h
    | (h::t) -> (impl h) ^ " " ^ (impl_for_list t)
  in
  impl at;;

let memv str at =
  contains str at 0 land 1 <> 0;;

let rec subst (s : algebraic_term) (x : string) (t : algebraic_term) : algebraic_term =
  match t with
  | Var y -> if (x = y) then s else t
  | Fun(f, l) -> Fun(f, List.map (subst s x) l);;

(* Major implementations: system_to_equation, apply_substitution,    *)
(* check_solution, solve_system.                                     *)
let apply_substitution_sys subst sys =
  let rec impl subst sys acc =
    match sys with
      [] -> List.rev acc
    | ((l, r)::t) -> impl subst t (((apply_substitution subst l), (apply_substitution subst r))::acc) in
  impl subst sys [];;


let rec solve_system (sys : system_of_equations) : substitution option =
  let get_args_sys l1 l2 =
    let rec impl l1 l2 ans =
      match (l1, l2) with
        ([], []) -> List.rev ans
      | (h1::t1, h2::t2) -> impl t1 t2 ((h1, h2)::ans)
      | _ -> failwith "never" in
    impl l1 l2 [] in

  let rec impl sys resolved =
    if StringSet.cardinal resolved = List.length sys then sys else
      match sys with
        [] -> raise (NoSolution "Empty system")
      | (lhs, rhs)::tail ->
        let cur = lhs, rhs in
        if lhs = rhs then impl tail resolved else
          match (lhs, rhs) with
            Var a, any -> if memv a any then raise (NoSolution "Fourth rule abused")
            else let resolved = StringSet.add a resolved in
              impl (List.append (apply_substitution_sys [a, any] tail) [cur]) resolved
          | any, Var a -> impl (List.append tail [rhs, lhs]) resolved
          | Fun(f, l1), Fun(g, l2) -> if f <> g || List.length l1 <> List.length l2 then raise (NoSolution "Third rule abused")
            else impl (List.append tail (get_args_sys l1 l2)) resolved in

  let dewrap sys =
    let rec impl sys ans =
      match sys with
        [] -> List.rev ans
      | ((Var a, rhs)::tail) ->
        impl tail ((a, rhs)::ans)
      | _ -> failwith "it's impossible, sorry" in
    impl sys [] in

  try
    let resolved_system = impl sys StringSet.empty in
    print_string "";
    (Some (dewrap resolved_system))

  with (NoSolution msg) ->
    print_string "";
    None;;
