(** The unification problem in first-order logic can be expressed as follows:
    Given two terms containing some variables, find, if it exists, the simplest
    substitution (i.e., an assignment of some term to every variable) which
    makes the two terms equal. The resulting substitution is called
    the most general unifier. *)

(** Algebraic term is a variable or a function of some variables *)
type algebraic_term =
  | Var of string
  | Fun of string * (algebraic_term list);;

(** Unique function name generator *)
let unique_name = Stream.from (fun i -> Some ("temp" ^ string_of_int i));;

(** Returns one equation equivalent to given system of equations *)
let system_to_equation system =
  let new_name = Stream.next unique_name in
  let (left, right) = List.split system in
  (Fun (new_name, left), Fun (new_name, right));;

(** Returns a term after applying given substitution *)
let apply_substitution substitution_list term =
  let rec apply substitution term =
    let (key, new_term) = substitution in
    match term with
    | Var v -> if (key = v) then new_term else term
    | Fun (name, args) -> Fun (name, List.map (apply substitution) args)
  in
  List.fold_right apply substitution_list term;;

(** Returns True if a substitution is a solution of a given system *)
let check_solution substitution_list system =
  let (l, r) = system_to_equation system in
  let substitute = apply_substitution substitution_list in
  substitute l = substitute r;;

exception NoSolution of string;;
module StringSet = Set.Make (String);;

(** Returns a solution of a given system or None, if there is no solution *)
let solve_system system =
  let error message = raise (NoSolution message) in
  let rec contains variable term =
    match term with
    | Var v -> v = variable
    | Fun (f, args) -> List.exists (contains variable) args
  in
  let rec solve system resolved =
    match system with
    | [] -> error "System is empty"
    | _ when StringSet.cardinal resolved = List.length system -> system
    | equation :: tail ->
      match equation with
      | (l, r) when l = r -> solve tail resolved
      | (Var v, term) when contains v term -> error "Error of type x = f(x)"
      | (Var v, term) ->
        let resolved = StringSet.add v resolved in
        let substitute = apply_substitution [(v, term)] in
        let tail = List.map (fun (l, r) -> (substitute l, substitute r)) tail in
        solve (tail @ [equation]) resolved
      | (term, Var v) -> solve (tail @ [(Var v, term)]) resolved
      | (Fun (f, l1), Fun (g, l2)) when f = g && List.length l1 = List.length l2 ->
        let new_equations = List.combine l1 l2 in
        solve (tail @ new_equations) resolved
      | (Fun _, Fun _) -> error "Equation with different functions"
  in
  let substitution_of_equation (left, right) =
    match left with
    | Var v -> (v, right)
    | _ -> failwith "Equation is not in form x = T"
  in
  try
    let resolved_system = solve system StringSet.empty in
    let solution = List.map substitution_of_equation resolved_system in
    Some solution
  with NoSolution message -> None;;
