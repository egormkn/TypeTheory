open Hw2_unify;;
open Hw2_inference;;
open Test;;

let rec term_of_hm_type hm_type =
  match hm_type with
  | HM_Elem(a) -> Hw2_unify.Var(a)
  | HM_Arrow(a, b) -> Hw2_unify.Fun("->", [(term_of_hm_type a);(term_of_hm_type b)])
  | _ -> failwith "Couldn't converted";;

let rec hm_type_of_term term =
  match term with
  | Hw2_unify.Var(a) -> HM_Elem(a)
  | Hw2_unify.Fun(name, [l;r]) -> HM_Arrow(hm_type_of_term l, hm_type_of_term r)
  | _ -> failwith "Never";;

let rec string_of_term x =
  match x with
  | Var v -> v
  | Fun (f, t) -> "(" ^ (String.concat f (List.map string_of_term t)) ^ ")";;



let test123 t =
  let ans1 = algorithm_w t in
  match ans1 with
  | Some (l, s) -> let ans2 = term_of_hm_type s in
    print_endline (string_of_term ans2)
  | None -> print_string "";;


let tt1 = HM_Abs("x", HM_Var("x"));;
let tt2 = HM_Let("w", HM_Abs("f", HM_Abs("x", HM_App(HM_Var("f"), HM_App(HM_Var("f"), HM_Var("x"))))), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_App(HM_Var("w"), HM_Var("w"))))))))))))));;

test123 tt1;;
test123 tt2;;
