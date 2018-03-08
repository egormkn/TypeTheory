open Hw1;;

let test_section title = print_string ("\n(* " ^ title ^ " *)\n");;
let test desc exp res = print_endline (desc ^ ": " ^ (if exp = res then "OK" else "FAIL"));;

test_section "Peano arithmetic";;

test "int_of_peano" 0 (int_of_peano Z);;
test "int_of_peano" 2 (int_of_peano (S(S(Z))));;

test "int_of_peano_of_int" 0 (int_of_peano (peano_of_int 0));;
test "int_of_peano_of_int" 5 (int_of_peano (peano_of_int 5));;

test "increment" 1 (int_of_peano (inc Z));;
test "increment" 3 (int_of_peano (inc (S(S(Z)))));;

test "decrement" 0 (int_of_peano (dec Z));;
test "decrement" 1 (int_of_peano (dec (S(S(Z)))));;

test "add" 5 (int_of_peano (add (S(S(Z))) (S(S(S(Z))))));;
test "add" 3 (int_of_peano (add Z (S(S(S(Z))))));;
test "add" 2 (int_of_peano (add (S(S(Z))) Z));;

test "subtract" 1 (int_of_peano (sub (S(S(S(Z)))) (S(S(Z)))));;
test "subtract" 0 (int_of_peano (sub Z (S(S(S(Z))))));;
test "subtract" 2 (int_of_peano (sub (S(S(Z))) Z));;
test "subtract" 4 (int_of_peano (sub (S(S(S(S(S(S(S(Z)))))))) (S(S(S(Z))))));;

test "multiply" 12 (int_of_peano (mul (S(S(S(S(Z))))) (S(S(S(Z))))));;
test "multiply" 0 (int_of_peano (mul Z (S(S(S(Z))))));;
test "multiply" 0 (int_of_peano (mul (S(S(S(S(Z))))) Z));;

test "power" 27 (int_of_peano (power (S(S(S(Z)))) (S(S(S(Z))))));;
test "power" 0 (int_of_peano (power Z (S(S(S(Z))))));;
test "power" 1 (int_of_peano (power (S Z) (S(S(S(Z))))));;
test "power" 1 (int_of_peano (power (S Z) Z));;
test "power" 3 (int_of_peano (power (S(S(S(Z)))) (S Z)));;

test_section "List operations";;

test "reverse" [] (rev []);;
test "reverse" [1] (rev [1]);;
test "reverse" [7;6;5;4;3;2;1] (rev [1;2;3;4;5;6;7]);;

test "merge" [1;2;3;4;5;6;7] (merge [1;3;5;7] [2;4;6]);;
test "merge" [1;3;5;7] (merge [1;3;5;7] []);;
test "merge" [2;4;6] (merge [] [2;4;6]);;

test "split" ([], [1;2;3;4;5;6;7]) (split 0 [1;2;3;4;5;6;7]);;
test "split" ([1;2;3], [4;5;6;7]) (split 3 [1;2;3;4;5;6;7]);;
test "split" ([1;2;3;4;5;6;7], []) (split 10 [1;2;3;4;5;6;7]);;

test "merge_sort" [1;2;3;4;5;6;7] (merge_sort [1;2;3;4;5;6;7]);;
test "merge_sort" [1;2;3;4;5;6;7] (merge_sort [7;6;5;4;3;2;1]);;
test "merge_sort" [1] (merge_sort [1]);;
test "merge_sort" [] (merge_sort []);;

test_section "Working with lambda expressions";;

test "string_of_lambda" "x" (string_of_lambda (lambda_of_string "(x)"));;
test "string_of_lambda" "x y" (string_of_lambda (lambda_of_string "(x) (y)"));;
test "string_of_lambda" "\\x.x" (string_of_lambda (lambda_of_string "\\x.x"));;
test "string_of_lambda" "\\x.\\y.x y" (string_of_lambda (lambda_of_string "\\x.\\y.x y"));;
test "string_of_lambda" "\\y.y" (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))"));;
test "string_of_lambda" "z (\\x.\\y.x y)" (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((x y)))"));;
test "string_of_lambda" "\\x.\\y.x f (y f)" (string_of_lambda (lambda_of_string "\\x.\\y.(x f) (y f)"));;
test "string_of_lambda" "x1" (string_of_lambda (lambda_of_string "x1"));;
test "string_of_lambda" "(\\x.x) (\\x.x) x" (string_of_lambda (lambda_of_string "(\\x.x) (\\x.x) x"));;
