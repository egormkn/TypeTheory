 open Hw1;;

let rec print_int_list x = 
    match x with 
    | [] -> print_string " "
    | a::xs -> print_int a; print_string " "; print_int_list xs;;

print_int (int_of_peano (S (S (Z))));;
print_string "\n";;
print_int (int_of_peano (peano_of_int 5));;
print_string "\n";;
print_int (int_of_peano (inc (S(S(Z)))));;
print_string "\n";;
print_int (int_of_peano (add (S(S(Z))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (mul (S(S(S(S(Z))))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S(S(S(Z)))) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power Z (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S Z) (S(S(S(Z))))));;
print_string "\n";;
print_int (int_of_peano (power (S Z) Z));;
print_string "\n";;
print_int (int_of_peano (power (S(S(S(Z)))) (S Z)));;
print_string "\n";;
print_int (int_of_peano (sub (S(S(S(S(S(S(S(Z)))))))) (S(S(S(Z))))));;
print_string "\n";;
print_int_list (rev [1;2;3;4;5;6;7]);;
print_string "\n";;
print_int_list (merge_sort [1;33;1;4;6;8;32;6;2;679;24;8;2;7]);;
print_string "\n";
print_int_list (merge_sort []);
print_string "\n";
print_int_list (merge_sort [1;5;2;8;3;4;0;9;7;6]);;
print_string "\n";
print_string (string_of_lambda (lambda_of_string "(x)(y)"));;
print_string "\n";
print_string (string_of_lambda (lambda_of_string "(x)"));
print_string "\n";
print_string (string_of_lambda (lambda_of_string "xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.x")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xy")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(x)")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "(((((((\\y.y)))))))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "((z))(\\x.\\y.((xy)))")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "\\x.\\y.xf yf")); print_string "\n";;
print_string (string_of_lambda (lambda_of_string "x1")); print_string "\n";; 
print_string (string_of_lambda (lambda_of_string "(\\x.x) (\\x.x) x")); print_string "\n";; 
