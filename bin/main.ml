let print_stat name num = Printf.printf "%s: %F\n%!" name num in
let string_of_stat name num = Printf.sprintf "%s: %F" name num in
print_endline "Hello cs3110";
print_stat "haha" 12.;
string_of_stat "haha" 12.
