open Common

let double x = 2 * x
let square x = x * x
let rec repeat f n x = if n = 0 then x else if n = 1 then f x else repeat f (n - 1) (f x)
let product_left lst = List.fold_left ( *. ) 1.0 lst
let _ = product_left []
let _ = product_left [ 1.; 2.; 3. ]
let product_right lst = List.fold_right ( *. ) lst 1.
let _ = product_right []
let _ = product_right [ 1.; 2.; 3. ]
let terse_product_left = List.fold_left ( *. ) 1.0
let terse_product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0
;;

let rec exists_rec f = function
  | [] -> false
  | x :: xs -> f x || exists_rec f xs
;;

let exists_fold f lst = List.fold_left (fun x y -> x || f y) false lst

(* let exists_lib f lst = List.exists f lst *)
let exists_lib = List.exists
let balance_fl lst = List.fold_left ( +. ) 0. lst
let balance_fr lst = List.fold_right ( +. ) lst 0.

let rec balance_tr = function
  | [] -> 0.
  | hd :: tl -> hd +. balance_tr tl
;;

let uncurried_append (lst1, lst2) = List.append lst1 lst2
let uncurried_compare (c1, c2) = Char.compare c1 c2
let uncurried_max (m1, m2) = max m1 m2

(* List.map (fun x -> f (g x)) lst *)
let greater_than_three = List.filter (fun s -> String.length s > 3)
let add_one = List.map (fun x -> x +. 1.)
let join sep = List.fold_left (fun acc e -> acc ^ e ^ sep) ""
let keys lst = lst |> List.rev_map fst |> List.sort_uniq compare

let rec is_valid_matrix = function
  | [] -> false
  | [ _ ] -> true
  | r1 :: (r2 :: _ as tl) -> List.length r1 = List.length r2 && is_valid_matrix tl
;;

(* let add_row_vectors = List.map2 (fun e1 e2 -> e1 + e2) *)
let add_row_vectors = List.map2 ( + )
let add_matrices = List.map2 add_row_vectors

let rec transpose ls =
  let rec transpose' acc = function
    | [] | [] :: _ -> List.rev acc
    | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
  in
  transpose' [] ls
;;

let dot = List.fold_left2 (fun acc x y -> acc + (x * y)) 0
let multiply_matrices m1 m2 = List.map (fun row -> List.map (dot row) (transpose m2)) m1
