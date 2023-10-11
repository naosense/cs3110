open Common

let lst = [ 1; 2; 3; 4; 5 ]
let lst = [ 1; 2; 3; 4; 5 ]
let lst = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

let rec product lst =
  match lst with
  | [] -> 1
  | hd :: rest -> hd * product rest
;;

let _ = product [ 1; 2; 3 ]

let rec concat lst =
  match lst with
  | [] -> ""
  | hd :: rest -> hd ^ concat rest
;;

let _ = concat [ "a"; "b"; "c" ]

(*pattern*)
let first_check lst =
  match lst with
  | [] -> false
  | hd :: _ -> hd = "bigred"
;;

let two_or_four lst =
  match lst with
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false
;;

let _ = two_or_four [ 1; 2 ]
let _ = two_or_four [ 1; 2; 3 ]
let _ = two_or_four [ 1; 2; 3; 4 ]
let _ = two_or_four [ 1; 2; 3; 4; 5 ]

let first_eq_second lst =
  match lst with
  | [ first; second ] -> first = second
  | _ -> false
;;

let _ = first_eq_second []
let _ = first_eq_second [ 1; 2 ]
let _ = first_eq_second [ 1; 1 ]
let fifth lst = if List.length lst >= 5 then List.nth lst 4 else 0
let sort_list_desc lst = List.sort (fun a b -> Stdlib.compare b a) lst
let last_element lst = List.nth lst (List.length lst - 1)
let any_zeros lst = List.exists (fun e -> e = 0) lst
let _ = any_zeros [ 1; 2; 3 ]
let _ = any_zeros [ 1; 2; 3; 0 ]

let rec take n lst =
  if n = 0
  then []
  else
    match lst with
    | [] -> []
    | hd :: rest -> hd :: take (n - 1) rest
;;

let rec take' n lst =
  let rec take_tr n lst front =
    if n = 0
    then List.rev front
    else
      match lst with
      | [] -> []
      | hd :: tl -> take_tr (n - 1) tl (hd :: front)
  in
  take_tr n lst []
;;

let _ = take' 3 [ 1; 2; 3; 4; 5 ]

let rec drop n lst =
  if n = 0
  then lst
  else
    match lst with
    | [] -> []
    | _ :: rest -> drop (n - 1) rest
;;

let _ = drop 0 [ 1; 2; 3 ]
let _ = drop 2 [ 1; 2; 3 ]
let _ = drop 2 []
let list_1_to_100 = 1 -- 100

let is_unimodal lst =
  let rec is_uni_dec = function
    | [] | [ _ ] -> true
    | h1 :: (h2 :: _ as tl) -> h1 >= h2 && is_uni_dec tl
  in
  let rec is_uni_inc_then_dec = function
    | [] | [ _ ] -> true
    | h1 :: (h2 :: _ as tl) as lst ->
      if h1 <= h2 then is_uni_inc_then_dec tl else is_uni_dec lst
  in
  is_uni_inc_then_dec lst
;;

let _ = is_unimodal (1 -- 3)
let _ = (1 -- 3) @ (5 -- 2) |> is_unimodal
let _ = is_unimodal [ 1; 1; 1; 1; 1 ]
let _ = is_unimodal [ 1; 2; 1; 2; 1 ]

let rec powerset = function
  | [] -> [ [] ]
  | hd :: tl ->
    let lst = powerset tl in
    List.map (fun set -> hd :: set) lst @ lst
;;

let rec print_int_list = function
  | [] -> ()
  | h :: t ->
    h |> string_of_int |> print_endline;
    print_int_list t
;;

let print_int_list' lst = List.iter (fun x -> print_endline (string_of_int x)) lst

type student =
  { first_name : string
  ; last_name : string
  ; gpa : float
  }

let extract_name = function
  | { first_name; last_name; _ } -> first_name, last_name
;;

let make_student first last g = { first_name = first; last_name = last; gpa = g }
let _ = make_student "joe" "snow" 50.

type poketype =
  | Normal
  | Fire
  | Water

type pokemon =
  { name : string
  ; hp : int
  ; ptype : poketype
  }

let _ = { name = "charizard"; hp = 78; ptype = Fire }
let _ = { name = "squirtle"; hp = 44; ptype = Water }

let safe_hd = function
  | [] -> None
  | hd :: _ -> Some hd
;;

let safe_tl = function
  | [] -> None
  | _ :: tl -> Some tl
;;

let rec max_hp = function
  | [] -> None
  | poke1 :: t ->
    (match max_hp t with
     | None -> Some poke1
     | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2))
;;

type date = int * int * int

let is_before (date1 : date) (date2 : date) =
  let y1, m1, d1 = date1 in
  let y2, m2, d2 = date2 in
  if y1 < y2
  then true
  else if y1 > y2
  then false
  else if m1 < m2
  then true
  else if m1 > m2
  then false
  else if d1 < d2
  then true
  else if d1 > d2
  then false
  else false
;;

let rec earlist = function
  | [] -> None
  | d1 :: tl ->
    (match earlist tl with
     | None -> Some d1
     | Some d2 -> Some (if is_before d1 d2 then d1 else d2))
;;

let insert k v d = (k, v) :: d

let rec lookup k = function
  | [] -> None
  | (k', v) :: tl -> if k = k' then Some v else lookup k tl
;;

let d = [] |> insert 1 "one" |> insert 2 "two" |> insert 3 "three"
let v = lookup 2 d

type suit =
  | Clubs
  | Hearts
  | Diamonds
  | Spades

(* type rank = int *)
type rank =
  | Number of int
  | Ace
  | Jack
  | Queen
  | King

type card =
  { rank : rank
  ; suit : suit
  }

let a = { rank = Ace; suit = Clubs }
let b = { rank = Queen; suit = Hearts }
let c = { rank = Number 2; suit = Diamonds }
let d = { rank = Number 7; suit = Spades }

type quad =
  | I
  | II
  | III
  | IV

type sign =
  | Neg
  | Zero
  | Pos

let sign x = if x > 0 then Pos else if x < 0 then Neg else Zero

let quadrant : int * int -> quad option =
  fun (x, y) ->
  match sign x, sign y with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None
;;

let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None
;;

type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let t =
  Node
    ( 4
    , Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))
    , Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )
;;

let rec depth = function
  | Leaf -> 0
  | Node (_, left, right) -> 1 + max (depth left) (depth right)
;;

let _ = depth t

let rec same_shape tree1 tree2 =
  match tree1, tree2 with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _ -> false
;;

let list_max =
  let rec list_max_safe x = function
    | [] -> x
    | hd :: tl -> list_max_safe (max x hd) tl
  in
  function
  | [] -> failwith "list_max"
  | hd :: tl -> list_max_safe hd tl
;;

let list_max_string =
  let rec list_max_safe x = function
    | [] -> string_of_int x
    | hd :: tl -> list_max_safe (max x hd) tl
  in
  function
  | [] -> "empty"
  | hd :: tl -> list_max_safe hd tl
;;

let sign x = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

let quadrant (x, y) =
  match sign x, sign y with
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IV
  | _ -> None
;;
