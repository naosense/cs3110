(* What is the type and value of each of the following OCaml expressions? 7 * (1 + 2 + 3)
   "CS " ^ string_of_int 3110 *)
let () = print_endline (string_of_int (3 * (1 + 2 + 3)))
let () = print_endline ("CS " ^ string_of_int 3110)
let _ = 42 * 10
let _ = 3.14 *. 2.0
let _ = 4.2 ** 7.0
let res = if 2 > 1 then 42 else 7
let double x = x * 2
let _ = assert (double 7 = 14)
let _ = assert (double 14 <> 14)
let _ = assert (double 0 = 0)
let cube x = x *. x *. x
let _ = assert (cube 1. <> 1.2)
let _ = assert (cube 2. = 8.)
let sign x = if x > 0 then 1 else if x = 0 then 0 else -1
let _ = assert (sign 10 = 1)
let _ = assert (sign 0 = 0)
let _ = assert (sign (-1) = -1)
let area r = Float.pi *. (r ** 2.)
let close_enough x y = Float.abs (x -. y) < 1e-5
let _ = assert (close_enough (area 1.) 3.1415926)
let rms x y = Float.sqrt (((x ** 2.) +. (y ** 2.)) /. 2.)
let _ = assert (close_enough (rms 2. 2.) 2.)
let _ = assert (close_enough (rms 7. 42.) 30.10813)

let valid_date d m =
  if m = "Jan"
     || m = "Mar"
     || m = "May"
     || m = "Jul"
     || m = "Aug"
     || m = "Oct"
     || m = "Dec"
  then 1 <= d && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sept" || m = "Nov"
  then 1 <= d && d <= 30
  else if m = "Feb"
  then 1 <= d && d <= 28
  else false
;;

let rec fib n = if n = 0 then 0 else if n = 1 then 1 else fib (n - 1) + fib (n - 2)

let fib_fast n =
  let rec h n pp p = if n = 1 then p else h (n - 1) p (pp + p) in
  if n = 0 then 0 else h n 0 1
;;

let f x = if x then x else x
let g x y = if y then x else x
let h x y z = if x then y else z
let i x y _z = if x then y else y
let divide ~numerator ~denominator = numerator /. denominator
let _ = divide ~denominator:2. ~numerator:3.
