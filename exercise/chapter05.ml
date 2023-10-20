module type ComplexSig = sig
  type t

  val zero : t
  val add : t -> t -> t
end

module Complex : ComplexSig = struct
  type t = float * float

  let zero = 0., 0.
  let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
end

(* remove zero: The value `zero' is required but not provided *)
(* remove add: The value `add' is required but not provided*)
(* change zero: Values do not match: val zero : int * int is not included in val zero : t
   The type int * int is not compatible with the type t = float * float
   Type int is not compatible with type float*)

module type Queue = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val front : 'a t -> 'a
  val dequeue : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end

module ListQueue : Queue = struct
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false
  ;;

  let enqueue x q = q @ [ x ]

  let front = function
    | [] -> raise Empty
    | x :: _ -> x
  ;;

  let dequeue = function
    | [] -> raise Empty
    | _ :: q -> q
  ;;

  let size = List.length
  let to_list = Fun.id
end

module BatchedQueue : Queue = struct
  type 'a t =
    { o : 'a list
    ; i : 'a list
    }

  exception Empty

  let empty = { o = []; i = [] }

  let is_empty = function
    | { o = []; _ } -> true
    | _ -> false
  ;;

  let enqueue x = function
    | { o = []; _ } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }
  ;;

  let front = function
    | { o = []; _ } -> raise Empty
    | { o = h :: _; _ } -> h
  ;;

  let dequeue = function
    | { o = []; _ } -> raise Empty
    | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> { o = t; i }
  ;;

  let size { o; i } = List.(length o + length i)
  let to_list { o; i } = o @ List.rev i
end

let _ = ListQueue.empty |> ListQueue.enqueue 1 |> ListQueue.enqueue 2 |> ListQueue.to_list

let _ =
  BatchedQueue.empty
  |> BatchedQueue.enqueue 1
  |> BatchedQueue.enqueue 2
  |> BatchedQueue.to_list
;;

module type Map = sig
  type ('k, 'v) t

  val empty : ('k, 'v) t
  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  val lookup : 'k -> ('k, 'v) t -> 'v
  val bindings : ('k, 'v) t -> ('k * 'v) list
end

module BstMap : Map = struct
  type 'a tree =
    | Leaf
    | Node of 'a * 'a tree * 'a tree

  type ('k, 'v) t = ('k * 'v) tree

  let empty = Leaf

  let rec insert k v = function
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) ->
      if k < k'
      then Node ((k', v'), insert k v l, r)
      else if k > k'
      then Node ((k', v'), l, insert k v r)
      else Node ((k, v), l, r)
  ;;

  let rec lookup k = function
    | Leaf -> failwith "Not found"
    | Node ((k', v'), l, r) ->
      if k < k' then lookup k l else if k > k' then lookup k r else v'
  ;;

  let rec bindings = function
    | Leaf -> []
    | Node ((k, v), l, r) -> (k, v) :: (bindings l @ bindings r)
  ;;
end

let _ = BstMap.(empty |> insert 1 2 |> insert 2 3 |> insert 1 5 |> bindings)

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list

  let empty = []
  let insert k v m = (k, v) :: m
  let lookup k m = List.assoc k m
  let keys m = List.(m |> map fst |> sort_uniq Stdlib.compare)
  let bindings m = m |> keys |> List.map (fun k -> k, lookup k m)
end

let _ = AssocListMap.(empty |> insert 1 "a" |> insert 2 "b" |> bindings)

module type Fraction = sig
  type t

  val make : int -> int -> t
  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end

module Frac : Fraction = struct
  type t = int * int

  let make a b =
    assert (b != 0);
    a, b
  ;;

  let numerator (a, _) = a
  let denominator (_, b) = b
  let to_string (a, b) = string_of_int a ^ " / " ^ string_of_int b
  let to_float (a, b) = float_of_int a /. float_of_int b
  let add (a1, b1) (a2, b2) = (a1 * b2) + (a2 * b1), b1 * b2
  let mul (a1, b1) (a2, b2) = a1 * a2, b1 * b2
end

module CharMap = Map.Make (Char)

let cm =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra" |> add 'V' "Victor")
;;

(* let v_for_E = CharMap.find 'E' *)
let remove_A = CharMap.remove 'A' cm
let _ = CharMap.mem 'A' remove_A
let _ = CharMap.bindings remove_A

module type T = sig
  type t

  val x : t
end

module Pair1 (M : T) = struct
  let p = M.x, 1
end

module P0 = Pair1 (struct
    type t = int

    let x = 0
  end)

module P1 = Pair1 (struct
    type t = char

    let x = 'a'
  end)

type date =
  { month : int
  ; day : int
  }

module Date = struct
  type t = date

  let compare { month = m1; day = d1 } { month = m2; day = d2 } =
    if m1 = m2 then d1 - d2 else m1 - m2
  ;;
end

module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let calendars =
  DateMap.(
    empty
    |> add { month = 5; day = 21 } "birthday"
    |> add { month = 5; day = 1 } "labor day"
    |> add { month = 10; day = 1 } "national day"
    |> add { month = 6; day = 1 } "children day")
;;

let print_calendar =
  DateMap.iter (fun date event -> Printf.printf "%s\t%d-%d\n" event date.month date.day)
;;

let _ = print_calendar calendars
let is_for = CharMap.mapi (fun k v -> String.make 1 k ^ " is for " ^ v)
let _ = is_for cm |> CharMap.bindings
let first_after calendar date = DateMap.find_first (fun d -> d > date) calendar |> snd
let _ = first_after calendars { month = 5; day = 1 }
(* let _ = first_after calendars { month = 12; day = 1 } *)

module CaseInsensitive = struct
  type t = string

  let compare s1 s2 =
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
  ;;
end

module CaseInsSet = Set.Make (CaseInsensitive)

let _ = CaseInsSet.(empty |> add "haha" |> add "heiehi" |> add "HAHA" |> elements)

module type ToString = sig
  type t

  val to_string : t -> string
end

module Print (M : ToString) = struct
  let print x = print_endline (M.to_string x)
end

module Int = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (Int)

let _ = PrintInt.print 1

module MyString = struct
  type t = string

  let to_string = Fun.id
end

module PrintString = Print (MyString)

let _ = PrintString.print "hello string"

module StringWithPrint = struct
  include String
  include Print (MyString)
end

let _ = StringWithPrint.print "ah"
let _ = StringWithPrint.length "hah"
