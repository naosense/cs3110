(** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)

let ( -- ) i j =
  let rec from i j lst = if i > j then lst else from i (j - 1) (j :: lst) in
  if i <= j then from i j [] else from j i [] |> List.rev
;;
