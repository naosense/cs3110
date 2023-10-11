open OUnit2
open Sum
open Chapter03

let make_sum_test name expected_output input =
  name >:: fun _ -> assert_equal expected_output (sum input) ~printer:string_of_int
;;

let tests =
  "test suite for sum"
  >::: [ make_sum_test "empty" 0 []
       ; make_sum_test "singeton" 2 [ 1 ]
       ; make_sum_test "two elements" 3 [ 1; 2 ]
       ]
;;

type day =
  | Sunday
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday

let next_weekday d = failwith "Unimplemented"

let tests =
  "test suite for next_weekday"
  >::: [ ("tue_after_mon" >:: fun _ -> assert_equal Tuesday (next_weekday Monday)) ]
;;

(* let _ = run_test_tt_main tests *)
let _ = product [ 1; 2; 3 ] |> print_int
