open Ast
open Unparser
open Primitives

exception Error of string

let identity =
  fun vs ->
  (begin
      match vs with
      |v :: [] -> v
      |_ ->
        raise
          (Error
             (Printf.sprintf
                "Incorrect argument count in call %s"
                (show_list show_exp_val vs)))
    end)
(* Unit tests for pair list primitive functions *)

let test_internal_is_pair candidate =
  let b0 = (candidate [Pair(Int 5,
                            Int 6)] = Boolean true)
  and b1 = (candidate [Pair(Boolean true,
                            Boolean false)] = Boolean true)
  and b2 = (candidate [Pair(String "yes",
                            String "no")] = Boolean true)
  and b3 = (candidate [Pair(Character 'c',
                            Character '5')] = Boolean true)
  and b4 = (candidate [Pair(Closure identity,
                            Closure internal_is_pair)] = Boolean true)
  and b5 = (candidate [Pair (Primitive internal_cons,
                             Primitive internal_car)] = Boolean true)
  and b6 = (candidate [Pair(Null,
                            Null)] = Boolean true)
  and b7 = (candidate [Pair(Pair(Int 6,
                                 Boolean false),
                            Character 'y')] = Boolean true)
  and b8 = (candidate [Int 10] = Boolean false)
  and b9 = (candidate [Boolean false] = Boolean false)
  and b10 = (candidate [String "no"] = Boolean false)
  and b11 = (candidate [Character '1'] = Boolean false)
  and b12 = (candidate [Closure identity] = Boolean false)
  and b13 = (candidate [Primitive internal_cdr] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13;;

(* let test_internal_is_pair_error candidate = *)
  
assert (test_internal_is_pair internal_is_pair);;
(* test_internal_is_pair_error internal_is_pair; *)


let test_internal_cons candidate =
  let b0 = (candidate [Int 5; Int 6] = Pair (Int 5,
                                             Int 6))
  and b1 = (candidate [Boolean true; Boolean false] = Pair (Boolean true,
                                                            Boolean false))
  and b2 = (candidate [String "yes"; String "no"] = Pair (String "yes",
                                                          String "no"))
  and b3 = (candidate [Character 'c'; Character '5'] = Pair(Character 'c',
                                                            Character '5'))
  and b4 = (candidate [Pair(Pair(Int 5,
                                 Int 6),
                            Boolean true);
                       Int 5] = Pair(Pair(Pair(Int 5,
                                               Int 6),
                                          Boolean true),
                                     Int 5))
  and b5 = (candidate [Int 5; Null] = Pair(Int 5,
                                           Null))
  in b0 && b1 && b2 && b3 && b4 && b5;;

                                        
(* let test_internal_is_cons_error candidate = *)
  
assert (test_internal_cons internal_cons);;
(* test_internal_cons internal_cons; *)

let test_internal_car candidate =
  let b0 = (candidate [Pair(Int 5,
                            Int 6)] = Int 5)
  and b1 = (candidate [Pair(Boolean false,
                            Boolean true)] = Boolean false)
  and b2 = (candidate [Pair(String "yes",
                            String "no")] = String "yes")
  and b3 = (candidate [Pair(Character 'c',
                            Character '5')] = Character 'c')
  and b4 = (candidate [Pair(Null,
                            Int 5)] = Null)
  and b5 = (candidate [Pair(Pair(Int 5,
                                 Int 6),
                            String "cdr")] = Pair(Int 5,
                                                  Int 6))
  in b0 && b1 && b2 && b3 && b4 && b5;;
                                                 
(* let test_internal_car_error candidate = *)

assert(test_internal_car internal_car);;
(* test_internal_car_error internal_car; *)

let test_internal_cdr candidate =
  let b0 = (candidate [Pair(Int 5,
                            Int 6)] = Int 6)
  and b1 = (candidate [Pair(Boolean false,
                            Boolean true)] = Boolean true)
  and b2 = (candidate [Pair(String "yes",
                            String "no")] = String "no")
  and b3 = (candidate [Pair(Character 'c',
                            Character '5')] = Character '5')
  and b4 = (candidate [Pair(Int 5,
                            Null)] = Null)
  and b5 = (candidate [Pair(String "cdr",
                            Pair(Int 5,
                                 Int 6))] = Pair(Int 5,
                                                 Int 6))
  in b0 && b1 && b2 && b3 && b4 && b5;;
                                                 
(* let test_internal_cdr_error candidate = *)

assert(test_internal_cdr internal_cdr);;
 (* test_internal_cdr_error internal_cdr; *) 

let test_internal_is_int candidate =
  let b0 = (candidate [Int 5] = Boolean true)
  and b1 = (candidate [Int 0] = Boolean true)
  and b2 = (candidate [Int (-1)] = Boolean true)
  and b3 = (candidate [Boolean true] = Boolean false)
  and b4 = (candidate [String "no"] = Boolean false)
  and b5 = (candidate [Character '1'] = Boolean false)
  and b6 = (candidate [Closure identity] = Boolean false)
  and b7 = (candidate [Primitive internal_is_pair] = Boolean false)
  and b8 = (candidate [Pair(Int 5,
                            Int 6)] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

(* let test_internal_is_int_error candidate = *)

assert(test_internal_is_int internal_is_int);;
(* test_internal_is_int_error is_int ;*)

let test_internal_is_zero candidate =
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 5] = Boolean false)
  and b2 = (candidate [Int (-5)] = Boolean false)
  in b0 && b1 && b2;;

(* let test_internal_is_zero_error candidate *)

assert(test_internal_is_zero internal_is_zero);;
(* test_internal_is_zero_error internal_is_zero; *)

let test_internal_is_positive candidate =
  let b0 = (candidate [Int 5] = Boolean true)
  and b1 = (candidate [Int 10] = Boolean true)
  and b2 = (candidate [Int 0] = Boolean false)
  and b3 = (candidate [Int (-1)] = Boolean false)
  and b4 = (candidate [Int (-10)] = Boolean false)
  in b0 && b1 && b2 && b3 && b4;;


(* let test_internal_is_positive_error candidate *)

assert(test_internal_is_positive internal_is_positive);;
(* test_internal_is_positive_error internal_is_positive ;*)

let test_internal_is_negative candidate =
  let b0 = (candidate [Int 5] = Boolean false)
  and b1 = (candidate [Int 10] = Boolean false)
  and b2 = (candidate [Int 0] = Boolean false)
  and b3 = (candidate [Int (-1)] = Boolean true)
  and b4 = (candidate [Int (-10)] = Boolean true)
  in b0 && b1 && b2 && b3 && b4;;

(* let test_internal_is_negative_error candidate *)

assert(test_internal_is_negative internal_is_negative);;
(* test_internal_is_negative_error internal_is_negative ;*)

let test_internal_is_even candidate =
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 2] = Boolean true)
  and b2 = (candidate [Int 40296] = Boolean true)
  and b3 = (candidate [Int (-2)] = Boolean true)
  and b4 = (candidate [Int (-20148)] = Boolean true)
  and b5 = (candidate [Int 1] = Boolean false)
  and b6 = (candidate [Int 3] = Boolean false)
  and b7 = (candidate [Int (-19)] = Boolean false)
  and b8 = (candidate [Int (-20149)] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

(* let test_internal_is_even_error candidate *)

assert(test_internal_is_even internal_is_even);;
(* test_internal_is_even_error internal_is_even ;*)

let test_internal_is_odd candidate =
  let b0 = (candidate [Int 0] = Boolean false)
  and b1 = (candidate [Int 2] = Boolean false)
  and b2 = (candidate [Int 40296] = Boolean false)
  and b3 = (candidate [Int (-2)] = Boolean false)
  and b4 = (candidate [Int (-20148)] = Boolean false)
  and b5 = (candidate [Int 1] = Boolean true)
  and b6 = (candidate [Int 3] = Boolean true)
  and b7 = (candidate [Int (-19)] = Boolean true)
  and b8 = (candidate [Int (-20149)] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

(* let test_internal_is_odd_error candidate *)

assert(test_internal_is_odd internal_is_odd);;
(* test_internal_is_odd_error internal_is_odd ;*)

let test_internal_add candidate =
  (* Test base case and only for positive integers *)
  let b0 = (candidate [] = Int 0)
  and b1 = (candidate [Int 1] = Int 1)
  and b2 = (candidate [Int 1;Int 3] = Int 4)
  and b3 = (candidate [Int 1;Int 3;Int 5;Int 7;Int 9;Int 11;Int 13;Int 15]
            = Int 64)
  and b4 = (candidate [Int 2000;Int 1978;Int 154;Int 131;Int 12] = Int 4275)
  (* Test for only negative integers *)
  and b5 = (candidate [Int (-1)] = Int (-1))
  and b6 = (candidate [Int (-1);Int (-3)] = Int (-4))
  and b7 = (candidate [Int (-1);Int (-3);Int (-5);Int (-7);Int (-9);Int (-11);Int (-13);Int (-15)]
            = Int (-64))
  and b8 = (candidate [Int (-2000);Int (-1978);Int (-154);Int (-131);Int (-12)]
            = Int (-4275))
  (* Test for mixed positive and negative integers *)
  and b9 = (candidate [Int 1;Int (-1)] = Int 0)
  and b10 = (candidate [Int 1;Int 2;Int 3;Int 4;Int 5;Int (-6);Int (-7);Int (-8);Int (-9)]
             = Int (-15))
  and b11 = (candidate [Int 1000;Int (-2000);Int 145;Int (-230)] = Int (-1085))
  and b12 = (candidate [Int 1000;Int (-200);Int (-145);Int (-5);Int 200] = Int 850)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12;;

(* let test_internal_add_error candidate *)

assert(test_internal_add internal_add);;
(* test_internal_add_error internal_add;*)

let test_internal_sub candidate =
  (* Test for only positive integers *)
  let b0 = (candidate [Int 1] = Int (-1))
  and b1 = (candidate [Int 1;Int 3] = Int (-2))
  and b2 = (candidate [Int 1;Int 3;Int 5;Int 7;Int 9;Int 11;Int 13;Int 15]
            = Int (-62))
  and b3 = (candidate [Int 2000;Int 1978;Int 154;Int 131;Int 12] = Int (-275))
  (* Test for only negative integers *)
  and b4 = (candidate [Int (-1)] = Int 1)
  and b5 = (candidate [Int (-1);Int (-3)] = Int 2)
  and b6 = (candidate [Int (-1);Int (-3);Int (-5);Int (-7);Int (-9);Int (-11);Int (-13);Int (-15)]
            = Int 62)
  and b7 = (candidate [Int (-2000);Int (-1978);Int (-154);Int (-131);Int (-12)]
            = Int 275)
  (* Test for mixed positive and negative integers *)
  and b8 = (candidate [Int 1;Int (-1)] = Int 2)
  and b9 = (candidate [Int 1;Int 2;Int 3;Int 4;Int 5;Int (-6);Int (-7);Int (-8);Int (-9)]
             = Int 17)
  and b10 = (candidate [Int 1000;Int (-2000);Int 145;Int (-230)] = Int 3085)
  and b11 = (candidate [Int 1000;Int (-200);Int (-145);Int (-5);Int 200] = Int 1150)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;;

(* let test_internal_sub_error candidate *)

assert(test_internal_sub internal_sub);;
(* test_internal_sub_error internal_sub;*)

let test_internal_mul candidate =
  (* Test base case and only for positive integers *)
  let b0 = (candidate [] = Int 1)
  and b1 = (candidate [Int 1] = Int 1)
  and b2 = (candidate [Int 1;Int 3] = Int 3)
  and b3 = (candidate [Int 1;Int 3;Int 5;Int 7;Int 9;Int 11;Int 13;Int 15]
            = Int 2027025)
  and b4 = (candidate [Int 20;Int 19;Int 5;Int 13;Int 2] = Int 49400)
  (* Test for only negative integers *)
  and b5 = (candidate [Int (-1)] = Int (-1))
  and b6 = (candidate [Int (-1);Int (-3)] = Int 3)
  and b7 = (candidate [Int (-1);Int (-3);Int (-5);Int (-7);Int (-9);Int (-11);Int (-13);Int (-15)]
            = Int 2027025)
  and b8 = (candidate [Int (-20);Int (-19);Int (-5);Int (-13);Int (-2)]
            = Int (-49400))
  (* Test for mixed positive and negative integers *)
  and b9 = (candidate [Int 1;Int (-1)] = Int (-1))
  and b10 = (candidate [Int 1;Int 2;Int 3;Int 4;Int 5;Int (-6);Int (-7);Int (-8);Int (-9)]
             = Int 362880)
  and b11 = (candidate [Int 10;Int (-20);Int 14;Int (-23)] = Int 64400)
  and b12 = (candidate [Int 100;Int (-2);Int (-14);Int (-5);Int 2] = Int (-28000))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12;;

(* let test_internal_mul_error candidate *)

assert(test_internal_mul internal_mul);;
(* test_internal_mul_error internal_mul;*)

let test_internal_div candidate =
  (* Test for the case where there is only 1 integer *)
  let b0 = (candidate [Int 1] = Int 1)
  and b1 = (candidate [Int 3] = Int (1/3))
  and b2 = (candidate [Int (-1)] = Int (-1))
  and b3 = (candidate [Int (-3)] = Int(-1/3))
  (* Test for the case where there are only positive integers *)
  and b4 = (candidate [Int 120; Int 5] = Int 24)
  and b5 = (candidate [Int 120; Int 6; Int 4] = Int 5)
  and b6 = (candidate [Int 256; Int 2; Int 4; Int 16] = Int 2)
  and b7 = (candidate [Int 96000; Int 48; Int 2] = Int 1000)
  and b8 = (candidate [Int 107; Int 2] = Int (107/2))
  and b9 = (candidate [Int 107; Int 2; Int 3] = Int (107/6))
  (* Test for the case where there are only negative integers *)
  and b10 = (candidate [Int (-120); Int (-5)] = Int 24)
  and b11 = (candidate [Int (-120); Int (-6); Int (-4)] = Int (-5))
  and b12 = (candidate [Int (-256); Int (-2); Int (-4); Int (-16)] = Int 2)
  and b13 = (candidate [Int (-96000); Int (-48); Int (-2)] = Int (-1000))
  and b14 = (candidate [Int (-107); Int (-2)] = Int (107/2))
  and b15 = (candidate [Int (-107); Int (-2); Int (-3)] = Int (-107/6))
  (* Test for the case where there are both positive and negative integers *)
  and b16 = (candidate [Int 120; Int (-5)] = Int (-24))
  and b17 = (candidate [Int 960; Int (-24); Int 4] = Int (-10))
  and b18 = (candidate [Int 107; Int (-2); Int 3] = Int (-107/6))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18;;

(* let test_internal_div_error candidate *)

assert(test_internal_div internal_div);;
(* test_internal_div_error internal_div;*)

let test_internal_quotient candidate =
  (* Test for case of positive integers with no remainder *)
  let b0 = (candidate [Int 10; Int 5] = Int 2)
  and b1 = (candidate [Int 24; Int 6] = Int 4)
  and b2 = (candidate [Int 4096; Int 16] = Int 256)
  (* Test for the case of negative integers with no remainder *)
  and b3 = (candidate [Int (-10); Int (-2)] = Int 5)
  and b4 = (candidate [Int (-24); Int (-6)] = Int 4)
  and b5 = (candidate [Int (-4096); Int (-16)] = Int 256)
  (* Test for mixed case *)
  and b6 = (candidate [Int 10; Int (-5)] = Int (-2))
  and b7 = (candidate [Int (-24); Int 6] = Int (-4))
  and b8 = (candidate [Int 4096; Int (-16)] = Int (-256))
  (* Test for cases where there are remainders *)
  and b9 = (candidate [Int 10; Int 3] = Int (10/3))
  and b10 = (candidate [Int 25; Int 4] = Int (25/4))
  and b11 = (candidate [Int (-10); Int (-3)] = Int (10/3))
  and b12 = (candidate [Int (-25); Int (-4)] = Int (25/4))
  and b13 = (candidate [Int 10; Int (-3)] = Int (-10/3))
  and b14 = (candidate [Int (-25); Int 4] = Int (-25/4))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(* let test_internal_quotient_error candidate *)

assert(test_internal_quotient internal_quotient);;
(* test_internal_quotient_error internal_quotient;*)

let test_internal_remainder candidate =
  (* Testing for positive integers only with remainder *)
  let b0 = (candidate [Int 10; Int 3] = Int 1)
  and b1 = (candidate [Int 29; Int 5] = Int 4)
  and b2 = (candidate [Int 109; Int 19] = Int 14)
  and b3 = (candidate [Int 659; Int 7] = Int 1)
  (* Testing for negative integers only with remainder *)
  and b4 = (candidate [Int (-10); Int (-3)] = Int (-1))
  and b5 = (candidate [Int (-29); Int (-5)] = Int (-4))
  and b6 = (candidate [Int (-109); Int (-19)] = Int (-14))
  and b7 = (candidate [Int (-659); Int (-7)] = Int (-1))
  (* Testing for mixed with remainder *)
  and b8 = (candidate [Int (-10); Int 3] = Int (-1))
  and b9 = (candidate [Int 29; Int (-5)] = Int 4)
  and b10 = (candidate [Int (-109); Int 19] = Int (-14))
  and b11 = (candidate [Int 659; Int (-7)] = Int 1)
  (* Testing for case of positive integers with no remainder *)
  and b12 = (candidate [Int 10; Int 5] = Int 0)
  and b13 = (candidate [Int 24; Int 6] = Int 0)
  and b14 = (candidate [Int 4096; Int 16] = Int 0)
  (* Test for the case of negative integers with no remainder *)
  and b15 = (candidate [Int (-10); Int (-2)] = Int 0)
  and b16 = (candidate [Int (-24); Int (-6)] = Int 0)
  and b17 = (candidate [Int (-4096); Int (-16)] = Int 0)
  (* Test for mixed case with no remainder *)
  and b18 = (candidate [Int 10; Int (-5)] = Int 0)
  and b19 = (candidate [Int (-24); Int 6] = Int 0)
  and b20 = (candidate [Int 4096; Int (-16)] = Int 0)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20;;

(* let test_internal_remainder_error candidate *)

assert(test_internal_remainder internal_remainder);;
(* test_internal_remainder_error internal_remainder;*)

let test_internal_exponentiation candidate =
  (* When both index is 0 for base case *)
  let b0 = (candidate [Int 2; Int 0] = Int 1)
  and b1 = (candidate [Int 1000; Int 0] = Int 1)
  and b2 = (candidate [Int (-2); Int 0] = Int 1)
  and b3 = (candidate [Int (-1000); Int 0] = Int 1)
  (* When both base and index are positive *)
  and b4 = (candidate [Int 2; Int 5] = Int 32)
  and b5 = (candidate [Int 7; Int 6] = Int 117649)
  and b6 = (candidate [Int 10; Int 5] = Int 100000)
  and b7 = (candidate [Int 3; Int 12] = Int 531441)
  (* When index is negative and base is positive *)
  and b8 = (candidate [Int 2; Int (-5)] = Int (1/32))
  and b9 = (candidate [Int 7; Int (-6)] = Int (1/117649))
  and b10 = (candidate [Int 10; Int (-5)] = Int (1/100000))
  and b11 = (candidate [Int 3; Int (-12)] = Int (1/531441))
  (* When index is positive and base is negative *)
  and b12 = (candidate [Int (-2); Int 5] = Int (-32))
  and b13 = (candidate [Int (-7); Int 6] = Int 117649)
  and b14 = (candidate [Int (-10); Int 5] = Int (-100000))
  and b15 = (candidate [Int (-3); Int 12] = Int 531441)
  (* When both base and index are negative *)
  and b16 = (candidate [Int (-2); Int (-5)] = Int (-1/32))
  and b17 = (candidate [Int (-7); Int (-6)] = Int (1/117649))
  and b18 = (candidate [Int (-10); Int (-5)] = Int (-1/100000))
  and b19 = (candidate [Int (-3); Int (-12)] = Int (1/531441))
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19;;

(* let test_internal_exponentiation_error candidate *)

assert(test_internal_exponentiation internal_exponentiation);;
(* test_internal_exponentiation_error internal_exponentiation;*)

let test_internal_lt candidate =
  (* Test for one element only *)
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 1000] = Boolean true)
  and b2 = (candidate [Int (-1000)] = Boolean true)
  (* Test for more than 1 element, all positive *)
  and b3 = (candidate [Int 0; Int 1; Int 2; Int 3] = Boolean true)
  and b4 = (candidate [Int 1; Int 10; Int 1000] = Boolean true)
  and b5 = (candidate [Int 1; Int 10; Int 9] = Boolean false)
  and b6 = (candidate [Int 1; Int 1; Int 2] = Boolean false)
  (* Test for more than 1 element, all negative *)
  and b7 = (candidate [Int (-1000); Int (-500); Int (-1)] = Boolean true)
  and b8 = (candidate [Int (-3); Int (-2); Int (-1)] = Boolean true)
  and b9 = (candidate [Int (-1000); Int (-500); Int (-600)] = Boolean false)
  and b10 = (candidate [Int (-3); Int (-3); Int (-1)] = Boolean false)
  (* Test for more than 1 element, mixed *)
  and b11 = (candidate [Int (-1000); Int 0; Int 1] = Boolean true)
  and b12 = (candidate [Int (-500); Int (-200); Int 50; Int 1000] = Boolean true)
  and b13 = (candidate [Int (-5); Int (-2); Int 5; Int 2] = Boolean false)
  and b14 = (candidate [Int (-5); Int (-2); Int 5; Int 5] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(* let test_internal_lt_error candidate *)

assert(test_internal_lt internal_lt);;
(* test_internal_lt_error internal_lt;*)

let test_internal_lte candidate =
  (* Test for one element only *)
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 1000] = Boolean true)
  and b2 = (candidate [Int (-1000)] = Boolean true)
  (* Test for more than 1 element, all positive *)
  and b3 = (candidate [Int 0; Int 1; Int 2; Int 3] = Boolean true)
  and b4 = (candidate [Int 1; Int 10; Int 1000] = Boolean true)
  and b5 = (candidate [Int 1; Int 10; Int 9] = Boolean false)
  and b6 = (candidate [Int 1; Int 1; Int 2; Int 3] = Boolean true)
  (* Test for more than 1 element, all negative *)
  and b7 = (candidate [Int (-1000); Int (-500); Int (-1)] = Boolean true)
  and b8 = (candidate [Int (-3); Int (-2); Int (-1)] = Boolean true)
  and b9 = (candidate [Int (-1000); Int (-500); Int (-600)] = Boolean false)
  and b10 = (candidate [Int (-3); Int (-3); Int (-2); Int (-1)] = Boolean true)
  (* Test for more than 1 element, mixed *)
  and b11 = (candidate [Int (-1000); Int 0; Int 1] = Boolean true)
  and b12 = (candidate [Int (-500); Int (-200); Int 50; Int 1000] = Boolean true)
  and b13 = (candidate [Int (-5); Int (-2); Int 5; Int 2] = Boolean false)
  and b14 = (candidate [Int (-5); Int (-2); Int 3; Int 5; Int 5] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(* let test_internal_lte_error candidate *)

assert(test_internal_lte internal_lte);;
(* test_internal_lte_error internal_lte;*)

let test_internal_gt candidate =
  (* Test for one element only *)
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 1000] = Boolean true)
  and b2 = (candidate [Int (-1000)] = Boolean true)
  (* Test for more than 1 element, all positive *)
  and b3 = (candidate [Int 0; Int 1; Int 2; Int 3] = Boolean false)
  and b4 = (candidate [Int 10; Int 9; Int 9] = Boolean false)
  and b5 = (candidate [Int 11; Int 10; Int 9; Int 8] = Boolean true)
  and b6 = (candidate [Int 1100; Int 165; Int 1; Int 0] = Boolean true)
  (* Test for more than 1 element, all negative *)
  and b7 = (candidate [Int (-12); Int (-11); Int (-10)] = Boolean false)
  and b8 = (candidate [Int (-15); Int (-15); Int (-19); Int (-30)] = Boolean false)
  and b9 = (candidate [Int (-200); Int (-500); Int (-700); Int (-800)] = Boolean true)
  and b10 = (candidate [Int (-3); Int (-4); Int (-5)] = Boolean true)
  (* Test for more than 1 element, mixed *)
  and b11 = (candidate  [Int 1500; Int 20; Int (-5); Int (-5)] = Boolean false)
  and b12 = (candidate [Int (-500); Int (-200); Int 50; Int 1000] = Boolean false)
  and b13 = (candidate [Int 1500; Int 20; Int (-5); Int (-20)] = Boolean true)
  and b14 = (candidate [Int 10; Int 5; Int (-5); Int (-10)] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(* let test_internal_gt_error candidate *)

assert(test_internal_gt internal_gt);;
(* test_internal_gt_error internal_gt;*)

let test_internal_gte candidate =
  (* Test for one element only *)
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int 1000] = Boolean true)
  and b2 = (candidate [Int (-1000)] = Boolean true)
  (* Test for more than 1 element, all positive *)
  and b3 = (candidate [Int 0; Int 1; Int 2; Int 3] = Boolean false)
  and b4 = (candidate [Int 10; Int 9; Int 9] = Boolean true)
  and b5 = (candidate [Int 11; Int 10; Int 9; Int 8] = Boolean true)
  and b6 = (candidate [Int 1100; Int 165; Int 1; Int 0] = Boolean true)
  (* Test for more than 1 element, all negative *)
  and b7 = (candidate [Int (-12); Int (-11); Int (-10)] = Boolean false)
  and b8 = (candidate [Int (-15); Int (-15); Int (-19); Int (-30)] = Boolean true)
  and b9 = (candidate [Int (-200); Int (-500); Int (-700); Int (-800)] = Boolean true)
  and b10 = (candidate [Int (-3); Int (-4); Int (-5)] = Boolean true)
  (* Test for more than 1 element, mixed *)
  and b11 = (candidate  [Int 1500; Int 20; Int (-5); Int (-5)] = Boolean true)
  and b12 = (candidate [Int (-500); Int (-200); Int 50; Int 1000] = Boolean false)
  and b13 = (candidate [Int 1500; Int 20; Int (-5); Int (-20)] = Boolean true)
  and b14 = (candidate [Int 10; Int 5; Int (-5); Int (-10)] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14;;

(* let test_internal_gte_error candidate *)

assert(test_internal_gte internal_gte);;
(* test_internal_gte_error internal_gte;*)

let test_internal_equal candidate =
  (* Test for only one element *)
  let b0 = (candidate [Int 0] = Boolean true)
  and b1 = (candidate [Int (-100)] = Boolean true)
  and b2 = (candidate [Int 100] = Boolean true)
  (* Test for more than 1 element, positive int only *)
  and b3 = (candidate [Int 1; Int 1; Int 1] = Boolean true)
  and b4 = (candidate [Int 100; Int 100; Int 100; Int 100] = Boolean true)
  and b5 = (candidate [Int 1; Int 1; Int 2; Int 1] = Boolean false)
  and b6 = (candidate [Int 100 ; Int 90; Int 80; Int 70] = Boolean false)
  (* Test for more than 1 element, negative int only *)
  and b7 = (candidate [Int (-1); Int (-1); Int (-1); Int (-1)] = Boolean true)
  and b8 = (candidate [Int (-100); Int (-100); Int (-100); Int (-100)] = Boolean true)
  and b9 = (candidate [Int (-1); Int (-1); Int (-1); Int (-2)] = Boolean false)
  and b10 = (candidate [Int (-100); Int (-10); Int (-1); Int (-100)] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10;;

(* let test_internal_equal_error candidate *)

assert(test_internal_equal internal_equal);;
(* test_internal_equal_error internal_equal;*)

let test_internal_is_char candidate =
  let b0 = (candidate [Character 'c'] = Boolean true)
  and b1 = (candidate [Character '0'] = Boolean true)
  and b2 = (candidate [Character 'H'] = Boolean true)
  and b3 = (candidate [Character '/'] = Boolean true)
  and b4 = (candidate [Character '~'] = Boolean true)
  and b5 = (candidate [Int 5] = Boolean false)
  and b6 = (candidate [Boolean true] = Boolean false)
  and b7 = (candidate [String "Hello"] = Boolean false)
  and b8 = (candidate [String "c"] = Boolean false)
  and b9 = (candidate [Closure identity] = Boolean false)
  and b10 = (candidate [Primitive internal_is_pair] = Boolean false)
  and b11 = (candidate [Pair (Character 'a',
                              Character 'b')] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;;

(* let test_internal_is_char_error candidate *)

assert(test_internal_is_char internal_is_char);;
(* test_internal_is_char_error internal_is_char;*)

let test_internal_char_equal candidate =
  (* Test for single argument case *)
  let b0 = (candidate [Character 'a'] = Boolean true)
  and b1 = (candidate [Character '5'] = Boolean true)
  and b2 = (candidate [Character 'N'] = Boolean true)
  (* Test for multiple arguments case *)
  and b3 = (candidate [Character 'a'; Character 'a'; Character 'a' ] = Boolean true)
  and b4 = (candidate [Character '5'; Character '5'; Character '5'] = Boolean true)
  and b5 = (candidate [Character 'N'; Character 'N'; Character 'N' ] = Boolean true)
  and b6 = (candidate [Character 'a'; Character 'a'; Character 'b'] = Boolean false)
  and b7 = (candidate [Character '5'; Character '6'; Character '5'] = Boolean false)
  and b8 = (candidate [Character 'M'; Character 'N'; Character 'N'] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

(* let test_internal_char_equal_error candidate *)

assert(test_internal_char_equal internal_char_equal);;
(* test_internal_char_equal_error internal_char_equal;*)

let test_internal_char_gt candidate =
  (* Test for single argument case *)
  let b0 = (candidate [Character 'c'] = Boolean true)
  and b1 = (candidate [Character '8'] = Boolean true)
  and b2 = (candidate [Character 'H'] = Boolean true)
  (* Test for more than 1 argument case for true cases. *)
  and b3 = (candidate [Character 'z'; Character 'x'; Character 'p';
                       Character 'l'; Character 'd'; Character 'a'] = Boolean true)
  and b4 = (candidate [Character 'a'; Character 'Z'; Character 'Y';
                       Character 'G'; Character 'B'] = Boolean true)
  and b5 = (candidate [Character 'a'; Character 'V'; Character 'A';
                       Character '8'; Character '2'] = Boolean true)
  and b6 = (candidate [Character 'Z'; Character '9'; Character '6';
                       Character '3'; Character '0'] = Boolean true)
  and b7 = (candidate [Character '9'; Character '6'; Character '3';
                       Character '2'; Character '0'] = Boolean true)
  (* Test for more than 1 argument case for false cases. *)
  and b8 = (candidate [Character 'Z'; Character 'c'; Character 'a'] = Boolean false)
  and b9 = (candidate [Character '9'; Character 'A'; Character 'B'] = Boolean false)
  and b10 = (candidate [Character '0'; Character '9'; Character 'A';
                        Character 'G'; Character 'Z'; Character 'b';
                        Character 'z'] = Boolean false)
  and b11 = (candidate [Character 'z'; Character 'x'; Character 'p';
                        Character 'l'; Character 'l'; Character 'a'] = Boolean false)
  and b12 = (candidate [Character 'a'; Character 'V'; Character 'A';
                        Character 'A'; Character '2'] = Boolean false)
  and b13 = (candidate [Character '9'; Character '6'; Character '3';
                        Character '3'; Character '0'] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13;;
                                                           
(* let test_internal_char_gt_error candidate *)

assert(test_internal_char_gt internal_char_gt);;
(* test_internal_char_gt_error internal_char_gt;*)

let test_internal_char_ge candidate =
  (* Test for single argument case *)
  let b0 = (candidate [Character 'c'] = Boolean true)
  and b1 = (candidate [Character '8'] = Boolean true)
  and b2 = (candidate [Character 'H'] = Boolean true)
  (* Test for more than 1 argument case for true cases. *)
  and b3 = (candidate [Character 'z'; Character 'x'; Character 'p';
                       Character 'p'; Character 'd'; Character 'a'] = Boolean true)
  and b4 = (candidate [Character 'a'; Character 'Z'; Character 'Y';
                       Character 'G'; Character 'B'] = Boolean true)
  and b5 = (candidate [Character 'a'; Character 'A'; Character 'A';
                       Character 'A'; Character '2'] = Boolean true)
  and b6 = (candidate [Character 'Z'; Character '9'; Character '6';
                       Character '3'; Character '0'] = Boolean true)
  and b7 = (candidate [Character '9'; Character '9'; Character '3';
                       Character '3'; Character '0'] = Boolean true)
  (* Test for more than 1 argument case for false cases. *)
  and b8 = (candidate [Character 'Z'; Character 'c'; Character 'a'] = Boolean false)
  and b9 = (candidate [Character '9'; Character 'A'; Character 'B'] = Boolean false)
  and b10 = (candidate [Character '0'; Character '9'; Character 'A';
                        Character 'G'; Character 'Z'; Character 'b';
                        Character 'z'] = Boolean false)
  and b11 = (candidate [Character '0'; Character '9'; Character 'A';
                        Character 'G'; Character 'b'; Character 'b';
                        Character 'z'] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;;
                                                           
(* let test_internal_char_ge_error candidate *)

assert(test_internal_char_ge internal_char_ge);;
(* test_internal_char_ge_error internal_char_ge;*)

let test_internal_char_lt candidate =
  (* Test for single argument case *)
  let b0 = (candidate [Character 'c'] = Boolean true)
  and b1 = (candidate [Character '8'] = Boolean true)
  and b2 = (candidate [Character 'H'] = Boolean true)
  (* Test for more than 1 argument case for true cases *)
  and b3 = (candidate [Character '0'; Character '3'; Character '5';
                       Character '8'; Character '9'] = Boolean true)
  and b4 = (candidate [Character '7'; Character '9'; Character 'C';
                       Character 'H'; Character 'Z'] = Boolean true)
  and b5 = (candidate [Character 'Y'; Character 'Z'; Character 'a';
                       Character 'g'; Character 'y'] = Boolean true)
  and b6 = (candidate [Character 'a'; Character 'c'; Character 'g';
                       Character 'u'; Character 'z'] = Boolean true)
  (* Test for more than 1 argument case for false cases *)
  and b7 = (candidate [Character '9'; Character '5'; Character '2';
                       Character '0'] = Boolean false)
  and b8 = (candidate [Character 'Z'; Character 'G'; Character 'A';
                       Character '9'; Character '2'] = Boolean false)
  and b9 = (candidate [Character 'y'; Character 'f'; Character 'a';
                       Character 'Z'; Character 'D'] = Boolean false)
  and b10 = (candidate [Character '0'; Character '3'; Character '5';
                        Character '5'; Character '9'] = Boolean false)
  and b11 = (candidate [Character 'Y'; Character 'Z'; Character 'g';
                        Character 'g'; Character 'y'] = Boolean false)
  and b12 = (candidate [Character 'a'; Character 'c'; Character 'g';
                        Character 'g'; Character 'z'] = Boolean false)
  in  b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12;;

(* let test_internal_char_lt_error candidate *)

assert(test_internal_char_lt internal_char_lt);;
(* test_internal_char_lt_error internal_char_lt;*)

let test_internal_char_le candidate =
  (* Test for single argument case *)
  let b0 = (candidate [Character 'c'] = Boolean true)
  and b1 = (candidate [Character '8'] = Boolean true)
  and b2 = (candidate [Character 'H'] = Boolean true)
  (* Test for more than 1 argument case for true cases *)
  and b3 = (candidate [Character '0'; Character '3'; Character '5';
                       Character '8'; Character '9'] = Boolean true)
  and b4 = (candidate [Character '7'; Character '9'; Character 'C';
                       Character 'H'; Character 'Z'] = Boolean true)
  and b5 = (candidate [Character 'C'; Character 'Y'; Character 'a';
                       Character 'g'; Character 'y'] = Boolean true)
  and b6 = (candidate [Character 'a'; Character 'c'; Character 'g';
                       Character 'u'; Character 'z'] = Boolean true)
  and b7 = (candidate [Character '0'; Character '3'; Character '5';
                       Character '5'; Character '9'] = Boolean true)
  and b8 = (candidate [Character 'Y'; Character 'Z'; Character 'g';
                        Character 'g'; Character 'y'] = Boolean true)
  and b9 = (candidate [Character 'a'; Character 'c'; Character 'g';
                        Character 'g'; Character 'z'] = Boolean true)
  (* Test for more than 1 argument case for false cases *)
  and b10 = (candidate [Character '9'; Character '5'; Character '2';
                       Character '0'] = Boolean false)
  and b11 = (candidate [Character 'Z'; Character 'G'; Character 'A';
                       Character '9'; Character '2'] = Boolean false)
  and b12 = (candidate [Character 'y'; Character 'f'; Character 'a';
                        Character 'Z'; Character 'D'] = Boolean false)
  and b13 = (candidate [Character '9'; Character '5'; Character '5';
                       Character '0'] = Boolean false)
  and b14 = (candidate [Character 'Z'; Character 'G'; Character 'G';
                       Character '9'; Character '2'] = Boolean false)
  and b15 = (candidate [Character 'y'; Character 'f'; Character 'f';
                       Character 'Z'; Character 'D'] = Boolean false)
  in  b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15;;

(* let test_internal_char_le_error candidate *)

assert(test_internal_char_le internal_char_le);;
(* test_internal_char_le_error internal_char_le;*)

let test_internal_char_numeric candidate =
  let b0 = (candidate [Character '0'] = Boolean true)
  and b1 = (candidate [Character '1'] = Boolean true)
  and b2 = (candidate [Character '2'] = Boolean true)
  and b3 = (candidate [Character '3'] = Boolean true)
  and b4 = (candidate [Character '4'] = Boolean true)
  and b5 = (candidate [Character '5'] = Boolean true)
  and b6 = (candidate [Character '6'] = Boolean true)
  and b7 = (candidate [Character '7'] = Boolean true)
  and b8 = (candidate [Character '8'] = Boolean true)
  and b9 = (candidate [Character '9'] = Boolean true)
  and b10 = (candidate [Character 'a'] = Boolean false)
  and b11 = (candidate [Character 'g'] = Boolean false)
  and b12 = (candidate [Character 'J'] = Boolean false)
  and b13 = (candidate [Character 'Y'] = Boolean false)
  and b14 = (candidate [Character '['] = Boolean false)
  and b15 = (candidate [Character '/'] = Boolean false)
  and b16 = (candidate [Character '+'] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16;;

(* let test_internal_char_numeric_error candidate *)

assert(test_internal_char_numeric internal_char_numeric);;
(* test_internal_char_numeric_error internal_char_numeric;*)

let test_internal_char_alphabetic_lower_case candidate =
  let b0 = (candidate [Character 'a'] = Boolean true)
  and b1 = (candidate [Character 'b'] = Boolean true)
  and b2 = (candidate [Character 'c'] = Boolean true)
  and b3 = (candidate [Character 'd'] = Boolean true)
  and b4 = (candidate [Character 'e'] = Boolean true)
  and b5 = (candidate [Character 'f'] = Boolean true)
  and b6 = (candidate [Character 'g'] = Boolean true)
  and b7 = (candidate [Character 'h'] = Boolean true)
  and b8 = (candidate [Character 'i'] = Boolean true)
  and b9 = (candidate [Character 'j'] = Boolean true)
  and b10 = (candidate [Character 'k'] = Boolean true)
  and b11 = (candidate [Character 'l'] = Boolean true)
  and b12 = (candidate [Character 'm'] = Boolean true)
  and b13 = (candidate [Character 'n'] = Boolean true)
  and b14 = (candidate [Character 'o'] = Boolean true)
  and b15 = (candidate [Character 'p'] = Boolean true)
  and b16 = (candidate [Character 'q'] = Boolean true)
  and b17 = (candidate [Character 'r'] = Boolean true)
  and b18 = (candidate [Character 's'] = Boolean true)
  and b19 = (candidate [Character 't'] = Boolean true)
  and b20 = (candidate [Character 'u'] = Boolean true)
  and b21 = (candidate [Character 'v'] = Boolean true)
  and b22 = (candidate [Character 'w'] = Boolean true)
  and b23 = (candidate [Character 'x'] = Boolean true)
  and b24 = (candidate [Character 'y'] = Boolean true)
  and b25 = (candidate [Character 'z'] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22 && b23 && b24 && b25;;

let test_internal_char_alphabetic_upper_case candidate =
  let b0 = (candidate [Character 'A'] = Boolean true)
  and b1 = (candidate [Character 'B'] = Boolean true)
  and b2 = (candidate [Character 'C'] = Boolean true)
  and b3 = (candidate [Character 'D'] = Boolean true)
  and b4 = (candidate [Character 'E'] = Boolean true)
  and b5 = (candidate [Character 'F'] = Boolean true)
  and b6 = (candidate [Character 'G'] = Boolean true)
  and b7 = (candidate [Character 'H'] = Boolean true)
  and b8 = (candidate [Character 'I'] = Boolean true)
  and b9 = (candidate [Character 'J'] = Boolean true)
  and b10 = (candidate [Character 'K'] = Boolean true)
  and b11 = (candidate [Character 'L'] = Boolean true)
  and b12 = (candidate [Character 'M'] = Boolean true)
  and b13 = (candidate [Character 'N'] = Boolean true)
  and b14 = (candidate [Character 'O'] = Boolean true)
  and b15 = (candidate [Character 'P'] = Boolean true)
  and b16 = (candidate [Character 'Q'] = Boolean true)
  and b17 = (candidate [Character 'R'] = Boolean true)
  and b18 = (candidate [Character 'S'] = Boolean true)
  and b19 = (candidate [Character 'T'] = Boolean true)
  and b20 = (candidate [Character 'U'] = Boolean true)
  and b21 = (candidate [Character 'V'] = Boolean true)
  and b22 = (candidate [Character 'W'] = Boolean true)
  and b23 = (candidate [Character 'X'] = Boolean true)
  and b24 = (candidate [Character 'Y'] = Boolean true)
  and b25 = (candidate [Character 'Z'] = Boolean true)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22 && b23 && b24 && b25;;

let test_internal_char_alphabetic_false candidate =
  let b0 = (candidate [Character '0'] = Boolean false)
  and b1 = (candidate [Character '1'] = Boolean false)
  and b2 = (candidate [Character '2'] = Boolean false)
  and b3 = (candidate [Character '3'] = Boolean false)
  and b4 = (candidate [Character '4'] = Boolean false)
  and b5 = (candidate [Character '5'] = Boolean false)
  and b6 = (candidate [Character '6'] = Boolean false)
  and b7 = (candidate [Character '7'] = Boolean false)
  and b8 = (candidate [Character '8'] = Boolean false)
  and b9 = (candidate [Character '9'] = Boolean false)
  and b10 = (candidate [Character '['] = Boolean false)
  and b11 = (candidate [Character ']'] = Boolean false)
  and b12 = (candidate [Character '+'] = Boolean false)
  and b13 = (candidate [Character '-'] = Boolean false)
  and b14 = (candidate [Character '/'] = Boolean false)
  and b15 = (candidate [Character ':'] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15;;

(* let test_internal_char_alphabetic_error candidate *)

assert(test_internal_char_alphabetic_lower_case internal_char_alphabetic);;
assert(test_internal_char_alphabetic_upper_case internal_char_alphabetic);;
assert(test_internal_char_alphabetic_false internal_char_alphabetic);;
(* test_internal_char_lt_error internal_char_lt;*)

let test_internal_is_str candidate =
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "g"] = Boolean true)
  and b2 = (candidate [String "hi"] = Boolean true)
  and b3 = (candidate [String "hello"] = Boolean true)
  and b4 = (candidate [String "test_primitives"] = Boolean true)
  and b5 = (candidate [String "This is a test for primitives.ml"] = Boolean true)
  and b6 = (candidate [String "HELLO WORLD."] = Boolean true)
  and b7 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  and b8 = (candidate [Int 5] = Boolean false)
  and b9 = (candidate [Int 100000] = Boolean false)
  and b10 = (candidate [Boolean false] = Boolean false)
  and b11 = (candidate [Boolean true] = Boolean false)
  and b12 = (candidate [Character 'a'] = Boolean false)
  and b13 = (candidate [Character 'G'] = Boolean false)
  and b14 = (candidate [Character '8'] = Boolean false)
  and b15 = (candidate [Character '['] = Boolean false)
  and b16 = (candidate [Pair(String "pair",
                             String "of strings")] = Boolean false)
  and b17 = (candidate [Pair(Int 5,
                             Int 6)] = Boolean false)
  and b18 = (candidate [Closure identity] = Boolean false)
  and b19 = (candidate [Primitive internal_is_str] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19;;

(* let test_internal_is_str_error candidate = *)

assert(test_internal_is_str internal_is_str);;
(* test_internal_is_str_error internal_is_str ;*)

let test_internal_str_equal candidate =
  (* Test for single argument case *)
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "hello"] = Boolean true)
  and b2 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  (* Test for more than one argument case, true *)
  and b3 = (candidate [String "A"; String "A"; String "A";
                       String "A"] = Boolean true)
  and b4 = (candidate [String "hello"; String "hello"; String "hello";
                       String "hello"] = Boolean true)
  and b5 = (candidate [String "This is a test for primitives.ml"; String "This is a test for primitives.ml"; String "This is a test for primitives.ml"] = Boolean true)
  (* Test for more than one argument case, false *)
  and b6 = (candidate [String "A"; String "A"; String "B";
                       String "A"] = Boolean false)
  and b7 = (candidate [String "hello"; String "world"; String "world";
                       String "world"] = Boolean false)
  and b8 = (candidate [String "This is a test for primitives.ml"; String "Thi is a test for primitives.ml"; String "This is a test for primitives.ml"] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8;;

(* let test_internal_str_equal_error candidate = *)

assert(test_internal_str_equal internal_str_equal);;
(* test_internal_str_equal_error internal_str_equal ;*)
                                                         
let test_internal_str_gt candidate =
  (* test for single argument case *)
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "hello"] = Boolean true)
  and b2 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  (* test for more than single argument case, true *)
  and b3 = (candidate [String "g"; String "d"; String "c";
                       String "a"] = Boolean true)
  and b4 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "0ello"] = Boolean true)
  and b5 = (candidate [String "z"; String "hello"; String "even though i am long my first character is smaller than h"] = Boolean true)
  and b6 = (candidate [String "hello"; String "hell"; String "helL";
                       String "hel"] = Boolean true)
  (* test for more than single argument case, false *)
  and b7 = (candidate [String "g"; String "d"; String "d";
                       String "a"] = Boolean false)
  and b8 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "9ello"] = Boolean false)
  and b9 = (candidate [String "a"; String "b"; String "c";
                       String "d"] = Boolean false)
  and b10 = (candidate [String "9ello"; String "Hello"; String "hello";
                        String "yello"] = Boolean false)
  and b11 = (candidate [String "yellow world"; String "zest"; String "zestria";
                        String "zestrian"] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11;;
        
 (* let test_internal_str_gt_error candidate = *)

assert(test_internal_str_gt internal_str_gt);;
(* test_internal_str_gt_error internal_str_gt ;*) 

let test_internal_str_ge candidate =
  (* test for single argument case *)
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "hello"] = Boolean true)
  and b2 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  (* test for more than single argument case, true *)
  and b3 = (candidate [String "g"; String "d"; String "c";
                       String "a"] = Boolean true)
  and b4 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "0ello"] = Boolean true)
  and b5 = (candidate [String "z"; String "hello"; String "even though i am long my first character is smaller than h"] = Boolean true)
  and b6 = (candidate [String "hello"; String "hell"; String "helL";
                       String "hel"] = Boolean true)
  and b7 = (candidate [String "g"; String "d"; String "d";
                       String "a"] = Boolean true)
  and b8 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "9ello"] = Boolean true)
  and b9 = (candidate [String "z"; String "hello"; String "hello";
                       String "even though i am long my first character is smaller than h"] = Boolean true)
  and b10 = (candidate [String "hell"; String "hell"; String "helL";
                       String "hel"] = Boolean true)
  (* test for more than single argument case, false *)
  and b11 = (candidate [String "a"; String "b"; String "c";
                       String "d"] = Boolean false)
  and b12 = (candidate [String "9ello"; String "Hello"; String "hello";
                        String "yello"] = Boolean false)
  and b13 = (candidate [String "yellow world"; String "zest"; String "zestria";
                        String "zestrian"] = Boolean false)
  and b14 = (candidate [String "a"; String "b"; String "b";
                        String "d"] = Boolean false)
  and b15 = (candidate [String "9ello"; String "9ello"; String "hello";
                        String "yello"] = Boolean false)
  and b16 = (candidate [String "yellow world"; String "zest"; String "zest";
                        String "zestrian"] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16;;
        
 (* let test_internal_str_ge_error candidate = *)

assert(test_internal_str_ge internal_str_ge);;
(* test_internal_str_ge_error internal_str_ge ;*) 

let test_internal_str_lt candidate =
  (* test for single argument case *)
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "hello"] = Boolean true)
  and b2 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  (* test for more than single argument case, true *)
  and b3 = (candidate [String "a"; String "b"; String "c";
                       String "d"] = Boolean true)
  and b4 = (candidate [String "9ello"; String "Hello"; String "hello";
                        String "yello"] = Boolean true)
  and b5 = (candidate [String "yellow world"; String "zest"; String "zestria";
                       String "zestrian"] = Boolean true)
  and b6 = (candidate [String "astroNauTical"; String "bistro"; String "cat";
                       String "d"] = Boolean true)
  (* test for more than single argument case, false *)
  and b7 = (candidate [String "g"; String "d"; String "c";
                       String "a"] = Boolean false)
  and b8 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "0ello"] = Boolean false)
  and b9 = (candidate [String "z"; String "hello"; String "even though i am long my first character is smaller than h"] = Boolean false)
  and b10 = (candidate [String "hello"; String "hell"; String "helL";
                       String "hel"] = Boolean false)
  and b11 = (candidate [String "g"; String "d"; String "d";
                       String "a"] = Boolean false)
  and b12 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "9ello"] = Boolean false)
  and b13 = (candidate [String "z"; String "hello"; String "hello";
                       String "even though i am long my first character is smaller than h"] = Boolean false)
  and b14 = (candidate [String "hell"; String "hell"; String "helL";
                        String "hel"] = Boolean false)
  and b15 = (candidate [String "a"; String "b"; String "b";
                       String "d"] = Boolean false)
  and b16 = (candidate [String "9ello"; String "9ello"; String "Hello";
                        String "yello"] = Boolean false)
  and b17 = (candidate [String "yellow world"; String "zest"; String "zest";
                       String "zestrian"] = Boolean false)
  and b18 = (candidate [String "astroNauTical"; String "cat"; String "cat";
                       String "d"] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18;;

(* let test_internal_str_lt_error candidate = *)

assert(test_internal_str_lt internal_str_lt);;
(* test_internal_str_lt_error internal_str_lt ;*) 

let test_internal_str_le candidate =
  (* test for single argument case *)
  let b0 = (candidate [String "a"] = Boolean true)
  and b1 = (candidate [String "hello"] = Boolean true)
  and b2 = (candidate [String "Hello! Mixed Upper and Lower case characters with symbols !?!"] = Boolean true)
  (* test for more than single argument case, true *)
  and b3 = (candidate [String "a"; String "b"; String "c";
                       String "d"] = Boolean true)
  and b4 = (candidate [String "9ello"; String "Hello"; String "hello";
                        String "yello"] = Boolean true)
  and b5 = (candidate [String "yellow world"; String "zest"; String "zestria";
                       String "zestrian"] = Boolean true)
  and b6 = (candidate [String "astroNauTical"; String "bistro"; String "cat";
                       String "d"] = Boolean true)
  and b7 = (candidate [String "a"; String "b"; String "b";
                       String "d"] = Boolean true)
  and b8 = (candidate [String "9ello"; String "9ello"; String "Hello";
                        String "yello"] = Boolean true)
  and b9 = (candidate [String "yellow world"; String "zest"; String "zest";
                       String "zestrian"] = Boolean true)
  and b10 = (candidate [String "astroNauTical"; String "cat"; String "cat";
                        String "d"] = Boolean true)
  (* test for more than single argument case, false *)
  and b11 = (candidate [String "g"; String "d"; String "c";
                       String "a"] = Boolean false)
  and b12 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "0ello"] = Boolean false)
  and b13 = (candidate [String "z"; String "hello"; String "even though i am long my first character is smaller than h"] = Boolean false)
  and b14 = (candidate [String "hello"; String "hell"; String "helL";
                        String "hel"] = Boolean false)
  and b15 = (candidate [String "g"; String "d"; String "d";
                       String "a"] = Boolean false)
  and b16 = (candidate [String "hello"; String "Hello"; String "9ello";
                       String "9ello"] = Boolean false)
  and b17 = (candidate [String "z"; String "hello"; String "hello";
                       String "even though i am long my first character is smaller than h"] = Boolean false)
  and b18 = (candidate [String "hell"; String "hell"; String "helL";
                       String "hel"] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18;;

(* let test_internal_str_le_error candidate = *)

assert(test_internal_str_le internal_str_le);;
(* test_internal_str_le_error internal_str_le ;*) 

let test_internal_str_length candidate =
  let b0 = (candidate [String ""] = Int 0)
  and b1 = (candidate [String "y"] = Int 1)
  and b2 = (candidate [String "hello"] = Int 5)
  and b3 = (candidate [String "hello world!"] = Int 12)
  and b4 = (candidate [String "this is test_primitives.ml!"] = Int 27)
  and b5 = (candidate [String " hello"] = Int 6)
  and b6 = (candidate [String "/////"] = Int 5)
  and b7 = (candidate [String "[1;2;3;4;5;6]"] = Int 13)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7;;
              
(* let test_internal_str_length_error candidate = *)

assert(test_internal_str_length internal_str_length);;
(* test_internal_str_length_error internal_str_length ;*)

let test_internal_char_to_str candidate =
  let b0 = (candidate [Character 'a'] = String "a")
  and b1 = (candidate [Character 'h'; Character 'e'; Character 'l';
                       Character 'l'; Character 'o'] = String "hello")
  and b2 = (candidate [Character 'H'; Character 'E'; Character 'L';
                       Character 'L'; Character 'O'; Character 'w';
                       Character 'o'; Character 'r'; Character 'l';
                       Character 'd'] = String "HELLOworld")
  and b3 = (candidate [Character 'p'; Character 'r'; Character 'i';
                       Character 'm'; Character 'e'; Character '7';
                       Character '1'; Character '!'] = String "prime71!")
  and b4 = (candidate [Character 'H'; Character 'a'; Character 'w';
                       Character 'k'; Character '9'; Character '5';
                       Character '9'; Character '8'; Character '!'] = String "Hawk9598!")
  in b0 && b1 && b2 && b3 && b4;;

(* let test_internal_char_to_str_error candidate = *)

assert(test_internal_char_to_str internal_char_to_str);;
(* test_internal_char_to_str_error internal_char_to_str ;*)