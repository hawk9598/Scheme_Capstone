open Ast
open Unparser
open Primitives

exception Error of string

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
  and b4 = (candidate [Pair(Closure
                              (fun vs ->
                                begin
                                  match vs with
                                  |v :: [] -> v
                                  |_ ->
                                    raise
                                      (Error
                                         (Printf.sprintf
                                            "Incorrect argument count in call %s"
                                            (show_list show_exp_val vs)))
                                end),
                            Closure
                              (fun vs ->
                                begin
                                  match vs with
                                  |v :: [] ->
                                    begin
                                      match v with
                                      |Pair(_,_) ->
                                        Boolean true
                                      |_ ->
                                        Boolean false
                                    end
                                  |_ ->
                                    raise
                                      (Error
                                         (Printf.sprintf
                                            "Incorrect argument count in call %s"
                                            (show_list show_exp_val vs)))
                                end))] = Boolean true)
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
  and b12 = (candidate [Closure
                          (fun vs ->
                            begin
                              match vs with
                              |v :: [] -> v
                              |_ ->
                                raise
                                  (Error
                                     (Printf.sprintf
                                        "Incorrect argument count in call %s"
                                        (show_list show_exp_val vs)))
                            end)] = Boolean false)
  and b13 = (candidate [Primitive internal_cdr] = Boolean false)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13;;

(* let internal_is_pair_error candidate = *)
  
assert (test_internal_is_pair internal_is_pair);;
(* test_internal_is_pair_error internal_is_pair; *)


                                                 
                                                 
