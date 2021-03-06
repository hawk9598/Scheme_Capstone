open Ast
open Interpreter
open Interpreter_essentials

exception Not_implemented_yet
exception Not_expected
        
(* Testing the eval function on parsed Scheme expressions *)
        
let test_eval_Scheme_base_values candidate =
  (* Test on integers *)
  let b0 = (candidate (get_final_parsed_exp "1")
              default_empty_alist = Int 1)
  and b1 = (candidate (get_final_parsed_exp "1000")
              default_empty_alist = Int 1000)
  and b2 = (candidate (get_final_parsed_exp "-1000")
              default_empty_alist = Int (-1000))
  and b3 = (candidate (get_final_parsed_exp "-2")
              default_empty_alist = Int (-2))
  and b4 = (candidate (get_final_parsed_exp "0")
              default_empty_alist = Int 0)
  (* Test on booleans *)
  and b5 = (candidate (get_final_parsed_exp "#f")
              default_empty_alist = Boolean false)
  and b6 = (candidate (get_final_parsed_exp "#t")
              default_empty_alist = Boolean true)
  (* Test on characters *)
  and b7 = (candidate (get_final_parsed_exp "#\\A")
              default_empty_alist = Character 'A')
  and b8 = (candidate (get_final_parsed_exp "#\\z")
              default_empty_alist = Character 'z')
  and b9 = (candidate (get_final_parsed_exp "#\\0")
              default_empty_alist = Character '0')
  and b10= (candidate (get_final_parsed_exp "#\\9")
              default_empty_alist = Character '9') 
  and b11 = (candidate (get_final_parsed_exp "\"string\"")
               default_empty_alist = String "\"string\"")
  and b12 = (candidate (get_final_parsed_exp "\"hawk9598\"")
               default_empty_alist = String "\"hawk9598\"")
  and b13 = (candidate (get_final_parsed_exp "\"capstone_2021!\"")
               default_empty_alist = String "\"capstone_2021!\"")
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13;;


let test_eval_Scheme_non_base_values candidate =
  (* Test on if else expressions *)
  let b0 = (candidate (get_final_parsed_exp "(if #t #\\Y 0)")
              default_empty_alist  = Character 'Y')
  and b1 = (candidate (get_final_parsed_exp "(if #f -100 #\\n)")
              default_empty_alist = Character 'n')
  and b2 = (candidate (get_final_parsed_exp "(if (= 5 5) -100 #\\n)")
              default_empty_alist = Int (-100))
  and b3 = (candidate (get_final_parsed_exp "(if #t (+ 5 3 1 2 4) -10)")
              default_empty_alist = Int 15)
  and b4 = (candidate (get_final_parsed_exp "(if -5 #\\Y (+ 1 2 3))")
              default_empty_alist = Character 'Y')
  and b5 = (candidate (get_final_parsed_exp "(if (= 5 1) (+ 1 3) (* 4 2 3))")
              default_empty_alist = Int 24)
  (* Test on expressions that apply Variables *)
  and b6 = (candidate (get_final_parsed_exp "(+ 5 3 1 2 40)")
              default_empty_alist = Int 51)
  and b7 = (candidate (get_final_parsed_exp "(integer? 5)")
              default_empty_alist = Boolean true)
  and b8 = (candidate (get_final_parsed_exp "(car '(1 2))")
              default_empty_alist = Int 1)
  and b9 = (candidate (get_final_parsed_exp "(car (cdr '(1 2)))")
              default_empty_alist = Int 2)
  and b10 = (candidate (get_final_parsed_exp "(/ 1000 200 5)")
               default_empty_alist = Int 1)
  and b11 = (candidate (get_final_parsed_exp "(expt 2 5)")
               default_empty_alist = Int 32)
  and b12 = (candidate (get_final_parsed_exp "(char? #\\Y)")
               default_empty_alist = Boolean true)
  and b13 = (candidate (get_final_parsed_exp "(string-ref \"string\" 1)")
               default_empty_alist = Character 's')
  (* Test on let expressions *)
  and b14 = (candidate (get_final_parsed_exp "(let ((five 5)) (+ five 3))")
               default_empty_alist = Int 8)
  and b15 = (candidate (get_final_parsed_exp "(let ((x #t)(y #f)) (if x #\\t y))")
               default_empty_alist = Character 't')
  and b16 = (candidate (get_final_parsed_exp "(let ((small_num 1)(big_num 1000)(more_than_hundred? (lambda (x)(if (> x 100) #t #f)))) (if (= 5 5 5)(more_than_hundred? small_num)(more_than_hundred? big_num)))") default_empty_alist = Boolean false)
  (* Test on letrec expressions *)
  and b17 = (candidate (get_final_parsed_exp "(letrec ((factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1)))))))(factorial 10))")
               default_empty_alist = Int 3628800)
  and b18 = (candidate (get_final_parsed_exp "(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1))))) (odd (lambda (n) (if (= n 0) #f (even (- n 1)))))) (even 5))")
               default_empty_alist = Boolean false)
  and b19 = (candidate (get_final_parsed_exp "(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1))))) (odd (lambda (n) (if (= n 0) #f (even (- n 1)))))) (odd 11))")
               default_empty_alist = Boolean true)
  (* Test on quote expressions *)
  and b20 = (candidate (get_final_parsed_exp "'a")
               default_empty_alist = Symbol "a")
  and b21 = (candidate (get_final_parsed_exp "'#\\a")
               default_empty_alist = Character 'a')
  and b22 = (candidate (get_final_parsed_exp "'(if #t 5 3)")
               default_empty_alist = Pair (Symbol "if",
                                           Pair (Boolean true,
                                                 Pair (Int 5,
                                                       Pair (Int 3,
                                                             Null)))))
  and b23 = (candidate (get_final_parsed_exp "'(let ((six 6)(seven 7)(compare <))(compare six seven))")
               default_empty_alist = Pair (Symbol "let",
                                           Pair (Pair (Pair (Symbol "six",
                                                             Pair (Int 6,
                                                                   Null)),
                                                       Pair (Pair (Symbol "seven",
                                                                   Pair (Int 7,
                                                                         Null)),
                                                             Pair (Pair (Symbol "compare",
                                                                         Pair (Symbol "<",
                                                                               Null)),
                                                                   Null))),
                                                 Pair (Pair (Symbol "compare",
                                                             Pair (Symbol "six",
                                                                   Pair (Symbol "seven",
                                                                         Null))),
                                                       Null))))
  and b24 = (candidate (get_final_parsed_exp "'(letrec ((factorial (lambda (x) (if (= x 0) 1 (* x (factorial (- x 1)))))))(factorial x))")
               default_empty_alist =
               Pair (Symbol "letrec",
                     Pair (Pair (Pair (Symbol "factorial",
                                       Pair (Pair (Symbol "lambda",
                                                   Pair (Pair (Symbol "x",
                                                               Null),
                                                         Pair (Pair (Symbol "if",
                                                                     Pair (Pair (Symbol "=",
                                                                                 Pair (Symbol "x",
                                                                                       Pair (Int 0,
                                                                                             Null))),
                                                                           Pair (Int 1,
                                                                                 Pair (Pair (Symbol "*",
                                                                                             Pair (Symbol "x",
                                                                                                   Pair (Pair (Symbol "factorial",
                                                                                                               Pair (Pair (Symbol "-",
                                                                                                                           Pair (Symbol "x",
                                                                                                                                 Pair (Int 1,
                                                                                                                                       Null))),
                                                                                                                     Null)),
                                                                                                         Null))),
                                                                                       Null)))),
                                                               Null))),
                                             Null)),
                                 Null),
                           Pair (Pair (Symbol "factorial",
                                       Pair (Symbol "x", Null)),
                                 Null))))
  (* Test on expressions that apply lambda functions *)
  and b25 = (candidate (get_final_parsed_exp "((lambda (x) (+ x 1)) 2)")
               default_empty_alist = Int 3)
  and b26 = (candidate (get_final_parsed_exp "((lambda (x y) (+ x y)) 3 4)")
               default_empty_alist = Int 7)
  and b27 = (candidate (get_final_parsed_exp "((lambda x (car x)) 1 2 3)")
               default_empty_alist = Int 1)
  and b28 = (candidate (get_final_parsed_exp "((lambda (x . y) (+ x (car y))) 1 2 3)")
               default_empty_alist = Int 3)
  in b0 && b1 && b2 && b3 && b4 && b5 && b6 && b7 && b8 && b9 && b10 && b11 && b12 && b13 && b14 && b15 && b16 && b17 && b18 && b19 && b20 && b21 && b22 && b23 && b24 && b25 && b26 && b27 && b28;;
  
assert (test_eval_Scheme_base_values eval);;
assert (test_eval_Scheme_non_base_values eval);;
