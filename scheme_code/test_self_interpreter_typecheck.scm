;;; test_self_interpreter_typecheck.scm

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/self_interpreter_typecheck.scm")

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/file_parser.scm")
;;; Defining tests for the interpreter and its auxiliary functions

;;; Defining tests for the interpreter

;;; normal version
(define test
  (lambda (g)
    (begin
      ;;; Testing for base values
      (unless (equal? (interpret 0 g) 0)
	      (printf "failed: (interpret 0 ~s)\n" g))
      (unless (equal? (interpret '#f g) #f)
	      (printf "failed: (interpret '#f ~s)\n" g))
      (unless (equal? (interpret '#\f g) #\f)
	      (printf "failed: (interpret '#\f ~s)\n" g))
      (unless (equal? (interpret "scheme_capstone" g) "scheme_capstone")
	      (printf "failed: (interpret \"scheme_capstone\" ~s)\n" g))
      (unless (equal? (interpret '() g) '())
	      (printf "failed: (interpret '() ~s)\n" g))
      ;;; Testing for if expressions
      (unless (equal? (interpret '(if #t 1 2) g) 1)
	      (printf "failed: (interpret '(if #t 1 2) ~s)\n" g))
      (unless (equal? (interpret '(if #f 1 2) g) 2)
	      (printf "failed: (interpret '(if #f 1 2) ~s)\n" g))
      (unless (equal? (interpret '(if 0 1 2) g) 1)
	      (printf "failed: (interpret '(if 0 1 2) ~s)\n" g))
      (unless (equal? (interpret '((lambda (x) x) 1) g) 1)
	      (printf "failed: (interpret '((lambda (x) x) 1) ~s)\n" g))
      ;;; Testing for lambda expressions
      (unless (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "failed: (interpret '((lambda (x y) x) 1 2) ~s)\n" g))
      (unless (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "failed: (interpret ((lambda (x y) x) 1 2) ~s)\n" g))
      (unless (equal? (interpret '((lambda (x y) y) 1 2) g) 2)
	      (printf "failed: (interpret ((lambda (x y) y) 1 2) ~s)\n" g))
      (unless (equal? (interpret '((lambda xs xs) 1 2) g) '(1 2))
	      (printf "failed: (interpret '((lambda xs xs) 1 2) ~s)\n" g))
      (unless (equal? (interpret '((lambda (x . xs) xs) 1 2 3) g) '(2 3))
	      (printf "failed: (interpret '((lambda (x . xs) xs) 1 2 3) ~s)\n" g))
      ;;; Testing for let expressions
      (unless (equal? (interpret '(let () 2) g) 2)
	      (printf "failed: (interpret '(let () 2) ~s)\n" g))
      (unless (equal? (interpret '(let ((x 1)) x) g) 1)
	      (printf "failed: (interpret '(let ((x 1)) x) ~s)\n" g))
      (unless (equal? (interpret '(let ((x 1) (y 2)) x) g) 1)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) x) ~s)\n" g))
      (unless (equal? (interpret '(let ((x 1) (y 2)) y) g) 2)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) y) ~s)\n" g))
      ;;; Testing for quote expressions and nested quote expressions
      (unless (equal? (interpret ''(if #t 1 2) g) '(if #t 1 2))
	      (printf "failed: (interpret ''(if #t 1 2) ~s)\n" g))
      (unless (equal? (interpret ''((lambda (x y) x) 1 2) g)'((lambda (x y) x) 1 2))
	      (printf "failed: (interpret ''((lambda (x y) x) 1 2) ~s)\n" g))
      (unless (equal? (interpret '''(if #t 1 2) g) ''(if #t 1 2))
	      (printf "failed: (interpret '''(if #t 1 2) ~s)\n" g))
      (unless (equal? (interpret '''((lambda (x y) x) 1 2) g)''((lambda (x y) x) 1 2))
	      (printf "failed: (interpret '''((lambda (x y) x) 1 2) ~s)\n" g))
      ;;; Testing for cond expression
      (unless (equal? (interpret '(cond (#t 3)(#f 4)(else 1)) g) 3)
	      (printf "failed: (interpret '(cond (#t 3)(#f 4)(else 1)) ~s)\n" g))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) g) 4)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (unless (equal? (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) g) 3)
	      (printf "failed: (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) g) 1)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) ~s)\n" g))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) g) 90)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) ~s)\n" g))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) g) 9)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) ~s)\n" g))
      (unless (equal? (interpret '(cond (else 2)) g) '2)
	      (printf "failed: (interpret '(cond (else 2)) ~s)\n" g))
      ;;; Testing for mathematical operator primitives
      (unless (equal? (interpret '(+) g) 0)
	      (printf "failed: (interpret '(+) ~s)\n" g))
      (unless (equal? (interpret '(+ 1) g) 1)
	      (printf "failed: (interpret '(+ 1) ~s)\n" g))
      (unless (equal? (interpret '(+ 1 10) g) 11)
	      (printf "failed: (interpret '(+ 1 10) ~s)\n" g))
      (unless (equal? (interpret '(+ 1 10 100) g) 111)
	      (printf "failed: (interpret '(+ 1 10 100) ~s)\n" g))
      (unless (equal? (interpret '(- 5) g) -5)
	      (printf "failed: (interpret '(- 5) ~s)\n" g))
      (unless (equal? (interpret '(- 5 1 2) g) 2)
	      (printf "failed: (interpret '(- 5 1 2) ~s)\n" g))
      (unless (equal? (interpret '(*) g) 1)
	      (printf "failed: (interpret '(*) ~s)\n" g))
      (unless (equal? (interpret '(* 5) g) 5)
	      (printf "failed: (interpret '(* 5) ~s)\n" g))
      (unless (equal? (interpret '(* 5 2 1) g) 10)
	      (printf "failed: (interpret '(* 5 2 1) ~s)\n" g))
      (unless (equal? (interpret '(* 5 2 100 0) g) 0)
	      (printf "failed: (interpret '(* 5 2 100 0) ~s)\n" g))
      (unless (equal? (interpret '(/ 5) g) 1/5)
	      (printf "failed: (interpret '(/ 5) ~s)\n" g))
      (unless (equal? (interpret '(/ 10 5) g) 2)
	      (printf "failed: (interpret '(/ 10 5) ~s)\n" g))
      (unless (equal? (interpret '(/ 1000 20 5) g) 10)
	      (printf "failed: (interpret '(/ 1000 20 5) ~s)\n" g))
      ;;; Testing for scheme list primitives
      (unless (equal? (interpret '(car (cons 1 2)) g) 1)
	      (printf "failed: (interpret '(car (cons 1 2)) ~s)\n" g))
      (unless (equal? (interpret '(cdr (cons 1 2)) g) '2)
	      (printf "failed: (interpret '(cdr (cons 1 2)) ~s)\n" g))
      (unless (equal? (interpret '(car '(1 2)) g) 1)
	      (printf "failed: (interpret '(car '(1 2)) ~s)\n" g))
      (unless (equal? (interpret '(cdr '(1 2)) g) '(2))
	      (printf "failed: (interpret '(cdr '(1 2)) ~s)\n" g))
      (unless (equal? (interpret '(list 1 2 3) g) '(1 2 3))
	      (printf "failed: (interpret '(list 1 2 3) ~s)\n" g))
      (unless (equal? (interpret '(list (list 1 2 3)) g) '((1 2 3)))
	      (printf "failed: (interpret '(list (list 1 2 3)) ~s)\n" g))
      ;;; Testing for call/cc
      (unless (equal? (interpret '(+ 5 (call/cc (lambda (k) 10))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) 10))) ~s)\n" g))
      (unless (equal? (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) ~s)\n" g))
      (unless (equal? (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) ~s)\n" g))
      (unless (equal? (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) g) 2)
	      (printf "failed: (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) ~s)\n" g))
      ;;; Testing for simple cases of apply
      (unless (equal? (interpret '(apply + '(1 2 3)) g) 6)
	      (printf "failed: (interpret '(apply + '(1 2 3)) ~s)\n" g))
      (unless (equal? (interpret '(apply (lambda (x) x) '(10)) g) 10)
	      (printf "failed: (interpret '(apply (lambda (x) x) '(10)) ~s)\n" g))
      (unless (equal? (interpret '(apply * '(10 5 -1)) g) -50)
	      (printf "failed: (interpret '(apply * '(10 5 -1)) ~s)\n" g))
      (unless (equal? (interpret '(apply (lambda () 1) '()) g) 1)
	      (printf "failed: (interpret '(apply (lambda () 1) '()) ~s)\n" g))
      ;;; Testing for more complex cases of apply
      (unless (equal? (interpret '(apply apply (list + (list 5 3 1))) g) 9)
	      (printf "failed: (interpret '(apply apply (list + (list 5 3 1))) ~s)\n" g))
      (unless (equal? (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) g) 31)
	      (printf "failed: (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) ~s)\n" g))
      (unless (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) ~s)\n" g))
      (unless (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) ~s)\n" g))
      (unless (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) ~s)\n" g))
      ;;; Testing for letrec expressions

      ;; Testing using factorial function
      (unless (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) g) 120)
	      (printf "failed: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) g) 5040)
	      (printf "failed: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #t
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 12)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 12)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 5)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 5)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 31)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 31)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #f
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 10)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 10)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 21)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 21)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 20)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 20)) ~s)\n" g))
      
      ;; Testing using even odd where result is expected to be #t
      (unless (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 10)) g) #t)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 10)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 7)) g) #t)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 7)) ~s)\n" g))

      ;; Testing using even odd where result is expected to be #f
      (unless (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 9)) g) #f)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 9)) ~s)\n" g))
      (unless (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 6)) g) #f)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 6)) ~s)\n" g))
      )))

;;; "verbose" version that prints out messages if test is fine. Hence, no error messages means that tests are NOT fine
(define test-verbose
  (lambda (g)
    (begin
      ;;; Testing for base values
      (when   (equal? (interpret 0 g) 0)
	      (printf "succeeded: (interpret 0 ~s)\n" g))
      (when   (equal? (interpret '#f g) #f)
	      (printf "succeeded: (interpret '#f ~s)\n" g))
      (when   (equal? (interpret '#\f g) #\f)
	      (printf "succeeded: (interpret '#\f ~s)\n" g))
      (when   (equal? (interpret "scheme_capstone" g) "scheme_capstone")
	      (printf "succeeded: (interpret \"scheme_capstone\" ~s)\n" g))
      (when   (equal? (interpret '() g) '())
	      (printf "succeeded: (interpret '() ~s)\n" g))
      ;;; Testing for if expressions
      (when   (equal? (interpret '(if #t 1 2) g) 1)
	      (printf "succeeded: (interpret '(if #t 1 2) ~s)\n" g))
      (when   (equal? (interpret '(if #f 1 2) g) 2)
	      (printf "succeeded: (interpret '(if #f 1 2) ~s)\n" g))
      (when   (equal? (interpret '(if 0 1 2) g) 1)
	      (printf "succeeded: (interpret '(if 0 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x) x) 1) g) 1)
	      (printf "succeeded: (interpret '((lambda (x) x) 1) ~s)\n" g))
      ;;; Testing for lambda expressions
      (when   (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "succeeded: (interpret '((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "succeeded: (interpret ((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x y) y) 1 2) g) 2)
	      (printf "succeeded: (interpret ((lambda (x y) y) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda xs xs) 1 2) g) '(1 2))
	      (printf "succeeded: (interpret '((lambda xs xs) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x . xs) xs) 1 2 3) g) '(2 3))
	      (printf "succeeded: (interpret '((lambda (x . xs) xs) 1 2 3) ~s)\n" g))
      ;;; Testing for let expressions
      (when   (equal? (interpret '(let () 2) g) 2)
	      (printf "succeeded: (interpret '(let () 2) ~s)\n" g))
      (when   (equal? (interpret '(let ((x 1)) x) g) 1)
	      (printf "succeeded: (interpret '(let ((x 1)) x) ~s)\n" g))
      (when   (equal? (interpret '(let ((x 1) (y 2)) x) g) 1)
	      (printf "succeeded: (interpret '(let ((x 1) (y 2)) x) ~s)\n" g))
      (when   (equal? (interpret '(let ((x 1) (y 2)) y) g) 2)
	      (printf "succeeded: (interpret '(let ((x 1) (y 2)) y) ~s)\n" g))
      ;;; Testing for quote expressions and nested quote expressions
      (when   (equal? (interpret ''(if #t 1 2) g) '(if #t 1 2))
	      (printf "succeeded: (interpret ''(if #t 1 2) ~s)\n" g))
      (when   (equal? (interpret ''((lambda (x y) x) 1 2) g)'((lambda (x y) x) 1 2))
	      (printf "succeeded: (interpret ''((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '''(if #t 1 2) g) ''(if #t 1 2))
	      (printf "succeeded: (interpret '''(if #t 1 2) ~s)\n" g))
      (when   (equal? (interpret '''((lambda (x y) x) 1 2) g)''((lambda (x y) x) 1 2))
	      (printf "succeeded: (interpret '''((lambda (x y) x) 1 2) ~s)\n" g))
      ;;; Testing for cond expression
      (when   (equal? (interpret '(cond (#t 3)(#f 4)(else 1)) g) 3)
	      (printf "succeeded: (interpret '(cond (#t 3)(#f 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) g) 4)
	      (printf "succeeded: (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) g) 3)
	      (printf "succeeded: (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) g) 1)
	      (printf "succeeded: (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) g) 90)
	      (printf "succeeded: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) g) 9)
	      (printf "succeeded: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) ~s)\n" g))
      (when   (equal? (interpret '(cond (else 2)) g) '2)
	      (printf "succeeded: (interpret '(cond (else 2)) ~s)\n" g))
      ;;; Testing for mathematical operator primitives
      (when   (equal? (interpret '(+) g) 0)
	      (printf "succeeded: (interpret '(+) ~s)\n" g))
      (when   (equal? (interpret '(+ 1) g) 1)
	      (printf "succeeded: (interpret '(+ 1) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 10) g) 11)
	      (printf "succeeded: (interpret '(+ 1 10) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 10 100) g) 111)
	      (printf "succeeded: (interpret '(+ 1 10 100) ~s)\n" g))
      (when   (equal? (interpret '(- 5) g) -5)
	      (printf "succeeded: (interpret '(- 5) ~s)\n" g))
      (when   (equal? (interpret '(- 5 1 2) g) 2)
	      (printf "succeeded: (interpret '(- 5 1 2) ~s)\n" g))
      (when   (equal? (interpret '(*) g) 1)
	      (printf "succeeded: (interpret '(*) ~s)\n" g))
      (when   (equal? (interpret '(* 5) g) 5)
	      (printf "succeeded: (interpret '(* 5) ~s)\n" g))
      (when   (equal? (interpret '(* 5 2 1) g) 10)
	      (printf "succeeded: (interpret '(* 5 2 1) ~s)\n" g))
      (when   (equal? (interpret '(* 5 2 100 0) g) 0)
	      (printf "succeeded: (interpret '(* 5 2 100 0) ~s)\n" g))
      (when   (equal? (interpret '(/ 5) g) 1/5)
	      (printf "succeeded: (interpret '(/ 5) ~s)\n" g))
      (when   (equal? (interpret '(/ 10 5) g) 2)
	      (printf "succeeded: (interpret '(/ 10 5) ~s)\n" g))
      (when   (equal? (interpret '(/ 1000 20 5) g) 10)
	      (printf "succeeded: (interpret '(/ 1000 20 5) ~s)\n" g))
      ;;; Testing for scheme list primitives
      (when   (equal? (interpret '(car (cons 1 2)) g) 1)
	      (printf "succeeded: (interpret '(car (cons 1 2)) ~s)\n" g))
      (when   (equal? (interpret '(cdr (cons 1 2)) g) '2)
	      (printf "succeeded: (interpret '(cdr (cons 1 2)) ~s)\n" g))
      (when   (equal? (interpret '(car '(1 2)) g) 1)
	      (printf "succeeded: (interpret '(car '(1 2)) ~s)\n" g))
      (when   (equal? (interpret '(cdr '(1 2)) g) '(2))
	      (printf "succeeded: (interpret '(cdr '(1 2)) ~s)\n" g))
      (when   (equal? (interpret '(list 1 2 3) g) '(1 2 3))
	      (printf "succeeded: (interpret '(list 1 2 3) ~s)\n" g))
      (when   (equal? (interpret '(list (list 1 2 3)) g) '((1 2 3)))
	      (printf "succeeded: (interpret '(list (list 1 2 3)) ~s)\n" g))
      ;;; Testing for call/cc
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) 10))) g) 15)
	      (printf "succeeded: (interpret '(+ 5 (call/cc (lambda (k) 10))) ~s)\n" g))
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) g) 15)
	      (printf "succeeded: (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) ~s)\n" g))
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) g) 15)
	      (printf "succeeded: (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) ~s)\n" g))
      (when   (equal? (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) g) 2)
	      (printf "succeeded: (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) ~s)\n" g))
      ;;; Testing for simple cases of apply
      (when   (equal? (interpret '(apply + '(1 2 3)) g) 6)
	      (printf "succeeded: (interpret '(apply + '(1 2 3)) ~s)\n" g))
      (when   (equal? (interpret '(apply (lambda (x) x) '(10)) g) 10)
	      (printf "succeeded: (interpret '(apply (lambda (x) x) '(10)) ~s)\n" g))
      (when   (equal? (interpret '(apply * '(10 5 -1)) g) -50)
	      (printf "succeeded: (interpret '(apply * '(10 5 -1)) ~s)\n" g))
      (when   (equal? (interpret '(apply (lambda () 1) '()) g) 1)
	      (printf "succeeded: (interpret '(apply (lambda () 1) '()) ~s)\n" g))
      ;;; Testing for more complex cases of apply
      (when   (equal? (interpret '(apply apply (list + (list 5 3 1))) g) 9)
	      (printf "succeeded: (interpret '(apply apply (list + (list 5 3 1))) ~s)\n" g))
      (when   (equal? (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) g) 31)
	      (printf "succeeded: (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) g) 11)
	      (printf "succeeded: (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) g) 11)
	      (printf "succeeded: (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) g) 11)
	      (printf "succeeded: (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) ~s)\n" g))
      ;;; Testing for letrec expressions

      ;; Testing using factorial function
      (when   (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) g) 120)
	      (printf "succeeded: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) g) 5040)
	      (printf "succeeded: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #t
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 12)) g) #t)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 12)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 5)) g) #t)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 5)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 31)) g) #t)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 31)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #f
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 10)) g) #f)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 10)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 21)) g) #f)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 21)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 20)) g) #f)
	      (printf "succeeded: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 20)) ~s)\n" g))

      ;; Testing using even odd where result is expected to be #t
      (when   (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 10)) g) #t)
	      (printf "succeeded: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 10)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 7)) g) #t)
	      (printf "succeeded: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 7)) ~s)\n" g))

      ;; Testing using even odd where result is expected to be #f
      (when (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					 (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 9)) g) #f)
	      (printf "succeeded: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 9)) ~s)\n" g))
      (when (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 6)) g) #f)
	      (printf "succeeded: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 6)) ~s)\n" g))
      )))

;;; Defining tests for the auxiliary functions of the interpreter

;;; Testing the is define function using simple examples
(define test-is-define
  (lambda ()
    (begin
      (unless (equal? (is-define? '(define one 1)) #t)
	      (printf "failed: (is-define? '(define one 1))\n"))
      (unless (equal? (is-define? '(define empty-env '())) #t)
	      (printf "failed: (is-define? '(define empty-env '()))\n"))
      (unless (equal? (is-define? '(define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) #t)
	      (printf "failed: (is-define? '(define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))))\n"))
      )))
  
;;; Testing the run function using simple examples
(define test-run
  (lambda ()
    (begin
      (unless (equal? (run '((define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))) (fac 5))) 120)
	      (printf "failed: (run '((define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))) (fac 5)))\n"))
      (unless (equal? (run '((define five 5) (define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))) (fac five))) 120)
	      (printf "failed: (run '((define five 5) (define fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1)))))) (fac five)))\n"))
      (unless (equal? (run '((define five 5) (define six 6) (+ five six))) 11)
	      (printf "failed: (run '((define five 5) (define six 6) (+ five six)))\n"))
      (unless (equal? (run '((define ackermann
			       (lambda (x y)
				 (cond ((equal? x 0)
					(+ y 1))
				       ((equal? y 0)
					(ackermann (- x 1) 1))
				       (else (ackermann (- x 1) (ackermann x (- y 1))))))) (ackermann 3 5))
			   ) 253)
	      (printf "failed: (run '(define ackermann ...) (ackermann 3 5))\n"))
      )))

;;; Testing the usage of the run function together with the file-parser function

(define test-run-on-parsed-res
  (lambda ()
    (begin
      (unless (equal? (run (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/test_file_parser.scm")) "hello")
	      (printf "failed: (run (file-parser \"test_file_parser.scm\"))\n"))
      )))

(define test-verbose-run-on-parsed-res
  (lambda ()
    (begin
      (when (equal? (run (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/test_file_parser.scm")) "hello")
	      (printf "succeeded: (run (file-parser \"test_file_parser.scm\"))\n"))
      )))
