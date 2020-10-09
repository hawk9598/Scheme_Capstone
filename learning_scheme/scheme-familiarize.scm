;;; scheme-familiarize.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 18 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is for me to familiarize myself with basic scheme syntax.


;; Operators on Numbers
;; + operator is variadic (can take any number of arguments)
(+ 5 2)
(+ 10 2 30 42)
;; - operator is variadic
(- 10 2)
(- 10 5 2)
;; * operator is variadic
(* 15 12)
(* 10 20 30)
;; / operator is variadic
(/ 10 5)
(/ 10 5 2)
;; quotient is binary
(quotient 15 3)
;; remainder is binary
(remainder 14 3)
;; exponentiation is binary
(expt 5 2)
;; comparison operators are variadic
(< 10 20)
(< 10 20 30)
(> 10 20)
(<= 10 10)
(<= 10 10 20)
(= 10 20)
(= 10 10 10)
(>= 31 30)
(>= 31 30 28)
;; predicate operators are unary
(zero? 5)
(number? 5)
(integer? 12.4)

;;; Operations on characters (and strings)
#\c
#\a
(string #\w #\a #\y #\n #\e)
(string-length "wayne")
(string-ref "wayne" 2) ; equivalent to indexing strings

;;; Operations on booleans
#t
#f
;; and operator is variadic and short-circuits. Returns #f or the last non-#f arg.
(and #f #t)
(and 32 "wayne" #t 1)
(and 32 "wayne" #f #t)
;; or operator is variadic and short-circuits. Returns the first #t or non-#f arg.
(or #t #f)
(or 32 #t "wayne" #f))
(or "wayne" #t 1 2 3 #f)
;;; not operator is unary
(not #t)
(not #f)
(not "wayne")

;;; If-else conditionals (which can be nested)
(if #t "wayne" "kane")
(if #f "wayne" "kane")
(if #t (if #f "kane" "wayne") "scheme")

;;; Lambda expressions (applied onto correct number of arguments)
((lambda (x y) (+ x y)) 3 5)
((lambda (x y z)(+ (* x y) z)) 3 5 2)

;;; Special traced-lambda expressions (with a trace for useful feedback)
((trace-lambda example (x1 x2 x3)(list x3 x2 x1)) 1 2 3)
((trace-lambda addition-3 (x1 x2 x3)(+ x1 (+ x2 x3))) 1 2 3)

;;; Define keyword (equivalent to let in OCaml)
(define addition (lambda (x y) (+ x y)))
(addition 3 5)

;;; Type defined equality operator
(equal? 5 5)
(string=? "wayne" "wayne")
(char=? #\c #\d)
(= 1000 100)
(boolean=? #f #t)

;;; End of scheme-familiarize.scm
"scheme-familiarize.scm"
