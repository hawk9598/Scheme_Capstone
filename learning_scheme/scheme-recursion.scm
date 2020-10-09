;;; scheme-recursion.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 18 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is for me to familiarize myself with recursion in scheme.

;;; Recursive Definition for Plus

(define test-plus
  (lambda (candidate)
    (and (equal? (candidate 0 0)
                 0)
         (equal? (candidate 0 5)
                 5)
         (equal? (candidate 5 0)
                 5)
         (equal? (candidate 3 4)
                 7)
         (equal? (candidate 123 4321)
                 4444)
         ((lambda (x1 x2)
            (equal? (candidate x1 x2)
                    (+ x1 x2)))
          10
          100)
         ;;; add more tests here
         )))

(define plus-traced
  (trace-lambda plus (n1 n2)
    (if (zero? n1)
        n2
        (+ 1 (plus-traced (- n1 1) n2)))))

(plus-traced 5 3)

(test-plus plus-traced)
;;; Recursive Definition for Multiplication

(define test-mult
  (lambda (candidate)
    (and (equal? (candidate 0 0)
                 0)
         (equal? (candidate 0 5)
                 0)
         (equal? (candidate 5 0)
                 0)
         (equal? (candidate 1 1)
                 1)
         (equal? (candidate 5 1)
                 5)
         (equal? (candidate 1 5)
                 5)
         (equal? (candidate 3 4)
                 12)
         ((lambda (x1 x2)
            (equal? (candidate x1 x2)
                    (* x1 x2)))
          10
          100)
         ;;; add more tests here
         )))

(define mult-traced
  (trace-lambda mult (n1 n2)
    (if (zero? n1)
        0
        (+ n2 (mult-traced (- n1 1) n2)))))

(mult-traced 5 3)

(test-mult mult-traced)

;;; Recursive Definition for Factorial
(define fac-traced
  (trace-lambda fac (n1)
    (if (zero? n1)
        1
        (* n1 (fac-traced (- n1 1))))))

(fac-traced 6)

(define test-fac
  (lambda (candidate)
    (and (equal? (candidate 0)
                 (*))
         (equal? (candidate 1)
                 (* 1))
         (equal? (candidate 2)
                 (* 2 1))
         (equal? (candidate 3)
                 (* 3 2 1))
         (equal? (candidate 4)
                 (* 4 3 2 1))
         (equal? (candidate 5)
                 (* 5 4 3 2 1))
         (equal? (candidate 6)
                 (* 6 5 4 3 2 1))
         (equal? (candidate 7)
                 (* 7 6 5 4 3 2 1))
         ;;; add more tests here
         )))

(test-fac fac-traced)

;;; Recursive Definition for Power
(define test-power
  (lambda (candidate)
    (and (equal? (candidate 3 2)
                 (* 3 3))
         (equal? (candidate 7 3)
                 (* 7 7 7))
         (equal? (candidate 5 4)
                 (* 5 5 5 5))
         (equal? (candidate 10 5)
                 (* 10 10 10 10 10))
         (equal? (candidate 2 0)
                 1)
         (equal? (candidate 2 1)
                 2)
         (equal? (candidate 2 2)
                 4)
         (equal? (candidate 2 3)
                 8)
         (equal? (candidate 2 4)
                 16)
         (equal? (candidate 2 10)
                 1024)
         (equal? (candidate 10 2)
                 100)
         ;;; add more tests here
         )))



(define power-traced
  (trace-lambda power (n1 n2)
    (if (zero? n2)
        1
        (* n1 (power-traced n1 (- n2 1))))))

(power-traced 5 3)

(test-power power-traced)
;;; Recursive Definition for Fibonacci

(define test-fibonacci
  (lambda (candidate)
    (and (equal? (candidate 0) 1)
         (equal? (candidate 1) 1)
         (equal? (candidate 2) 2)
         (equal? (candidate 3) 3)
         (equal? (candidate 4) 5)
         (equal? (candidate 5) 8)
         (equal? (candidate 6) 13)
         (equal? (candidate 7) 21)
         ;;; add more tests here
         )))

(define fibonacci-traced
  (trace-lambda fib (n1)
    (if (zero? n1)
        1
        (if (zero? (- n1 1))
            1
            (+ (fibonacci-traced (- n1 1))(fibonacci-traced (- n1 2)))))))

(fibonacci-traced 5)
(test-fibonacci fibonacci-traced)

;;; Definition of mutually recursive functions is-even and is-odd
(define test-is-even?
  (lambda (candidate)
    (and (equal? (candidate 0)
                 #t)
         (equal? (candidate 1)
                 #f)
         (equal? (candidate 2)
                 #t)
         (equal? (candidate 3)
                 #f)
         (equal? (candidate 4)
                 #t)
         (equal? (candidate 100)
                 #t)
         (equal? (candidate 101)
                 #f)
         ;;;
         )))

(define test-is-odd?
  (lambda (candidate)
    (and (equal? (candidate 0)
                 #f)
         (equal? (candidate 1)
                 #t)
         (equal? (candidate 2)
                 #f)
         (equal? (candidate 3)
                 #t)
         (equal? (candidate 4)
                 #f)
         (equal? (candidate 100)
                 #f)
         (equal? (candidate 101)
                 #t)
         ;;;
         )))

(define is-even?
  (trace-lambda even (n)
    (if (= n 0)
        #t
        (is-odd? (- n 1)))))

(define is-odd?
  (trace-lambda odd (n)
    (if (= n 0)
        #f
        (is-even? (- n 1)))))

(test-is-even? is-even?)
(test-is-odd? is-odd?)
;;; Recursive and non-recursive definitions for sum function
(define test-sum-from-1-to-max
  (lambda (candidate)
    (and (equal? (candidate 0)
                 0)
         (equal? (candidate 1)
                 (+ 1 0))
         (equal? (candidate 2)
                 (+ 2 (+ 1 0)))
         (equal? (candidate 3)
                 (+ 3 (+ 2 (+ 1 0))))
         (equal? (candidate 4)
                 (+ 4 (+ 3 (+ 2 (+ 1 0)))))
         (equal? (candidate 5)
                 (+ 5 (+ 4 (+ 3 (+ 2 (+ 1 0))))))
         ;;; observe how the additions above
         ;;; match the structure of sum-from-1-to-max_linear
         ;;;
         (equal? (candidate 10)
                 (quotient (* 10 (+ 10 1)) 2))
         (equal? (candidate 100)
                 (quotient (* 100 (+ 100 1)) 2))
         (equal? (candidate 1000)
                 (quotient (* 1000 (+ 1000 1)) 2))
         ;;; observe how the additions above
         ;;; match the structure of sum-from-1-to-max_constant
         ;;;
         )))

(define sum-from-1-to-max-linear
  (trace-lambda sum (n)
    (if (zero? n)
        0
        (+ n (sum-from-1-to-max-linear (- n 1))))))

(define sum-from-1-to-max-constant
  (trace-lambda sum-constant (n)
    (quotient (* n (+ n 1)) 2)))

(test-sum-from-1-to-max sum-from-1-to-max-linear)
(test-sum-from-1-to-max sum-from-1-to-max-constant)


;;; End of scheme-recursion.scm

"scheme-recursion.scm"
