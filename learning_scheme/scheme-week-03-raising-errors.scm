;;; scheme-week-03-raising-errors.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 31 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Week 3 of Scheme Lecture Notes

;;; Raising errors

(define cautious-quotient
  (lambda (i j)
    (if (number? i)
        (if (number? j)
            (if (= j 0)
                (errorf 'cautious-quotient
                        "cannot divide anything by zero")
                (quotient i j))
            (errorf 'cautious-quotient
                    "~s is not a number"
                    j))
        (errorf 'cautious-quotient
                "~s is not a number"
                i))))

;; errorf takes: Name of the faulty procedure, a formatting directive (if required) and further optional arguments (like text to print)


(define safe-fac
  (lambda (n)
    (letrec ([visit (lambda (i)
                      (if (= i 0)
                          1
                          (* i (visit (- i 1)))))])
      (if (and (integer? n)
               (>= n 0))
          (visit n)
          (errorf 'safe-fac "not a non-negative integer: ~s" n)))))

;; Implementation of proper-list-head, proper-list-tail, proper-list-ref

(define test-proper-list-ref
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 0)
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 3)
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 5)
         ;;;
         )))

(define test-proper-list-head
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 '())
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 '(0 1 2))
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 '(0 1 2 3 4))
         (equal? (candidate '(0 1 2 3 4 5) 6)
                 '(0 1 2 3 4 5))
         ;;;
         )))

(define test-proper-list-tail
  (lambda (candidate)
    (and (equal? (candidate '(0 1 2 3 4 5) 0)
                 '(0 1 2 3 4 5))
         (equal? (candidate '(0 1 2 3 4 5) 3)
                 '(3 4 5))
         (equal? (candidate '(0 1 2 3 4 5) 5)
                 '(5))
         (equal? (candidate '(0 1 2 3 4 5) 6)
                 '())
         ;;;
         )))

(define proper-list-ref
  (lambda (xs n)
    (letrec ([visit (lambda (xs1 n1)
                      (if (null? xs1)
                          (errorf 'proper-list-ref
                                  "index ~s is out of range for list ~s"
                                  n
                                  xs)
                          (if (equal? n1 0)
                              (car xs1)
                              (visit (cdr xs1) (- n1 1)))))])
      (if (< n 0)
          (errorf 'proper-list-ref
                  "index ~s is not an exact nonnegative integer"
                  n)
          (visit xs n)))))

(test-proper-list-ref proper-list-ref)
                          
(define proper-list-head
  (lambda (xs n)
    (letrec ([visit (lambda (xs1 n1)
                      (if (and (null? xs1)
                               (> n1 0))
                          (errorf 'proper-list-head
                                  "index ~s is out of range for list ~s"
                                  n
                                  xs)
                          (if (= n1 0)
                              (list)
                              (cons (car xs1) (visit (cdr xs1)(- n1 1))))))])
       (if (< n 0)
          (errorf 'proper-list-head
                  "index ~s is not an exact nonnegative integer"
                  n)
          (visit xs n)))))

(test-proper-list-head proper-list-head)

(define proper-list-tail
  (lambda (xs n)
    (letrec ([visit (lambda (xs1 n1)
                      (if (and (null? xs1)
                               (> n1 0))
                          (errorf 'proper-list-head
                                  "index ~s is out of range for list ~s"
                                  n
                                  xs)
                          (if (= n1 0)
                              xs1
                              (visit (cdr xs1)(- n1 1)))))])
       (if (< n 0)
          (errorf 'proper-list-tail
                  "index ~s is not an exact nonnegative integer"
                  n)
          (visit xs n)))))

(test-proper-list-tail proper-list-tail)

;; Structural equality vs Identity (for compound values)

(define xs (list 1 2 3))
(define ys (list 1 2 3))

(equal? xs ys) ;; gives #t because xs and ys are structurally equal
(eq? xs ys)  ;; gives #f because xs and ys are constructed independenty, hence not equal.

;; memq uses eq? whilst member uses equal?

(memq 2 '(1 2 3))
(member 2 '(1 2 3))

(memq '(2) '((1)(2)(3))) ;; gives #f because (2) not identical to any element inside list.
(member '(2) '((1)(2)(3))) ;; gives ((2)(3)) because (2) is structurally equal to second element of ((1)(2)(3))

;; end of scheme-week-03-raising-errors.scm
"scheme-week-03-raising-errors.scm"
