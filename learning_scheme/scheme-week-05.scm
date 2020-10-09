;;; scheme-week-05.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 5 September 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Currying and Uncurrying

(define curry
  (lambda (p)
    (lambda (x1)
      (lambda (x2)
        (p x1 x2))))) ;; function p not applied on x1 then x2, hence needs to be applied first to x1 then x2, i.e. currying where: fun x1 -> fun x2 -> res

(define uncurry
  (lambda (p)
    (lambda (x1 x2)
      ((p x1) x2)))) ; application of function p occurs in definition of uncurry ((p x1) x2)

;; Folding right and left over numbers

(define fold-right_nat
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (lambda (i)
                  (if (zero? i)
                      zero-case
                      (succ-case (visit (- i 1)))))])
        (visit n)))))

(define fold-right_nat-traced
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (trace-lambda visit_fold-right_nat (i)
                  (if (zero? i)
                      zero-case
                      (succ-case (visit (- i 1)))))])
        (visit n)))))

(define fold-left_nat
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (lambda (i a)
                  (if (zero? i)
                      a
                      (visit (- i 1) (succ-case a))))])
        (visit n zero-case)))))

(define fold-left_nat-traced
  (lambda (zero-case succ-case)
    (lambda (n)
      (letrec ([visit
                (trace-lambda visit_fold-left_nat (i a)
                  (if (zero? i)
                      a
                      (visit (- i 1) (succ-case a))))])
        (visit n zero-case)))))
;; Folding right and left over lists.

(define fold-right_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws)
                        (if (null? ws)
                            nil-case
                            (cons-case (car ws)
                                       (visit (cdr ws)))))])
        (visit vs)))))

(define fold-left_proper-list
  (lambda (nil-case cons-case)
    (lambda (vs)
      (letrec ([visit (lambda (ws a)
                        (if (null? ws)
                            a
                            (visit (cdr ws) (cons-case (car ws) a))))])
        (visit vs nil-case)))))

;; Apply

(apply + (list 1 10 100)) ;; gives 111

(apply apply (list + (list 1 10 100))) ; gives 111

(apply apply (list apply (list + (list 1 10 100)))) ; gives 111

;; (apply v0 (list x1 x2 x3)) same as (v0 x1 x2 x3)
;; (apply apply (list + (list 1 10 100))) same as (apply + (list 1 10 100))
;; (apply apply (list apply (list + (list 1 10 100)))) same as (apply apply (list + (list 1 10 100)))

;; Tricky question for apply:

(apply (lambda xs (reverse xs)) (list 1 2 3 4 5)) ; gives (5 4 3 2 1)

;; lambda is a variadic procedure, hence (list 1 2 3 4 5) is packaged as ((list 1 2 3 4 5)). Apply removes the outer list and hence allows proper reversal of (list 1 2 3 4 5) to occur. ((lambda xs (reverse xs))(list 1 2 3 4 5)) gives ((1 2 3 4 5)) due to lambda xs _ implicitly packaging the non-specified number of arguments as a list.


;; Set!

'(set! <variable> <expression>)

(define x 3)
(set! x 4)
;; now, x gives 4 instead of 3

(let ([x 1])
    (begin
      (set! x 2)
      x))

;; evaluating x in the local environment where x is mutated using set! gives x = 2,  whilst evlauating x in the global environment still gives x = 4 from previous result

;;; end of scheme-week-05.scm
"scheme-week-05.scm"
