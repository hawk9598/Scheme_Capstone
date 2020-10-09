;;; scheme-pairs-and-binary-trees.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 19 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is for me to familiarize myself with binary trees in scheme.

(pair? (cons 10 20)) ;; Formation of pairs using the cons operator
(cons 10 (cons 20 (cons 30 (cons 40 50)))) ;; pairs can be nested using cons
;; the head of a pair can be accessed using car
(car (cons 10 (cons 20 (cons 30 (cons 40 50)))))
;; the tail of a pair can be accessed using cdr
(cdr (cons 10 (cons 20 (cons 30 (cons 40 50)))))
;; car and cdr can be nested when used to access nested pairs
(car (cdr (cdr (cons 10 (cons 20 (cons 30 (cons 40 50)))))))


;;; Binary trees in Scheme have leaves of any datatype (i.e. not just numbers) and
;;; are structured using Scheme's native pair constructor

;; Binary trees using pairs in Scheme

(define test-number-of-leaves
  (lambda (candidate)
    (and (equal? (candidate 32)
                 1)
         (equal? (candidate candidate)
                 1)
         (equal? (candidate (cons 32 33))
                 2)
         (equal? (candidate (cons 32 number-of-leaves))
                 2)
         (equal? (candidate (cons (cons 1 2) (cons 3 4)))
                 4)
         (equal? (candidate (cons 0 (cons (cons 1 2) (cons 3 4))))
                 5)
         (equal? (candidate (cons (cons 0 (cons (cons 1 2) (cons 3 4))) (cons 5 6)))
                 7)
         ;;; add more tests here
         )))

(define number-of-leaves
  (lambda (t)
    (if (not (pair? t))
        1
        (+ (number-of-leaves (car t)) (number-of-leaves (cdr t))))))

(test-number-of-leaves number-of-leaves)

(define test-number-of-nodes
  (lambda (candidate)
    (and (equal? (candidate (cons (cons 10 20) 30))
                 2)
         (equal? (candidate 10)
                 0)
         (equal? (candidate candidate)
                 0)
         ;;; add more tests here
         )))

(define number-of-nodes
  (lambda (t)
    (if (not (pair? t))
        0
        (+ (number-of-nodes (car t))(number-of-nodes (cdr t)) 1)
        )))

(test-number-of-nodes number-of-nodes)

;;; Defining the equality predicate for all types

(define test-equal?
  (lambda (candidate)
    (and ;;; numbers:
         (equal? (candidate 2
                            2)
                 #t)
         (equal? (candidate 2
                            3)
                 #f)
         (equal? (candidate 2
                            #t)
                 #f)
         ;;; Booleans:
         (equal? (candidate #t
                            #t)
                 #t)
         (equal? (candidate #f
                            #f)
                 #t)
         (equal? (candidate #t
                            #f)
                 #f)
         (equal? (candidate #t
                            33)
                 #f)
         ;;; characters:
         (equal? (candidate #\c
                            #\c)
                 #t)
         (equal? (candidate #\c
                            #\d)
                 #f)
         (equal? (candidate #\c
                            33)
                 #f)
         ;;; strings
         (equal? (candidate "hello"
                            "hello")
                 #t)
         (equal? (candidate "hello"
                            "hola")
                 #f)
         (equal? (candidate "hello"
                            33)
                 #f)
         (equal? (candidate "32"
                            32)
                 #f)
         ;;; procedures
         (equal? (candidate (lambda (x) x)
                            (lambda (y) y))
                 #f)
         ;;; pairs
         (equal? (candidate (cons 1 2)
                            (cons 1 2))
                 #t)
         (equal? (candidate (cons (cons 1 2) (cons 3 4))
                            (cons (cons 1 2) (cons 3 4)))
                 #t)
         (equal? (candidate (cons (cons 1 2) (cons 3 4))
                            (cons (cons 1 2) (cons 4 3)))
                 #f)
         (equal? (candidate (cons (cons 1 2) (cons 3 4))
                            (cons (cons 2 1) (cons 3 4)))
                 #f)
         (equal? (candidate (cons (cons 1 2) (cons 3 4))
                            (cons (cons 1 2) (cons 3 #f)))
                 #f)
         ;;; add more tests here
         )))

(test-equal? equal?)

(define equal?_revisited
  (lambda (v1 v2)
    (if (number? v1)
        (if (number? v2)
            (= v1 v2)
            #f)
        (if (boolean? v1)
            (if (boolean? v2)
                (boolean=? v1 v2)
                #f)
            (if (char? v1)
                (if (char? v2)
                    (char=? v1 v2)
                    #f)
                (if (string? v1)
                    (if (string? v2)
                        (string=? v1 v2)
                        #f)
                    (if (pair? v1)
                        (if (pair? v2)
                            (if (equal?_revisited (car v1) (car v2))
                                (equal?_revisited (cdr v1) (cdr v2))
                                #f)
                            #f)
                        #f)))))))

(test-equal? equal?_revisited)
;;; end of scheme-pairs-and-binary-trees.scm
"scheme-pairs-and-binary-trees.scm"
