;;; scheme-week-03-lists.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 31 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For me to familiarize myself with lists in scheme.

;; Unlike binary trees, lists are nested cons expressions where one element is a non-cons expression with the other being a cons-expression (that ends on the empty list if it is the final element)

(list? (list 1 2 3 4 5)) ;; Formation of lists using the in-built list operator

(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))) ;; Formation of lists using the cons operator. Proper lists must end with the empty list, or '() operator.

(list? (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 3)))))) ;; returns #f, because does not terminate with the empty list operator.

;; car refers to head of list, cdr refers to tail of list.

(define length-proper-list
  (lambda (xs)
    (letrec ([visit (lambda (ws)
                     (if (null? ws)
                         0
                         (+ (1 (visit (cdr ws))))))])
      (visit xs))))

(define test-append_proper-list
  (lambda (candidate)
    (and (equal? (candidate (list) (list))
                 (list))
         (equal? (candidate (list) (list 4 5 6))
                 (list 4 5 6))
         (equal? (candidate (list 4) (list 5 6))
                 (list 4 5 6))
         (equal? (candidate (list 4 5) (list 6))
                 (list 4 5 6))
         (equal? (candidate (list 4 5 6) (list))
                 (list 4 5 6))
         ;;; add more tests here
         )))

(define append-proper-list
  (lambda (xs1 xs2)
    (letrec ([visit (lambda (ws)
                      (if (null? ws)
                          xs2
                          (cons (car ws) (visit (cdr ws)))))])
      (visit xs1))))

(test-append_proper-list append-proper-list)

(define test-reverse_proper-list
  (lambda (candidate)
    (and (equal? (candidate (list)) (list))
         (equal? (candidate (list 1 2 3))(list 3 2 1))
         (equal? (candidate (list 1 2 3 4 5 6))(list 6 5 4 3 2 1))
         (equal? (candidate (list 4 3 2 1))(list 1 2 3 4))
         ;;; add more tests later
         )))

(define reverse-proper-list
  (lambda (xs)
    (letrec ([visit
             (lambda (xs)
               (if (null? xs)
                   (list)
                   (append-proper-list (visit (cdr xs))(list (car xs)))))])
    (visit xs))))


(test-reverse_proper-list reverse-proper-list)
;; Note: Lists with 1 element and more are already considered as pairs.

;;; end of scheme-week-03-lists.scm
"scheme-week-03-lists.scm"
