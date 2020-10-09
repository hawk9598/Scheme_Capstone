;;; scheme-week-04.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 1 September 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This is for me to familiarize myself with Danvy's Week 4 lecture notes in scheme.

;;; Variadic Procedures

;; Syntax.
;; Specify only one variable and do not place parenthesis around said variable. The variable number of arguments will be wrapped up into a list, which we can then choose to operate on by specifying the instructions in the next ()



((lambda xs xs) 1 2 3 4 5) ;; gives 1 2 3 4 5

((lambda (xs) xs) 1 2 3 4 5) ;; gives incorrect arguments error

;; Alternative syntax (specifying pre-defined number of formal arguments with the remaining number of arguments being variable.

((lambda (x1 x2 . xs) (list x1 x2 xs)) 10 20 30 40) ;; where the . xs denotes that anything after 2nd argument is variable

((lambda (x1 x2 . xs) (list x1 x2 xs)) 10) ;; will raise the incorrect number of arguments error because we are expecting at least 2 arguments.

;; Example of plus variadic procedure

(define plus-dyadic
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1)
                      (if (= n1 0)
                          n2
                          (+ 1 (visit (- n1 1)))))])
     (if (and (integer? n1)
              (>= n1 0))
         (visit n1)
         (errorf 'plus-dyadic "not a non-negative integer: ~s" n1)))))

(define plus-variadic
  (lambda ns
    (letrec ([visit (lambda (ns)
                      (if (null? ns)
                          0
                          (plus-dyadic (car ns)
                                       (visit (cdr ns)))))])
      (visit ns))))

;;; Core special form begin

;;'(begin e1 e2 ... eN-1 eN) stands for

;;'(let* ([_ e1]
;;       [_ e2]
;;       ..
;;       [_ eN-1])
;;  eN)

;; where begin evalutes each of the eN-1 statements with the result of the begin expression being the result of evaluating eN only.

(define chatty-factorial
  (lambda (n)
    (begin
      (printf "(chatty-factorial ~s)~n" n)
      (if (= n 0)
          1
          (* n (chatty-factorial (- n 1)))))))

;; where the printf procedure is run first, then the if procedure (with the result of the if procedure being the result of begin). Note that this is a recursive function via the use of begin.

;;; Core special form unless

'(unless e1 e2) ;; If e1 evaluates to #f then e2 is evaluated. If e1 does not evaluate to #f, (unless e1 e2) is meaningless.

(begin
    (unless #t
      (printf "hello~n"))
    (unless #f
      (printf "world~n"))
    'done)

;; gives world done

;; Programming using Accumulators in Scheme

;; List Reverse

(define reverse_proper-list-v4
  (lambda (vs_init)
    (letrec ([visit (lambda (vs a)
                      (cond
                        [(null? vs)
                         a]
                        [(pair? vs)
                         (visit (cdr vs) (cons (car vs) a))]
                        [else
                         (errorf 'reverse_proper-list-v4
                                 "not a proper list: ~s"
                                 vs_init)]))])
      (visit vs_init '()))))

;;; end of scheme-week-04.scm
"scheme-week-04.scm"
