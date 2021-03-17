;;; self_interpreter.scm

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/file_parser.scm")

(define eval-cps
  (lambda (e r g k)
    (cond
     ((integer? e)
      (k e))
     ((boolean? e)
      (k e))
     ((char? e)
      (k e))
     ((string? e)
      (k e))
     ((null? e)
      (k e))
     ((symbol? e)
      (k (lookup e r g)))
     ((pair? e)
      (let ((e0 (car e))
	    (es (cdr e)))
	(if (symbol? e0)
	    (cond
	     ((equal? e0 'if)
	      (eval-cps (car es)
			r
			g
			(lambda (v0)
			  (if v0
			      (eval-cps (car (cdr es)) r g k)
			      (eval-cps (car (cdr (cdr es))) r g k)))))
	     ((equal? e0 'lambda)
	      (let ((formals (car es))
		    (body (car (cdr es))))
		(k (lambda (vs g k)
		     (eval-cps body
			       (extend-env formals vs r)
			       g
			       k)))))
	     ((equal? e0 'let)
	      (evlet-cps (car es)
			 (car (cdr es))
			 r
			 g
			 k))
	     ((equal? e0 'cond)
	      (letrec ((loop (lambda (c cs)
			       (if (null? cs)
				   (if (equal? (car c) 'else)
				       (eval-cps (car (cdr c)) r g k)
				       (errorf 'eval-cps "cond should end with else"))
				   (let ((test (car c))
					 (consequent (car (cdr c))))
				     (eval-cps test r g (lambda (v)
							  (if v
							      (eval-cps consequent r g k)
							      (loop (car cs) (cdr cs))))))))))
		(loop (car es) (cdr es))))
	     ((equal? e0 'letrec)
	      (eval-cps (macro-expand-letrec es) r g k))
	     ((equal? e0 'quote)
	      (k (car es)))
	     (else
	      (evlis-cps es r g (lambda (vs)
				  ((lookup e0 r g) vs g k)))))
	    (eval-cps e0 r g (lambda (v0)
			       (evlis-cps es r g (lambda (vs)
						   (v0 vs g k))))))))
     (else
      (errorf 'eval-cps "UFO")))))

(define evlis-cps
  (lambda (es r g k)
    (cond
     ((null? es)
      (k '()))
     ((pair? es)
      (eval-cps (car es) r g (lambda (v)
                                  (evlis-cps (cdr es) r g (lambda (vs)
                                                               (k (cons v vs)))))))
     (else
      (errorf 'evlis-cps "Improper list of actuals")))))

(define evlet-cps-aux
  (lambda (bs r g k)
    (cond
     ((null? bs)
      (k '() '()))
     ((pair? bs)
      (let ((b (car bs))
	    (bsp (cdr bs)))
	(let ((x (car b))
	      (d (car (cdr b))))
	  (eval-cps d r g (lambda (v)
                               (evlet-cps-aux bsp r g (lambda (xs vs)
                                                           (k (cons x xs) (cons v vs)))))))))
     (else
      (errorf 'evlet-cps-aux "Improper let header")))))

(define evlet-cps
  (lambda (bs e r g k)
    (evlet-cps-aux bs r g (lambda (xs vs)
                               (eval-cps e (extend-env xs vs r) g k)))))

(define extend-env
  (lambda (xs vs r)
    (cond
     ;;; Handling variadic procedures
     ((symbol? xs)
      (cons (cons xs vs) r))
     ((pair? xs)
      (extend-env-aux xs vs r))
     ((null? xs)
      r)
     (else
      (errorf 'extend-env "Illegal formals in extend-env")))))

;;; Lambda procedures with fixed arity or mixed arity are handled here.
(define extend-env-aux
  (lambda (xs vs r)
    (cond
     ((null? xs)
      (if (null? vs)
	  r
	  (errorf 'extend-env-aux "Too many actual parameters")))
     ((pair? xs)
      (if (pair? vs)
	  (cons (cons (car xs) (car vs))
		(extend-env-aux (cdr xs) (cdr vs) r))
	  (errorf 'extend-env-aux "Too few actual parameters")))
     ((symbol? xs)
      (cons (cons xs vs) r))
     (else
      (errorf 'extend-env-aux "Illegals formals in extend-env-aux")))))

;;; For looking up names, we first start by looking up in the local environment r, before looking up in the global environment g, before finally looking up in the initial environment which contains the defined primitive functions.

(define lookup-init
  (lambda (x r)
    (if (null? r)
        (errorf 'lookup "Unbound variable: ~s" x)
        (if (equal? x (car (car r)))
            (cdr (car r))
            (lookup-init x (cdr r))))))

(define lookup-global
  (lambda (x g)
    (if (null? g)
        (lookup-init x env-init)
        (if (equal? x (car (car g)))
	    (cdr (car g))
	    (lookup-global x (cdr g))))))

(define lookup
  (lambda (x r g)
    (if (null? r)
	(lookup-global x g)
	(if (equal? x (car (car r)))
	    (cdr (car r))
	    (lookup x (cdr r) g)))))

;;; Macro expanding letrec into a form that contains Curry's variadic YCombinator, which will be used in eval to implement recursion
(define macro-expand-letrec
  (lambda (es)
    (let ((header (car es))
          (body (car (cdr es))))
      (let ((names (map car header)))
        (cons '(lambda fs
                 (let ((xs
                        (internal-map1
                         (lambda (fi)
                           (lambda xs
                             (apply fi
                                    (internal-map1 (lambda (xi)
                                                     (lambda args
                                                       (apply (apply xi xs) args)))
                                                   xs))))
                         fs)))
                   (apply (car xs) xs)))
              (cons (list 'lambda (cons 'self names) body)
                    (map (lambda (binding)
                           (list 'lambda (cons 'self names)
                                 (cadr binding)))
                         header)))))))

;;; Defining initial environment which must contain all of the primitives used to define the interpreter too.
(define env-init
  (list
   (cons 'car (lambda (vs g k)
                (k (car (car vs)))))
   (cons 'cdr (lambda (vs g k)
                (k (cdr (car vs)))))
   (cons 'cons (lambda (vs g k)
                 (k (cons (car vs) (car (cdr vs))))))
   (cons 'list (lambda (vs g k)
		 (k vs)))
   (cons 'call/cc (lambda (vs g k)
		    ((car vs) (cons (lambda (vsp g kp) (k (car vsp))) '()) g k)))
   (cons 'apply (lambda (vs g k)
		  (let ((f (car vs))
			(a (car (cdr vs))))
		    (f a g k))))
   (cons 'integer? (lambda (vs g k)
		     (k (integer? (car vs)))))
   (cons 'boolean? (lambda (vs g k)
		     (k (boolean? (car vs)))))
   (cons 'char? (lambda (vs g k)
		  (k (char? (car vs)))))
   (cons 'string? (lambda (vs g k)
		    (k (string? (car vs)))))
   (cons 'null? (lambda (vs g k)
		  (k (null? (car vs)))))
   (cons 'symbol? (lambda (vs g k)
		    (k (symbol? (car vs)))))
   (cons 'equal? (lambda (vs g k)
		   (k (equal? (car vs) (car (cdr vs))))))
   (cons 'pair? (lambda (vs g k)
		  (k (pair? (car vs)))))
   (cons '+ (lambda (vs g k)
              (letrec ((loop (lambda (vs a)
                               (if (null? vs)
                                   (k a)
                                   (loop (cdr vs) (+ (car vs) a))))))
                (loop vs 0))))
   (cons '- (lambda (vs g k)
              (if (null? vs)
		  (errorf '- "incorrect argument count in call (-)")
		  (if (null? (cdr vs))
		      (k (- (car vs)))
		      (letrec ((loop (lambda (vs a)
				       (if (null? vs)
					   (k a)
					   (loop (cdr vs)(- a (car vs)))))))
			(loop (cdr vs)(car vs)))))))
   (cons '* (lambda (vs g k)
              (letrec ((loop (lambda (vs a)
                               (if (null? vs)
                                   (k a)
                                   (loop (cdr vs) (* (car vs) a))))))
		(loop vs 1))))
   (cons '/ (lambda (vs g k)
              (if (null? vs)
		  (errorf '/ "incorrect argument count in (/)")
		  (if (null? (cdr vs))
		      (k (/ (car vs)))
		      (letrec ((loop (lambda (vs a)
				       (if (null? vs)
					   (k a)
					   (loop (cdr vs)(/ a (car vs)))))))
			(loop (cdr vs)(car vs)))))))
   (cons 'not (lambda (vs g k)
		(k (not (car vs)))))
   (cons 'errorf (lambda (vs g k)
		   (apply errorf vs)))
   (cons '= (lambda (vs g k)
              (k (= (car vs) (car (cdr vs))))))
   ))

(define interpret
  (lambda (e g)
    (eval-cps '(lambda (f vs)
                 (if (null? vs)
                     '()
                     (cons (f (car vs)) (internal-map1 f (cdr vs)))))
              '()
              g
              (lambda (internal-map1)
                (eval-cps e '() (cons (cons 'internal-map1 internal-map1) g) (lambda (v) v))))))

;;; Defining tests for the interpreter

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
	      (printf "failed: (interpret (if #t 1 2) ~s)\n" g))
      (unless (equal? (interpret '(if #f 1 2) g) 2)
	      (printf "failed: (interpret (if #f 1 2) ~s)\n" g))
      (unless (equal? (interpret '(if 0 1 2) g) 1)
	      (printf "failed: (interpret (if 0 1 2) ~s)\n" g))
      (unless (equal? (interpret (interpret '((lambda (x) x) 1) g) g) 1)
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
      (unless (equal? (interpret (interpret '(let ((x 1) (y 2)) x) g) g) 1)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) x) ~s)\n" g))
      (unless (equal? (interpret '(let ((x 1) (y 2)) y) g) 2)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) y) ~s)\n" g))
      ;;; Testing for quote expressions and nested quote expressions
      (unless (equal? (interpret ''(if #t 1 2) g) '(if #t 1 2))
	      (printf "failed: (interpret '('(if #t 1 2)) ~s)\n" g))
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

(define test-not
  (lambda (g)
    (begin
      ;;; Testing for base values
      (when   (equal? (interpret 0 g) 0)
	      (printf "failed: (interpret 0 ~s)\n" g))
      (when   (equal? (interpret '#f g) #f)
	      (printf "failed: (interpret '#f ~s)\n" g))
      (when   (equal? (interpret '#\f g) #\f)
	      (printf "failed: (interpret '#\f ~s)\n" g))
      (when   (equal? (interpret "scheme_capstone" g) "scheme_capstone")
	      (printf "failed: (interpret \"scheme_capstone\" ~s)\n" g))
      (when   (equal? (interpret '() g) '())
	      (printf "failed: (interpret '() ~s)\n" g))
      ;;; Testing for if expressions
      (when   (equal? (interpret '(if #t 1 2) g) 1)
	      (printf "failed: (interpret (if #t 1 2) ~s)\n" g))
      (when   (equal? (interpret '(if #f 1 2) g) 2)
	      (printf "failed: (interpret (if #f 1 2) ~s)\n" g))
      (when   (equal? (interpret '(if 0 1 2) g) 1)
	      (printf "failed: (interpret (if 0 1 2) ~s)\n" g))
      (when   (equal? (interpret (interpret '((lambda (x) x) 1) g) g) 1)
	      (printf "failed: (interpret '((lambda (x) x) 1) ~s)\n" g))
      ;;; Testing for lambda expressions
      (when   (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "failed: (interpret '((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x y) x) 1 2) g) 1)
	      (printf "failed: (interpret ((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x y) y) 1 2) g) 2)
	      (printf "failed: (interpret ((lambda (x y) y) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda xs xs) 1 2) g) '(1 2))
	      (printf "failed: (interpret '((lambda xs xs) 1 2) ~s)\n" g))
      (when   (equal? (interpret '((lambda (x . xs) xs) 1 2 3) g) '(2 3))
	      (printf "failed: (interpret '((lambda (x . xs) xs) 1 2 3) ~s)\n" g))
      ;;; Testing for let expressions
      (when   (equal? (interpret '(let () 2) g) 2)
	      (printf "failed: (interpret '(let () 2) ~s)\n" g))
      (when   (equal? (interpret '(let ((x 1)) x) g) 1)
	      (printf "failed: (interpret '(let ((x 1)) x) ~s)\n" g))
      (when   (equal? (interpret (interpret '(let ((x 1) (y 2)) x) g) g) 1)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) x) ~s)\n" g))
      (when   (equal? (interpret '(let ((x 1) (y 2)) y) g) 2)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) y) ~s)\n" g))
      ;;; Testing for quote expressions and nested quote expressions
      (when   (equal? (interpret ''(if #t 1 2) g) '(if #t 1 2))
	      (printf "failed: (interpret '('(if #t 1 2)) ~s)\n" g))
      (when   (equal? (interpret ''((lambda (x y) x) 1 2) g)'((lambda (x y) x) 1 2))
	      (printf "failed: (interpret ''((lambda (x y) x) 1 2) ~s)\n" g))
      (when   (equal? (interpret '''(if #t 1 2) g) ''(if #t 1 2))
	      (printf "failed: (interpret '''(if #t 1 2) ~s)\n" g))
      (when   (equal? (interpret '''((lambda (x y) x) 1 2) g)''((lambda (x y) x) 1 2))
	      (printf "failed: (interpret '''((lambda (x y) x) 1 2) ~s)\n" g))
      ;;; Testing for cond expression
      (when   (equal? (interpret '(cond (#t 3)(#f 4)(else 1)) g) 3)
	      (printf "failed: (interpret '(cond (#t 3)(#f 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) g) 4)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) g) 3)
	      (printf "failed: (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) g) 1)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) g) 90)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))) ~s)\n" g))
      (when   (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) g) 9)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)) ~s)\n" g))
      (when   (equal? (interpret '(cond (else 2)) g) '2)
	      (printf "failed: (interpret '(cond (else 2)) ~s)\n" g))
      ;;; Testing for mathematical operator primitives
      (when   (equal? (interpret '(+) g) 0)
	      (printf "failed: (interpret '(+) ~s)\n" g))
      (when   (equal? (interpret '(+ 1) g) 1)
	      (printf "failed: (interpret '(+ 1) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 10) g) 11)
	      (printf "failed: (interpret '(+ 1 10) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 10 100) g) 111)
	      (printf "failed: (interpret '(+ 1 10 100) ~s)\n" g))
      (when   (equal? (interpret '(- 5) g) -5)
	      (printf "failed: (interpret '(- 5) ~s)\n" g))
      (when   (equal? (interpret '(- 5 1 2) g) 2)
	      (printf "failed: (interpret '(- 5 1 2) ~s)\n" g))
      (when   (equal? (interpret '(*) g) 1)
	      (printf "failed: (interpret '(*) ~s)\n" g))
      (when   (equal? (interpret '(* 5) g) 5)
	      (printf "failed: (interpret '(* 5) ~s)\n" g))
      (when   (equal? (interpret '(* 5 2 1) g) 10)
	      (printf "failed: (interpret '(* 5 2 1) ~s)\n" g))
      (when   (equal? (interpret '(* 5 2 100 0) g) 0)
	      (printf "failed: (interpret '(* 5 2 100 0) ~s)\n" g))
      (when   (equal? (interpret '(/ 5) g) 1/5)
	      (printf "failed: (interpret '(/ 5) ~s)\n" g))
      (when   (equal? (interpret '(/ 10 5) g) 2)
	      (printf "failed: (interpret '(/ 10 5) ~s)\n" g))
      (when   (equal? (interpret '(/ 1000 20 5) g) 10)
	      (printf "failed: (interpret '(/ 1000 20 5) ~s)\n" g))
      ;;; Testing for scheme list primitives
      (when   (equal? (interpret '(car (cons 1 2)) g) 1)
	      (printf "failed: (interpret '(car (cons 1 2)) ~s)\n" g))
      (when   (equal? (interpret '(cdr (cons 1 2)) g) '2)
	      (printf "failed: (interpret '(cdr (cons 1 2)) ~s)\n" g))
      (when   (equal? (interpret '(car '(1 2)) g) 1)
	      (printf "failed: (interpret '(car '(1 2)) ~s)\n" g))
      (when   (equal? (interpret '(cdr '(1 2)) g) '(2))
	      (printf "failed: (interpret '(cdr '(1 2)) ~s)\n" g))
      (when   (equal? (interpret '(list 1 2 3) g) '(1 2 3))
	      (printf "failed: (interpret '(list 1 2 3) ~s)\n" g))
      (when   (equal? (interpret '(list (list 1 2 3)) g) '((1 2 3)))
	      (printf "failed: (interpret '(list (list 1 2 3)) ~s)\n" g))
      ;;; Testing for call/cc
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) 10))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) 10))) ~s)\n" g))
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) (k 10)))) ~s)\n" g))
      (when   (equal? (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) g) 15)
	      (printf "failed: (interpret '(+ 5 (call/cc (lambda (k) (/ (k 10) 0)))) ~s)\n" g))
      (when   (equal? (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) g) 2)
	      (printf "failed: (interpret '(/ 50 (call/cc (lambda (k) (+ 0 (k 25))))) ~s)\n" g))
      ;;; Testing for simple cases of apply
      (when   (equal? (interpret '(apply + '(1 2 3)) g) 6)
	      (printf "failed: (interpret '(apply + '(1 2 3)) ~s)\n" g))
      (when   (equal? (interpret '(apply (lambda (x) x) '(10)) g) 10)
	      (printf "failed: (interpret '(apply (lambda (x) x) '(10)) ~s)\n" g))
      (when   (equal? (interpret '(apply * '(10 5 -1)) g) -50)
	      (printf "failed: (interpret '(apply * '(10 5 -1)) ~s)\n" g))
      (when   (equal? (interpret '(apply (lambda () 1) '()) g) 1)
	      (printf "failed: (interpret '(apply (lambda () 1) '()) ~s)\n" g))
      ;;; Testing for more complex cases of apply
      (when   (equal? (interpret '(apply apply (list + (list 5 3 1))) g) 9)
	      (printf "failed: (interpret '(apply apply (list + (list 5 3 1))) ~s)\n" g))
      (when   (equal? (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) g) 31)
	      (printf "failed: (interpret '(apply apply (list (lambda (a b) (+ a (+ b 1))) (list 10 20))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) 10)))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) (k 10))))) ~s)\n" g))
      (when   (equal? (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) g) 11)
	      (printf "failed: (interpret '(+ 1 (apply call/cc (list (lambda (k) (/ (k 10) 0))))) ~s)\n" g))
      ;;; Testing for letrec expressions

      ;; Testing using factorial function
      (when   (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) g) 120)
	      (printf "failed: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 5)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) g) 5040)
	      (printf "failed: (interpret '(letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))))))) (fac 7)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #t
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 12)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 12)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 5)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 5)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 31)) g) #t)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 31)) ~s)\n" g))
      
      ;; Testing using ternary preternary postternary where result is expected to be #f
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (ternary 10)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(ternary 10)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (preternary 21)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(preternary 21)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))
					   (postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))
					   (preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))
				    (postternary 20)) g) #f)
	      (printf "failed: (interpret '(letrec ((ternary (lambda (n) (if (= n 0) #t (preternary (- n 1)))))(postternary (lambda (n) (if (= n 0) #f (ternary (- n 1)))))(preternary (lambda (n) (if (= n 0) #f (postternary (- n 1))))))(postternary 20)) ~s)\n" g))

      ;; Testing using even odd where result is expected to be #t
      (when   (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 10)) g) #t)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 10)) ~s)\n" g))
      (when   (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 7)) g) #t)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 7)) ~s)\n" g))

      ;; Testing using even odd where result is expected to be #f
      (when (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					 (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (even 9)) g) #f)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(even 9)) ~s)\n" g))
      (when (equal? (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))
					   (odd (lambda (n) (if (= n 0) #f (even (- n 1))))))
				    (odd 6)) g) #f)
	      (printf "failed: (interpret '(letrec ((even (lambda (n) (if (= n 0) #t (odd (- n 1)))))(odd (lambda (n) (if (= n 0) #t (even (- n 1))))))(odd 6)) ~s)\n" g))
      )))

;;; Defining the auxiliary functions that will be used to parse through list of definitions

(define is-define?
  (lambda (expression)
    (and (pair? expression)
         (let ((car-expression (car expression))
               (cdr-expression (cdr expression)))
           (and (equal? (car expression) 'define)
                (pair? cdr-expression)
                (let ((cadr-expression (car cdr-expression))
                      (cddr-expression (cdr cdr-expression)))
                  (and (symbol? cadr-expression)
                       (pair? cddr-expression)
                       (let ((caddr-expression (car cddr-expression))
                             (cdddr-expression (cdr cddr-expression)))
                         (null? cdddr-expression)))))))))

(define run
  (lambda (dse)
    (letrec ((loop (lambda (e es g)
                     (cond
                       ((pair? es)
                        (if (is-define? e)
                            (let ((name (car (cdr e)))
                                  (definiens (car (cdr (cdr e)))))
                              (eval-cps definiens '() g (lambda (v)
                                                              (loop (car es) (cdr es) (cons (cons name v) g)))))
                            (errorf 'run "not a definition")))
                       ((null? es)
                        (eval-cps e '() g (lambda (v)
                                                v)))
                       (else
                        (errorf 'run "improper input"))))))
      (if (pair? dse)
          (loop (car dse) (cdr dse) '())
          (errorf 'run "improper input")))))

;;; Testing the auxiliary functions

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
      )))

;;; Testing the usage of the run function together with the file-parser function

(define test-run-on-parsed-res
  (lambda ()
    (begin
      (unless (equal? (run (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/test_file_parser.scm")) "hello")
	      (printf "failed: (run (file-parser \"test_file_parser.scm\"))\n"))
      )))

(define test-not-run-on-parsed-res
  (lambda ()
    (begin
      (when (equal? (run (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/test_file_parser.scm")) "hello")
	      (printf "failed: (run (file-parser \"test_file_parser.scm\"))\n"))
      )))

