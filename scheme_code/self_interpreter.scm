;;; self_interpreter.scm
;;; Time-stamp: <2021-03-03 14:49:30 olivier>

(define eval-cps
  (lambda (e r xs k)
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
      (if (member e xs)
	  (k (lookup-rec e r))
	  (k (lookup e r))))
     ((pair? e)
      (let ((e0 (car e))
	    (es (cdr e)))
	(if (symbol? e0)
	    (cond
	     ((equal? e0 'if)
	      (eval-cps (car es)
			r
			xs
			(lambda (v0)
			  (if v0
			      (eval-cps (car (cdr es)) r xs k)
			      (eval-cps (car (cdr (cdr es))) r xs k)))))
	     ((equal? e0 'lambda)
	      (let ((formals (car es))
		    (body (car (cdr es))))
		(k (lambda (vs k)
                     (eval-cps body
                               (extend-env formals vs r)
                               xs
                               k)))))
	     ((equal? e0 'let)
	      (evlet-cps (car es)
			 (car (cdr es))
			 r
			 xs
			 k))
	     ((equal? e0 'cond)
              (letrec ((loop (lambda (c cs)
			       (if (null? cs)
				   (if (equal? (car c) 'else)
				       (eval-cps (car (cdr c)) r xs k)
				       (errorf 'eval-cps "cond should end with else"))
				   (let ((test (car c))
					 (consequent (car (cdr c))))
				     (eval-cps test r xs (lambda (v)
							   (if v
							       (eval-cps consequent r xs k)
							       (loop (car cs) (cdr cs))))))))))
                (loop (car es) (cdr es))))
	     ((equal? e0 'letrec)
	      (errorf 'eval-cps "Not implemented yet"))
	     ((equal? e0 'quote)
	      (k (car es)))
	     (else
	      (evlis-cps es r xs (lambda (vs)
				   ((lookup e0 r) vs k)))))
	    (eval-cps e0 r xs (lambda (v0)
				(evlis-cps es r xs (lambda (vs)
						     (v0 vs k))))))))
     (else
      (errorf 'eval-cps "UFO")))))

(define evlis-cps
  (lambda (es r xs k)
    (cond
     ((null? es)
      (k '()))
     ((pair? es)
      (eval-cps (car es) r xs (lambda (v)
				(evlis-cps (cdr es) r xs (lambda (vs)
							   (k (cons v vs)))))))
     (else
      (errorf 'evlis-cps "Improper list of actuals")))))

(define evlet-cps-aux
  (lambda (bs r ys k)
    (cond
     ((null? bs)
      (k '() '()))
     ((pair? bs)
      (let ((b (car bs))
	    (bsp (cdr bs)))
	(let ((x (car b))
	      (d (car (cdr b))))
	  (eval-cps d r ys (lambda (v)
			     (evlet-cps-aux bsp r ys (lambda (xs vs)
						       (k (cons x xs) (cons v vs)))))))))
     (else
      (errorf 'evlet-cps-aux "Improper let header")))))

(define evlet-cps
  (lambda (bs e r ys k)
    (evlet-cps-aux bs r ys (lambda (xs vs)
			     (eval-cps e (extend-env xs vs r) ys k)))))

(define extend-env
  (lambda (xs vs r)
    (cond
     ((symbol? xs)
      (cons (cons xs vs) r))
     ((pair? xs)
      (extend-env-aux xs vs r))
     ((null? xs)
      r)
     (else
      (errorf 'extend-env "Illegal formals in extend-env")))))
	
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

(define lookup
  (lambda (x r)
    (if (null? r)
	(errorf 'lookup "Unbound variable: ~s" x)
	(if (equal? x (car (car r)))
	    (cdr (car r))
	    (lookup x (cdr r))))))

;;; Defining initial environment which must contain all of the primitives used to define the interpreter too.
(define env-init
  (list
   (cons 'car (lambda (vs k)
                (k (car (car vs)))))
   (cons 'cdr (lambda (vs k)
                (k (cdr (car vs)))))
   (cons 'cons (lambda (vs k)
                 (k (cons (car vs) (car (cdr vs))))))
   (cons 'list (lambda (vs k)
		 (k vs)))
   (cons 'call/cc (lambda (vs k)
		    ((car vs) (cons (lambda (vsp kp) (k (car vsp))) '()) k)))
   (cons 'apply (lambda (vs k)
		  (let ((f (car vs))
			(a (car (cdr vs))))
		    (f a k))))
   (cons 'integer? (lambda (vs k)
		     (k (integer? (car vs)))))
   (cons 'boolean? (lambda (vs k)
		     (k (boolean? (car vs)))))
   (cons 'char? (lambda (vs k)
		  (k (char? (car vs)))))
   (cons 'string? (lambda (vs k)
		    (k (string? (car vs)))))
   (cons 'null? (lambda (vs k)
		  (k (null? (car vs)))))
   (cons 'symbol? (lambda (vs k)
		    (k (symbol? (car vs)))))
   (cons 'equal? (lambda (vs k)
		   (k (equal? (car vs)(car (cdr vs))))))
   (cons 'pair? (lambda (vs k)
		  (k (pair? (car vs)))))
   (cons '+ (lambda (vs k)
              (letrec ((loop (lambda (vs a)
                               (if (null? vs)
                                   (k a)
                                   (loop (cdr vs) (+ (car vs) a))))))
                (loop vs 0))))
   (cons '- (lambda (vs k)
              (if (null? vs)
		  (errorf '- "incorrect argument count in call (-)")
		  (if (null? (cdr vs))
		      (k (- (car vs)))
		      (letrec ((loop (lambda (vs a)
				       (if (null? vs)
					   (k a)
					   (loop (cdr vs)(- a (car vs)))))))
			(loop (cdr vs)(car vs)))))))
   (cons '* (lambda (vs k)
              (letrec ((loop (lambda (vs a)
                               (if (null? vs)
                                   (k a)
                                   (loop (cdr vs) (* (car vs) a))))))
		(loop vs 1))))
   (cons '/ (lambda (vs k)
              (if (null? vs)
		  (errorf '/ "incorrect argument count in (/)")
		  (if (null? (cdr vs))
		      (k (/ (car vs)))
		      (letrec ((loop (lambda (vs a)
				       (if (null? vs)
					   (k a)
					   (loop (cdr vs)(/ a (car vs)))))))
			(loop (cdr vs)(car vs)))))))
   (cons 'not (lambda (vs k)
		(k (not (car vs)))))
   (cons 'errorf (lambda (vs k)
		   (apply errorf vs)))
   ))
      
(define interpret
  (lambda (e)
    (eval-cps e env-init '() (lambda (v) v))))

;;; > (interpret 0)
;;; 0
;;; > (interpret (if #t 1 2))
;;; 1
;;; > (interpret (if #f 1 2))
;;; 2
;;; > (interpret (if 0 1 2))
;;; 1
;;; > (interpret '((lambda (x) x) 1))
;;; 1
;;; > (interpret '((lambda (x y) x) 1 2))
;;; 1
;;; > (interpret '((lambda (x y) y) 1 2))
;;; 2
;;; > (interpret '((lambda xs xs) 1 2))
;;; (1 2)
;;; > (interpret '((lambda (x . xs) xs) 1 2 3))
;;; (2 3)
;;; > (interpret '(let () 2))
;;; 2
;;; > (interpret '(let ((x 1)) x))
;;; 1
;;; > (interpret '(let ((x 1) (y 2)) x))
;;; 1
;;; > (interpret '(let ((x 1) (y 2)) y))
;;; 2
;;; > 

(define test
  (lambda ()
    (begin
      ;;; Testing for base values
      (unless (equal? (interpret 0) 0)
	      (printf "failed: (interpret 0)\n"))
      (unless (equal? (interpret '#f) #f)
	      (printf "failed: (interpret '#f)\n"))
      (unless (equal? (interpret '#\f) #\f)
	      (printf "failed: (interpret '#\f)\n"))
      (unless (equal? (interpret "scheme_capstone") "scheme_capstone")
	      (printf "failed: (interpret \"scheme_capstone\")\n"))
      (unless (equal? (interpret '()) '())
	      (printf "failed: (interpret '())\n"))
      ;;; Testing for if expressions
      (unless (equal? (interpret '(if #t 1 2)) 1)
	      (printf "failed: (interpret (if #t 1 2))\n"))
      (unless (equal? (interpret '(if #f 1 2)) 2)
	      (printf "failed: (interpret (if #f 1 2))\n"))
      (unless (equal? (interpret '(if 0 1 2)) 1)
	      (printf "failed: (interpret (interpret (if 0 1 2)))\n"))
      (unless (equal? (interpret (interpret '((lambda (x) x) 1))) 1)
	      (printf "failed: (interpret '((lambda (x) x) 1))\n"))
      ;;; Testing for lambda expressions
      (unless (equal? (interpret '((lambda (x y) x) 1 2)) 1)
	      (printf "failed: (interpret '((lambda (x y) x) 1 2))\n"))
      (unless (equal? (interpret '((lambda (x y) x) 1 2)) 1)
	      (printf "failed: (interpret ((lambda (x y) x) 1 2))\n"))
      (unless (equal? (interpret '((lambda (x y) y) 1 2)) 2)
	      (printf "failed: (interpret ((lambda (x y) y) 1 2))\n"))
      (unless (equal? (interpret '((lambda xs xs) 1 2)) '(1 2))
	      (printf "failed: (interpret '((lambda xs xs) 1 2))\n"))
      (unless (equal? (interpret '((lambda (x . xs) xs) 1 2 3)) '(2 3))
	      (printf "failed: (interpret '((lambda (x . xs) xs) 1 2 3))\n"))
      ;;; Testing for let expressions
      (unless (equal? (interpret '(let () 2)) 2)
	      (printf "failed: (interpret '(let () 2))\n"))
      (unless (equal? (interpret '(let ((x 1)) x)) 1)
	      (printf "failed: (interpret '(let ((x 1)) x))\n"))
      (unless (equal? (interpret (interpret '(let ((x 1) (y 2)) x))) 1)
	      (printf "failed: (interpret (interpret '(let ((x 1) (y 2)) x)))\n"))
      (unless (equal? (interpret '(let ((x 1) (y 2)) y)) 2)
	      (printf "failed: (interpret '(let ((x 1) (y 2)) y))\n"))
      ;;; Testing for quote expressions and nested quote expressions
      (unless (equal? (interpret ''(if #t 1 2)) '(if #t 1 2))
	      (printf "failed: (interpret '('(if #t 1 2)))\n"))
      (unless (equal? (interpret ''((lambda (x y) x) 1 2))'((lambda (x y) x) 1 2))
	      (printf "failed: (interpret ''((lambda (x y) x) 1 2))\n"))
      (unless (equal? (interpret '''(if #t 1 2)) ''(if #t 1 2))
	      (printf "failed: (interpret '''(if #t 1 2))\n"))
      (unless (equal? (interpret '''((lambda (x y) x) 1 2))''((lambda (x y) x) 1 2))
	      (printf "failed: (interpret '''((lambda (x y) x) 1 2))\n"))
      ;;; Testing for cond expression
      (unless (equal? (interpret '(cond (#t 3)(#f 4)(else 1))) 3)
	      (printf "failed: (interpret '(cond (#t 3)(#f 4)(else 1)))\n"))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1))) 4)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) 4)(else 1)))\n"))
      (unless (equal? (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1))) 3)
	      (printf "failed: (interpret '(cond (#t 3)((equal? 5 5) 4)(else 1)))\n"))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9))) 1)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 5) (- 5 3 1))(else 9)))\n"))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9)))) 90)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else (* 10 9))))\n"))
      (unless (equal? (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9))) 9)
	      (printf "failed: (interpret '(cond (#f 3)((equal? 5 4) (- 5 3 1))(else 9)))\n"))
      (unless (equal? (interpret '(cond (else 2))) '2)
	      (printf "failed: (interpret '(cond (else 2)))\n"))
      ;;; Testing for mathematical operator primitives
      (unless (equal? (interpret '(+)) 0)
	      (printf "failed: (interpret '(+))\n"))
      (unless (equal? (interpret '(+ 1)) 1)
	      (printf "failed: (interpret '(+ 1))\n"))
      (unless (equal? (interpret '(+ 1 10)) 11)
	      (printf "failed: (interpret '(+ 1 10))\n"))
      (unless (equal? (interpret '(+ 1 10 100)) 111)
	      (printf "failed: (interpret '(+ 1 10 100))\n"))
      (unless (equal? (interpret '(- 5)) -5)
	      (printf "failed: (interpret '(- 5))\n"))
      (unless (equal? (interpret '(- 5 1 2)) 2)
	      (printf "failed: (interpret '(- 5 1 2))\n"))
      (unless (equal? (interpret '(*)) 1)
	      (printf "failed: (interpret '(*))\n"))
      (unless (equal? (interpret '(* 5)) 5)
	      (printf "failed: (interpret '(* 5))\n"))
      (unless (equal? (interpret '(* 5 2 1)) 10)
	      (printf "failed: (interpret '(* 5 2 1))\n"))
      (unless (equal? (interpret '(* 5 2 100 0)) 0)
	      (printf "failed: (interpret '(* 5 2 100 0))\n"))
      (unless (equal? (interpret '(/ 5)) 1/5)
	      (printf "failed: (interpret '(/ 5))\n"))
      (unless (equal? (interpret '(/ 10 5)) 2)
	      (printf "failed: (interpret '(/ 10 5))\n"))
      (unless (equal? (interpret '(/ 1000 20 5)) 10)
	      (printf "failed: (interpret '(/ 1000 20 5))\n"))
      ;;; Testing for scheme list primitives
      (unless (equal? (interpret '(car (cons 1 2))) 1)
	      (printf "failed: (interpret '(car (cons 1 2)))\n"))
      (unless (equal? (interpret '(cdr (cons 1 2))) '2)
	      (printf "failed: (interpret '(cdr (cons 1 2)))\n"))
      (unless (equal? (interpret '(car '(1 2))) 1)
	      (printf "failed: (interpret '(car '(1 2)))\n"))
      (unless (equal? (interpret '(cdr '(1 2))) '(2))
	      (printf "failed: (interpret '(cdr '(1 2)))\n"))
      ;;; TO DO: Add tests for call/cc and apply and list
      )))

;;; To add cond form to interpret
;;; inside the interpreter, and since we have to interpret files like this as well, we need to handle define too.
