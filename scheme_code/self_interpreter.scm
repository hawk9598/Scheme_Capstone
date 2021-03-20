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
	  
