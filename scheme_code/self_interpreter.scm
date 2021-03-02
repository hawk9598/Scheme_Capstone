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
	      (let ((formals (car (cdr e0)))
		    (body (car (cdr (cdr e0)))))
		(k (trace-lambda "hello" (vs k)
				 (eval-cps body
					   (extend-env formals vs r)
					   xs
					   k)))))
	     ((equal? e0 'let)
	      (evlet-cps (car (cdr e0))
			 (car (cdr (cdr e0)))
			 r
			 xs
			 k))
	     ((equal? e0 'letrec)
	      (errorf 'eval-cps "Not implemented yet"))
	     (else
	      (evlis-cps es r xs (lambda (vs)
				   ((lookup-env e0 r) vs k)))))
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

(define lookup-env
  (lambda (x r)
    (if (null? r)
	(errorf 'lookup-env "Unbound variable: ~s" x)
	(if (equal? x (car (car r)))
	    (cdr (car r))
	    (lookup-env x (cdr r))))))
