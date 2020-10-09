;;; interpreter-for-Scheme-with-thunks.scm
;;; dProgSprog 2014-2015, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 19 May 2015

;;; This file is a clone of the file "self-interpreter.scm",
;;; with one uniform change:
;;; - all variables now denote thunks (ie, parameterless procedures), and
;;; - looking them up forces the thunk they denote.

;;; This file requires a definition of the procedures _delay and _show,
;;; as in the files
;;;    "interpreter-for-Scheme-with-call-by-value.scm"
;;;    "interpreter-for-Scheme-with-call-by-name.scm"
;;;    "interpreter-for-Scheme-with-call-by-need.scm"

;;; In addition,
;;; - for minimalism, equal? has been changed to eqv?

;;;;;;;;;;

;;; self-interpreter.scm
;;; dProgSprog 2014-2015, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; a deliberately unreadable self-interpreter in direct style for (a subset of) Scheme
;;; Version 2.0 -- 06 April 2014

;;; Use: start Petite Chez Scheme and load "self-interpreter.scm"
;;; then type (start-the-interpreter ">> ")
;;
;;; This interpreter is totally meta-circular.
;;; It can be loaded in a session with
;;;       (load "self-interpreter.scm")
;;; and started with
;;;       (start-the-interpreter ">>> ")
;;; at the price of a measurable slowness.
;;; NB. The prompts grow to reflect the growing layers of interpretation.
;;; 
;;; To exit, type
;;;       (exit)
;;; at the toplevel.

;;; Syntax:
;;; 
;;;       <toplevel-form> ::= (define <identifier> <expression>)
;;;                         | <expression>
;;;                         | (load <string>)
;;;                         | (exit)
;;;      
;;;          <expression> ::= <integer>
;;;                         | <char>
;;;                         | <string>
;;;                         | <boolean>
;;;                         | <identifier>
;;;                         | (quote <representable-value>)
;;;                         | <lambda-expression>
;;;                         | (if <expression> <expression> <expression>)
;;;                         | (cond
;;;                             {[<expression> <expression>]}*
;;;                             [else <expression>])
;;;                         | (set! <identifier> <expression>)
;;;                         | (and <expression>*)
;;;                         | (or <expression>*)
;;;                         | (begin {<expression>}+)
;;;                         | (case <expression>
;;;                             {[({<representable-value>}*) <expression>]}*
;;;                             [else <expression>])
;;;                         | (let ({[<identifier> <expression>]}*)
;;;                             <expression>)
;;;                         | (let* ({[<identifier> <expression>]}*)
;;;                             <expression>)
;;;                         | (letrec ({[<identifier> <lambda-expression>]}*)
;;;                             <expression>)
;;;                         | (time <expression>)
;;;                         | (<expression> {<expression>}*)
;;;   
;;;   <lambda-expression> ::= (lambda ({<identifier>}*) <expression>)
;;; 
;;;          <identifier> ::= <symbol>
;;;                           that is not a keyword
;;; 
;;; <representable-value> ::= <integer>
;;;                         | <string>
;;;                         | <boolean>
;;;                         | <symbol>
;;;                         | ()
;;;                         | (<representable-value> . <representable-value>)
;;; 
;;; where <symbol>, <integer>, <char>, <string>, and <boolean> are the usual
;;; non-terminals:
;;; 
;;;             <boolean> ::= #t
;;;                         | #f
;;; 
;;; etc.

;;; predefined procedures:
;;;	< <= >= >
;;;     car cdr
;;;     caar cadr
;;;     cdar cddr
;;;     caddr cdddr
;;;     list-ref
;;;     list-tail
;;;     last-pair
;;;     null? pair?
;;;     integer? number? string? symbol?
;;;     zero? 1+ 1-
;;;     + - *
;;;     cons eqv?
;;;     = boolean?
;;;     negative? positive?
;;;     procedure?
;;;     list
;;;     set-car! set-cdr!
;;;     display
;;;     pretty-print newline
;;;     not length
;;;     load read
;;;     open-input-file eof-object?
;;;     close-input-port
;;;     member
;;;     time
;;;     errorf

;;; Semantics:
;;;
;;; Values:
;;; Val = Num + String + Ide + Pair + Proc + Subr + Fsubr
;;;
;;; Environments -- lexical extensions, then global, then pre-defined:
;;; Env = (Ide* x Val*)*
;;;
;;; Procedures, primitive functions and special forms:
;;; Proc =  Val* -> val
;;; Subr = Val* -> Val
;;; Fsubr = Expr* x Env -> Val

;;; ------ auxiliaries ------------------------------------------------------

(define _constant?
  (lambda (x)
    (or (number? x)
        (char? x)
        (string? x)
        (boolean? x))))

;;; (define _identifier?
;;;   (lambda (v)
;;;     (and (symbol? v)
;;;          (not (member v (list 'define 'load 'exit 'quote 'lambda 'if 'cond 'else 'set! 'and 'or 'begin 'case 'let 'let* 'letrec 'time))))))

(define _identifier?
  (lambda (v)
    (symbol? v)))

(define _applicable?
  (lambda (x)
    (and (pair? x)
         (case (car x)
           [(subr fsubr proc)
            (and (= 3 (length x))
                 (number? (cadr x))
                 (procedure? (caddr x)))]
           [else
	    #f]))))

(define _index
  (lambda (i is)
    (letrec ([loop
	      (lambda (n is)
		(cond
		  [(null? is)
		   -1]
		  [(eqv? i (car is))
		   n]
		  [else
		   (loop (+ n 1) (cdr is))]))])
      (loop 0 is))))

(define _fetch-ftype car)
(define _fetch-arity cadr)
(define _fetch-value caddr)

(define _inProc
  (lambda (n a)
    (list 'proc n a)))

;;; Basic lexical environment extension:
(define _extend_env
  (lambda (xs vs env)
    (cons (cons xs vs) env)))

(define _access
    car)

(define _update
    set-car!)

(define _force
  (lambda (suspension)
    (suspension)))

(define _thunkify
  (lambda (e)
    (if (pair? e)
	(cons (_delay
		(lambda ()
		  (_thunkify (car e))))
	      (_delay (lambda ()
			(_thunkify (cdr e)))))
	e)))

;;; ----- the core -----------------------------------------------------------

;;; Expr * Env -> Val
(define _eval
  (lambda (e r)
    (cond
      [(_constant? e)
       e]
      [(_identifier? e)
       (_lookup e r)]
      [(pair? e)
       (_apply (_eval (car e) r) (cdr e) r)]
      [else
       (errorf '_eval "unknown form: ~s" e)])))

; Ide * Env -> Val
(define _lookup
  (lambda (i r)
    (if (null? r)
        (let ([pos (_index i table-toplevel-identifiers)])
          (if (not (negative? pos))
              (_force (_access (list-tail table-toplevel-values pos)))
              (errorf '_lookup "unbound identifier: ~s" i)))
        (let ([pos (_index i (caar r))])
          (if (not (negative? pos))
              (_force (_access (list-tail (cdar r) pos)))
              (_lookup i (cdr r)))))))

; Fun * List-of-Expr * Env -> Val
(define _apply
  (lambda (fo es r)
    (if (_applicable? fo)
        (case (_fetch-ftype fo)
          ((subr)
           (_apply_subr fo es r))
          ((fsubr)
           (_apply_fsubr fo es r))
          ((proc)
           (_apply_procedure fo es r))
          (else
            (errorf '_apply "unknown functional object: ~s" (car fo))))
        (errorf '_apply "unapplicable value: ~s" fo))))

; Subr * List-of-Expr * Env -> Val
(define _apply_subr
  (lambda (f es r)
    (if (not (= (length es) (_fetch-arity f)))
        (errorf '_apply_subr "arity mismatch: ~s" es)
        (case (_fetch-arity f)
          [(0)
           ((_fetch-value f))]
          [(1)
           ((_fetch-value f) (_delay (lambda () (_eval (car es) r))))]
          [(2)
           ((_fetch-value f) (_delay (lambda () (_eval (car es) r)))
                             (_delay (lambda () (_eval (cadr es) r))))]
          [(3)
           ((_fetch-value f) (_delay (lambda () (_eval (car es) r)))
                             (_delay (lambda () (_eval (cadr es) r)))
                             (_delay (lambda () (_eval (caddr es) r))))]
          [else
           (errorf '_apply_subr "arity: ~s" f)]))))

; Fsubr * List-of-Expr * Env -> Val
(define _apply_fsubr
  (lambda (fv es r)
    (if (or (= (length es) (_fetch-arity fv))
            (zero? (_fetch-arity fv))) ; arbitrary number of arguments
        ((_fetch-value fv) es r)
        (errorf '_apply_fsubr "arity mismatch: ~s" es))))

; Proc * List-of-Expr * Env * Cont * Meta-Cont -> Val
(define _apply_procedure
  (lambda (p es r)
    (if (not (= (length es) (_fetch-arity p)))
        (errorf '_apply_procedure "arity mismatch: ~s" es)
        ((_fetch-value p) (_evlis es r)))))

; List-of-Expr * Env -> Val
(define _evlis
  (lambda (es r)
    (if (null? es)
        '()
        ;;; left-to-right evaluation:
        (let ([v (_delay (lambda () (_eval (car es) r)))])
          (cons v (_evlis (cdr es) r))))))

;;; ----- the special forms: -------------------------------------------------

(define _quote
  (lambda (es r)
    (_thunkify (car es))))

(define _fetch-test car)
(define _fetch-then cadr)
(define _fetch-else caddr)

(define _if
  (lambda (es r)
    (case (_eval (_fetch-test es) r)
      [(#f)
       (_eval (_fetch-then es) r)]
      [else
       (_eval (_fetch-else es) r)])))

;;; simpler definition:
(define _if
  (lambda (es r)
    (if (_eval (_fetch-test es) r)
        (_eval (_fetch-then es) r)
        (_eval (_fetch-else es) r))))

(define _cond
  (lambda (cs r)
    (if (null? cs)
        (errorf '_cond "out of clauses: ~s" cs)
        (let ([c (car cs)])
          (cond
            [(eqv? (car c) 'else)
             (_eval (cadr c) r)]
            [(_eval (car c) r)
             (_eval (cadr c) r)]
            [else
             (_cond (cdr cs) r)])))))

(define _lambda
  (lambda (es r)
    (let ([xs (car es)]
          [e (cadr es)])
      (_inProc (length xs)
               (lambda (vs)
                 (_eval e (_extend_env xs vs r)))))))

(define _set!
  (lambda (es r)
    (let ([x (car es)]
          [e (cadr es)])
      (if (not (_identifier? x))
          (errorf '_set! "not an identifier: ~s" x)
          (_L_set! x (_delay (lambda () (_eval e r))) r)))))

(define _L_set!
  (lambda (i v r)
    (let ((pos (_index i (caar r))))
      (cond
	[(not (negative? pos))
	 (let* ([location (list-tail (cdar r) pos)]
		[previous-value (_access location)])
	   (begin
	     (_update location v)
	     previous-value))]
	[(null? (cdr r))
	 (let ([pos (_index i table-toplevel-identifiers)])
	   (if (not (negative? pos))
	       (begin
		 (set-car! (car r) (cons i (caar r)))
		 (set-cdr! (car r) (cons v (cdar r)))
		 (_access (list-tail table-toplevel-values pos)))
	       (errorf '_L_set! "undefined variable: ~s" i)))]
	[else
	 (_L_set! i v (cdr r))]))))

(define _case 
  (lambda (es r)
    (let ([v (_eval (car es) r)]) 
      (letrec ([loop
		(lambda (es)
		  (cond
		    [(null? es)
		     (errorf '_case "unmatched: ~s" v)]
		    [(eqv? (caar es) 'else)
		     (_eval (cadr (car es)) r)]
		    [(member v (caar es))
		     (_eval (cadr (car es)) r)]
		    [else
		     (loop (cdr es))]))])
	(loop (cdr es))))))

(define _and
  (lambda (es r)
    (letrec ([visit (lambda (e es)
                      (if (null? es)
                          (_eval e r)
                          (case (_eval e r)
                            [(#f)
                             #f]
                            [else
                             (visit (car es) (cdr es))])))])
      (if (null? es)
          #t
          (visit (car es) (cdr es))))))

(define _or
  (lambda (es r)
    (letrec ([visit (lambda (e es)
                      (if (null? es)
                          (_eval e r)
                          (let ([v (_eval e r)])
                            (case v
                              [(#f)
                               (visit (car es) (cdr es))]
                              [else
                               v]))))])
      (if (null? es)
          #f
          (visit (car es) (cdr es))))))

(define _begin
  (lambda (es r)
    (if (null? (cdr es))
        (_eval (car es) r)
        (begin
          (_eval (car es) r)
          (_begin (cdr es) r)))))

(define _let                  ; assumes a well-formed let construction
  (lambda (es r)
    (let ([bs (car es)]
          [e (cadr es)])
      (if (null? bs)
          (_eval e r)
          (_eval e (_extend_env (_let_idlis bs)
                                (_let_evlis bs r)
				r))))))

(define _let_evlis
  (lambda (bs r)
    (if (null? bs)
        '()
        (cons (_delay (lambda () (_eval (cadr (car bs)) r)))
              (_let_evlis (cdr bs) r)))))

(define _let_idlis
  (lambda (bs)
    (if (null? bs)
        '()
        (cons (car (car bs)) (_let_idlis (cdr bs))))))

(define _letrec            ; assumes a well-formed letrec construction
  (lambda (es r)
    (let ([bs (car es)]
          [e (cadr es)])
      (if (null? bs)
          (_eval e r)
          (let* ([r (_extend_env (_let_idlis bs) '() r)]
                 [vs (_let_evlis bs r)])
            (begin
              (set-cdr! (car r) vs)
              (_eval e r)))))))

(define _let*                   ; assumes a well-formed let* construction
    (lambda (es r)
      (_let*_evlis (car es) (cadr es) r)))

(define _let*_evlis
  (lambda (bs e r)
    (if (null? bs)
	(_eval e r)
	(let ([b (car bs)])
	  (_let*_evlis (cdr bs)
		       e
		       (_extend_env (list (car b))
				    (list (_delay (lambda () (_eval (cadr b) r))))
				    r))))))

(define _time
  (lambda (es r)
    (time (_eval (car es) r))))

;;; ----- the predefined procedures ------------------------------------------

(define _read
  (lambda (es r)
    (_thunkify (cond
		 [(null? es)
		  (read)]
		 [(null? (cdr es))
		  (read (_eval (car es) r))]
		 [else
		  (errorf '_read "arity mismatch: ~s" es)]))))

; ----- the initial environment -----------------------------------------------

(define table-toplevel-identifiers
      '(< <= >= >
	car cdr
        caar cadr
        cdar cddr
        caddr cdddr
        list-ref
        list-tail
        last-pair
        null? pair?
        integer? number? string? symbol?
        zero? 1+ 1-
        + - * /
        cons eqv? equal?
        = string=? char=? boolean=? boolean?
        negative? positive?
        procedure?
        quote
        lambda
        if
        cond
        set!
        case
        and or
        list
        set-car! set-cdr!
        begin
        display print
        pretty-print newline
        not length
	read
        open-input-file eof-object?
        close-input-port
        let letrec
        let*
	member
	time
	errorf
        random
        ))

(define make-subr-1
  (lambda (s)
    (_delay
      (lambda ()
	(list 'subr 1 (lambda (t1)
			(s (_force t1))))))))

(define make-subr-2
  (lambda (s)
    (_delay
      (lambda ()
	(list 'subr 2 (lambda (t1 t2)
			(s (_force t1) (_force t2))))))))

(define make-subr-3
  (lambda (s)
    (_delay 
      (lambda ()
	(list 'subr 3 (lambda (t1 t2 t3)
			(s (_force t1) (_force t2) (_force t3))))))))

(define table-toplevel-values
  (list (make-subr-2 <)
	(make-subr-2 <=)
	(make-subr-2 >=)
	(make-subr-2 >)
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (car (_force t)))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (cdr (_force t)))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (car (_force (car (_force t)))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (car (_force (cdr (_force t)))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (cdr (_force (car (_force t)))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (cdr (_force (cdr (_force t)))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (car (_force (cdr (_force (cdr (_force t)))))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (_force (cdr (_force (cdr (_force (cdr (_force t)))))))))))
	(_delay
	  (lambda ()
	    (list 'subr 2 (lambda (t1 t2)
			    (letrec ([visit
				      (lambda (vs n)
					(cond
					  [(pair? vs)
                                           (if (= n 0)
                                               (_force (car vs))
                                               (visit (_force (cdr vs)) (- n 1)))]
					  [(null? vs)
					   (errorf 'list-ref "empty list: ~s" n)]
                                          [else
                                           (errorf 'list-ref "not a pair: ~s" vs)]))])
                              (let* ([vs (_force t1)]
                                     [n (_force t2)])
                                (if (and (integer? n)
                                         (>= n 0))
                                    (visit vs n)
                                    (errorf 'list-ref "not a natural number: ~s" n))))))))
	(_delay
	  (lambda ()
	    (list 'subr 2 (lambda (t1 t2)
			    (letrec ([visit
				      (lambda (vs n)
					(cond
                                          [(= n 0)
					   vs]
					  [(pair? vs)
                                           (visit (_force (cdr vs)) (- n 1))]
					  [else
					   (errorf 'list-tail "improper tail: ~s" vs)]))])
                              (let* ([vs (_force t1)]
                                     [n (_force t2)])
                                (if (and (integer? n)
                                         (>= n 0))
                                    (visit vs n)
                                    (errorf 'list-tail "not a natural number: ~s" n))))))))
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (let ([v (_force t)])
			      (if (pair? v)
				  (letrec ([visit
					    (lambda (v)
					      (let ([w (_force (cdr v))])
						(if (pair? w)
						    (visit w)
						    w)))])
				    (visit v))
				  (errorf 'last-pair
					 "improper argument: ~s"
					 v)))))))
	(make-subr-1 null?)
	(make-subr-1 pair?)
	(make-subr-1 integer?)
	(make-subr-1 number?)
	(make-subr-1 string?)
	(make-subr-1 symbol?)
	(make-subr-1 zero?)
	(make-subr-1 1+)
	(make-subr-1 sub1)
	(make-subr-2 +)
	(make-subr-2 -)
	(make-subr-2 *)
	(make-subr-2 /)
	(_delay
	  (lambda ()
	    (list 'subr 2 cons)))
	(make-subr-2 eqv?)
        (make-subr-2 (lambda (v1 v2)
                       (letrec ([visit (lambda (v1 v2)
                                         (cond
                                           [(integer? v1)
                                            (and (integer? v2)
                                                 (= v1 v2))]
                                           [(string? v1)
                                            (and (string? v2)
                                                 (string=? v1 v2))]
                                           [(boolean? v1)
                                            (and (boolean? v2)
                                                 (boolean=? v1 v2))]
                                           [(symbol? v1)
                                            (and (symbol? v2)
                                                 (eqv? v1 v2))]
                                           [(null? v1)
                                            (null? v2)]
                                           [(pair? v1)
                                            (and (pair? v2)
                                                 (let* ([av1 (_force (car v1))]
                                                        [dv1 (_force (cdr v1))]
                                                        [av2 (_force (car v2))]
                                                        [dv2 (_force (cdr v2))])
                                                   (and (visit av1 av2)
                                                        (visit dv1 dv2))))]
                                           [else
                                            #f]))])
                         (visit v1 v2))))
	(make-subr-2 =)
	(make-subr-2 string=?)
	(make-subr-2 char=?)
	(make-subr-2 boolean=?)
	(make-subr-1 boolean?)
	(make-subr-1 negative?)
	(make-subr-1 positive?)
	(make-subr-1 _applicable?)
	(_delay
	  (lambda ()
	    (list 'fsubr 1 _quote)))
	(_delay
	  (lambda ()
	    (list 'fsubr 2 _lambda)))
	(_delay
	  (lambda ()
	    (list 'fsubr 3 _if)))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _cond)))
	(_delay
	  (lambda ()
	    (list 'fsubr 2 _set!)))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _case)))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _and)))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _or)))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 (lambda (es r)
			     (letrec ([visit
				       (lambda (es)
					 (if (null? es)
					     '()
					     (cons (_delay
						     (lambda ()
						       (_eval (car es) r)))
						   (_delay
						     (lambda ()
						       (visit (cdr es)))))))])
			       (visit es))))))
	(make-subr-2 set-car!)
	(make-subr-2 set-cdr!)
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _begin)))
        (make-subr-1 (lambda (v)
		       (display (_show v))))
	(make-subr-1 (lambda (v)
		       (pretty-print (_show v))))
	(make-subr-1 (lambda (v)
		       (pretty-print (_show v))))
	(_delay
	  (lambda ()
	    (list 'subr 0 newline)))
	(make-subr-1 not)
	(_delay
	  (lambda ()
	    (list 'subr 1 (lambda (t)
			    (letrec ([visit
				      (lambda (t n)
					(if (null? t)
					    n
					    (visit (_force (cdr t)) (+ n 1))))])
			      (visit (_force t) 0))))))
	(_delay
	  (lambda ()
	    (list 'fsubr 0 _read)))
	(make-subr-1 open-input-file)
	(make-subr-1 eof-object?)
	(make-subr-1 close-input-port)
	(_delay
	  (lambda ()
	    (list 'fsubr 2 _let)))
	(_delay
	  (lambda ()
	    (list 'fsubr 2 _letrec)))
	(_delay
	  (lambda ()
	    (list 'fsubr 2 _let*)))
	(make-subr-2 (lambda (x vs)
                       (letrec ([visit (lambda (vs)
                                         (cond
                                           [(null? vs)
                                            #f]
                                           [(pair? vs)
                                            (let ([v (_force (car vs))])
                                              (if (equal? x v)
                                                  vs
                                                  (visit (_force (cdr vs)))))]
                                           [else
                                            (errorf 'member
                                                    "not the tail of a proper list: ~s"
                                                    vs)]))])
                         (visit vs))))                                                  
	(_delay
	  (lambda ()
	    (list 'fsubr 1 _time)))
	(make-subr-3 errorf)
	(make-subr-1 random)
        ))

;;; ----- the read-eval-print toplevel loop ----------------------------------

(define _banner
  "There we go again, with delays.")

;;; treatment of a definition at the toplevel
(define _define
  (lambda (es toplevel-environment)
    (let ([x (car es)]
          [e (cadr es)])
      (if (not (_identifier? x))
          (errorf '_define "not an identifier: ~s" x)
          (let ([v (_delay (lambda () (_eval e toplevel-environment)))]
		[toplevel-names (car (car toplevel-environment))]
		[toplevel-values (cdr (car toplevel-environment))])
            (let* ([pos (_index x toplevel-names)])
              (if (not (negative? pos))
                  (begin
                    (_update (list-tail toplevel-values pos) v)
                    x)
                  (begin
                    (set-car! (car toplevel-environment)
                              (cons x toplevel-names))
                    (set-cdr! (car toplevel-environment)
                              (cons v toplevel-values))
                    x))))))))

;;; treatment of a definition in a loaded file
(define _load-define
  (lambda (es toplevel-environment)
    (let ([x (car es)]
          [e (cadr es)])
      (if (not (_identifier? x))
          (errorf '_define "undefinable: ~s" x)
          (let ([v (_delay (lambda () (_eval e toplevel-environment)))]
		[toplevel-names (car (car toplevel-environment))]
		[toplevel-values (cdr (car toplevel-environment))])
            (let ([pos (_index x toplevel-names)])
              (if (not (negative? pos))
                  (begin
                    (_update (list-tail toplevel-values pos) v)
                    x)
                  (begin
                    (set-car! (car toplevel-environment)
                              (cons x toplevel-names))
                    (set-cdr! (car toplevel-environment)
                              (cons v toplevel-values))
                    x))))))))

;;; treatment of a file to load
(define _load
  (lambda (es toplevel-environment)
    (let ([filename (car es)])
      (if (not (string? filename))
	  (errorf '_load "not a string: ~s" filename)
	  (letrec ([loop
		    (lambda (port)
		      (let ([e (read port)])
			(cond
			  [(eof-object? e)
			   (begin
			     (close-input-port port)
			     filename)]
			  [(and (pair? e)
				(eqv? (car e) 'define))
			   (begin
			     (_load-define (cdr e) toplevel-environment)
			     (loop port))]
			  [(and (pair? e)
				(eqv? (car e) 'load))
			   (begin
			     (_load (cdr e) toplevel-environment)
			     (loop port))]
			  [else
			   (begin
			     (_eval e toplevel-environment)
			     (loop port))])))])
	    (loop (open-input-file filename)))))))

;;; toplevel loop
(define start-the-interpreter
  (lambda (prompt)
    (let ([toplevel-environment (list (cons '() '()))])
      (letrec ([read-eval-print-loop
		(lambda ()
		  (begin
		    (display prompt)
		    (let ([e (read)])
		      (cond
		       [(eof-object? e)
			(begin
			  (newline)
			  "So long.")]
		       [(and (pair? e)
			     (eqv? (car e) 'exit))
			"So long."]
		       [(and (pair? e)
			     (eqv? (car e) 'define))
			(begin
			  (_define (cdr e) toplevel-environment)
			  (read-eval-print-loop))]
		       [(and (pair? e)
			     (eqv? (car e) 'load))
			(begin
			  (_load (cdr e) toplevel-environment)
			  (read-eval-print-loop))]
		       [else
			(begin
			  (pretty-print
			    (_show
			      (_eval e toplevel-environment)))
			  (read-eval-print-loop))]))))])
	(begin
	  (display _banner)
	  (newline)
	  (read-eval-print-loop))))))

;;; ----- end of "self-interpreter.scm" --------------------------------------
