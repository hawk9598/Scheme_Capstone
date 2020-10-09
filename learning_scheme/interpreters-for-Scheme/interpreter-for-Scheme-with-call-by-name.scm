;;; interpreter-for-Scheme-with-call-by-name.scm
;;; dProgSprog 2014-2015, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 19 May 2015

;;;;;;;;;;

(define _delay
  (lambda (t)
    t))

(define _show
  (lambda (v)
    (cond
      [(or (number? v)
	   (string? v)
	   (boolean? v)
	   (symbol? v))
       v]
      [(pair? v)
       (case (car v)
	 [(subr proc)
	  '*procedure*]
	 [(fsubr)
	  '*special-form*]
	 [else
	  '*pair*])]
      [else
       '*unevaluated*])))

(load "interpreter-for-Scheme-with-thunks.scm")

(define _banner
  "There we go again, in call by name.")

;;;;;;;;;;

;;; end of interpreter-for-Scheme-with-call-by-name.scm
