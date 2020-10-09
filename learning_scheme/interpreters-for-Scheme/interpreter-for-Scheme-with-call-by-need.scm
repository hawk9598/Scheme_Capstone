;;; interpreter-for-Scheme-with-call-by-need.scm
;;; dProgSprog 2014-2015, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 19 May 2015

;;;;;;;;;;

(define _delay
  (lambda (t)
    (let ([flag #f]
	  [result "nothing yet"])
      (lambda ()
	(if flag
	    result
	    (begin
	      (set! result (t))
	      (set! flag #t)
              (set! t (lambda ()
                        (errorf '_delay
                                "this thunk was already forced")))
	      result))))))

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
  "There we go again, in call by need.")

;;;;;;;;;;

;;; end of interpreter-for-Scheme-with-call-by-need.scm
