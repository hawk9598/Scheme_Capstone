;;; interpreter-for-Scheme-with-call-by-value.scm
;;; dProgSprog 2014-2015, Q4
;;; Olivier Danvy <danvy@cs.au.dk>
;;; Version of 19 May 2015

;;;;;;;;;;

(define _delay
  (lambda (t)
    (let ([v (t)])
      (lambda ()
	v))))

(define _show
  (lambda (v)
    (if (pair? v)
	(case (car v)
	  [(subr fsubr proc)
	   v]
	  [else
	   (cons (_show ((car v)))
		 (_show ((cdr v))))])
	v)))

(load "interpreter-for-Scheme-with-thunks.scm")

(define _banner
  "There we go again, in call by value.")

;;;;;;;;;;

;;; end of interpreter-for-Scheme-with-call-by-value.scm
