;;; tower_self_interpreters_typecheck.scm

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/file_parser.scm")

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/executing_self_interpreter_typecheck.scm")

;;; Defining the list of definitions as a result of parsing our interpreter
(define parsed-res-typecheck
  (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/self_interpreter_typecheck.scm"))

;;; interpreter running on scheme
(define run-prog-typecheck
  (lambda (e)
    (run (list e))))

;;; intepreter running interpreter running on Scheme
(define run-2-prog-typecheck
  (lambda (e)
    (run (append parsed-res
		 (list
		  `(run (list ',e)))))))


;;; interpreter running interpreter running interpreter on Scheme
(define run-3-prog-typecheck
  (lambda (e)
    (run (append parsed-res
		 (list
		  `(run (append ',parsed-res
				(list
				 ',`(run (list ',e))))))))))

;;; An n-ary version of run-prog where user can specifiy number of layers of interpreters
(define run-star-prog-typecheck
  (lambda (n e)
    (if (<= n 0)
	(errorf 'run-star-prog "n must be greater than 0")
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      (run (append parsed-res (list e)))
			      (visit (- n 1) `(run (append ',parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))

;;; Returns the quoted program that will be run if it was run-star-prog-qq
(define run-star-prog-qqq-typecheck
  (lambda (n e)
    (if (<= n 0)
	(errorf 'run-star-prog "n must be greater than 0")
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      `(run (append ',parsed-res (list ',e)))
			      (visit (- n 1) `(run (append ',parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))

;;; Use to visualize the nested running of the interpreters for n >= 2. Quote magritte on parsed-res
(define run-star-prog-qqqq-typecheck
  (lambda (n e)
    (if (<= n 0)
	(errorf 'run-star-prog "n must be greater than 0")
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      `(run (append 'parsed-res (list ',e)))
			      (visit (- n 1) `(run (append 'parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))
