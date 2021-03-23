;;; tower_self_interpreters.scm

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/file_parser.scm")

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/executing_self_interpreter.scm")

;;; Defining the list of definitions as a result of parsing our interpreter
(define parsed-res
  (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/self_interpreter.scm"))

;;; Evaluating expression using Scheme
(define run-0-prog
  (lambda (e)
    (eval e)))

;;; interpreter running on scheme
(define run-prog
  (lambda (e)
    (run (list e))))

;;; intepreter running interpreter running on Scheme
(define run-2-prog
  (lambda (e)
    (run (append parsed-res
		 (list
		  `(run (list ',e)))))))


;;; interpreter running interpreter running interpreter on Scheme
(define run-3-prog
  (lambda (e)
    (run (append parsed-res
		 (list
		  `(run (append ',parsed-res
				(list
				 ',`(run (list ',e))))))))))

;;; An n-ary version of run-prog where user can specifiy number of layers of interpreters
(define run-star-prog-qq
  (lambda (n e)
    (if (<= n 0)
	(eval e)
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      (run (append parsed-res (list e)))
			      (visit (- n 1) `(run (append ',parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))

;;; Returns the quoted program that will be run if it was run-star-prog-qq
(define run-star-prog-qqq
  (lambda (n e)
    (if (<= n 0)
	(eval e)
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      `(run (append ',parsed-res (list ',e)))
			      (visit (- n 1) `(run (append ',parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))

;;; Use to visualize the nested running of the interpreters for n >= 2. Quote magritte on parsed-res
(define run-star-prog-qqqq
  (lambda (n e)
    (if (<= n 0)
	(eval e)
	(letrec ((visit (lambda (n e)
			  (if (= n 0)
			      `(run (append 'parsed-res (list ',e)))
			      (visit (- n 1) `(run (append 'parsed-res (list ',e))))))))
	  (if (= n 1)
	      (run (list e))
	      (visit (- n 2) `(run (list ',e))))))))
