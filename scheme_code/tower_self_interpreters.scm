;;; tower_self_interpreters.scm

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/file_parser.scm")

(load "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/executing_self_interpreter.scm")

;;; Defining the list of definitions as a result of parsing our interpreter
(define parsed-res
  (file-parser "~/home/Desktop/Yale NUS stuff/Y4S1/Capstone/Scheme_Capstone/scheme_code/self_interpreter.scm"))

;;; intepreter running interpreter running on Scheme
(define run-prog
  (lambda (e)
    (run (append parsed-res
		 (list e)))))
;;; interpreter running interpreter running interpreter on Scheme
(define run-2-prog-qq
  (lambda (e)
    (run (append parsed-res
		 (list `(define run-prog 
			  (lambda (e)
			    (run (append ',parsed-res
					 (list e))))))
		 (list `(run-prog ',e))))))
;;; last line was (list `(run-prog ,e)) instead, applied the fix there.

(define run-2-prog
  (lambda (e)
    (run (append parsed-res
		 (list (list 'define 'run-prog 
			     (list 'lambda '(e)
				   (list 'run
					 (list 'append (list 'quote parsed-res)
					       '(list e))))))
		 (list (list 'run-prog (list 'quote e)))))))
;;; last line was (list (list 'run-prog e)) instead, applied the fix there.

;; (define run-3-prog
;;   (lambda (e)
;;     (run (append parsed-res
;; 		 (list (list 'define 'run-prog 
;; 			     (list 'lambda '(e)
;; 				   (list 'run
;; 					 (list 'append (list 'quote parsed-res)
;; 					       (list 'list (list (list 'define 'run-prog 
;; 								       (list 'lambda '(e)
;; 									     (list 'run
;; 										   (list 'append (list 'quote parsed-res)
;; 											 '(list e))))))
;; 						     (list (list 'run-prog e))))))))
;; 		 (list (list 'run-prog e))))))

;; (define run-star-prog
;;   (lambda (n e)
;;     (letrec ((visit (lambda (n e)
;; 		      (if (= n 0)
;; 			  e
;; 			  (run (append parsed-res
;; 				       (list `(define run-prog
;; 						(lambda (e)
						  
;; 				       (list `(run-prog ,e))))))))
		    
;;       (if (<= n 0)
;; 	  (eval e)
;; 	  (visit (- n 1) e)))))
