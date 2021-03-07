(define file-parser
  (lambda (s)
    (call-with-input-file s
      (lambda (p)
	(let f ((x (read p)))
	  (if (eof-object? x)
	      '()
	      (cons x (f (read p)))))))))
