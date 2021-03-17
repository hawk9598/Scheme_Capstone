(define five 5) 

(define hello "hello")

(define char-d #\d)

(define bool-t #t)

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

(if bool-t hello five)
