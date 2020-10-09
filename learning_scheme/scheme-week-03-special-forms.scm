;;; scheme-week-03-special-forms.scm
;;; Capstone AY2020/2021 Sem 1
;;; Qin Wayne Toh  <toh.qin.wayne@u.yale-nus.edu.sg>
;;; Version of 23 August 2020
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Week 3 of Scheme Lecture Notes

;;; Further special forms in Scheme


;;; quote, or quotation '.

(lambda (x) x) ;; evaluates to #<procedure>

'(lambda (x) x) ;; evaluates to (lambda (x) x)

(define fib3 (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (if (= n 2) 2 (+ (fib3 (- n 1)) (fib3 (- n 2)) (fib3 (- n 3)))))))) ;; evaluates to store fib 3 inside the environment as a variable

'(define fib4 (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (if (= n 2) 2 (+ (fib4 (- n 1)) (fib4 (- n 2)) (fib4 (- n 3)))))))) ;; pretty prints the function with the correct indentations, does not store inside the scheme environment.

;; Evaluating fib4 will give error, fib3 will evaluate to be a procedure.

;; Quotations can be represented as '<quotation> or (quote <quotation>), and are equivalent to each other.

(symbol? 'x) ;; gives #t. symbol? checks whether the object is a representation.
(symbol?= 'foo 'foo) ;; gives #t

;; Nested quotations are possible, and evaluating a nested quotation is equivalent to removing a quotation from the original expression

;;; let special form. DIffers from define which is a global binding. let allows the user to declare local bindings inside the code.

(let ([a 1] [b 2]) (list a b)) ;; gives (1 2). a is assigned as 1 and b assigned as 2 inside the environment, which the list operator evaluates a and b against.

(let ([a 1])
  (let [b 2]
    (list a b))) ;; also gives (1 2). However, there is a nesting of lets which in turn translates to LEXICAL SCOPE, where the most recently declared variable is the one that has its binding in effect.

(let ([a 1])
  (let ([a 2])
    (list a))) ;; gives (2) and not (1) because according to the lexical scope, a = 2 is the one most recently declared


(let ([a 1] [b 2] [c 3])
  (let ([c b])
    (let ([b 22])
      (list a b c)))) ;; gives (1 22 2)

(let ([x 11])
  (list (let ([y 22])
          (list x y))
        (let ([y 33])
          (list x y)))) ;; gives ((11 22)(11 33))

(let ([x 11])
  (list (let ([y 22])
          (let ([x 44])
            (list x y)))
        (list x x)
        (let ([y 33])
          (let ([y 55])
            (list x y))))) ;; gives ((44 22)(11 11)(11 55))


(let ([x 10] [y 20] [z 30])
  (let ([foo (lambda (y t)
               (list x y t))])
    (let ([x 20])
      (list x (foo 40 z))))) ;; gives (20 (10 40 30))

;;; let* special form

;; Simply an abbreviation of nested let statements, where let* sequentially binds each of the header elements in a nested sequential way.


(let* ([a 1][b 2])
  (list a b))

;; equivalent to

(let ([a 1])
  (let ([b 2])
    (list a b)))


;;; letrec special form, recursive version of the let special form.

(define plus_revisited
  (lambda (n1 n2)
    (letrec ([visit (lambda (n1 n2)
                      (if (= n1 0)
                          n2
                          (+ 1 (visit (- n1 1) n2))))])
      (visit n1 n2))))


;;; cond expressions, and expressing 'or' and 'and' in terms of if/else or cond expressions

(if 1
    10
    (if #\a
        #\A
        (if "hello world"
            "HELLO WORLD"
            #t)))

;; equivalent to

(cond
  [1
   10]
  [#\a
   #\A]
  ["hello world"
   "HELLO WORLD"]
  [else
   #t])

(cond ["wayne"][else "kane"]) ;; inconsequential cond expression, where if the cond clause is evaluated to true, it itself will be returned. "wayne" will be returned

'(and e1 e2 e3 e4)

;; equivalent to 

'(if e1
     (if e2
         (if e3
             e4
             #f)
         #f)
     #f)


'(or e1 e2 e3 e4)

;; equivalent to

'(cond [e1][e2][e3][else e4])


;;; Using cond expression to substitute nested if expressions.

;; Note: Scheme programmers usually use cond to substitute nested if expressions if the same value is tested repeatedly


;; original state function

(define state
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (if (char=? (string-ref s i) #\0)
            (state s (+ i 1) max b)
            (if (char=? (string-ref s i) #\1)
                (state s (+ i 1) max (not b))
                "error")))))

(define other-odd-number-of-ones?
  (lambda (s)
    (state s 0 (string-length s) #f)))

;; state function rewritten using cond expressions for same value that is repeatedly tested

(define state_re2visited
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (cond
          [(char=? (string-ref s i) #\0)
           (state_re2visited s (+ i 1) max b)]
          [(char=? (string-ref s i) #\1)
           (state_re2visited s (+ i 1) max (not b))]
          [else
           "error"]))))

(define other-odd-number-of-ones?_re2visited
  (lambda (s)
    (state_re2visited s 0 (string-length s) #f)))

;; Not computing string-ref multiple times, via the use of a let expression

(define state_re3visited
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (let ([c (string-ref s i)])
             (cond
               [(char=? c #\0)
                (state_re3visited s (+ i 1) max b)]
               [(char=? c #\1)
                (state_re3visited s (+ i 1) max (not b))]
               [else
                "error"])))))

(define other-odd-number-of-ones?_re3visited
  (lambda (s)
    (state_re3visited s 0 (string-length s) #f)))

;; Using case expression instead for the comparison of string-ref

(define state_re4visited
  (lambda (s i max b)
    (if (= i max)
        (if b
            "yes"
            "stuck")
        (case (string-ref s i)
          [(#\0)
           (state_re4visited s (+ i 1) max b)]
          [(#\1)
           (state_re4visited s (+ i 1) max (not b))]
          [else
           "error"]))))

(define other-odd-number-of-ones?_re4visited
  (lambda (s)
    (state_re4visited s 0 (string-length s) #f)))

;; Combining state_re4visited and other-odd-number-of-ones?_re4visited

(define other-odd-number-of-ones?_re5visited
  (lambda (s)
    (let ([max (string-length s)])
      (letrec ([state
                (lambda (i b)
                  (if (= i max)
                      (if b
                          "yes"
                          "stuck")
                      (case (string-ref s i)
                        [(#\0)
                         (state (+ i 1) b)]
                        [(#\1)
                         (state (+ i 1) (not b))]
                        [else
                         "error"])))])
        (state 0 #f)))))

;; Using mutual recursion to redefine the function, removing the need for the boolean b and using 2 mutually recursive local functions state-with-even-number-of-ones and state-with-odd-number-of-ones.

(define odd-number-of-ones?_re6visited
  (lambda (s)
    (let ([max (string-length s)])
      (letrec ([state-with-even-number-of-ones
                (lambda (i)
                  (if (= i max)
                      "stuck"
                      (case (string-ref s i)
                        [(#\0)
                         (state-with-even-number-of-ones (+ i 1))]
                        [(#\1)
                         (state-with-odd-number-of-ones (+ i 1))]
                        [else
                         "error"])))]
               [state-with-odd-number-of-ones
                (lambda (i)
                  (if (= i max)
                      "yes"
                      (case (string-ref s i)
                        [(#\0)
                         (state-with-odd-number-of-ones (+ i 1))]
                        [(#\1)
                         (state-with-even-number-of-ones (+ i 1))]
                        [else
                         "error"])))])
        (state-with-even-number-of-ones 0)))))

;;; end of scheme-week-03-special-forms.scm

"scheme-week-03-special-forms.scm"

