(assert eqv? (symbol? 'a) #t)
(assert eqv? (symbol? 0) #f)
(assert eqv? (number? 0) #t)
(assert eqv? (number? 'a) #f)
(assert eqv? (pair? '(0)) #t)
(assert eqv? (pair? 'a) #f)
(assert eqv? (null? '()) #t)
(assert eqv? (null? '(0)) #f)
(assert eqv? (list? '(0)) #t)
(assert eqv? (list? '()) #t)
(assert eqv? (list? 0) #f)
(assert eqv? (procedure? display) #t)
(assert eqv? (procedure? macro-expand) #t)
(assert eqv? (procedure? 0) #f)

(define var 'hoge)
(assert eqv? var 'hoge)
(set! var 'fuga)
(assert eqv? var 'fuga)

(let ((var 1) (b (+ 1 1)))
  (assert eqv? var 1)
  (assert eqv? b 2))

(define one 1)
(assert eqv? (+ one 1) 2)
(assert eqv? (eq? one 1) #t)

(assert eqv?(car '(1 2)) 1)
(assert eqv?(cdr '(1 2)) 2)

(assert eqv? (if #t 1 2) 1)
(assert eqv? (if #f 1 2) 2)
(assert eqv? (if #f 1 2 3) 3)

(define +1 (lambda (x) (+ x 1)))
(assert eqv? (+1 3) 4)

;; (define +n (lambda (x) (lambda (y) (+ x y))))
;; (define +3 (+n 3))
;; (define +4 (+n 4))
;; (display (+3 1))
;; (display (+3 (+4 0)))

;; (define x 100)
;; (display `(1 (2 ,x)))

;; (display '#FILE #LINE )
;; (display '#FILE:LINE 'hoge )

;(assert eqv? (eq? 1 2) #t)
;(assert eqv? (+ 1 2) 2)

;; (define m+1 (macro (x) (list '+ x 1)))

;; (display (m+1 3))

;; (display (macro-expand (m+1 1)))

;; (display '())

;; (define +2 (lambda (x) (m+1 1)))

;; (display (+2 1))

;; (define n 0)
;; (loop
;;  (display n)
;;  (set! n (+ n 1))
;;  (if (eq? n 10) (break)))

;; (display (gemsym))
;; (display (gemsym))
;; (define sym (gemsym))
;; (display (eq? sym sym))

;; (display (car '(1 2)))
;; (display (cdr '(1 2 3)))

	

;; (define x 0)
;; (display (zero? (zero? x)))

;; (define hoge (lambda (a b) (+ a b)))
;; (display 'apply (apply hoge '(1 2)))
;; (display 'apply (apply + '(1 2)))

;; ;(define *debug* #t)
;; (define a 100)
;; (display `(1 `(1 ,,a) ,(+ 1 2)))

;; (display (or #f #f 1))
;; (display 'hoge)
;; (display (and 1 (+ 2 3) #t))

;; (display 'eq? (eq? 1 1 1))
;; (display 'eqv? (eqv? 1 2 1))

;; (display (macro-expand '(zero? a)))

;; (define x 3)
;; (cond
;;  ((eq? x 1) (display 1))
;;  ((eq? x 2) (display 2)))

;; (display (eval '(+ 1 2)))
