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

(set! var '(1))
(assert eqv? (eq? '() '()) #t)
(assert eqv? (eq? var '(1)) #f)
(assert eqv? (eq? 1 1) #t)
(assert eqv? (eq? 1 2) #f)
(assert eqv? (eq? 'hoge 'hoge) #t)
(assert eqv? (eq? 'hoge 'fuga) #f)
(assert eqv? (eq? "a" "a") #f)
(assert eqv? (eqv? "a" "a") #t)
(assert eqv? (eqv? "a" "b") #f)
(assert eqv? (eqv? var '(1)) #f)
(assert eqv? (equal? var '(1)) #t)
(assert eqv? (equal? var '(1 2)) #f)
(assert eqv? (equal? '(1 2) '(1 3)) #f)

(let ((var 1) (b (+ 1 1)))
  (assert eqv? var 1)
  (assert eqv? b 2))

(define one 1)
(assert eqv? (+ one 1) 2)
(assert eqv? (eq? one 1) #t)

(assert eqv? (car '(1 2)) 1)
(assert equal? (cdr '(1 2)) '(2))
(assert eqv? (cdr '(1)) '())

(assert eqv? (if #t 1 2) 1)
(assert eqv? (if #f 1 2) 2)
(assert eqv? (if #f 1 2 3) 3)

(define +1 (lambda (x) (+ x 1)))
(assert eqv? (+1 3) 4)

(define +n (lambda (x) (lambda (y) (+ x y))))
(define +3 (+n 3))
(define +4 (+n 4))
(assert eqv? (+3 1) 4)
(assert eqv? (+3 (+4 0)) 7)

(define x 100)
(assert equal? `(1 (2 ,x)) '(1 (2 100)))

(define m+1 (macro (x) (list '+ x 1)))
(assert eqv? (m+1 3) 4)
(assert equal? (macro-expand '(m+1 2)) '(+ 2 1))

;; (define n 0)
;; (loop
;;  (display n)
;;  (set! n (+ n 1))
;;  (if (eq? n 10) (break)))

;; (display (gemsym))
;; (display (gemsym))
;; (define sym (gemsym))
;; (display (eq? sym sym))

(assert eqv? (car '(1 2)) 1)
(assert equal? (cdr '(1 2 3)) '(2 3))

(define apply-test (lambda (a b) (+ a b)))
(assert eqv? (apply apply-test '(1 2)) 3)
(assert eqv? (apply + '(1 2)) 3)

(assert eqv? (or #f #f 1) 1)
(assert eqv? (or #f #f #f) #f)
(assert eqv? (and 1 2 3) 3)
(assert eqv? (and 1 #f 3) #f)

(define cond-test
  (lambda (x)
	(cond
	 ((eq? x 1) 'one)
	 ((eq? x 2) 'two)
	 (#t 'other))))
(assert eqv? (cond-test 1) 'one)
(assert eqv? (cond-test 2) 'two)
(assert eqv? (cond-test 3) 'other)

(assert eqv? (eval '(+ 1 2)) 3)
