;; (display (symbol? 'a))
;; (display (symbol? 1))
;; (display (number? 1))
;; (display (number? 'a))
;; (display (pair? '(1)))
;; (display (pair? 'a))
;; (display (null? '()))
;; (display (null? '(1)))
;; (display (list? '()))
;; (display (list? 1))
;; (display (procedure? display))
;; (display (procedure? 1))

;; (define a 'hoge)
;; (display a #f)

;; (display (let ((a 1) (b 2))
;; 		   (display a b) 1))

;; (define one 1)
;; (display 'add1+2 (+ 1 2))
;; (display (- 2 1))
;; (display (eq? one 1))
;; (display (car '(1 2)))

;; (set! a 'fuga)
;; (display a #t)

;; (if #t
;;  	(display 1)
;;   (display 2))

;; (if #f
;;  	(display 1)
;;   (display 2))

;; (let ((a 1) (x (+ x 1)))
;;   (display a x))

;; (define +1 (lambda (x) (+ x 1)))
;; (display (+1 3))

;; (define +n (lambda (x) (lambda (y) (+ x y))))
;; (define +3 (+n 3))
;; (define +4 (+n 4))
;; (display (+3 1))
;; (display (+3 (+4 0)))

(define x 100)
(display `(1 (2 ,x)))

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
