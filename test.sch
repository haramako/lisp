;; test type operator
(assert #t (symbol? 'a))
(assert #f (symbol? 0))
(assert #t (number? 0))
(assert #f (number? 'a))
(assert #t (pair? '(0)))
(assert #f (pair? 'a))
(assert #t (null? '()))
(assert #f (null? '(0)))
(assert #t (list? '(0)))
(assert #t (list? '()))
(assert #f (list? 0))
(assert #t (procedure? display))
(assert #t (procedure? cddr))
(assert #f (procedure? 0))

;; test define
(let ((var 'hoge))
  (assert 'hoge var)
  (set! var 'fuga)
  (assert 'fuga var))

;; test eq? eqv?
(let ((var '(1)))
  (assert #t (eq? '() '()))
  (assert #f (eq? var '(1)))
  (assert #t (eq? 1 1))
  (assert #f (eq? 1 2))
  (assert #t (eq? 'hoge 'hoge))
  (assert #f (eq? 'hoge 'fuga))
  (assert #f (eq? "a" "a"))
  (assert #t (eqv? "a" "a"))
  (assert #f (eqv? "a" "b"))
  (assert #f (eqv? var '(1)))
  (assert #t (equal? var '(1)))
  (assert #f (equal? var '(1 2)))
  (assert #f (equal? '(1 2) '(1 3))))

; test let
(let ((a 0))
  (let ((a 1) (b (+ 1 1)))
	(assert 1 a)
	(assert 2 b))

  (assert 55
		  (let loop ((sum 0)
					 (var 10))
			(if (<= var 0) sum
				(loop (+ sum var) (- var 1))))))

(define one 1)
(assert 2 (+ one 1))
(assert #t (eq? one 1))

(assert 1 (car '(1 2)))
(assert '(2) (cdr '(1 2)) )
(assert '() (cdr '(1)))

(assert 1 (if #t 1 2))
(assert 2 (if #f 1 2))
(assert 3 (if #f 1 2 3))

(define +1 (lambda (x) (+ x 1)))
(assert 4 (+1 3))

(define +n (lambda (x) (lambda (y) (+ x y))))
(define +3 (+n 3))
(define +4 (+n 4))
(assert 4 (+3 1))
(assert 7 (+3 (+4 0)))

(define x 100)
(assert '(1 (2 100)) `(1 (2 ,x)))

(define m+1 (macro (x) (list '+ x 1)))
(assert 4 (m+1 3))
(assert '(+ 2 1) (macro-expand '(m+1 2)))

;; (define n 0)
;; (loop
;;  (display n)
;;  (set! n (+ n 1))
;;  (if (eq? n 10) (break)))

;; (display (gemsym))
;; (display (gemsym))
;; (define sym (gemsym))
;; (display (eq? sym sym))

(assert 1 (car '(1 2)))
(assert '(2 3) (cdr '(1 2 3)))

(define apply-test (lambda (a b) (+ a b)))
(assert 3 (apply apply-test '(1 2)))
(assert 3 (apply + '(1 2)))
(assert 6 (apply + 1 '(2 3)))

(assert 1 (or #f #f 1))
(assert #f (or #f #f #f))
(assert 3 (and 1 2 3))
(assert #f (and 1 #f 3))

(define cond-test
  (lambda (x)
	(cond
	 ((eq? x 1) 'one)
	 ((eq? x 2) 'two)
	 (x => (lambda (x) 'other))
	 (#t 'other))))
(assert 'one (cond-test 1))
(assert 'two (cond-test 2))
(assert 'other (cond-test 3))

(assert 3 (eval '(+ 1 2)))

;; test list*
(assert '() (list*))
(assert 1 (list* 1))
(assert '(1 . 2) (list* 1 2))
(assert '(1 2 3 4) (list* 1 2 '(3 4)))

;; test do
(assert 55
		(do ((sum 0 (+ sum i))
			 (i 10 (- i 1)))
			((<= i 0) sum)))

;; test receive
(assert '(1 2)
		(receive (a b) (values 1 2)
				 (list a b)))

;; test call-with-values
(assert 3 (call-with-values (values 1 2) +))
(assert 1 (call-with-values 1 +))

;; test list-tail
(assert '(1 2) (list-tail '(1 2) 0))
(assert '(2) (list-tail '(1 2) 1))
(assert '() (list-tail '(1 2) 2))

;; test length
(assert 0 (length '() ))
(assert 2 (length '(1 2)))

(begin
  (assert 1
		  (call/cc
		   (lambda (cont)
			 (cont 1)
			 (assert eqv? #t #f))))
  (assert (values 1 2)
		  (call/cc
		   (lambda (cont)
			 (cont 1 2)
			 (assert eqv? #t #f)))))

(exit (if *minitest-failed* 1 0))
