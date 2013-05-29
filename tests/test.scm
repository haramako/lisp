(require minitest)

;; test define
(let ((var 'hoge))
  (assert 'hoge var)
  (set! var 'fuga)
  (assert 'fuga var))

;; test or and
(assert 1 (or #f #f 1))
(assert #f (or #f #f #f))
(assert 3 (and 1 2 3))
(assert #f (and 1 #f 3))

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

;; test define
(define one 1)
(assert 2 (+ one 1))
(assert #t (eq? one 1))

(assert 1 (car '(1 2)))
(assert '(2) (cdr '(1 2)) )
(assert '() (cdr '(1)))

(assert 1 (if #t 1 2))
(assert 2 (if #f 1 2))
(assert 3 (if #f 1 2 3))

;; test lambda
(define +1 (lambda (x) (+ x 1)))
(assert 4 (+1 3))

;; test closure
(define +n (lambda (x) (lambda (y) (+ x y))))
(define +3 (+n 3))
(define +4 (+n 4))
(assert 4 (+3 1))
(assert 7 (+3 (+4 0)))

;; tesut quasiquote
(define x 100)
(assert '(1 (2 100)) `(1 (2 ,x)))
(set! x '(100 200))
(assert '(1 (2 (100 200))) `(1 (2 ,x)))
(assert '(1 (2 100 200)) `(1 (2 ,@x)))
(assert '(1 (2 100 200 3)) `(1 (2 ,@x 3)))

;; test macro
(define m+1 (macro (x) (list '+ x 1)))
(assert 4 (m+1 3))
(assert '(+ 2 1) (macro-expand '(m+1 2)))

;; (display (gemsym))
;; (display (gemsym))
;; (define sym (gemsym))
;; (display (eq? sym sym))

;; check cfunc argument number
(assert-exception #t (car))
(assert-exception #t (car 1 2))
(assert-exception #t (display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; test cfunc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; test identity
(assert '(1) (identity '(1)) )

;; test eq? eqv? equal?
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

(assert #t (eq?))
(assert #t (eq? 1))
(assert #t (eq? 1 1))
(assert #t (eq? 1 1 1))
(assert #f (eq? 1 1 0))
(assert #f (eq? 0 1 1))

;; test define?
(assert #t (define? 'x))
(assert #f (define? 'not-defined))
(assert-exception #t (define? 1))

;; test +, -, *, /, modulo
(assert 1 (+ 1))
(assert 3 (+ 1 2))
(assert 6 (+ 1 2 3))
(assert -4 (- 1 2 3))
(assert 6 (* 1 2 3))
(assert 4 (/ 24 2 3))
(assert 4 (quotient 24 2 3))
(assert 24 (modulo 100 51 25))
(assert-exception #t (+ 1 #t))
(assert-exception #t (- 1 #t))
(assert-exception #t (* 1 #t))
(assert-exception #t (/ 1 #t))
(assert-exception #t (modulo 1 #t))

;; test =, <, <=, >, >=
(assert #t (=))
(assert #t (= 1))
(assert #t (= 1 1))
(assert #t (= 1 1 1))
(assert #f (= 1 2))
(assert #f (= 1 1 2))
(assert #f (= 2 1 1))
(assert #t (<))
(assert #t (< 1 2))
(assert #t (< 1 2 3))
(assert #f (< 1 2 2))
(assert #f (< 2 2 1))
(assert #t (<=))
(assert #t (<= 1 2 2))
(assert #f (<= 1 2 1))
(assert #t (>))
(assert #t (> 3 2 1))
(assert #f (> 2 1 1))
(assert #t (>=))
(assert #t (>= 2 2 1))
(assert #f (>= 2 1 2))

;; test symbol->string
(assert "symbol" (symbol->string 'symbol))
(assert-exception #t (symbol->string "hoge"))

;; test car, cdr
(assert 1 (car '(1 2)))
(assert '(2 3) (cdr '(1 2 3)))
(assert-exception #f (car 1))
(assert-exception #f (cdr 1))

;; test cons, set-car! set-cdr!
(assert '(1 . 2) (cons 1 2))
(assert '(1 2 3) (cons 1 (cons 2 (cons 3 '()))))
(assert '(3 . 2) (let ((x '(1 . 2))) (set-car! x 3) x))
(assert '(1 . 3) (let ((x '(1 . 2))) (set-cdr! x 3) x))
(assert-exception #t (set-car! 1 1))
(assert-exception #t (set-cdr! 1 1))

;; test list
(assert '() (list))
(assert '(1 2 3) (list 1 2 3))
(assert '() (list*))
(assert 1 (list* 1))
(assert '(1 . 2) (list* 1 2))
(assert '(1 2 3 4) (list* 1 2 '(3 4)))

;; test not
(assert #t (not #f))
(assert #f (not #t))
(assert #f (not 1))

;; test type operator
(assert #t (symbol? 'a))
(assert #f (symbol? 0))
(assert #t (number? 0))
(assert #f (number? 'a))
(assert #t (char? #\a))
(assert #t (char? #\space))
(assert #f (char? 0))
(assert #f (pair? 'a))
(assert #t (null? '()))
(assert #f (null? '(0)))
(assert #t (list? '(0)))
(assert #t (list? '()))
(assert #f (list? 0))
(assert #t (procedure? display))
(assert #t (procedure? cddr))
(assert #f (procedure? 0))

;; test apply
(define apply-test (lambda (a b) (+ a b)))
(assert 3 (apply apply-test '(1 2)))
(assert 3 (apply + '(1 2)))
(assert 6 (apply + 1 '(2 3)))

;; test syntax-expand1
(define-syntax test-syntax
  (syntax-rules ()
	((_ x) (1 x 2))))
(assert '(1 0 2) (syntax-expand1 '(test-syntax 0)))

;; test current-environment, eval
(assert 3 (eval '(+ 1 2)))
(let ((x 1))
  (define env (current-environment))
  (let ((x 2))
	(assert 1 (eval 'x env))
	(assert 2 (eval 'x))))

;; test display, write, read
(assert-exception #t (display))
(assert-exception #t (display 1 1))
(assert-exception #t (write))
(assert-exception #t (write 1 1))
(assert-exception #t (open-input-string 1))
(assert-exception #t (open-input-file 1))
(assert-exception #t (open-output-file 1))
(let ((os (open-output-string)))
  (write 1 os)
  (display " " os)
  (write "a" os)
  (let ((is (open-input-string (get-output-string os))))
	(assert 1 (read is))
	(assert "a" (read is))))

(define cond-test
  (lambda (x)
	(cond
	 ((eq? x 1) 'one)
	 ((eq? x 2) 'two)
	 (x => (lambda (x) 'other))
	 (else #f))))
(assert 'one (cond-test 1))
(assert 'two (cond-test 2))
(assert 'other (cond-test 3))
(assert #f (cond-test #f))



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
(assert 3 (call-with-values (lambda () (values 1 2)) +))
(assert 1 (call-with-values (lambda () 1) identity))

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
  '(assert (values 1 2)
		  (call/cc
		   (lambda (cont)
			 (cont 1 2)
			 (assert eqv? #t #f)))))

;; test let, let*
(let ((a 1))
  (let ((a (+ a 1))
		(b (+ a 1)))
	(assert '(2 2) (list a b)))
  (let* ((a (+ a 1))
		(b (+ a 1)))
	(assert '(2 3) (list a b)))
  (letrec ((a b)
		   (b (cons 1 a)))
	(assert '(() (1)) (list a b)))
  )

;; test define-syntax
(define-syntax my-set!
  (syntax-rules ()
	((_ sym x) (set! sym x))))

(define-syntax nil!
  (syntax-rules ()
	((_ x) (my-set! x '()))))

(let ((hoge 1))
  (my-set! hoge 2)
  (assert 2 hoge)
  (nil! hoge)
  (assert '() hoge))
	

(minitest-finish)
