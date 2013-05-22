;; (load "srfi-1.scm")

(when 1 (puts 1))

(define-syntax hoge
  (syntax-rules ()
	((_ a b ...) (list a b ...))
	))

(puts (hoge 'fuga 'piyo 'hage))
(puts (hoge 'fuga '(piyo hage) 'nyao))

(exit)


(define-syntax nil!
  (syntax-rules ()
	((_ x)
	 (set! x '()))
	))

(puts (syntax-expand1 '(nil! hoge)))

(begin
  (define hoge 1)
  (nil! hoge)
  (puts 'hoge hoge))

(let ((x 1))
  (puts 'dynamic-wind)
  (call/cc (lambda (break)
			 (dynamic-wind
				 (lambda () (puts 1))
				 (lambda () (break 0) (puts 2))
				 (lambda () (puts 3))
			   )
			 ))
  (puts 4)
  )

(define (fib n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1)) (fib (- n 2)))))

(puts "fib" (fib 10))

(let ((x 1))
  (define ce (current-environment))
  (puts ce)
  (let ((x 2))
	(puts (eval 'x ce)))
  )

; (puts (fib 29))

(puts (xcons 1 2))

'(begin
  (define break #f)
  (puts (call/cc (lambda (cont) (set! break cont))))

  (puts 'a)
  (puts 'c (call/cc (lambda (cont) (break 10))))
  (puts 'b))


(define (fuga x)
  (hoge x)
  (puts 1))

(define (hoge x)
  ;; (puts (backtrace))
  (puts 1))

(fuga 1)

(puts 1)


(let ((a 2))
  ;;(puts 'append)
  (puts (append '(1 2) '(3 4) '() '(5 6)))
  (puts (append '()))
  (puts (append '() '(1 2)))
  (define x '(100 200))
  (puts (append x '(3)))
  (puts (append))
  )


;; (define (rec n)
;;   (rec (+ 1 n)))

;; (rec 1)
	 
;; ;(display current-input-port)
;; (define (eval-loop)
;;   (display "> ")
;;   (let ((v (eval (read))))
;; 	(puts "=>" v)
;; 	(eval-loop)))

;; ;(eval-loop)

;; (puts *argv*)
