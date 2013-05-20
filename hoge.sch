(load "srfi-1.sch")

(puts `(1 2 ,@'(3 4) 5))

(puts 'a)
(puts '(1 2))

(puts (call/cc (lambda (cont) (cont 1) 2)))

(define (fib n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1) (fib (- n 2))))))

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
  (puts (backtrace))
  (puts 1))

(fuga 1)

(puts 1)


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
