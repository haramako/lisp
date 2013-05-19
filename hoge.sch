(puts 'a)
(puts '(1 2))

(puts (call/cc (lambda (cont) (cont 1) 2)))

(define (fib n)
  (define (fib-r v n)
	(if (<= n 1)
		(+ v 1)
		(fib-r (fib-r v (- n 2)) (- n 1))))
  (fib-r 0 n))

(puts (<= 1 3 3 5))
(puts (<= 4 1))
(puts (< 1 3 3 5))
(puts (fib 20))

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
