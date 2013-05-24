(define (fib n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1)) (fib (- n 2)))))

(puts (fib (string->number (car *argv*))))
