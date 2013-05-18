(puts 'a 'b)
(puts '(1 2))

;(display current-input-port)
(define eval-loop
  (lambda ()
	  (display "> ")
	  (let ((v (eval (read))))
		(puts "=>" v)
		(eval-loop))))

;(eval-loop)
