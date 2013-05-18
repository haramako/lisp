(puts 'a)
(puts '(1 2))

(puts (call/cc (lambda (cont) (cont 1) 2)))

;; ;(display current-input-port)
;; (define (eval-loop)
;;   (display "> ")
;;   (let ((v (eval (read))))
;; 	(puts "=>" v)
;; 	(eval-loop)))

;; ;(eval-loop)

;; (puts *argv*)
