(define (write-tree tree . rest)
  (let-optionals*
   rest ((port current-output-port))
   (let recur ((tree tree))
	 (cond ((null? tree))
		   ((pair? tree)
			(dolist (v tree) (recur v)))
		   (else (display tree port))))))

