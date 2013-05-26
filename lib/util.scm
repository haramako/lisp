
;;; (for (?v ?list) ?body ...)
(define-syntax for
  (syntax-rules ()
	((_ (?v ?list) ?body ...)
	 (let loop ((*list* ?list))
	   (if (not (pair? *list*)) #f
		   (let ((?v (car *list*)))
			 ?body ...
			 (loop (cdr *list*))))))))

(define (with-output-string func)
  (let ((s (open-output-string)))
	(func s)
	(get-output-string s)))

(define (call-with-input-file filename func)
  (let ((s (open-input-file filename)))
	(func s)))

(define (call-with-output-file filename func)
  (let ((s (open-output-file filename)))
	(func s)))


(define (format-display s . lis)
  (display (apply format lis) s))

(define (string-split s delimiter)
  (string-tokenize s (lambda (x) (not (char=? delimiter x)))))
