
;;; (for (?v ?list) ?body ...)

(define (with-output-string func)
  (let ((s (open-output-string)))
	(func s)
	(get-output-string s)))

(define (call-with-input-file filename func)
  (let ((s (open-input-file filename)))
	(let ((result (func s)))
	  (close-input-port s)
	  result)))

(define (call-with-output-file filename func)
  (let ((s (open-output-file filename)))
	(let ((result (func s)))
	  (close-output-port s)
	  result)))


(define (format-display s . lis)
  (display (apply format lis) s))

(define (string-split s delimiter)
  (string-tokenize s (lambda (x) (not (char=? delimiter x)))))
