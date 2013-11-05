;; TODO: <...>には未対応
(define-macro (cut . args)
  (define (convert args)
	(if (not (pair? args)) (values '() '())
		(cond ((eq? (car args) '<>)
			   (let ((sym (gensym)))
				 (let-values (((param vals) (convert (cdr args))))
				   (values (cons sym param) (cons sym vals)))))
			  (else
			   (let-values (((param vals) (convert (cdr args))))
				 (values param (cons (car args) vals)))))))
  (let-values (((param vals) (convert args)))
	`(lambda ,param ,vals)))

