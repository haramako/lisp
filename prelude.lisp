;; macro-expand-all, quasi-quote に必要な物は早めに
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define cdar (lambda (x) (cdr (car x))))
(define cddr (lambda (x) (cdr (cdr x))))

(define cond
  (macro code
		 (define foo
		   (lambda (x)
			 (if (not (pair? x))
				 #f
			   (define c (car x))
			   (define rest (cdr x))
			   (list 'if (car c) (cons 'begin (cdr c)) (foo rest)))))
		 (foo code)))

(define zero? (macro (x) (list 'eqv? x 0)))

(define and
  (macro code
	(define one
	  (lambda (x)
		(if (null? (cdr x))
			(car x)
		  (list 'if (car x) (one (cdr x)) #f))))
	(one code)))

(define or
  (macro code
	(define one
	  (lambda (x)
		(if (null? (cdr x))
			(car x)
		  (list 'if (car x) #t (one (cdr x))))))
	(one code)))

;;; macro expander
(define macro-form?
  (lambda (form)
	(and (pair? form)
		 (symbol? (car form))
		 (define? (car form))
		 (macro? (eval (car form))))))
	
(define macro-expand
  (lambda (form)
	(if (macro-form? form)
		(begin
		 (apply (eval (car form)) (cdr form)))
	  form)))

							
(define macro-expand-all
  (lambda (form)
	(define macro-expand-list
	  (lambda (form)
		(if (not (pair? form))
			form
		  (cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))
	(if (not (pair? form))
		form
	  (set! form (macro-expand form))
	  (cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))

(define *compile-hook* macro-expand-all)

(define map (lambda (x f)
			  (if (pair? x)
				  (cons (car x) (f (cdr x)))
				x)))


