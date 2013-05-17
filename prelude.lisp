;; macro-expand-all, quasi-quote に必要な物は早めに
(define *lambda-hook* value)
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

;; copy from tinyscheme.
;;
;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis
(define quasiquote 
  (macro (l)
		 (define mcons
		   (lambda (f l r)
			 ; (display 'mcons f l r)
			 (if (and (pair? r)
					  (eq? (car r) 'quote)
					  (eq? (car (cdr r)) (cdr f))
					  (pair? l)
					  (eq? (car l) 'quote)
					  (eq? (car (cdr l)) (car f)))
				 (if (or (procedure? f) (number? f))
					 f
				   (list 'quote f))
			   ;(if (eqv? l vector)
				;   (apply l (eval r))
				 (list 'cons l r))))
		 (define mappend
		   (lambda (f l r)
			 (if (or (null? (cdr f))
					 (and (pair? r)
						  (eq? (car r) 'quote)
						  (eq? (car (cdr r)) '())))
				 l
			   (list 'append l r))))
		 (define foo
		   (lambda (level form)
			 ; (display 'foo level form)
			 (cond ((not (pair? form))
					(if (or (procedure? form) (number? form))
						form
					  (list 'quote form))
					)
				   ((eq? 'quasiquote (car form))
					(mcons form ''quasiquote (foo (+ level 1) (cdr form))))
				   (#t (if (zero? level)
						   (cond ((eq? (car form) 'unquote) (car (cdr form)))
								 ((eq? (car form) 'unquote-splicing)
								  (error "Unquote-splicing wasn't in a list:"
										 form))
								 ((and (pair? (car form))
									   (eq? (car (car form)) 'unquote-splicing))
								  (mappend form (car (cdr (car form)))
										   (foo level (cdr form))))
								 (#t (mcons form (foo level (car form))
											(foo level (cdr form)))))
						 (cond ((eq? (car form) 'unquote)
								(mcons form ''unquote (foo (- level 1)
														   (cdr form))))
							   ((eq? (car form) 'unquote-splicing)
								(mcons form ''unquote-splicing
									   (foo (- level 1) (cdr form))))
							   (#t (mcons form (foo level (car form))
										  (foo level (cdr form))))))))))
		 (foo 0 l)))

(display 'unquote-splicing)

(define macro-form?
  (lambda (form)
	(and (pair? form)
		 (symbol? (car form))
		 (define? (car form))
		 (macro? (eval (car form))))))
	
(define macro-expand
  (lambda (form)
	(if (macro-form? form)
		(apply (eval (car form)) (cdr form))
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
	  (cons (macro-expand (car form)) (macro-expand-list (cdr form))))))

(define map (lambda (x f)
			  (if (pair? x)
				  (cons (car x) (f (cdr x)))
				x)))

(define *compile-hook* macro-expand-all)
