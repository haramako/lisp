;; macro-expand-all, quasi-quote に必要な物は早めに
(define define-macro
  (macro form
		 (list 'define
			   (car (car form))
			   (cons 'macro (cons (cdr (car form)) (cdr form))))))

(define-macro (cond . code)
  (define foo
	(lambda (x)
	  (if (not (pair? x))
		  #f
		(define c (car x))
		(define rest (cdr x))
		(list 'if (car c) (cons 'begin (cdr c)) (foo rest)))))
  (foo code))

(define-macro (zero? x) (list 'eqv? x 0))

(define-macro (and . form)
  (if (null? (cdr form))
	  (car form)
	(list 'if (car form) (apply and (cdr form)) #f)))

(define-macro (or . form)
  (if (null? (cdr form))
	  (car form)
	(list 'if (car form) #t (apply or (cdr form)))))

;;; macro expander
(define (macro-form? form)
  (and (pair? form)
	   (symbol? (car form))
	   (define? (car form))
	   (macro? (eval (car form)))))

(define (macro-expand form)
  (if (macro-form? form)
	  (begin
	   (apply (eval (car form)) (cdr form)))
	form))

(define (macro-expand-all form)
  (define macro-expand-list
	(lambda (form)
	  (if (not (pair? form))
		  form
		(cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))
  (if (not (pair? form))
	  form
	(set! form (macro-expand form))
	(cons (macro-expand-all (car form)) (macro-expand-list (cdr form)))))

(define *compile-hook* macro-expand-all)

;; copy from tinyscheme.
;;
;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis
(define-macro (quasiquote l)
  (define (mcons f l r)
	;; (display (list 'mcons f l r))
	(if (and (pair? r)
			 (eq? (car r) 'quote)
			 (eq? (car (cdr r)) (cdr f))
			 (pair? l)
			 (eq? (car l) 'quote)
			 (eq? (car (cdr l)) (car f)))
		(if (or (procedure? f) (number? f))
			f
		  (list 'quote f))
	  ;;(if (eqv? l vector)
	  ;;   (apply l (eval r))
	  (list 'cons l r)))
  (define (mappend f l r)
	(if (or (null? (cdr f))
			(and (pair? r)
				 (eq? (car r) 'quote)
				 (eq? (car (cdr r)) '())))
		l
	  (list 'append l r)))
  (define (foo level form)
	;; (display (list 'foo level form ))
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
								 (foo level (cdr form)))))))))
  (foo 0 l))

;; utilities
(define-macro (caar x) `(car (car ,x)))
(define-macro (cadr x) `(car (cdr ,x)))
(define-macro (cdar x) `(cdr (car ,x)))
(define-macro (cddr x) `(cdr (cdr ,x)))

(define (map x f)
  (if (pair? x)
	  (cons (car x) (f (cdr x)))
	x))

(define-macro (not-pair? x) `(not (pair? ,x)))

;; (puts obj1 ...)
(define (puts . x)
  (if (not-pair? x)
	  (display end-of-line)
	(display (car x))
	(display " ")
	(apply puts (cdr x))))

;;; unit-test
(define-macro (assert _eq _test _expect)
  `(let ((test ,_test)
		 (expect ,_expect))
	 (if (,_eq test expect) #t (puts "FAILED:" ',_test "expect" expect "but" test))))
