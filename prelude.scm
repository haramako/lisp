;; macro-expand-all, quasi-quote に必要な物は早めに
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))
(define (caaar x) (car (car (car x))))
(define (caadr x) (car (car (cdr x))))
(define (cadar x) (car (cdr (car x))))
(define (caddr x) (car (cdr (cdr x))))
(define (cdaar x) (cdr (car (car x))))
(define (cdadr x) (cdr (car (cdr x))))
(define (cddar x) (cdr (cdr (car x))))
(define (cdddr x) (cdr (cdr (cdr x))))
(define (caaaar x) (car (caaar x)))
(define (caaadr x) (car (caadr x)))
(define (caadar x) (car (cadar x)))
(define (caaddr x) (car (caddr x)))
(define (cadaar x) (car (cdaar x)))
(define (cadadr x) (car (cdadr x)))
(define (caddar x) (car (cddar x)))
(define (cadddr x) (car (cdddr x)))
(define (cdaaar x) (cdr (caaar x)))
(define (cdaadr x) (cdr (caadr x)))
(define (cdadar x) (cdr (cadar x)))
(define (cdaddr x) (cdr (caddr x)))
(define (cddaar x) (cdr (cdaar x)))
(define (cddadr x) (cdr (cdadr x)))
(define (cdddar x) (cdr (cddar x)))
(define (cddddr x) (cdr (cdddr x)))

(define define-macro
  (macro form
		 (list 'define
			   (car (car form))
			   (cons 'macro (cons (cdr (car form)) (cdr form))))))

(define (map f li)
  (let recur ((f f) (li li))
	(if (pair? li)
		(cons (f (car li)) (recur f (cdr li)))
		li)))

(define (for-each f l)
  (let recur ((f f) (l l))
	(when (pair? l)
		  (f (car l))
		  (recur f (cdr l)))))

(define (newline)
  (display end-of-line))

;; (puts obj1 ...)
(define (puts . x)
  (if (not (pair? x))
	  (display end-of-line)
	(display (car x))
	(display " ")
	(apply puts (cdr x))))

(define cond
  (macro form
	(let recur ((form form))
	  (if (not (pair? form)) '(#f)
		  (let ((c (car form))
				(rest (cdr form)))
			(if (and (pair? c)
					 (eq? (car (cdr c)) '=>))
				(list 'if (car c) (list (car (cdr (cdr c))) (car c)) (recur rest))
				(list 'if (car c) (cons 'begin (cdr c)) (recur rest))))))))

(define else #t)

(define (error . mes)
  (puts "error:" mes)
  (backtrace)
  (exit 1))

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

1

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
;; ここからマクロが有効化

(define-macro (when c . t)
  (list 'if c (cons 'begin t)))

(define (reverse lis)
  (let recur ((r '()) (lis lis))
	(if (pair? lis)
		(recur (cons (car lis) r) (cdr lis))
		r)))

(define (append . lists)
  (define (prepend-1 r lis)
	(if (pair? lis)
		(cons (car lis) (prepend-1 r (cdr lis)))
		r))
  (if (pair? lists)
	  (let recur ((r '())
				  (list1 (car lists))
				  (lists (cdr lists)))
		(if (pair? lists)
			(prepend-1 (recur r (car lists) (cdr lists)) list1)
			(prepend-1 r list1)))
	  '()))

;; copy from tinyscheme.
;;
;; The following quasiquote macro is due to Eric S. Tiedemann.
;;   Copyright 1988 by Eric S. Tiedemann; all rights reserved.
;;
;; Subsequently modified to handle vectors: D. Souflis
(define-macro (quasiquote+ l)
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
  )

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
		  (#t (if (eqv? level 0)
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

;(define-macro (not-pair? x) `(not (pair? ,x)))



(define (mac form)
  (puts (macro-expand-all form)))
  
(define-macro (do arg . body)
  (let ((arg-form (map (lambda (x) (list (car x) (cadr x))) arg))
		(next-form (map caddr arg)))
	`(let *loop* ,arg-form
	   (if ,(caar body) (begin ,@(cdar body))
		   ,@(cdr body)
		   (*loop* ,@next-form)))))

(define (values . x)
  (if (null? (cdr x)) (car x) (cons 'VALUES x)))

(define (call-with-values v f)
  (if (and (pair? v) (eq? 'VALUES (car v)))
	  (apply f (cdr v))
	  (f v)))

(define-macro (receive args vals . body)
  `(let ((*vals* (cdr ,vals)))
	 ,(let loop ((args args))
		(cond
		 ((pair? args)
		  `(let ((,(car args) (car *vals*))
				 (*vals* (cdr *vals*)))
			 ,(loop (cdr args))))
		 (#t (cons 'begin body))))))

(define (zero? x) (eqv? x 0))
(define (negative? x) (< x 0))
(define (positive? x) (>= x 0))
(define (even? x) (eqv? (modulo x 2) 0))
(define (odd? x) (eqv? (modulo x 2) 1))

(define (list-tail li n)
  (let loop ((li li) (n n))
	(if (zero? n) li (loop (cdr li) (- n 1)))))

(define (length li)
  (let loop ((li li) (n 0))
	(if (null? li) n (loop (cdr li) (+ n 1)))))

(define-macro (define-unless . form)
  (let ((sym (car (car form)))
		(args (cdr (car form))))
	`(if (define? ',sym)
		 (set! ,sym (lambda ,args ,@(cdr form)))
		 (define ,@form))))

(define-macro (:optional sym val)
  `(if (pair? ,sym) (car ,sym) ,val))
  

;;************************************************************
;; from http://srfi.schemers.org/srfi-1/srfi-1-reference.scm
;;************************************************************
(define (tree-copy x_)
  (let recur ((x x_))
	(if (not (pair? x)) x
		(cons (recur (car x)) (recur (cdr x))))))

(define (check-arg pred val caller)
  (if (pred val) val (check-arg (error "Bad argument" val pred caller))))
