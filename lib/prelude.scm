;; macro-expand-all, quasi-quote に必要な物は早めに
(define integer? number?)
(define quotient /)
(define call-with-current-continuation call/cc)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

#;(
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
)

(define (error . mes)
  (display "error:")
  (display mes)
  (newline)
  (backtrace)
  (exit 1))

(define newline (lambda ()
				  (display end-of-line)))

(define define-macro
  (macro form
		 (list 'define
			   (car (car form))
			   (cons 'macro (cons (cdr (car form)) (cdr form))))))

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
  (if (or (not (pair? form)) (eq? '%define-syntax (car form)))
	  form
	  (let ((form (macro-expand (syntax-expand1 form))))
		(cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))

(define *compile-hook* macro-expand-all)
;; ここからマクロが有効化
