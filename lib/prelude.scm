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
	(if (not (pair? l)) #f
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

(define (*tee* x)
  (puts x)
  x)

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
  (apply puts (cons "error:" mes))
  (backtrace)
  (exit 1))

;;; macro expander
(define (macro-form? form)
  (and (pair? form)
	   (symbol? (car form))
	   (defined? (car form))
	   (macro? (eval (car form)))))

(define (macro-expand form)
  (if (macro-form? form)
	  (begin
	   (apply (eval (car form)) (cdr form)))
	(syntax-expand1 form)))

(define (macro-expand-all form)
  (define macro-expand-list
	(lambda (form)
	  (if (not (pair? form))
		  form
		(cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))
  (if (not (pair? form))
	  form
	  (let ((form (macro-expand form)))
		(cons (macro-expand-all (car form)) (macro-expand-list (cdr form))))))

(define *compile-hook* macro-expand-all)
;; ここからマクロが有効化

(define-syntax require
  (syntax-rules ()
	((_ mod) (%require 'mod))))

(define-syntax when 
  (syntax-rules ()
	  ((_ cnd body ...) (if cnd (begin body ...)))))

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
(define-macro (quasiquote l)
  (define (mcons f l r)
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

(define-syntax unless
  (syntax-rules ()
	((_ cnd body ...) (if cnd #f body ...))))

(define-macro (do arg . body)
  (let ((arg-form (map (lambda (x) (list (car x) (cadr x))) arg))
		(next-form (map caddr arg)))
	`(let *loop* ,arg-form
	   (if ,(caar body) (begin ,@(cdar body))
		   ,@(cdr body)
		   (*loop* ,@next-form)))))

;; values

(define (values . x)
  (if (null? (cdr x)) (car x) (cons 'VALUES x)))

(define (call-with-values v f)
  (let ((v (apply v)))
	(if (and (pair? v) (eq? 'VALUES (car v)))
		(apply f (cdr v))
		(f v))))

(define-syntax receive
  (syntax-rules ()
	((_ (binds ...) val body ...)
	 (call-with-values (lambda () val)
	   (lambda (binds ...) body ...)))))

(define-syntax let-values
  (syntax-rules ()
	((_ (((args ...) val)) body ...)
	 (call-with-values (lambda () val)
	   (lambda (args ...) body ...)))
	((_ (((args ...) val) rest ...) body ...)
	 (call-with-values (lambda () val)
	   (lambda (args ...)
	  	 (let-values (rest ...) body ...))))
	))

;; number

(define (zero? x) (eqv? x 0))
(define (negative? x) (< x 0))
(define (positive? x) (>= x 0))
(define (even? x) (eqv? (modulo x 2) 0))
(define (odd? x) (eqv? (modulo x 2) 1))

(define-syntax inc!
  (syntax-rules ()
	((_ x) (set! x (+ x 1)))))

(define-syntax dec!
  (syntax-rules ()
	((_ x) (set! x (+ x 1)))))

;; list

(define (list-tail li n)
  (let loop ((li li) (n n))
	(if (zero? n) li (loop (cdr li) (- n 1)))))

(define (length li)
  (let loop ((li li) (n 0))
	(if (null? li) n (loop (cdr li) (+ n 1)))))

(define-syntax let1
  (syntax-rules ()
	((_ var body ...)
	 (let (var) body ...))))

;;************************************************************
;; from http://srfi.schemers.org/srfi-1/srfi-1-reference.scm
;;************************************************************

(define-syntax :optional
  (syntax-rules ()
	((_ sym val) (if (pair? sym) (car sym) val))))

(define (tree-copy x_)
  (let recur ((x x_))
	(if (not (pair? x)) x
		(cons (recur (car x)) (recur (cdr x))))))

(define (check-arg pred val caller)
  (if (pred val) val (check-arg (error "Bad argument" val pred caller))))


;;************************************************************
;; dynamic-wind from tiny-scheme
;; URL: http://tinyscheme.sourceforge.net/
;;************************************************************

;;;;;Helper for the dynamic-wind definition.  By Tom Breton (Tehom)
(define (shared-tail x y)
   (let ((len-x (length x))
         (len-y (length y)))
      (define (shared-tail-helper x y)
         (if
            (eq? x y)
            x
            (shared-tail-helper (cdr x) (cdr y))))

      (cond
         ((> len-x len-y)
            (shared-tail-helper
               (list-tail x (- len-x len-y))
               y))
         ((< len-x len-y)
            (shared-tail-helper
               x
               (list-tail y (- len-y len-x))))
         (#t (shared-tail-helper x y)))))

;;;;;Dynamic-wind by Tom Breton (Tehom)

;;Guarded because we must only eval this once, because doing so
;;redefines call/cc in terms of old call/cc
(unless (defined? 'dynamic-wind)
   (let
      ;;These functions are defined in the context of a private list of
      ;;pairs of before/after procs.
      (  (*active-windings* '())
         ;;We'll define some functions into the larger environment, so
         ;;we need to know it.
         (outer-env (current-environment)))

      ;;Poor-man's structure operations
      (define before-func car)
      (define after-func  cdr)
      (define make-winding cons)

      ;;Manage active windings
      (define (activate-winding! new)
         ((before-func new))
         (set! *active-windings* (cons new *active-windings*)))
      (define (deactivate-top-winding!)
         (let ((old-top (car *active-windings*)))
            ;;Remove it from the list first so it's not active during its
            ;;own exit.
            (set! *active-windings* (cdr *active-windings*))
            ((after-func old-top))))

      (define (set-active-windings! new-ws)
         (unless (eq? new-ws *active-windings*)
            (let ((shared (shared-tail new-ws *active-windings*)))

               ;;Define the looping functions.
               ;;Exit the old list.  Do deeper ones last.  Don't do
               ;;any shared ones.
               (define (pop-many)
                  (unless (eq? *active-windings* shared)
                     (deactivate-top-winding!)
                     (pop-many)))
               ;;Enter the new list.  Do deeper ones first so that the
               ;;deeper windings will already be active.  Don't do any
               ;;shared ones.
               (define (push-many new-ws)
                  (unless (eq? new-ws shared)
                     (push-many (cdr new-ws))
                     (activate-winding! (car new-ws))))

               ;;Do it.
               (pop-many)
               (push-many new-ws))))

      ;;The definitions themselves.
      (eval
         `(define call-with-current-continuation
             ;;It internally uses the built-in call/cc, so capture it.
             ,(let ((old-c/cc call-with-current-continuation))
                 (lambda (func)
                    ;;Use old call/cc to get the continuation.
                    (old-c/cc
                       (lambda (continuation)
                          ;;Call func with not the continuation itself
                          ;;but a procedure that adjusts the active
                          ;;windings to what they were when we made
                          ;;this, and only then calls the
                          ;;continuation.
                          (func
                             (let ((current-ws *active-windings*))
                                (lambda x
								  (set-active-windings! current-ws)
								  (apply continuation x)))))))))
         outer-env)
      ;;We can't just say "define (dynamic-wind before thunk after)"
      ;;because the lambda it's defined to lives in this environment,
      ;;not in the global environment.
      (eval
         `(define dynamic-wind
             ,(lambda (before thunk after)
                 ;;Make a new winding
                 (activate-winding! (make-winding before after))
                 (let ((result (thunk)))
                    ;;Get rid of the new winding.
                    (deactivate-top-winding!)
                    ;;The return value is that of thunk.
                    result)))
         outer-env)))

(define call/cc call-with-current-continuation)

;;************************************************************
;; other
;;************************************************************

(define-syntax mac
  (syntax-rules ()
	((_ form) (puts (macro-expand-all 'form)))))
