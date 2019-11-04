(define integer? number?)
(define quotient /)
(define call-with-current-continuation call/cc)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (error err)
  (display err)
  (newline)
  (exit))

(define (newline)
  (display end-of-line))
  
(define (puts x)
  (display x)
  (newline))

(define (close-syntax sym mac-env)
  sym)

(define make-transformer
  (lambda (transformer)
    (lambda (expr use-env mac-env)
	  (transformer expr))))

(%define-syntax define-syntax
				(lambda (expr use-env mac-env)
				  (puts expr)
				  (list (close-syntax '%define-syntax mac-env)
						(cadr expr)
						(list (close-syntax 'make-transformer mac-env)
							  (car (cddr expr))))))

(define free-identifier=?
  (lambda (x y)
	#f))

(define (current-renamer . rest)
  (lambda (x) x))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr)
      (f expr (current-renamer) free-identifier=?))))

(define-syntax if2
  (lambda (expr)
	(cons 'if (cdr expr))))

(define-syntax deftest
  (er-macro-transformer
   (lambda (expr rename compare)
	 (list
	  'begin
	  (cons 'display (cdr expr))
	  (cons 'display (cdr expr))))))

#;(define x 1)
#;(if2 (eq? x 1)
	 (display 2))

(deftest 99)

(puts 1)
