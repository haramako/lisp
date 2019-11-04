(define integer? number?)
(define quotient /)
(define call-with-current-continuation call/cc)

(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (error err)
  (display err)
  (exit))

(define (newline)
  (display end-of-line))
  
(define (puts x)
  (display x)
  (newline))

(define (close-syntax sym mac-env)
  sym)

(puts "hoge")

(%define-syntax define-syntax
				(lambda (expr use-env mac-env)
				  (list (close-syntax '%define-syntax mac-env)
						(cadr expr)
						(list (close-syntax 'make-transformer mac-env)
							  (car (cddr expr))))))

(define-syntax deftest
  (er-macro-transformer
   (lambda (expr rename compare)
     expr)))

(puts 1)
