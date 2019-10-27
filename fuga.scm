(define current-renamer (lambda () (lambda (x) x)))

#;(define free-identifier=?
  (lambda (x y)
    ((lambda (use-env cur-env)
       (identifier=? (if use-env use-env cur-env) x
		     (if use-env use-env cur-env) y))
     (current-usage-environment)
     (current-environment))))

(define free-identifier=? (lambda (x y) (eq? x y)))


(define er-macro-transformer
  (lambda (f)
    (lambda expr
      (f (cons 'dummy expr) (current-renamer) free-identifier=?))))

;(define-macro (%define-syntax name expr)
;  (list name expr))

(%define-syntax cond2
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr))
         (if #f #f)
         ((lambda (cl)
            (if (compare (rename 'else) (car cl))
                (if (pair? (cddr expr))
                    (error "non-final else in cond" expr)
                    (cons (rename 'begin) (cdr cl)))
                (if (if (null? (cdr cl)) #t (compare (rename '=>) (cadr cl)))
                    (list (list (rename 'lambda) (list (rename 'tmp))
                                (list (rename 'if) (rename 'tmp)
                                      (if (null? (cdr cl))
                                          (rename 'tmp)
                                          (list (car (cddr cl)) (rename 'tmp)))
                                      (cons (rename 'cond2) (cddr expr))))
                          (car cl))
                    (list (rename 'if)
                          (car cl)
                          (cons (rename 'begin) (cdr cl))
                          (cons (rename 'cond2) (cddr expr))))))
          (cadr expr))))))

(define x 1)
(display (cond2
		  ((eq? x 1) 10)
		  ((eq? x 2) 20)
		  (else 30)))

(exit)
		  
;(display ((er-macro-transformer (lambda (expr rename compare) expr)) '(a b)))


(display (list 1 2 . (3 4)))
(newline)


(%define-syntax x2
				(er-macro-transformer (lambda (expr rename compare)
										(cons (rename 'list) expr))))




;(display (macro-expand-all '(x2 10 20)))
;(display (x2 10 20))

#;(newline)
#;(display (x2 1 10 20))

#;(define (close-syntax x) x)




#;(define-macro (aa l))
#;(list 'display l)

#;(mac (aa 1))
(newline)
