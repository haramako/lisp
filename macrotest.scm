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
				  (list (close-syntax '%define-syntax mac-env)
						(cadr expr)
						(list (close-syntax 'make-transformer mac-env)
							  (car (cddr expr))))))

(define free-identifier=?
  (lambda (x y)
	(eq? x y)))

(define (current-renamer . rest)
  (lambda (x) x))

(define er-macro-transformer
  (lambda (f)
    (lambda (expr)
      (f expr (current-renamer) free-identifier=?))))

(define (vector? x) #f)

(define identifier? symbol?)


(define-syntax cond
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
                                      (cons (rename 'cond) (cddr expr))))
                          (car cl))
                    (list (rename 'if)
                          (car cl)
                          (cons (rename 'begin) (cdr cl))
                          (cons (rename 'cond) (cddr expr))))))
          (cadr expr))))))

(define-syntax or
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)) #f)
           ((null? (cddr expr)) (cadr expr))
           (else
            (list (rename 'let) (list (list (rename 'tmp) (cadr expr)))
                  (list (rename 'if) (rename 'tmp)
                        (rename 'tmp)
                        (cons (rename 'or) (cddr expr)))))))))

(define-syntax and
  (er-macro-transformer
   (lambda (expr rename compare)
     (cond ((null? (cdr expr)))
           ((null? (cddr expr)) (cadr expr))
           (else (list (rename 'if) (cadr expr)
                       (cons (rename 'and) (cddr expr))
                       #f))))))

(define-syntax quasiquote
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (qq x d)
       (cond
        ((pair? x)
         (cond
          ((compare (rename 'unquote) (car x))
           (if (<= d 0)
               (cadr x)
               (list (rename 'list) (list (rename 'quote) 'unquote)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'unquote-splicing) (car x))
           (if (<= d 0)
               (list (rename 'cons) (qq (car x) d) (qq (cdr x) d))
               (list (rename 'list) (list (rename 'quote) 'unquote-splicing)
                     (qq (cadr x) (- d 1)))))
          ((compare (rename 'quasiquote) (car x))
           (list (rename 'list) (list (rename 'quote) 'quasiquote)
                 (qq (cadr x) (+ d 1))))
          ((and (<= d 0) (pair? (car x))
                (compare (rename 'unquote-splicing) (caar x)))
           (if (null? (cdr x))
               (cadr (car x))
               (list (rename 'append) (cadr (car x)) (qq (cdr x) d))))
          (else
           (list (rename 'cons-source) (qq (car x) d) (qq (cdr x) d)
                 (list (rename 'quote) x)))))
        ((vector? x) (list (rename 'list->vector) (qq (vector->list x) d)))
        ((if (identifier? x) #t (null? x)) (list (rename 'quote) x))
        (else x)))
     (qq (cadr expr) 0))))

(define-syntax let
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr)) (error "empty let" expr))
     (if (null? (cddr expr)) (error "no let body" expr))
     ((lambda (bindings)
        (if (list? bindings) #f (error "bad let bindings"))
        (if (every (lambda (x)
                     (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                   bindings)
            ((lambda (vars vals)
               (if (identifier? (cadr expr))
                   `((,(rename 'lambda) ,vars
                      (,(rename 'letrec) ((,(cadr expr)
                                           (,(rename 'lambda) ,vars
                                            ,@(cdr (cddr expr)))))
                       (,(cadr expr) ,@vars)))
                     ,@vals)
                   `((,(rename 'lambda) ,vars ,@(cddr expr)) ,@vals)))
             (map car bindings)
             (map cadr bindings))
            (error "bad let syntax" expr)))
      (if (identifier? (cadr expr)) (car (cddr expr)) (cadr expr))))))


(define (reverse-rec r ls)
  (if (null? ls)
	  r
	  (reverse-rec (cons (car ls) r) (cdr ls))))

(define (reverse ls)
  (reverse-rec '() ls))

(define (apply proc arg1 . args)
  (if (null? args)
      (apply1 proc arg1)
      ((lambda (lol)
         (apply1 proc (append2 (reverse (cdr lol)) (car lol))))
       (reverse (cons arg1 args)))))

(define (map proc ls . lol)
  (define (map1 proc ls res)
    (if (pair? ls)
        (map1 proc (cdr ls) (cons (proc (car ls)) res))
        (reverse res)))
  (define (mapn proc lol res)
    (if (every pair? lol)
        (mapn proc
              (map1 cdr lol '())
              (cons (apply proc (map1 car lol '())) res))
        (reverse res)))
  (if (null? lol)
      (map1 proc ls '())
      (mapn proc (cons ls lol) '())))


(define (every pred ls . lol)
  (define (every1 pred ls)
    (if (null? (cdr ls))
        (pred (car ls))
        (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
  (if (null? lol)
      (if (pair? ls) (every1 pred ls) #t)
      (not (apply any (lambda xs (not (apply pred xs))) ls lol))))


(define (vector? ls) #f)
(define (cons-source kar kdr) (cons kar kdr))

(define (append-helper ls res)
  (if (null? ls)
      res
      (append-helper (cdr ls) (append2 (car ls) res))))

(define (append . o)
  (if (null? o)
      '()
      ((lambda (lol)
         (append-helper (cdr lol) (car lol)))
       (reverse o))))

(define (append-rec rb ra)
  (if (null? ra)
	  rb
	  (append-rec (cons (car ra) rb) (cdr ra))))
  
(define (append2 a b) (append-rec b (reverse a)))
  

(define x 2)
(cond
 ((eq? x 1) (puts "one"))
 ((eq? x 2) (puts "two"))
 (else (puts "more")))

(and #t (puts "OK"))
(and #f (puts "NG"))
(or #t (puts "NG"))
(or #f (puts "OK"))

(define y '(3 4))
(puts `(1 ,x ,@y 2))


