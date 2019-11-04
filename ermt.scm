(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

(define (cons-source kar kdr source)
  ((lambda (pair source)
     (if (pair? source)
         (pair-source-set! pair (pair-source source)))
     pair)
   (cons kar kdr)
   (strip-syntactic-closures source)))

(define (not x) (if x #f #t))
(define (boolean? x) (if (eq? x #t) #t (eq? x #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; basic utils

(define (procedure? x) (if (closure? x) #t (opcode? x)))

(define (length ls)
  (if (list? ls) (length* ls) (error "length: not a list" ls)))

(define (list . args) args)

(define (list-tail ls k)
  (if (eq? k 0)
      ls
      (list-tail (cdr ls) (- k 1))))

(define (list-ref ls k) (car (list-tail ls k)))

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

#;(define (apply proc arg1 . args)
  (if (null? args)
      (apply1 proc arg1)
      ((lambda (lol)
         (apply1 proc (append2 (reverse (cdr lol)) (car lol))))
       (reverse (cons arg1 args)))))

;; map with a fast-path for single lists

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

(define (for-each f ls . lol)
  (define (for1 f ls) (if (pair? ls) (begin (f (car ls)) (for1 f (cdr ls)))))
  (if (null? lol) (for1 f ls) (begin (apply map f ls lol) (if #f #f))))

(define (any pred ls . lol)
  (define (any1 pred ls)
    (if (pair? (cdr ls))
        ((lambda (x) (if x x (any1 pred (cdr ls)))) (pred (car ls)))
        (pred (car ls))))
  (define (anyn pred lol)
    (if (every pair? lol)
        ((lambda (x) (if x x (anyn pred (map cdr lol))))
         (apply pred (map car lol)))
        #f))
  (if (null? lol) (if (pair? ls) (any1 pred ls) #f) (anyn pred (cons ls lol))))

(define (every pred ls . lol)
  (define (every1 pred ls)
    (if (null? (cdr ls))
        (pred (car ls))
        (if (pred (car ls)) (every1 pred (cdr ls)) #f)))
  (if (null? lol)
      (if (pair? ls) (every1 pred ls) #t)
      (not (apply any (lambda xs (not (apply pred xs))) ls lol))))

; TODO
;(define (error msg . args)
;  (raise (make-exception 'user msg args #f #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax
(define *current-renamer* (lambda (x) x))
(define (current-renamer . rest)
  *current-renamer*)


(current-renamer (lambda (x) x))

(define close-syntax
  (lambda (form env)
    (make-syntactic-closure env '() form)))

(define make-renamer
  (lambda (mac-env)
    (define rename
      ((lambda (renames)
         (lambda (identifier)
	   ((lambda (cell)
	      (if cell
		  (cdr cell)
	  	  ((lambda (name)
		     (set! renames (cons (cons identifier name) renames))
		     name)
                   ((lambda (id)
                      (syntactic-closure-set-rename! id rename)
                      id)
 		    (close-syntax identifier mac-env)))))
	    (assq identifier renames))))
       '()))
    rename))

(define make-transformer
  (lambda (transformer)
    (lambda (expr use-env mac-env)
      ((lambda (old-use-env old-mac-env old-renamer)
		 (current-usage-environment use-env)
		 (current-transformer-environment mac-env)
		 (current-renamer (make-renamer mac-env))
		 ((lambda (result)
			(current-usage-environment old-use-env)
			(current-transformer-environment old-mac-env)
			(current-renamer old-renamer)
			result)
		  (transformer expr)))
       (current-usage-environment)
       (current-transformer-environment)
       (current-renamer)))))

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


(define free-identifier=?
  (lambda (x y)
    ((lambda (use-env cur-env)
       (identifier=? (if use-env use-env cur-env) x
		     (if use-env use-env cur-env) y))
     (current-usage-environment)
     (current-environment))))

(define sc-macro-transformer
  (lambda (f)
    (lambda (expr)
      (close-syntax (f expr (current-usage-environment))
		    (current-transformer-environment)))))

(define rsc-macro-transformer
  (lambda (f)
    (lambda (expr)
      (f expr (current-transformer-environment)))))

(define er-macro-transformer
  (lambda (f)
    (lambda expr
      (f (cons 'dummy expr) (current-renamer) free-identifier=?))))

#;(define er-macro-transformer
  (lambda (f)
    (lambda (expr)
      (f expr (current-renamer) free-identifier=?))))

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

(define-syntax letrec
  (er-macro-transformer
   (lambda (expr rename compare)
     ((lambda (defs)
        `((,(rename 'lambda) () ,@defs ,@(cddr expr))))
      (map (lambda (x) (cons (rename 'define) x)) (cadr expr))))))

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

(define-syntax let*
  (er-macro-transformer
   (lambda (expr rename compare)
     (if (null? (cdr expr)) (error "empty let*" expr))
     (if (null? (cddr expr)) (error "no let* body" expr))
     (if (null? (cadr expr))
         `(,(rename 'let) () ,@(cddr expr))
         (if (if (list? (cadr expr))
                 (every
                  (lambda (x)
                    (if (pair? x) (if (pair? (cdr x)) (null? (cddr x)) #f) #f))
                  (cadr expr))
                 #f)
             `(,(rename 'let) (,(caar (cdr expr)))
               (,(rename 'let*) ,(cdar (cdr expr)) ,@(cddr expr)))
             (error "bad let* syntax"))))))

(define-syntax case
  (er-macro-transformer
   (lambda (expr rename compare)
     (define (body exprs)
       (cond
        ((null? exprs)
         (rename 'tmp))
        ((compare (rename '=>) (car exprs))
         `(,(cadr exprs) ,(rename 'tmp)))
        (else
         `(,(rename 'begin) ,@exprs))))
     (define (clause ls)
       (cond
        ((null? ls) #f)
        ((compare (rename 'else) (caar ls))
         (body (cdar ls)))
        ((and (pair? (car (car ls))) (null? (cdr (car (car ls)))))
         `(,(rename 'if) (,(rename 'eqv?) ,(rename 'tmp)
                          (,(rename 'quote) ,(car (caar ls))))
           ,(body (cdar ls))
           ,(clause (cdr ls))))
        (else
         `(,(rename 'if) (,(rename 'memv) ,(rename 'tmp)
                          (,(rename 'quote) ,(caar ls)))
           ,(body (cdar ls))
           ,(clause (cdr ls))))))
     `(let ((,(rename 'tmp) ,(cadr expr)))
        ,(clause (cddr expr))))))

(define-syntax do
  (er-macro-transformer
   (lambda (expr rename compare)
     (let* ((body
             `(,(rename 'begin)
               ,@(cdr (cddr expr))
               (,(rename 'lp)
                ,@(map (lambda (x)
                         (if (pair? (cddr x))
                             (if (pair? (cdr (cddr x)))
                                 (error "too many forms in do iterator" x)
                                 (car (cddr x)))
                             (car x)))
                       (cadr expr)))))
            (check (car (cddr expr)))
            (wrap
             (if (null? (cdr check))
                 `(,(rename 'let) ((,(rename 'tmp) ,(car check)))
                   (,(rename 'if) ,(rename 'tmp)
                    ,(rename 'tmp)
                    ,body))
                 `(,(rename 'if) ,(car check)
                   (,(rename 'begin) ,@(cdr check))
                   ,body))))
       `(,(rename 'let) ,(rename 'lp)
         ,(map (lambda (x) (list (car x) (cadr x))) (cadr expr))
         ,wrap)))))

(define-syntax delay-force
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'promise) #f (,(rename 'lambda) () ,(cadr expr))))))

(define-syntax delay
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'delay-force) (,(rename 'promise) #t ,(cadr expr))))))

(define-syntax define-auxiliary-syntax
  (er-macro-transformer
   (lambda (expr rename compare)
     `(,(rename 'define-syntax) ,(cadr expr)
       (,(rename 'er-macro-transformer)
        (,(rename 'lambda) (expr rename compare)
         (,(rename 'error) "invalid use of auxiliary syntax" ',(cadr expr))))))))

(define-auxiliary-syntax _)
(define-auxiliary-syntax =>)
(define-auxiliary-syntax ...)
(define-auxiliary-syntax else)
(define-auxiliary-syntax unquote)
(define-auxiliary-syntax unquote-splicing)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; syntax-rules

(define (syntax-rules-transformer expr rename compare)
  (let ((ellipsis-specified? (identifier? (cadr expr)))
        (count 0)
        (_er-macro-transformer (rename 'er-macro-transformer))
        (_lambda (rename 'lambda))      (_let (rename 'let))
        (_begin (rename 'begin))        (_if (rename 'if))
        (_and (rename 'and))            (_or (rename 'or))
        (_eq? (rename 'eq?))            (_equal? (rename 'equal?))
        (_car (rename 'car))            (_cdr (rename 'cdr))
        (_cons (rename 'cons))          (_pair? (rename 'pair?))
        (_null? (rename 'null?))        (_expr (rename 'expr))
        (_rename (rename 'rename))      (_compare (rename 'compare))
        (_quote (rename 'syntax-quote)) (_apply (rename 'apply))
        (_append (rename 'append))      (_map (rename 'map))
        (_vector? (rename 'vector?))    (_list? (rename 'list?))
        (_len (rename 'len))            (_length (rename 'length*))
        (_- (rename '-))   (_>= (rename '>=))   (_error (rename 'error))
        (_ls (rename 'ls)) (_res (rename 'res)) (_i (rename 'i))
        (_reverse (rename 'reverse))
        (_vector->list (rename 'vector->list))
        (_list->vector (rename 'list->vector))
        (_cons3 (rename 'cons-source))
        (_underscore (rename '_)))
    (define ellipsis (if ellipsis-specified? (cadr expr) (rename '...)))
    (define lits (if ellipsis-specified? (car (cddr expr)) (cadr expr)))
    (define forms (if ellipsis-specified? (cdr (cddr expr)) (cddr expr)))
    (define (next-symbol s)
      (set! count (+ count 1))
      (rename (string->symbol (string-append s (%number->string count)))))
    (define (expand-pattern pat tmpl)
      (let lp ((p (cdr pat))
               (x (list _cdr _expr))
               (dim 0)
               (vars '())
               (k (lambda (vars)
                    (list _cons (expand-template tmpl vars) #f))))
        (let ((v (next-symbol "v.")))
          (list
           _let (list (list v x))
           (cond
            ((identifier? p)
             (cond
              ((ellipsis-mark? p)
               (error "bad ellipsis" p))
              ((memq p lits)
               (list _and
                     (list _compare v (list _rename (list _quote p)))
                     (k vars)))
              ((compare p _underscore)
               (k vars))
              (else
               (list _let (list (list p v)) (k (cons (cons p dim) vars))))))
            ((ellipsis? p)
             (cond
              ((not (null? (cdr (cdr p))))
               (cond
                ((any (lambda (x) (and (identifier? x) (ellipsis-mark? x)))
                      (cddr p))
                 (error "multiple ellipses" p))
                (else
                 (let ((len (length* (cdr (cdr p))))
                       (_lp (next-symbol "lp.")))
                   `(,_let ((,_len (,_length ,v)))
                      (,_and (,_>= ,_len ,len)
                             (,_let ,_lp ((,_ls ,v)
                                          (,_i (,_- ,_len ,len))
                                          (,_res (,_quote ())))
                                    (,_if (,_>= 0 ,_i)
                                        ,(lp `(,(cddr p)
                                               (,(car p) ,(car (cdr p))))
                                             `(,_cons ,_ls
                                                      (,_cons (,_reverse ,_res)
                                                              (,_quote ())))
                                             dim
                                             vars
                                             k)
                                        (,_lp (,_cdr ,_ls)
                                              (,_- ,_i 1)
                                              (,_cons3 (,_car ,_ls)
                                                       ,_res
                                                       ,_ls))))))))))
              ((identifier? (car p))
               (list _and (list _list? v)
                     (list _let (list (list (car p) v))
                           (k (cons (cons (car p) (+ 1 dim)) vars)))))
              (else
               (let* ((w (next-symbol "w."))
                      (_lp (next-symbol "lp."))
                      (new-vars (all-vars (car p) (+ dim 1)))
                      (ls-vars (map (lambda (x)
                                      (next-symbol
                                       (string-append
                                        (symbol->string
                                         (identifier->symbol (car x)))
                                        "-ls")))
                                    new-vars))
                      (once
                       (lp (car p) (list _car w) (+ dim 1) '()
                           (lambda (_)
                             (cons
                              _lp
                              (cons
                               (list _cdr w)
                               (map (lambda (x l)
                                      (list _cons (car x) l))
                                    new-vars
                                    ls-vars)))))))
                 (list
                  _let
                  _lp (cons (list w v)
                            (map (lambda (x) (list x (list _quote '()))) ls-vars))
                  (list _if (list _null? w)
                        (list _let (map (lambda (x l)
                                          (list (car x) (list _reverse l)))
                                        new-vars
                                        ls-vars)
                              (k (append new-vars vars)))
                        (list _and (list _pair? w) once)))))))
            ((pair? p)
             (list _and (list _pair? v)
                   (lp (car p)
                       (list _car v)
                       dim
                       vars
                       (lambda (vars)
                         (lp (cdr p) (list _cdr v) dim vars k)))))
            ((vector? p)
             (list _and
                   (list _vector? v)
                   (lp (vector->list p) (list _vector->list v) dim vars k)))
            ((null? p) (list _and (list _null? v) (k vars)))
            (else (list _and (list _equal? v p) (k vars))))))))
    (define ellipsis-mark?
      (if (if ellipsis-specified?
              (memq ellipsis lits)
              (any (lambda (x) (compare ellipsis x)) lits))
          (lambda (x) #f)
          (if ellipsis-specified?
              (lambda (x) (eq? ellipsis x))
              (lambda (x) (compare ellipsis x)))))
    (define (ellipsis-escape? x) (and (pair? x) (ellipsis-mark? (car x))))
    (define (ellipsis? x)
      (and (pair? x) (pair? (cdr x)) (ellipsis-mark? (cadr x))))
    (define (ellipsis-depth x)
      (if (ellipsis? x)
          (+ 1 (ellipsis-depth (cdr x)))
          0))
    (define (ellipsis-tail x)
      (if (ellipsis? x)
          (ellipsis-tail (cdr x))
          (cdr x)))
    (define (all-vars x dim)
      (let lp ((x x) (dim dim) (vars '()))
        (cond ((identifier? x)
               (if (or (memq x lits)
                       (compare x _underscore))
                   vars
                   (cons (cons x dim) vars)))
              ((ellipsis? x) (lp (car x) (+ dim 1) (lp (cddr x) dim vars)))
              ((pair? x) (lp (car x) dim (lp (cdr x) dim vars)))
              ((vector? x) (lp (vector->list x) dim vars))
              (else vars))))
    (define (free-vars x vars dim)
      (let lp ((x x) (free '()))
        (cond
         ((identifier? x)
          (if (and (not (memq x free))
                   (cond ((assq x vars) => (lambda (cell) (>= (cdr cell) dim)))
                         (else #f)))
              (cons x free)
              free))
         ((pair? x) (lp (car x) (lp (cdr x) free)))
         ((vector? x) (lp (vector->list x) free))
         (else free))))
    (define (expand-template tmpl vars)
      (let lp ((t tmpl) (dim 0) (ell-esc #f))
        (cond
         ((identifier? t)
          (cond
           ((find (lambda (v) (eq? t (car v))) vars)
            => (lambda (cell)
                 (if (<= (cdr cell) dim)
                     t
                     (error "too few ...'s"))))
           (else
            (list _rename (list _quote t)))))
         ((pair? t)
          (cond
           ((and (ellipsis-escape? t) (not ell-esc))
            (lp (if (and (pair? (cdr t)) (null? (cddr t))) (cadr t) (cdr t)) dim #t))
           ((and (ellipsis? t) (not ell-esc))
            (let* ((depth (ellipsis-depth t))
                   (ell-dim (+ dim depth))
                   (ell-vars (free-vars (car t) vars ell-dim)))
              (cond
               ((null? ell-vars)
                (error "too many ...'s"))
               ((and (null? (cdr (cdr t))) (identifier? (car t)))
                ;; shortcut for (var ...)
                (lp (car t) ell-dim ell-esc))
               (else
                (let* ((once (lp (car t) ell-dim ell-esc))
                       (nest (if (and (null? (cdr ell-vars))
                                      (identifier? once)
                                      (eq? once (car vars)))
                                 once ;; shortcut
                                 (cons _map
                                       (cons (list _lambda ell-vars once)
                                             ell-vars))))
                       (many (do ((d depth (- d 1))
                                  (many nest
                                        (list _apply _append many)))
                                 ((= d 1) many))))
                  (if (null? (ellipsis-tail t))
                      many ;; shortcut
                      (list _append many (lp (ellipsis-tail t) dim ell-esc))))))))
           (else (list _cons3 (lp (car t) dim ell-esc) (lp (cdr t) dim ell-esc) (list _quote t)))))
         ((vector? t) (list _list->vector (lp (vector->list t) dim ell-esc)))
         ((null? t) (list _quote '()))
         (else t))))
    (list
     _er-macro-transformer
     (list _lambda (list _expr _rename _compare)
           (list
            _car
            (cons
             _or
             (append
              (map
               (lambda (clause) (expand-pattern (car clause) (cadr clause)))
               forms)
              (list
               (list _cons
                     (list _error "no expansion for"
                           (list (rename 'strip-syntactic-closures) _expr))
                     #f)))))))))

(define-syntax syntax-rules
  (er-macro-transformer
   (lambda (expr rename compare)
     (syntax-rules-transformer expr rename compare))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
