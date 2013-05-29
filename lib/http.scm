(define (http-params)
  (let* ((query (or (sys-getenv "QUERY_STRING") ""))
		 (kvs (string-split query #\&)))
	(map (lambda (kvstr)
		   (let ((kv (string-split kvstr #\=)))
			 (if (pair? (cdr kv))
				 (cons (car kv) (cadr kv))
				 (cons (car kv) ""))))
		 kvs)))

(define (http-redirect url)
  (display (format "Location: ~a\n\n" url))
  (exit 0))

(define (html-render tmpl alist)
  (with-output-string
   (lambda (s)
	 (define (indent level)
	   (make-string (* 2 level) #\space))
	 
	 (define (output-tag tmpl level alist)
	   (if (not-pair? tmpl)
		   (display (format "~a~a\n" (indent level) tmpl) s)
		   (cond
			;; = タグ
			((eq? '= (car tmpl))
			 (display (indent level) s)
			 (if (number? (cadr tmpl))
				 (display (nth (cadr tmpl) alist) s)
				 (display (cdr (assoc (cadr tmpl) alist)) s))
			 (display "\n" s))
			;; %each タグ
			((eq? '%each (car tmpl))
			 (let ((lis (cdr (assoc (cadr tmpl) alist)))
				   (sub-tmpl (caddr tmpl)))
			   (dolist (e lis)
					   (output-tag sub-tmpl (1+ level) e))))
			(else
			 ;; 通常のpair
			 (let* ((name (car tmpl))
					(has-attr? (and (pair? (cdr tmpl)) (pair? (cadr tmpl)) (pair? (caadr tmpl))))
					(attr (if has-attr? (cadr tmpl) '()))
					(rest (if has-attr? (cddr tmpl) (cdr tmpl))))
			   (display (format "~a<~a~a>\n" (indent level) (car tmpl) (attr->string attr)) s)
			   (dolist (x rest) (output-tag x (1+ level) alist))
			   (display (format "~a</~a>\n" (indent level) (car tmpl)) s))))))

	 (define (attr->string attr)
	   (with-output-string
		(lambda (s)
		  (dolist (kv attr)
			   (display (format " ~a=\"~a\"" (car kv) (cadr kv)) s)))))

	 (output-tag tmpl 0 alist))))

