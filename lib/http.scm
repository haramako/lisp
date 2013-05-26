(define (http-params)
  (let* ((query (or (sys-getenv "QUERY_STRING") ""))
		 (kvs (string-split query #\&)))
	(map (lambda (kvstr)
		   (let ((kv (string-split kvstr #\=)))
			 (cons (car kv) (cadr kv))))
		 kvs)))

(define (html-render tmpl alist)
  (with-output-string
   (lambda (s)
	 (define (indent level)
	   (make-string (* 2 level) #\space))
	 
	 (define (output-tag tmpl level alist)
	   (cond ((string? tmpl)
			  (format-display s "~a~a\n" (indent level) tmpl))
			 ;; = タグ
			 ((and (pair? tmpl) (eq? '= (car tmpl)))
			  (if (number? (cadr tmpl))
				  (display (nth (cadr tmpl) alist) s)
				  (display (cdr (assoc (cadr tmpl) alist)) s)))
			 ;; %for タグ
			 ((and (pair? tmpl) (eq? '%for (car tmpl)))
			  (let ((lis (cdr (assoc (cadr tmpl) alist)))
					(sub-tmpl (caddr tmpl)))
				(for (e lis)
					 (output-tag sub-tmpl (1+ level) e))))
			 ((pair? tmpl)
			  (let* ((name (car tmpl))
					 (has-attr? (and (pair? (cdr tmpl)) (pair? (cadr tmpl)) (pair? (caadr tmpl))))
					 (attr (if has-attr? (cadr tmpl) '()))
					 (rest (if has-attr? (cddr tmpl) (cdr tmpl))))
				(format-display s "~a<~a~a>\n" (indent level) (car tmpl) (attr->string attr))
				(for (x rest) (output-tag x (1+ level) alist))
				(format-display s "~a</~a>\n" (indent level) (car tmpl))))))

	 (define (attr->string attr)
	   (with-output-string
		(lambda (s)
		  (for (kv attr)
			   (format-display s " ~a=\"~a\"" (car kv) (cadr kv))))))

	 (output-tag tmpl 0 alist))))

