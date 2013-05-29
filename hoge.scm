#!/usr/bin/env mlisp

(require text.tree)

(write-tree '(1 ("2" 4) 3))

(exit)

(define is (open-input-string "(1 2) #t"))
(puts is)
#p(read is)
#p(read is)
(puts 'port (open-input-file "hoge.scm"))

(define str (substring "hogefuga" 1 3))
(puts 'str-length (string-length str) str)
(write (substring "hogefuga" 1 3))
(write "\n\n")

(define os (open-output-string))
(write "hoge" os)
(write '(1 2) os)
(display "hoge-fuga" os)
(puts (get-output-string os))

(try (lambda x (puts "catch!" 4))
	 (puts 1)
	 (puts 2)
	 ;;(error 1)
	 (puts 3)
	 )

(write "\n \t \" \r \x41")
(puts (string->list "hoge"))
(puts (list->string (string->list "hoge")))

(write "hoge\
       fuga  \
       piyo  \
  ")

(puts (char=? #\a #\a))
(puts (char=? #\a #\b))
(puts (char->integer #\a))

(let ((str (make-string 8 #\_)))
  (puts str)
  (string-set! str 3 #\A)
  (puts (string-ref str 3))
  )

(puts (char-upcase #\a) (char-upcase #\A) (char-downcase #\a) (char-downcase #\A))


(require srfi-1)
(require srfi-1)
(require srfi-1)
(require srfi-13)

(mac (when 1 1))

(when 1 (puts 1))

(define-syntax hoge
  (syntax-rules ()
	((_ a b ...) (list a b ...))
	))

(puts (hoge 'fuga 'piyo 'hage))
(puts (hoge 'fuga '(piyo hage) 'nyao))

(define-syntax nil!
  (syntax-rules ()
	((_ x)
	 (set! x '()))
	))

(puts (syntax-expand1 '(nil! hoge)))

(begin
  (define hoge 1)
  (nil! hoge)
  (puts 'hoge hoge))

(let ((x 1))
  (puts 'dynamic-wind)
  (call/cc (lambda (break)
			 (dynamic-wind
				 (lambda () (puts 1))
				 (lambda () (break 0) (puts 2))
				 (lambda () (puts 3))
			   )
			 ))
  (puts 4)
  )

(define (fib n)
  (if (<= n 1)
	  1
	  (+ (fib (- n 1)) (fib (- n 2)))))

(puts "fib(10)" (fib 10))

(let ((x 1))
  (define ce (current-environment))
  (puts ce)
  (let ((x 2))
	(puts (eval 'x ce)))
  )

; (puts (fib 29))

'(begin
  (define break #f)
  (puts (call/cc (lambda (cont) (set! break cont))))

  (puts 'a)
  (puts 'c (call/cc (lambda (cont) (break 10))))
  (puts 'b))


(define (fuga x)
  (hoge x)
  (puts 1))

(define (hoge x)
  ;; (puts (backtrace))
  (puts 1))

(fuga 1)

(puts 1)


(let ((a 2))
  ;;(puts 'append)
  (puts (append '(1 2) '(3 4) '() '(5 6)))
  (puts (append '()))
  (puts (append '() '(1 2)))
  (define x '(100 200))
  (puts (append x '(3)))
  (puts (append))
  )


;; (define (rec n)
;;   (rec (+ 1 n)))

;; (rec 1)
	 
;; ;(display current-input-port)
;; (define (eval-loop)
;;   (display "> ")
;;   (let ((v (eval (read))))
;; 	(puts "=>" v)
;; 	(eval-loop)))

;; ;(eval-loop)

;; (puts *argv*)
