;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Eight Queens with board symmetry
;;; Scheme version
;;;
;;; by T.Shido
;;; August 17, 2005
;;; last modified: July 18, 2008
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; utilities

;;(require rnrs/base-6)         ; vector-for-each
;;(require rnrs/hashtables-6)   ; hashtable
;;(require rnrs/control-6)      ; when ‚ðŽg‚¤‚½‚ß

(define (1+ x)
  (+ x 1))

(define (1- x)
  (- x 1))

(define (range n)
  (let loop((i 0) (ls1 '()))
    (if (= i n)
        (reverse ls1)
      (loop (1+ i) (cons i ls1)))))

(define (my-remove x ls)
  (let loop((ls0 ls) (ls1 '()))
    (if (null? ls0) 
        (reverse ls1)            
      (loop
        (cdr ls0)
        (if (eqv? x (car ls0))
            ls1
          (cons (car ls0) ls1))))))
          
(define (position x ls)
  (let loop((ls0 ls) (i 0))
    (cond
     ((null? ls0) #f)
     ((eqv? x (car ls0)) i)
     (else (loop (cdr ls0) (1+ i))))))

(define (print-lines . lines)
  (let loop((ls0 lines))
    (when (pair? ls0)
         (display (car ls0))
         (newline)
         (loop (cdr ls0)))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check if queens conflict each other
(define (conflict? q qs)
  (let loop((inc (1+ q)) (dec (1- q)) (ls0 qs))
    (if (null? ls0)
        #f
      (let ((c (car ls0)))
        (or
         (= c inc)
         (= c dec)
         (loop (1+ inc) (1- dec) (cdr ls0)))))))

;;; convert int <-> list
(define (q2int n qs)
  (let loop((ls0 qs) (i 0))
    (if (null? ls0)
        i
      (loop (cdr ls0) (+ (* i n) (car ls0))))))

(define (int2q n i)
  (let loop((j i) (ls1 '()))
    (if (= j 0)
        ls1
      (loop (quotient j n) (cons (modulo j n) ls1)))))

;;; symmetry operations
;; turn 90 degree
(define (t90 qs)
  (let ((n (length qs)))
    (let loop ((ls1 '()) (i 0))
      (if (= i n)
          ls1
        (loop (cons (position i qs) ls1) (1+ i))))))

;; turn 180 degree
(define (t180 qs)
  (usd (reverse qs)))

;; turn 270 degree
(define (t270 qs)
  (t90 (t180 qs)))

;; up side down
(define (usd qs)
  (let ((n (1- (length qs))))
    (map (lambda (x) (- n x)) qs)))

;; reflection on diagonal 1
(define (d1 qs)
  (reverse (t90 qs)))

;; reflection on diagonal 2
(define (d2 qs)
  (usd (t90 qs)))


;;; plotting using gnuplot
;; drawing grid
(define (draw-grid n)
  (let ((p (number->string (1+ n))))
    (let loop((i 0))
      (when (<= i n)
          (let ((s (number->string (1+ i))))
            (print-lines
             (string-append "set arrow from " s ", 1 to " s ", " p " nohead lt 5")
             (string-append "set arrow from 1, " s " to " p ", " s " nohead lt 5"))
            (loop (1+ i)))))))
    

;; plotting data file of the solution
(define (plot-queen len)
  (let loop((i 0))
    (when (< i len)
         (print-lines
          (string-append "set title \"solution: " (number->string (1+ i)) "\"")
          (string-append "plot \"q" (number->string i) ".dat\" title \"queen\" with point pointsize 3")
          "pause -1 \"Hit return to continue\"")
         (loop (1+ i)))))

;; writing data files
(define (q-write-dat qls)
  (let loop((i 0) (ls qls))
    (when (pair? ls)
         (with-output-to-file (string-append "q" (number->string i) ".dat")
            (lambda ()
              (let rec((j 0) (ls1 (car ls)))
                (when (pair? ls1)
                     (print-lines (string-append (number->string (+ j 1.5)) " " (number->string (+ (car ls1) 1.5))))
                     (rec (1+ j) (cdr ls1))))))
         (loop (1+ i) (cdr ls)))))
                   
;; making a command file for gnuplot                
(define (qplot n qls)
  (q-write-dat qls)
  (with-output-to-file "queen.plt"
    (lambda ()
      (let ((s (number->string (+ n 2))))
        (print-lines
         "reset"
         "set size square"
         (string-append "set xrange [0:" s "]")
         (string-append "set yrange [0:" s "]"))
        (draw-grid n)
        (plot-queen (length qls))))))
      

;;; the main function
(define (queen n)
  (let ((qsol (make-eqv-hashtable))
        (qlist '()))
    (letrec ((q-sethash (lambda (qs)                 ;; registrate on the hash table
                          (let ((qi (q2int n qs)))
                            (when (eq? (hashtable-ref qsol qi 'not-yet) 'not-yet)
                                 (for-each
                                  (lambda (op)
                                    (hashtable-set! qsol (q2int n (op qs)) #f))
                                  (list t90 t180 t270 reverse usd d1 d2))
                                 (hashtable-set! qsol qi #t)))))
                          
              
             (q-add (lambda (qs i pool)             ;; adding new queen
                      (if (= i n)
                          (q-sethash qs)
                        (for-each (lambda (x)
                                    (or (conflict? x qs)
                                        (q-add (cons x qs) (1+ i) (my-remove x pool))))
                                  pool)))))
                      
            (q-add '() 0 (range n)))
    (let-values (((key value) (hashtable-entries qsol)))  ; pick up distinct solutions
                (vector-for-each
                 (lambda (k v)
                   (when v (set! qlist (cons (int2q n k) qlist))))
                 key value))
	(plot n qlist)
    ;; (qplot n qlist)   ;; plotting the distinct solutions
    (length qlist)))




;; make my own memv as memv in MzScheme require a 'proper list'
(define (my-memv obj ls)
  (cond
   ((null? ls) #f)
   ((= obj (car ls)) #t)
   (else (my-memv obj (cdr ls)))))

;;; the main function to find symmetrical solutions
(define (queen_sym n)
  (let ((qsol (make-eqv-hashtable))
        (qlist '()))
    (letrec ((q-sethash (lambda (qs)                 ;; registrate on the hash table
                          (let ((qi (q2int n qs)))
                            (when (not (hashtable-contains? qsol qi))
                              (let ((ls_sym (map (lambda (op) (q2int n (op qs))) `(,t90 ,t180 ,t270 ,reverse ,usd ,d1 ,d2))))
                                (when (my-memv qi ls_sym)
                                      (for-each
                                       (lambda (v)
                                         (hashtable-set! qsol v #f))
                                       ls_sym)
                                  (hashtable-set! qsol qi #t)))))))

                          
             (q-add (lambda (qs i pool)             ;; adding new queen
                      (if (= i n)
                          (q-sethash qs)
                        (for-each (lambda (x)
                                    (or (conflict? x qs)
                                        (q-add (cons x qs) (1+ i) (my-remove x pool))))
                                  pool)))))
                      
            (q-add '() 0 (range n)))
    (let-values (((key value) (hashtable-entries qsol)))  ; pick up distinct solutions
                (vector-for-each
                 (lambda (k v)
                   (when v (set! qlist (cons (int2q n k) qlist))))
                 key value))
    (qplot qlist)   ;; plotting the distinct solutions
    (length qlist)))


;; for my lisp

(require srfi-1)
(require minitest)

(define (make-eqv-hashtable)
  (list-copy '(())))

(define (hashtable-ref hash key def)
  (let ((val (assoc key (cdr hash))))
	(if val (cdr val) def)))

(define (hashtable-set! hash key val)
  (set-cdr! hash (alist-cons key val (alist-delete! key (cdr hash)))))

(define (hashtable-contains? hash key)
  (assoc key (cdr hash)))

(define (hashtable-keys hash)
  (let recur ((hash (cdr hash)))
	(if (null? hash) '()
		(cons (caar hash) (recur (cdr hash))))))

(define (hashtable-vals hash)
  (let recur ((hash (cdr hash)))
	(if (null? hash) '()
		(cons (cdar hash) (recur (cdr hash))))))

(define (hashtable-entries hash)
  (values (hashtable-keys hash) (hashtable-vals hash)))

(define (range n)
  (let loop ((lis '())
			 (n n))
	(if (zero? n) lis
		(loop (cons (- n 1) lis) (- n 1)))))

(define (vector-for-each f . lis)
  (let loop ((lis lis))
	(when (pair? (car lis))
		  (apply f (map car lis))
		  (loop (map cdr lis)))))

(define-syntax for
  (syntax-rules ()
	  ((_ x lis body ...) (for-each (lambda (x) body ...) lis))))

(define (plot n qs)
  (for q qs
	   (puts (make-string 80 #\=))
	   (for a q
			(puts (string-append
				   (make-string a #\.)
				   "Q"
				   (make-string (- n a 1) #\.)))))
  (puts (make-string 80 #\=)))


(assert 1 (queen 4))
(assert 2 (queen 5))
(assert 1 (queen 6))
(assert 6 (queen 7))
(assert 12 (queen 8))

(minitest-finish)
