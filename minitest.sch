;;; minitest
(define *minitest-failed* 0)
(define *minitest-count* 0)

(define-macro (inc! x)
  `(set! ,x (+ 1 ,x)))

(define-macro (assert _expect _test . maybe-elt= )
  (let ((elt= (if (pair? maybe-elt=) (car maybe-elt=) equal?)))
	(inc! *minitest-count*)
	(display ".")
	(if (zero? (modulo *minitest-count* 80)) (newline))
	`(let ((*test* ,_test)
		   (*expect* ,_expect))
	   (if (,elt= *test* *expect*) #t
		   (newline)
		   (puts "FAILED:" ',_test "EXPECT" *expect* "BUT" *test*)
		   (inc! *minitest-failed*)))))

(define (minitest-finish)
  (newline)
  (puts "finished" *minitest-count* "tests" *minitest-failed* "failed")
  (exit (if (zero? *minitest-failed*) 0 1)))


