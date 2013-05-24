;;; minitest
(define *minitest-failed* 0)
(define *minitest-count* 0)

(define-syntax inc!
  (syntax-rules ()
	((_ x) (set! x (+ 1 x)))))

(define (minitest-dot)
  (inc! *minitest-count*)
  (display ".")
  (if (zero? (modulo *minitest-count* 80)) (newline)))
  
(define-syntax assert
  (syntax-rules ()
	((_ _expect _test)
	 (assert _expect _test equal?))
	((_ _expect _test elt=)
	 (begin
	   (minitest-dot)
	   (let ((*test* _test)
			  (*expect* _expect))
		 (if (elt= *test* *expect*) #t
			 (newline)
			 (puts "FAILED:" '_test "EXPECT" *expect* "BUT" *test*)
			 (inc! *minitest-failed*)))))
	))

(define-syntax assert-exception
  (syntax-rules ()
	((_ _exeption _test)
	 (try (lambda (x) )
		  (minitest-dot)
		  _test
		  (newline)
		  (puts "FAILED:" '_test "EXPECT RAISE" '_exception "BUT NOT" )
		  (inc! *minitest-failed*)))
	))

(define (minitest-finish)
  (newline)
  (puts "finished" *minitest-count* "tests" *minitest-failed* "failed")
  (exit (if (zero? *minitest-failed*) 0 1)))


