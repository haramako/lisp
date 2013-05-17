;;; unit-test
(define assert
  (macro (_eq _test _expect)
		 `(let ((test ,_test)
				(expect ,_expect))
			(if (,_eq test expect) #t (display 'test-failed: ',_test 'expect expect 'but test)))))
