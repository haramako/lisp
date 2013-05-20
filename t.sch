(display
 (let loop ((sum 0)
		   (var 10))
  (if (<= var 0) sum
	  (loop (+ sum var) (- var 1))))
 )
