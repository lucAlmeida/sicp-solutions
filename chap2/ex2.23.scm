(define (for-each proc items)
  (cond ((null? items) (begin (newline) #t))
		(else (begin (proc (car items))
					 (for-each proc (cdr items))))))
