#;
(define (scale-list items factor)
  (if (null? items)
	  '()
	  (cons (* (car items) factor)
			(scale-list (cdr items)
						factor))))

(define (map proc items)
  (if (null? items)
	  '()
	  (cons (proc (car items))
			(map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

