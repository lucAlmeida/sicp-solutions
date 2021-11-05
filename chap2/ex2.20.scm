(define (same-parity a . b)
  (let ((parity (mod a 2)))
    (define (same-parity-aux lst)
      (cond ((null? lst) '())
	    ((= parity (mod (car lst) 2))
	     (cons (car lst) (same-parity-aux (cdr lst))))
	    (else (same-parity-aux (cdr lst)))))
    (same-parity-aux (cons a b))))
