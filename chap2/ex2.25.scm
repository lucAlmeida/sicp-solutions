(define x '(1 3 (5 7) 9))
(define y '((7)))
(define z '(1 (2 (3 (4 (5 (6 7)))))))

(define a (car (cdr (car (cdr (cdr x))))))
(define b (car (car y)))
(define c (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr z)))))))))))))

(define (run-tests)
  (and (eq? a 7)
	   (eq? b 7)
	   (eq? c 7)))
