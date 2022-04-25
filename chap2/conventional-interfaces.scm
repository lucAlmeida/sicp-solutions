(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

#|
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
		((not (pair? tree))
		 (if (odd? tree) (square tree) 0))
		(else (+ (sum-odd-squares (car tree))
				 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
	(if (> k n)
		'()
		(let ((f (fib k)))
		  (if (even? f)
			  (cons f (next (+ k 1)))
			  (next (+ k 1))))))
  (next 0))
|#

(define (my-filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (my-filter predicate (cdr sequence))))
		(else (my-filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
		((not (pair? tree)) (list tree))
		(else (append (enumerate-tree (car tree))
					  (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate + 0 (map square (my-filter odd? (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons '() (my-filter even? (map fib (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons '() (map square (map fib (enumerate-interval 0 n)))))

(define (product-of-squares-of-odd-elements sequence)
  (accumulate * 1 (map square (filter odd? sequence))))

