(define (square x) (* x x))

(define (prime? n)
  (define (smallest-divisor n) (find-divisor n 2))
  (define (find-divisor n test-divisor)
	(cond ((> (square test-divisor) n) n)
		  ((divides? test-divisor n) test-divisor)
		  (else (find-divisor n (+ test-divisor 1)))))
  (define (divides? a b) (= (remainder b a) 0))
  (= n (smallest-divisor n)))

(define (enumerate-interval low high)
  (if (> low high)
	  '()
	  (cons low (enumerate-interval (+ low 1) high))))

(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (my-map proc items)
  (if (null? items)
	  '()
	  (cons (proc (car items))
			(my-map proc (cdr items)))))

(define (my-filter predicate sequence)
  (cond ((null? sequence) '())
		((predicate (car sequence))
		 (cons (car sequence)
			   (my-filter predicate (cdr sequence))))
		(else (my-filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
			 (map (lambda (j) (list i j))
				  (enumerate-interval 1 (- i 1))))
		   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (my-map make-pair-sum (my-filter prime-sum? (unique-pairs n))))
