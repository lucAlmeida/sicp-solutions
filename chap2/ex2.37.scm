(define (accumulate op initial sequence)
  (if (null? sequence)
	  initial
	  (op (car sequence)
		  (accumulate op initial (cdr sequence)))))

(define (my-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
	  '()
	  (cons (accumulate op init (my-map car seqs))
			(accumulate-n op init (my-map cdr seqs)))))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (my-map (lambda (x) (dot-product x v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
	(my-map (lambda (x)
			  (matrix-*-vector cols x)) m)))

