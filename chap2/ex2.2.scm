(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (midpoint-segment segment)
  (let ((start-seg (start-segment segment))
	(end-seg (end-segment segment)))
    (make-segment (average (x-point start-seg)
			   (x-point end-seg))
		  (average (y-point start-seg)
			   (y-point end-seg)))))

(define (average a b)
  (/ (+ a b) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))
