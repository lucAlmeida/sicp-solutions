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

(define (make-rectangle x y width height)
  (make-segment (make-point x y)
		(make-point (+ x width) (+ y height))))

(define (top rectangle)
  (make-segment (make-point (x-point (start-segment rectangle))
			    (y-point (start-segment rectangle)))
		(make-point (x-point (end-segment rectangle))
			    (y-point (start-segment rectangle)))))

(define (bottom rectangle)
  (make-segment (make-point (x-point (start-segment rectangle))
			    (y-point (end-segment rectangle)))
		(make-point (x-point (end-segment rectangle))
			    (y-point (end-segment rectangle)))))

(define (left rectangle)
  (make-segment (make-point (x-point (start-segment rectangle))
			    (y-point (start-segment rectangle)))
		(make-point (x-point (start-segment rectangle))
			    (y-point (end-segment rectangle)))))

(define (right rectangle)
  (make-segment (make-point (x-point (end-segment rectangle))
			    (y-point (start-segment rectangle)))
		(make-point (x-point (end-segment rectangle))
			    (y-point (end-segment rectangle)))))

(define (width segment)
  (abs (- (x-point (start-segment segment))
	  (x-point (end-segment segment)))))

(define (height segment)
  (abs (- (y-point (start-segment segment))
	  (y-point (end-segment segment)))))

(define (perimeter rectangle)
  (+ (* 2 (width (top rectangle)))
     (* 2 (height (left rectangle)))))

(define (area rectangle)
  (* (width (top rectangle))
     (height (left rectangle))))

(define rect1 (make-rectangle 5 10 30 20))
