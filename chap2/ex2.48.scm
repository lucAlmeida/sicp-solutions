(define (vect-make x y) (list x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (segment-make from to)
  (list from to))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

