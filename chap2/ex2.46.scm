(define (vect-make x y) (list x y))

(define (xcor-vect v) (car v))
(define (ycor-vect v) (cadr v))

(define (add-vect v1 v2)
  (vect-make (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (vect-make (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (vect-make (* s (xcor-vect v)) (* s (ycor-vect v))))
