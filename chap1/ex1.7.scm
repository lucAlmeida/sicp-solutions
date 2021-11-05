(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? previous-guess current-guess)
  (<= (abs (- current-guess previous-guess))
      (/ current-guess 99999999999.9)))
 
(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))
