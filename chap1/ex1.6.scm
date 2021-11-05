(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
	(else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
	  guess
	  (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x)
  (* x x))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;; sqrt-iter end test will be ignored (both clauses evaluated).
;; The program will not stop when it finds a good-enough approximation.
;; This happens due to the applicative-order evaluation of the interpreter,
;; which assure that all operands are evaluated before the procedure application.
;; This way sqrt-iter will recursively call itself infinitely many times.
