(define (split after before)
  (define (aux painter n)
    (if (= n 0)
        painter
        (let ((smaller (aux painter (- n 1))))
          (after painter (before smaller smaller)))))
  (lambda (painter n)
    (aux painter n)))

(define right-split (split beside below))
(define up-split (split below beside))
