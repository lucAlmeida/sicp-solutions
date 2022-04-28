(define painter-outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 0 1))
         (make-segment (make-vect 0 0) (make-vect 1 0))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 1 1)))))

(define painter-x
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define painter-diamond
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))
