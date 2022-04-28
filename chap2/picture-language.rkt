#lang sicp
(#%require sicp-pict)

(define (split after before)
  (define (aux painter n)
    (if (= n 0)
        painter
        (let ((smaller (aux painter (- n 1))))
          (after painter (before smaller smaller)))))
  (lambda (painter n)
    (aux painter n)))

#;
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define right-split (split beside below))

#;
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define up-split (split below beside))

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

#;
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

#;
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

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

(define (frame-make origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (caddr f))

#|
(define (frame-make origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f) (car f))
(define (edge1-frame f) (cadr f))
(define (edge2-frame f) (cddr f))
|#

(define (my-frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(define (segment-make from to)
  (list from to))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cadr s))

(define (my-segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (segment-make
        ((frame-coord-map frame)
         (start-segment segment))
        ((frame-coord-map frame)
         (end-segment segment))))
     segment-list)))

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;; Usage examples:

;;(define einstein2 (beside einstein (flip-vert einstein)))
;;(define einstein4 (below einstein2 einstein2))
;;(define einstein4 (flipped-pairs einstein))

;;(paint einstein2)
;;(paint einstein4)

(define a-frame (make-frame (make-vect 0 0) (make-vect 3 2) (make-vect 4 5)))
(define a-coord-map (frame-coord-map a-frame))
(define segments-to-paint (segments->painter (list (make-segment (make-vect 0 0) (make-vect 4 4)))))
#|
(define a-frame (make-frame (make-vect 0 0) (make-vect 3 2) (make-vect 4 5)))
(define a-coord-map (frame-coord-map a-frame))
(define segments-to-paint (segments->painter (list (make-segment (make-vect 0 0) (make-vect 4 4)))))

(define my-frame (frame-make (vect-make 0 0) (vect-make 3 2) (vect-make 4 5)))
(define my-coord-map (my-frame-coord-map my-frame))
|#

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
