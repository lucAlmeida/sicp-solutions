#lang racket

(require threading)

(define (sum-square-two-largest lst)
  (~> lst
      (sort >)
      (take 2)
      (map (λ (x) (* x x)) _)
      (foldl + 0 _)))