(define (subsets s)
  (if (null? s)
	  (list '())
	  (let ((rest (subsets (cdr s))))
		(append rest
				(map (lambda (x)
					   (append (list (car s)) x)) rest)))))

;; The procedure works because it generates all possible subsets for set s,
;; by percolating the elements of the given set, and after reaching end of original set,
;; appending a new element, from right to left, to all previously computed subsets.
;; At each procedure call, it appends the previously computed values (subsets for the rest of s),
;; to each of the computed values preppended with the current element (car s)

;; Example:
;; (())
;; curr elt: 3, rest: () => (() (3))
;; curr elt: 2, rest: (() (3)) => (() (3) (2) (2 3))
;; curr elt: 1, rest: (() (3) (2) (2 3)) => (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))
;; res: (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

