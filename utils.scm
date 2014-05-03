(declare (usual-integrations))

;; TODO: Make generic
(define (general-sort lst)
  (define (boolean<? a b) (not (and a b)))
  (cond
    ((null? lst) lst)
    ((boolean? (car lst)) (sort lst boolean<?))
    ((number?  (car lst)) (sort lst <))
    ((char?    (car lst)) (sort lst char<?))
    ((string?  (car lst)) (sort lst string<?))
    ((symbol?  (car lst)) (sort lst symbol<?))
    (else lst)))

(define (identity x) x)

(define (any? x) #t)


(define ((compose f g) x) (f (g x)))

;; Deduplicating a sorted list can be done efficiently
(define (dedup lst)
  (letrec ((lp (lambda (prev rest)
		 (cond ((null? rest) (list prev))
		       ((eqv? prev (car rest)) (lp prev (cdr rest))) ;skip
		       (else (cons prev (lp (car rest) (cdr rest))))))))
    (if (null? lst)
        lst
	(lp (car lst) (cdr lst)))))
#|

#|

;;; This is to keep the Scheme printer from going into an infinite
;;; loop if you try to print a circular data structure, such as an
;;; environment

(set! *unparser-list-depth-limit* 10)
(set! *unparser-list-breadth-limit* 10)
