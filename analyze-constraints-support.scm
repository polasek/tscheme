;;;; Supporting code for constraint analyzer

;;; Substitution maps

(define-record-type substitution
    (substitution:make old new ids)
    substitution?
    (old sub:old)
    (new sub:new)
    (ids sub:ids))

;;Compose two substitution maps, so that for example running
;; (multi-substitute-into-environment
;;   (multi-substitute-into-environment environment subsA) subsB)
;; is equivalent to
;; (multi-substitute-into-environment environment
;;   (compose-substitutions subsA subsB))
;; Where
;; (multi-substitute-into-environment environment (cons '(old new) rest))
;; is equivalent to
;; (multi-substitute-into-environment
;;   (substitute-into-environment environment old new) rest)
;; Assumes all substitutions were properly composed before.

(define (compose-substitutions subsA subsB)
  (if (null? subsB) subsA
    (append
      (map (lambda (a)
             (fold-left (lambda (a b)
                          (if (eqv? (sub:old b) (sub:new a))
                              ;;If there is something substituting for our result
                              ;;later, we might as well do it straight away.
                              (substitution:make
                                (sub:old a) (sub:new b)
                                (union-finite-sets (sub:ids b) (sub:ids a)))
                              a))
                        a
                        subsB))
           subsA)
     ;;Remove all elements from b that have been already substituted for by a.
      (list-transform-negative
        subsB
        (lambda (b)
          (there-exists? subsA (lambda (a) (eqv? (sub:old a) (sub:old b)))))))))

(define (add-substitution subs sub)
  (if (substitution? sub)
      (compose-substitutions subs (list sub))
      subs))

#|
(for-each
  print-substitution
  (compose-substitutions (list (substitution:make 'a 'b (finite-set 1))
                               (substitution:make 'c 'd (finite-set 2))
                               (substitution:make 'e 'f (finite-set 3)))
                         (list (substitution:make 'g 'h (finite-set 4))
                               (substitution:make 'i 'j (finite-set 5))
                               (substitution:make 'k 'l (finite-set 6)))))
;a -> b (1)
;c -> d (2)
;e -> f (3)
;g -> h (4)
;i -> j (5)
;k -> l (6)

(for-each
  print-substitution
  (compose-substitutions (list (substitution:make 'a 'b (finite-set 1))
                               (substitution:make 'c 'd (finite-set 2))
                               (substitution:make 'e 'f (finite-set 3)))
                         (list (substitution:make 'b 'h (finite-set 4))
                               (substitution:make 'c 'j (finite-set 5))
                               (substitution:make 'f 'l (finite-set 6)))))
;a -> h (1 4)
;c -> d (2)
;e -> l (3 6)
;b -> h (4)
;f -> l (6)

(for-each
  print-substitution
  (compose-substitutions (list (substitution:make 'a 'b (finite-set 1))
                               (substitution:make 'c 'd (finite-set 2))
                               (substitution:make 'e 'f (finite-set 3)))
                         (list (substitution:make 'a 'h (finite-set 4))
                               (substitution:make 'c 'j (finite-set 5))
                               (substitution:make 'e 'l (finite-set 6)))))
;a -> b (1)
;c -> d (2)
;e -> f (3)
|#

