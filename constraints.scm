;;;; Core definitions of types and functions for working with constraints.

(define-record-type constraint
    (constraint:make left relation right)
    constraint?
    (left      constraint:left)
    (relation  constraint:relation)
    (right     constraint:right))

(define-record-type type
  (type:make boolean number char string symbol pair procedure)
  type?
  (boolean    type:boolean)
  (number     type:number)
  (char       type:char)
  (string     type:string)
  (symbol     type:symbol)
  (pair       type:pair)
  (procedure  type:procedure))

(define *none* 'none)
(define *all*  'all)

(define (type:none?) (eqv? obj *none*))
(define (type:all?) (eqv? obj *all*))

(define (make-finite-set% elts) (cons 'finite-set elts))

(define (type:finite-set? ob)
  (and (list? obj)
       (not (null? obj))
       (eqv? (car obj) 'finite-set)))

(define type:empty (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top   (type:make *all* *all* *all* *all* *all* *all* *all*))
(define type:make-boolean (type:make *all* *none* *none* *none* *none* *none* *none*))
(define type:make-number  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:make-char    (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:make-string  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:make-symbol  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:make-pair    (type:make *none* *all* *none* *none* *none* *none* *none*))

;;Creates a new type with elts (not necessarily of the same type) as finite-sets in the
;;appropriate fields of the type record
(define (type:finite-set . elts)
  (define (sort-to-bins e bins)
    (cond ((boolean? e) (vector-set! bins 0 (cons e (vector-ref bins 0))))
          ((number?  e) (vector-set! bins 1 (cons e (vector-ref bins 1))))
          ((char?    e) (vector-set! bins 2 (cons e (vector-ref bins 2))))
          ((string?  e) (vector-set! bins 3 (cons e (vector-ref bins 3))))
          ((symbol?  e) (vector-set! bins 4 (cons e (vector-ref bins 4))))
          ((pair?    e) (vector-set! bins 5 (cons e (vector-ref bins 5)))))
    bins)
  (define (sort-within-bin lst)
    (if (null? lst)
        *none*
        (make-finite-set% (general-sort (delete-duplicates lst)))))
  (let ((bins (vector-map
                (fold-right sort-to-bins '#( () () () () () () ) elts)
                sort-within-bin)))
    (type:make (vector-ref bins 0)
               (vector-ref bins 1)
               (vector-ref bins 2)
               (vector-ref bins 3)
               (vector-ref bins 4)
               (vector-ref bins 5)
               *none*)))

;;Takes two finite sets of the same type
(define (intersect-finite-sets setA setB)
  (make-finite-set%
        (general-sort (filter (lambda (e) (memv e setB)) setA))))

(define (union-finite-sets setA setB)
  (make-finite-set%
        (general-sort (remove-duplicates (append (cdr setA) (cdr setB))))))

;;Dangerous and order-dependent, but compact and nice
(define type:accessors
  (list type:boolean type:number type:char type:string type:symbol type:pair type:procedure))
;;A generic map over types
(define (type:map f . types)
  (apply type:make (map (lambda (acc) (apply f (map acc types))) type:accessors)))
;;A generic map over types that passes the accessor as the first argument
(define (type:tagged-map f . types)
  (apply type:make (map (lambda (acc) (apply f (cons acc (map acc types)))) type:accessors)))

#|
(pp (type:tagged-map list type:top type:make-boolean))
;; #[type 21]
;; (boolean (boolean all all))
;; (number (number all none))
;; (char (char all none))
;; (string (string all none))
;; (symbol (symbol all none))
;; (pair (pair all none))
;; (procedure (procedure all none))

(map type:boolean (list type:top type:make-boolean))
|#

(define (intersect-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *none*) (eqv? typeB *none*)) *none*)
	((eqv? typeA *all*) typeB)
	((eqv? typeB *all*) typeA)
	((eq? type-tag type:procedure)
	 (raise "TODO intersect procedure types"))
	(else (intersect-finite-sets typeA typeB))))

(define (union-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *all*) (eqv? typeB *all*)) *all*)
	((eqv? typeA *none*) typeB)
	((eqv? typeB *none*) typeA)
	((eq? type-tag type:procedure)
	 (raise "TODO union procedure types"))
	(else (union-finite-sets typeA typeB))))

;;Computes the intersection of two types
(define (intersect typeA typeB)
  (type:tagged-map intersect-type typeA typeB))
(define (union typeA typeB)
  (type:tagged-map union-type typeA typeB))

(define empty-environment '())
(define (lookup-variable environment var) (assv var environment))
(define (update-variable environment var type)
  (cons (list var type) (del-assv var environment)))
(define (substitute-into-environment environment old new)
  (map (lambda (mapping) ((car mapping) . (substitute-into-type (cadr mapping) old new)))
       environment)

(define (substitute-into-type env old new)
  (raise "TODO substitute-into-type"))

;;(tscheme:make-proc-type ret-tv arg-tvs)))
;;A naming conflict!
;;TODO check other files for type:procedure
;;Renamed from type:procedure to type:make-procedure
(define (type:make-procedure ret-tv arg-tvs)
  (type:make *none* *none* *none* *none* *none* *none* (cons ret-tv arg-tvs)))
;;Renamed from type:procedure to type:make-any-procedure
(define type:make-any-procedure
  (type:make *none* *none* *none* *none* *none* *none* *all*))


(define *equals*   'EQUALS)
(define *requires* 'REQUIRES)
(define *permits*  'PERMITS)

(define (equality? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *equals*)))

(define (requirement? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *requires*)))

(define (permission? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *permits*)))

;;; Methods to construct instances

;; Fresh type variables
(define **type-var-counter** 0)
(define (fresh #!optional prefix)
  (set! **type-var-counter** (+ **type-var-counter** 1))
  (symbol-append (if (equal? prefix #!default)
		     'x
		     prefix)
		 **type-var-counter**))

(define (fresh-argvar) (fresh 'argvar))
(define (fresh-retvar) (fresh 'retvar))
(define (fresh-procvar) (fresh 'proc))

(define (constraint:make-equal left right)
  (constraint:make left *equals* right))

(define (constraint:make-require left right)
  (constraint:make left *requires* right))

(define (constraint:make-permit left right)
  (constraint:make left *permits* right))

