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

(define type:empty   (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top     (type:make *all* *all* *all* *all* *all* *all* *all*))
(define type:boolean (type:make *all* *none* *none* *none* *none* *none* *none*))
(define type:number  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:char    (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:string  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:symbol  (type:make *none* *all* *none* *none* *none* *none* *none*))
(define type:pair    (type:make *none* *all* *none* *none* *none* *none* *none*))

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

(define (intersect typeA typeB)
  (type-map (lambda (binA binB)
              (cond ((or (type:none? binA) (type:none? binB)) *none*)
                    ((type:all? binA) binB)
                    ((type:all? binB) binA)
                    (else (intersect-finite-sets binA binB))))
            typeA typeB))

(define (union typeA typeB)
  (type-map (lambda (binA binB)
              (cond ((or (type:all? binA) (type:all? binB)) *all*)
                    ((type:none? binA) binB)
                    ((type:none? binB) binA)
                    (else (union-finite-sets binA binB))))
            typeA typeB))

(define (type-map f . types)
  (type:make
    (apply f (map type:boolean types))
    (apply f (map type:number  types))
    (apply f (map type:char    types))
    (apply f (map type:string  types))
    (apply f (map type:symbol  types))
    (apply f (map type:pair    types))
    "TODO: Merge Procedures"))

;;; Environment mapping type-variables to types

(define empty-environment '())
(define (lookup-variable environment var) (assv var environment))
(define (update-variable environment var type)
  (cons (var . type) (del-assv var environment)))
(define (substitute-into-environment environment old new)
  (map (lambda (mapping) ((car mapping) . (substitute-into-type (cadr mapping) old new)))
       environment))

;;(tscheme:make-proc-type ret-tv arg-tvs)))
(define (type:procedure ret-tv arg-tvs)
  (type:make *none* *none* *none* *none* *none* *none* (cons ret-tv arg-tvs)))
(define type:any-procedure
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

