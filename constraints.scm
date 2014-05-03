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
  (boolean type:boolean)
  (number  type:number)
  (char    type:char)
  (string   type:string)
  (symbol     type:symbol)
  (pair       type:pair)
  (procedure  type:procedure))

(define *none* 'none)
(define *all*  'all)

(define type:empty (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top   (type:make *all* *all* *all* *all* *all* *all* *all*))
;;TODO renamed to type:make-* as it was conflicting with getters
(define type:make-boolean (type:make *all* *none* *none* *none* *none* *none* *none*))

;;Creates a new type with elts (not necessarily of the same type) as finite-sets in the
;;appropriate fields of the type record
(define (type:finite-set . elts) (raise "TODO type:finite-set"))

;;Takes two finite sets of the same type
(define (intersect-finite-sets setA setB) (raise "TODO intersect-finite-sets"))
(define (union-finite-sets setA setB) (raise "TODO union-finite-sets"))

;;Applies function f over typeA typeB, passing the type tag as a first argument,
;;the corresponding element of typeA as second, and the corresponding element
;;of typeB as the third argument to f
;;I didn't find an easy way to convert to lists and back, therefore the map
;;is done manually

;;Dangerous and order-dependent
(define type:accessors (list type:number type:char type:string type:symbol type:pair type:procedure))
;;A generic map over types
(define (type:map f . types)
  (apply type:make (map (lambda (acc) (apply f (map acc types))) type:accessors)))
;;A generic map over types that passes the accessor as the first argument
(define (type:tagged-map f . types)
  (apply type:make (map (lambda (acc) (apply f (cons acc (map acc types)))) type:accessors)))

(define (type-map2 f typeA typeB)
  (type:tagged-map f `(,typeA ,typeB)))

  (type:make (apply f (cons *boolean* (map type:boolean types)))


  typeA) (type:boolean   typeB))
	     (f *number*    (type:number    typeA) (type:number    typeB))
	     (f *char*      (type:char      typeA) (type:char      typeB))
	     (f *string*    (type:string    typeA) (type:string    typeB))
	     (f *symbol*    (type:symbol    typeA) (type:symbol    typeB))
	     (f *pair*      (type:pair      typeA) (type:pair      typeB))
	     (f *procedure* (type:procedure typeA) (type:procedure typeB))))


(define (type-map2 f typeA typeB)
  (type:make (f *boolean*   (type:boolean   typeA) (type:boolean   typeB))
	     (f *number*    (type:number    typeA) (type:number    typeB))
	     (f *char*      (type:char      typeA) (type:char      typeB))
	     (f *string*    (type:string    typeA) (type:string    typeB))
	     (f *symbol*    (type:symbol    typeA) (type:symbol    typeB))
	     (f *pair*      (type:pair      typeA) (type:pair      typeB))
	     (f *procedure* (type:procedure typeA) (type:procedure typeB))))
#|
(pp (type-map2 list type:top type:make-boolean))
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
	((eq? type-tag *procedure*)
	 (raise "TODO intersect procedure types"))
	(else (intersect-finite-sets typeA typeB))))

(define (union-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *all*) (eqv? typeB *all*)) *all*)
	((eqv? typeA *none*) typeB)
	((eqv? typeB *none*) typeA)
	((eq? type-tag *procedure*)
	 (raise "TODO union procedure types"))
	(else (union-finite-sets typeA typeB))))

;;Computes the intersection of two types
(define (intersect typeA typeB)
  (type-map2 intersect-type typeA typeB))
(define (union typeA typeB)
  (type-map2 union-type typeA typeB))

(define empty-environment '())
(define (lookup-variable environment var) (assv var environment))
(define (update-variable environment var type)
  (cons (list var type) (del-assv var environment)))
(define (substitute-into-environment environment old new)
  (map (lambda (mapping) ((car mapping) . (substitute-into-type (cadr mapping) old new)))
       environment)

(define (substitute-into-type env old new)
  (type-map2 (lambda (tag 

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

