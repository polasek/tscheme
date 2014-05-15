;;;; Data structures, abstractions, and support code for working with types.

;;; Type variables

(define **type-var-counter** 0)
(define (fresh #!optional prefix)
  (set! **type-var-counter** (+ **type-var-counter** 1))
  (symbol-append (if (equal? prefix #!default) 'x prefix)
                 **type-var-counter**))

(define (fresh-argvar) (fresh 'argvar))
(define (fresh-retvar) (fresh 'retvar))
(define (fresh-procvar) (fresh 'proc))
(define (fresh-branchvar) (fresh 'branch))

;;; Abstractions for dealing with ret and arg references
(define (return-type tv)
  (list 'ret tv))

(define (arg-of tv num)
  (list 'arg tv num))

(define (tv-ret? v) (and (pair? v) (eqv? (car v) 'ret)))
(define (tv-arg? v) (and (pair? v) (eqv? (car v) 'arg)))
(define (tv-proc-name v) (cadr v))
(define (tv-arg-num v) (caddr v))

(define (proc:get-ret proc) (car proc))
(define (proc:get-arg proc n) (list-ref proc (+ n 1)))


;;; Finite sets

(define (make-finite-set% elts)
  (if (null? elts)
      *none*
      (cons 'finite-set elts)))

;; in other words,
(define (finite-set . elts)
  (make-finite-set% elts))

(define (type:finite-set? obj)
  (and (list? obj)
       (not (null? obj))
       (eqv? (car obj) 'finite-set)))

(define (finite-set-elts fs)
  (if (eq? fs *none*)
    '()
    (cdr fs)))

;; Set operations on finite sets
(define (intersect-two-finite-sets setA setB)
  (make-finite-set%
    (general-sort (filter (lambda (e) (memv e (finite-set-elts setB)))
                          (finite-set-elts setA)))))

(define (union-two-finite-sets setA setB)
  (make-finite-set%
    (dedup (general-sort (append (finite-set-elts setA)
                                 (finite-set-elts setB))))))

(define (intersect-finite-sets . sets)
  (reduce-left intersect-two-finite-sets #f (filter type:finite-set? sets)))

(define (union-finite-sets . sets)
  (reduce-left union-two-finite-sets #f (filter type:finite-set? sets)))


;;; Types

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

;;A generic map over any record type
;;Dangerous and order-dependent, but compact
(define (record-map ordered-accessors)
  (lambda (f . types)
    (apply type:make
           (map (lambda (acc)
                  (apply f (map acc types))) ordered-accessors))))

(define (record-tagged-map ordered-accessors)
  (lambda (f . types)
    (apply type:make
           (map (lambda (acc)
                  (apply f (cons acc (map acc types)))) ordered-accessors))))

(define type:accessors
  (list type:boolean type:number type:char type:string
        type:symbol  type:pair   type:procedure))

(define type:accessors-proc
  (list type:boolean type:number type:char type:string
        type:symbol  type:pair))

(define (type:empty? type)
  (for-all?
    (map (lambda (acc) (acc type)) type:accessors)
    (lambda (t) (eq? t *none*))))

(define type:map (record-map type:accessors))
(define type:tagged-map (record-tagged-map type:accessors))

;;I think the easiest way to have an equality over records is to define
;;a dynamic dispatch procedure over record and other types.
;;To distinguish this from standard scheme equality, we use tscheme:eq?
(define tscheme:equal?
  (make-generic-operator 2
                         'tscheme:equal?
                         equal?))
;;lists
(defhandler tscheme:equal? (lambda (l1 l2)
                             (or (and (null? l1) (null? l2))
                                 (and (pair? l1) (pair? l2)
                                      (tscheme:equal? (car l1) (car l2))
                                      (tscheme:equal? (cdr l1) (cdr l2)))))
  list? list?)

;;A generic defhandler for any record type. Could be implemented a tad
;;more efficiently (terminating when failure is detected), but that
;;sort of optimization is not really needed here.
(define (defhandler-tscheme:equal? accessors member?)
  (defhandler tscheme:equal? (lambda (t1 t2)                           
                               (for-all?
                                (map tscheme:equal?                               
                                     (map (lambda (acc) (acc t1)) accessors)
                                     (map (lambda (acc) (acc t2)) accessors))
                                identity))
    member? member?))

;;Define equal for the type record.
(defhandler-tscheme:equal? type:accessors type?)


#|
(pp (type:tagged-map list type:top (type:make-boolean)))
;; #[type 21]
;; (boolean (boolean all all))
;; (number (number all none))
;; (char (char all none))
;; (string (string all none))
;; (symbol (symbol all none))
;; (pair (pair all none))
;; (procedure (procedure all none))

(map type:boolean (list type:top (type:make-boolean)))
|#

;;; More specific instances, including constructors for finite-sets

(define *none* 'none)
(define *all*  'all)

(define (type:none? obj) (eqv? obj *none*))
(define (type:all?  obj) (eqv? obj *all*))

(define type:empty        (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top          (type:make *all*  *all*  *all*  *all*  *all*  *all*  *all*))
(define (type:make-boolean) (type:make *all*  *none* *none* *none* *none* *none* *none*))
(define (type:make-number)  (type:make *none* *all*  *none* *none* *none* *none* *none*))
(define (type:make-char)    (type:make *none* *none* *all*  *none* *none* *none* *none*))
(define (type:make-string)  (type:make *none* *none* *none* *all*  *none* *none* *none*))
(define (type:make-symbol)  (type:make *none* *none* *none* *none* *all*  *none* *none*))
(define (type:make-pair)    (type:make *none* *none* *none* *none* *none* *all*  *none*))

(define (type:make-procedure ret-tv arg-tvs)
  (type:make *none* *none* *none* *none* *none* *none* (cons ret-tv arg-tvs)))

(define (type:make-any-procedure)
  (type:make *none* *none* *none* *none* *none* *none* *all*))

;; The type can be a procedure that has type variable bindings
(define (complex-procedure? type)
  (list? (type:procedure type)))

(define (type:empty-procedure? type)
  (and (for-all? type:accessors-proc
                 (lambda (acc) (eq? (acc type) *none*)))))

;;Creates a new type with elts (not necessarily of the same type) as finite-sets in the
;;appropriate fields of the type record
;;I don't think we ever want pairs or procedures to be parts of finite-list
(define type:predicates
  (list boolean? number? char? string? symbol? (lambda (x) #f) (lambda (x) #f)))
(define (type:finite-set . elts) 
  (if (not (null? (fold-right ;;Check that elements are of the correct types
                    (lambda (type-pred lst)
                      (list-transform-negative lst type-pred))
                    elts
                    type:predicates)))
    (error "Provided list contains elements that cannot be part of a finite set")
    (apply type:make ;The method itself; for each type, filter out the
           (map      ;corresponging elements, sort and deduplicate them.
            (lambda (type-pred)              
              (make-finite-set%
               (dedup (general-sort (list-transform-positive elts type-pred)))))
            type:predicates))))


;;; Intersecting types

;;; Requires recursive execution on procedure arguments and return types, if present.
;;; This method is non-recursive and ignores those - they are handled separately.

;; Collect pairs of type variables from procedure types that have to be
;; dealt with.
(define (collect-proc-types typeA typeB)
  (let* ((pA (type:procedure typeA))
         (pB (type:procedure typeB)))
    (if (and (list? pA) (list? pB))
        (list-transform-positive
          (map (lambda (tvarA tvarB)
                 (if (eqv? tvarA tvarB) #t (list tvarA tvarB)))
               pA pB)
          symbol?)
        '())))

;; Computes the intersection of two types, does not recurse on function
;; arguments/return types
(define (intersect-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *none*) (eqv? typeB *none*)) *none*)
          ((eqv? typeA *all*) typeB)
          ((eqv? typeB *all*) typeA)
          ((eq? type-tag type:procedure) ;;At this point, we know both are lists
           (if (not (= (length typeA) (length typeB)))
               *none*
               typeA))
          (else (intersect-finite-sets typeA typeB))))

(define (intersect typeA typeB)
  (type:tagged-map intersect-type typeA typeB))

