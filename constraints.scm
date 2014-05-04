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

(load "utils")

;;;; Core definitions of types and functions for working with constraints.

(define *none* 'none)
(define *all*  'all)

(define (type:none?) (eqv? obj *none*))
(define (type:all?) (eqv? obj *all*))

(define (make-finite-set% elts)
  (if (null? elts)
      *none*
      (cons 'finite-set elts)))

(define (type:finite-set? ob)
  (and (list? obj)
       (not (null? obj))
       (eqv? (car obj) 'finite-set)))

(define type:empty        (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top          (type:make *all*  *all*  *all*  *all*  *all*  *all*  *all*))
(define type:make-boolean (type:make *all*  *none* *none* *none* *none* *none* *none*))
(define type:make-number  (type:make *none* *all*  *none* *none* *none* *none* *none*))
(define type:make-char    (type:make *none* *none* *all*  *none* *none* *none* *none*))
(define type:make-string  (type:make *none* *none* *none* *all*  *none* *none* *none*))
(define type:make-symbol  (type:make *none* *none* *none* *none* *all*  *none* *none*))
(define type:make-pair    (type:make *none* *none* *none* *none* *none* *all*  *none*))

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
             (make-finite-set%
               (dedup (general-sort (list-transform-positive elts type-pred)))))
           type:predicates)))
#|
(pp (type:finite-set 'a 'b 9 'a 1 2 3 3 2 1 #f 32 1 2 3))
;; #[type 65]
;; (boolean (finite-set #f))
;; (number (finite-set 1 2 3 9 32))
;; (char none)
;; (string none)
;; (symbol (finite-set a b))
;; (pair none)
;; (procedure none)
|#

;;Takes two finite sets of the same type
(define (intersect-finite-sets setA setB)
  (make-finite-set%
    (general-sort (filter (lambda (e) (memv e setB)) setA))))

(define (union-finite-sets setA setB)
  (make-finite-set%
    (dedup (general-sort (append (cdr setA) (cdr setB))))))

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

(define (type:empty? type)
  (for-all?
    (map (lambda (acc) (acc type)) type:accessors)
    (lambda (t) (eq? t *none*))))

(define type:map (record-map type:accessors))
(define type:tagged-map (record-tagged-map type:accessors))

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

;; A placeholder for type failure. Will probably have additional arguments later.
(define (report-failure msg)
  (error msg))

;; Requires recursive execution on procedure arguments and return types, if present.
;; This method is non-recursive and ignores those - they are handled separately.
(define (intersect-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *none*) (eqv? typeB *none*)) *none*)
        ((eqv? typeA *all*) typeB)
        ((eqv? typeB *all*) typeA)
        ((eq? type-tag type:procedure) ;;At this point, we know both are lists
         (if (!= (length typeA) (length typeB))
             (report-failure
               "Cannot unify: procedures have different number of arguments")
             typeA)) ;;Simply return the first one. TODO make sure this is correct
        (else (intersect-finite-sets typeA typeB))))

;;Collect pairs of type variables from procedure types that have to be
;;dealt with.
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

(define (union-type type-tag typeA typeB)
  (cond ((or (eqv? typeA *all*) (eqv? typeB *all*)) *all*)
        ((eqv? typeA *none*) typeB)
	((eqv? typeB *none*) typeA)
        ((eq? type-tag type:procedure)
         ;;TODO rewrite similarly as intersect-type
         (error "TODO union procedure types"))
        (else (union-finite-sets typeA typeB))))

;;Computes the intersection of two types, does not recurse on function
;;arguments/return types
(define (intersect typeA typeB)
  (type:tagged-map intersect-type typeA typeB))
(define (union typeA typeB)
  (type:tagged-map union-type typeA typeB))

(define empty-environment '())
(define (lookup-variable environment var)
  (let ((res (assv var environment)))
    (if (eq? res #f) type:top res)))

(define (update-variable environment var type)
  (cons (list var type) (del-assv var environment)))
(define (substitute-into-environment environment old new)
  (map (lambda (mapping)
         (list (car mapping) (substitute-into-type (cadr mapping) old new)))
       environment))

(define (substitute-into-type type old new)
  (type:tagged-map (lambda (tag t)
                     ;;Type variables can only appear in the procedure field
                     (if (and (eq? tag type:procedure) (list? t))
                         (map (lambda (tvar)
                                (if (eqv? tvar old) new tvar))
                              t)
                         t))
                   type))

;;More abstraction for dealing with ret and arg references
(define (tv-ret? v) (and (pair? v) (eqv? (car v) 'ret)))
(define (tv-arg? v) (and (pair? v) (eqv? (car v) 'arg)))
(define (tv-proc-name v) (cadr v))
(define (tv-arg-num v) (caddr v))

(define (proc:get-ret proc) (car proc))
(define (proc:get-arg proc n) (list-ref proc (+ n 1)))

(define (lookup-proc-variable env v)
  (if (or (tv-ret? v) (tv-arg? v))
      (type:procedure (lookup-variable env (tv-proc-name v)))
      #f))

(define (substitute-constraints old new constraints)
  (map (lambda (c)
         (let* ((left (constraint:left c))
                (ctype (constraint:relation c))
                (right (constraint:right c)))
           (cond ((equal? left old)
                  (constraint:make new ctype right))
                 ((and (tv-ret? left) (equal? (tv-proc-name left) old))
                  (constraint:make (return-type new) ctype right))
                 ((and (tv-arg? left) (equal? (tv-proc-name left) old))
                  (constraint:make (arg-of new (tv-arg-num left)) ctype right))
                 ((equal? right old)
                  (constraint:make left ctype new))
                 ((and (tv-ret? right) (equal? (tv-proc-name right) old))
                  (constraint:make left ctype (return-type new)))
                 ((and (tv-arg? right) (equal? (tv-proc-name right) old))
                  (constraint:make left ctype (arg-of new (tv-arg-num left))))
                 (else c))))
       constraints))

#|
(pp (cadr (substitute-constraints
            'a 'b `(,(constraint:make 'a 'equals 'b)
                    ,(constraint:make 'b 'equals 'a)))))
;;#[constraint 11]
;;(left b)
;;(relation equals)
;;(right b)
;;;Unspecified return value
|#

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
  (symbol-append (if (equal? prefix #!default) 'x prefix)
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

;;TODO I am not entirely sure about adding constraints this way. I think it
;;should work for requires and equals, it might not work for permits in some
;;cases.
;;As we might loop over the constraints and look for a fix point, we
;;probably should not be adding these constraints in but should only process
;;them.
;;Processes one constraint passed as the constraint argument,
;;doesn't examine other constraints (only substitutes into them, if appropriate).
;:I factored the code out so that it handles all types of constraints, although
;;it is probably just more confusing and complicated now.
(define (enforce-constraint constraints environment constraint)
  (let* ((left  (constraint:left     constraint))
         (right (constraint:right    constraint))
         (ctype (constraint:relation constraint)))
    (cond ((not (symbol? left)) (error "TODO handle ret and args"))
          ;;Two of the same variable, always fine
          ((eqv? left right) (list environment constraints))
          (else (let* ((tA (lookup-variable environment left))
                       (tB (if (symbol? right)
                               (lookup-variable environment right)
                               right))
                       (newType (intersect tA tB))
                       ;;In the case of permits, simply adding these new
                       ;;relations is not correct. TODO fix that.
                       (newConstraints
                         (if (or (eq? ctype *equals*) (eq? ctype *requires*))
                           ;;Collect aditional constraints and add them
                           (append (map
                                     (lambda (l-and-r)
                                       (constraint:make
                                         (car l-and-r) ctype (cadr l-and-r)))
                                     (collect-proc-types tA tB))
                                   constraints)
                           constraints)))
                  (if (type:empty? newType)
                      ;;TODO enforcing that the returned type is not empty.
                      (report-failure
                        "There is no possible type for this variable")
                      ;;Update variable binding if not permits
                      (list (update-variable
                              ;;Update environment if equals
                              (if (and (eq? ctype *equals*) (symbol? right))
                                  (substitute-into-environment
                                    environment left right)
                                  environment)
                              left
                              (if (eq? ctype *permits*) tA newType))
                            ;;Return updated constraints
                            (if (and (eq? ctype *equals*) (symbol? right))
                                (substitute-constraints left right newConstraints)
                                constraints))))))))
;;If my understanding of the difference between equals and requires is correct,
;;then the difference in the implementation is that if requires is of form
;;tvar1 requires tvar2, we do not substitute one for the other. Otherwise, the two
;;should be the same, I think. Permits then doesn't update any bindings whatsoever,
;;only checks that the intersections are non-empty.

#|
(pp (enforce-constraint '() '() (constraint:make 'a *equals* 'b)))
(pp (map record->list
         (cadr (enforce-constraint
                 `(,(constraint:make 'b *equals* type:make-boolean))
                 '()
                 (constraint:make 'a *equals* 'b)))))

(pp (caaddr (enforce-constraint `(,(constraint:make 'b *equals* type:make-boolean))
                                '()
                                (constraint:make 'a *equals* 'b))))
(pp (constraint:make 'a *equals* 'b))
|#
