;;; Analyze constraints to generate typing information

;; A placeholder for type failure. Will probably have additional arguments later.
(define (report-failure msg)
  (error msg))

;;Takes two finite sets of the same type
(define (intersect-finite-sets setA setB)
  (make-finite-set%
    (general-sort (filter (lambda (e) (memv e setB)) setA))))

(define (union-finite-sets setA setB)
  (make-finite-set%
    (dedup (general-sort (append (cdr setA) (cdr setB))))))

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
(define base-environment `((number  ,type:make-number)
                           (string  ,type:make-string)                           
                           (plus    ,(type:make-procedure 'number '(number number)))
                           (minus   ,(type:make-procedure 'number '(number number)))
                           (string-append ,(type:make-procedure 'string '(string string)))))

(define (lookup-variable environment var)
  (let ((res (assv var environment)))
    (if (eq? res #f) type:top (cadr res))))

(define (update-variable environment var type)
  (cons (list var type) (del-assv var environment)))

(define (lookup-proc-variable env v)
  (if (or (tv-ret? v) (tv-arg? v))
      (type:procedure (lookup-variable env (tv-proc-name v)))
      #f))

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

;;TODO I am not entirely sure about adding constraints this way. I think it
;;should work for requires and equals, it might not work for permits in some
;;cases.
;;As we might loop over the constraints and look for a fix point, we
;;probably should not be adding these constraints in but should only process
;;them.
;;Processes one constraint passed as the constraint argument,
;;doesn't examine other constraints (only substitutes into them, if appropriate).
;;I factored the code out so that it handles all types of constraints, although
;;it is probably just more confusing and complicated now.
(define (enforce-constraint constraints environment constraint)
  (let* ((left  (lookup-proc-variable environment (constraint:left constraint)))
         (right (lookup-proc-variable environment (constraint:right constraint)))
         (ctype (constraint:relation constraint)))
        ;;Two of the same variable, or no info about arg/ret -> do nothing
    (if (or (not left) (not right) (eqv? left right))
        (list environment constraints)
        ;; Otherwise left is a type variable, right is a type var or a type
        (let* ((tA (lookup-variable environment left))
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
                        constraints)))))))
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

(pp (constraint:make 'a *equals* 'b))
|#

;;Enforce constraints until fixed point is reached,
;;Assumes length and ordering of constraints does not change.
(define (enforce-all-constraints constraints)
  (let ((n (length constraints)))
    (let until-fixation ((constraints constraints)
                         (environment base-environment)
                         (old (cons '() '()))
                         (iter 0))
      ;;Exit condition
;      (if (record-equal? (cons constraints environment) old) old ;;TODO fix this
      (if (= iter 1000)
          old
        (let lp ((k 0)
                 (current-constraints constraints)
                 (current-environment environment))
;;          (pp current-constraints)
;;          (pp (map (lambda (pair) (list (car pair) (record->list (cadr pair)))) current-environment))
          (let* ((res (enforce-constraint current-constraints
                                          current-environment
                                          (list-ref current-constraints k)))
                 (new-environment (car res))
                 (new-constraints (cadr res)))
            (if (>= k (- n 1))
                ;;Test for fixation after enforcing all constraints
                (until-fixation new-constraints new-environment
                                (cons constraints environment)
                                (+ 1 iter))
                (lp (+ k 1) new-constraints new-environment))))))))

#|
(pp (map record->list
         (cadr (enforce-all-constraints
                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b))))))

(pp (map record->list
         (cadr (enforce-all-constraints
                (get-constraints-for test1)))))
(get-constraints-for test1)

                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b))))))



(enforce-all-constraints
                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b)))
|#

