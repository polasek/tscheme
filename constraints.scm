;;; Analyze constraints to generate typing information

;; A placeholder for type failure. Will probably have additional arguments later.
(define (report-failure v env ids fail)
  (fail v (union-finite-sets (lookup-variable-ids env v) ids)))

;;Takes two finite sets of the same type
(define (intersect-two-finite-sets setA setB)
  (make-finite-set%
    (general-sort (filter (lambda (e) (memv e (cdr setB))) (cdr setA)))))

(define (intersect-finite-sets . sets)
  (reduce-left intersect-two-finite-sets #f (filter type:finite-set? sets)))

(define (union-two-finite-sets setA setB)
  (make-finite-set%
    (dedup (general-sort (append (cdr setA) (cdr setB))))))

(define (union-finite-sets . sets)
  (reduce-left union-two-finite-sets #f (filter type:finite-set? sets)))

;; Requires recursive execution on procedure arguments and return types, if present.
;; This method is non-recursive and ignores those - they are handled separately.

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

;;Computes the intersection of two types, does not recurse on function
;;arguments/return types
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

(define empty-environment '())

;;TODO: Find a better way to identify parts of the base env
(define base-environment `((number  (,type:make-number ,(finite-set -1)))
                           (string  (,type:make-string ,(finite-set -2)))
                           (plus    (,(type:make-procedure 'number '(number number))
                                     ,(finite-set -3)))
                           (minus   (,(type:make-procedure 'number '(number number))
                                     ,(finite-set -4)))
                           (string-append (,(type:make-procedure
                                              'string '(string string))
                                           ,(finite-set -5)))))

(define (lookup-variable environment var)
  (let ((res (assv var environment)))
    (if res (caadr res) type:top)))

(define (lookup-variable-ids environment var)
  (let ((res (assv var environment)))
    (if res (cadadr res) #f)))

(define (update-variable environment var type ids)
  (cons (list var (list type ids)) (del-assv var environment)))

(define (lookup-proc-variable env v ids fail)
  (cond ((type? v) v)
        ((symbol? v) v)
        ((or (tv-ret? v) (tv-arg? v))
         (let* ((proc-list (type:procedure (lookup-variable env (tv-proc-name v))))
                (k (if (tv-ret? v)
                       0
                       (+ 1 (tv-arg-num v)))))
           (cond ((eq? proc-list *all*) #f)
                 ((eq? proc-list *none*) (report-failure
                                           fail (tv-proc-name v) env ids))
                 ((and (list? proc-list) (< k (length proc-list)))
                  (lookup-proc-variable env (list-ref proc-list k) ids fail))
                 (else (report-failure fail (tv-proc-name v) env ids)))))
        (else (begin (pp v)
                     (error "Bug in lookup-proc-variable" v env)))))

(define (substitute-into-environment environment old new ids)
  (map (lambda (mapping)
         (let* ((m-type (caadr mapping))
                (new-type (substitute-into-type m-type old new))
                (m-ids (cadadr mapping))
                ;; Update ids iff a substitution was made
                (new-ids (if (tscheme:equal? new-type m-type)
                             m-ids
                             (union-finite-sets ids m-ids))))
           (list (car mapping) (list new-type new-ids))))
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

(define (substitute-constraint old new ids    constraint)
  (let* ((left        (constraint:left        constraint))
	 (ctype       (constraint:relation    constraint))
         (right       (constraint:right       constraint))
         (usercode    (constraint:usercode    constraint))
         (left-annot  (constraint:left-annot  constraint))
         (right-annot (constraint:right-annot constraint))
         (new-ids     (union-finite-sets (constraint:ids constraint) ids)))
    (cond ((equal? left old)
	   (constraint:make-with-ids
             new ctype right new-ids usercode left-annot right-annot))
	  ((and (tv-ret? left) (equal? (tv-proc-name left) old))
	   (constraint:make-with-ids
             (return-type new) ctype right new-ids usercode left-annot right-annot))
	  ((and (tv-arg? left) (equal? (tv-proc-name left) old))
	   (constraint:make-with-ids
             (arg-of new (tv-arg-num left)) ctype right
             new-ids usercode left-annot right-annot))
	  ((equal? right old)
	   (constraint:make-with-ids
             left ctype new new-ids usercode left-annot right-annot))
	  ((and (tv-ret? right) (equal? (tv-proc-name right) old))
	   (constraint:make-with-ids
             left ctype (return-type new) new-ids usercode left-annot right-annot))
	  ((and (tv-arg? right) (equal? (tv-proc-name right) old))
	   (constraint:make-with-ids
             left ctype (arg-of new (tv-arg-num left))
             new-ids usercode left-annot right-annot))
	  (else constraint))))

(define (substitute-constraints old new ids constraints)
  (map (lambda (c) (substitute-constraint old new ids c)) constraints))

(define (multi-substitute-constraint subs constraint)
  (if (null? subs)
      constraint
      (multi-substitute-constraint
       (cdr subs)
       (substitute-constraint (caar subs) (cadar subs) (caddar subs) constraint))))

;; TODO: New tests
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
  (if (null? subsB)
      subsA
      (append
        (map (lambda (a)
               (cons (car a)
                     (fold-left (lambda (v b)
                                  (if (eq? (car b) (car v))
				     ;;If there is something substituting for our
				     ;;result later, we might as well do it
				      (list (cadr b) ;straight away.
                                            (union-finite-sets (caddr b) (cadr v)))
				      v))
                                  (cdr a)
                                  subsB)))
              subsA)
       ;;Remove all elements from b that have been already substituted for by a.
       (list-transform-negative 
	   subsB
	 (lambda (b)
	   (there-exists? subsA (lambda (a) (eq? (car a) (car b)))))))))
  
;;Add an element to the end of list
(define (cons-last a l)
  (if (null? l)
      (list a)
      (cons (car l) (cons-last a (cdr l)))))

;; XXX: If we're going to use this, must update to track ids
(define (add-substitution subs sub)
  (let ((subs-comp
	 (map (lambda (s)
		(list (car a) (if (eq? (car sub) (cadr a))
				  (cadr sub)
				  (cadr a))))
	      subs)))
    (if (there-exists? subs-comp (lambda (a) (eq? (car a) (car b))))
	subs-comp
	(cons-last sub subs-comp))))
	       
(define (multi-substitute-into-environment environment subs)
  (if (null? subs)
      environment
      (multi-substitute-into-environment
        (substitute-into-environment
          environment (caar subs) (cadar subs) (caddar subs))
        (cdr subs))))

#|
(pp (compose-substitutions `((a b ,(finite-set 1)) (c d ,(finite-set 2))
                                                   (e f ,(finite-set 3)))
                           `((g h ,(finite-set 4)) (i j ,(finite-set 5))
                                                   (k l ,(finite-set 6)))))
;((a b (finite-set 1)) (c d (finite-set 2)) (e f (finite-set 3))
; (g h (finite-set 4)) (i j (finite-set 5)) (k l (finite-set 6)))
(pp (compose-substitutions `((a b ,(finite-set 1)) (c d ,(finite-set 2))
                                                   (e f ,(finite-set 3)))
                           `((b h ,(finite-set 4)) (c j ,(finite-set 5))
                                                   (f l ,(finite-set 6)))))
;((a h (finite-set 1 4)) (c d (finite-set 2)) (e l (finite-set 3 6))
;                        (b h (finite-set 4)) (f l (finite-set 6)))
(pp (compose-substitutions `((a b ,(finite-set 1)) (c d ,(finite-set 2))
                                                   (e f ,(finite-set 3)))
                           `((a h ,(finite-set 4)) (c j ,(finite-set 5))
                                                   (e l ,(finite-set 6)))))
;((a b (finite-set 1)) (c d (finite-set 2)) (e f (finite-set 3)))
|#

;; The type can be a procedure that has type variable bindings
(define (complex-procedure? type)
  (list? (type:procedure type)))

(define (type:empty-procedure? type)
  (and (for-all? type:accessors-proc
                 (lambda (acc) (eq? (acc type) *none*)))))

;; First argument is a pair of environment and current substitutions,
;; the second one is a constraint which hasn't yet been substituted into.
(define (enforce-constraint-with-fail-continuation fail)
  (define (enforce-constraint env-subs constraint_)
    (let* ((environment (car env-subs))
           (prev_subs   (cadr env-subs)) ;;Previously made substitutions on this run
           ;; Update the constraint according to our current substitutions
           (constraint (multi-substitute-constraint prev_subs constraint_))
           (ids   (constraint:ids constraint))
           (left  (lookup-proc-variable
                    environment (constraint:left constraint) ids fail))
           (right (lookup-proc-variable
                    environment (constraint:right constraint) ids fail))
           (ctype (constraint:relation constraint)))
      (if (or (not left) (not right) (eqv? left right))
          ;;Two of the same variable, or no info about arg/ret -> do nothing
          (list environment prev_subs)
          ;; Otherwise left is a type variable, right is a type var or a type
          (let* ((tA (lookup-variable environment left))
                 (tB (if (symbol? right)
                         (lookup-variable environment right)
                         right))
                 (left-ids (lookup-variable-ids environment left))
                 (right-ids (lookup-variable-ids environment right))
                 (all-ids (union-finite-sets ids left-ids right-ids))
                 (newType (intersect tA tB))
                 (newConstraints
                  (if (type:empty-procedure? newType)
                      ;;A check on procedure arguments is required.
                      (map
                       (lambda (l-and-r)
                         (constraint:make-with-ids
                           (car l-and-r) ctype (cadr l-and-r) all-ids))
                       (collect-proc-types tA tB))
                      '()))
                 ;;A new substitution
                 (newSub (if (and (eq? ctype *equals*)
                                  (symbol? right)
                                  (or (not (complex-procedure? newType))
                                      (type:empty-procedure? newType)))
                             `((,left ,right ,all-ids))
                             '()))
                 ;;Aggregate the new substitution (if any) with exisiting ones
                 (subs (compose-substitutions prev_subs newSub))
                 ;;New environment after new substitution and type restriction
                 (newEnv-temp
                  (if (or (eq? ctype *equals*) (eq? ctype *requires*))
                      (update-variable
                       (multi-substitute-into-environment environment newSub)
                       left
                       newType
                       all-ids)
                      environment))
                 (newEnvironment
                  (if (and (eq? ctype *equals*)
                           (symbol? right)
                           (complex-procedure? newType)
                           (complex-procedure? tA)
                           (complex-procedure? tB)
                           (not (type:empty-procedure? newType)))
                      (update-variable newEnv-temp right (intersect tB tA) ids)
                      newEnv-temp)))
            (if (type:empty? newType)
                (report-failure left environment ids fail)
                ;;Process the newly generated constraints (they won't be
                ;; kept for running to convergence, once processed,
                ;; these generated constraints are thrown away).
                (fold-left enforce-constraint
                           (list newEnvironment subs)
                           newConstraints))))))
  enforce-constraint)

;TODO: New tests
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
(define (enforce-all-constraints constraints fail)
  (define enforce-constraint (enforce-constraint-with-fail-continuation fail))
  (let until-fixation ((constraints constraints)
		       (environment base-environment))
    (let* ((env-subs (fold-left enforce-constraint
				(list environment '()) constraints)))
      (if (tscheme:equal? environment (car env-subs))
	  ;'*SUCCESS* ;; This could perhaps return the mapping or other values
          (list environment constraints)
	  (until-fixation (map (lambda (c)
                                 (multi-substitute-constraint (cadr env-subs) c))
			       constraints)
			  (car env-subs))))))
#|
(define test1
  '(lambda (x y)
     (begin
       (+ x 5)
       (string-append y "a b"))))

(define prestest-success
  '((lambda (x y)
      (begin
        (+ x y)
        "done"))
    3
    4))

(define prestest-fail
  '((lambda (x y)
      (begin
        (+ x y)
        "done"))
    3
    "a"))

(pp
(call-with-current-continuation
  (lambda (k)
    (let* ((constraints (car (get-constraints-for prestest-fail)))
           (fail-continuation (lambda (v ids)
                                (for-each print-constraint constraints)
                                (k (list v ids))))
           (p (enforce-all-constraints constraints fail-continuation)))
      (for-each print-constraint (cadr p))
      (for-each (lambda (m)
                  (pp (list (car m) (record->list (caadr m)) (cadadr m)))
                  (read))
                (car p))
      (for-each print-constraint (cadr p))))))

(let ((p (enforce-all-constraints
           `(,(constraint:make 'b *equals* type:make-boolean)
             ,(constraint:make 'a *equals* 'b)))))
  (map print-constraint (cadr p))
  (map (lambda (m)
         (pp (list (car m) (record->list (caadr m)) (cadadr m)))
         (read))
       (car p))
  (map print-constraint (cadr p)))

(enforce-all-constraints (car (get-constraints-for prestest-fail)))
(enforce-all-constraints (car (get-constraints-for prestest-success)))

(print-recursive (car (get-constraints-for test1)))

(enforce-all-constraints
                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b)))
|#
