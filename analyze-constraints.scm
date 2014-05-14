;;;; Analyze constraints to generate typing information

;;; Handle type failure with a fail continuation
(define (report-failure v env ids fail)
  (fail v (union-finite-sets (lookup-variable-ids env v) ids)))

;;; Environment mapping type variables to types and to constraint ids

(define (env-mapping:var  mapping) (car mapping))
(define (env-mapping:type mapping) (caadr mapping))
(define (env-mapping:ids  mapping) (cadadr mapping))

(define empty-environment '())

(define base-environment empty-environment)

(define (lookup-variable environment var)
  (let ((res (assv var environment)))
    (if res (env-mapping:type res) type:top)))

(define (lookup-variable-ids environment var)
  (let ((res (assv var environment)))
    (if res (env-mapping:ids res) #f)))

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


;;; Substitutions: See analyze-constraints-support.scm for related definitions.

(define (substitute-into-environment environment sub)
  (if (not (substitution? sub)) environment
    (map (lambda (mapping)
           (let* ((m-type (env-mapping:type mapping))
                  (new-type (substitute-into-type
                              m-type (sub:old sub) (sub:new sub)))
                  (m-ids (env-mapping:ids mapping))
                  ;; Update ids iff a substitution was made
                  (new-ids (if (tscheme:equal? new-type m-type)
                               m-ids
                               (union-finite-sets (sub:ids sub) m-ids))))
             (list (car mapping) (list new-type new-ids))))
         environment)))

(define (substitute-into-type type old new)
  (type:tagged-map (lambda (tag t)
                     ;;Type variables can only appear in the procedure field
                     (if (and (eq? tag type:procedure) (list? t))
                         (map (lambda (tvar)
                                (if (eqv? tvar old) new tvar))
                              t)
                         t))
                   type))

(define (substitute-constraint constraint sub)
  (if (not (substitution? sub)) constraint
    (let* ((old (sub:old sub))
           (new (sub:new sub))
           (ids (sub:ids sub))
           (left        (constraint:left        constraint))
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
            (else constraint)))))

(define (multi-substitute-constraint constraint subs)
  (fold-left substitute-constraint constraint subs))

(define (multi-substitute-into-environment environment subs)
  (fold-left substitute-into-environment environment subs))

;; XXX
(define (substitute-constraints old new ids constraints)
  (map (lambda (c) (substitute-constraint old new ids c)) constraints))

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


;; First argument is a pair of environment and current substitutions,
;; the second one is a constraint which hasn't yet been substituted into.
(define (enforce-constraint-with-fail-continuation fail)
  (define (enforce-constraint env-subs constraint_)
    (let* ((environment (car env-subs))
           (prev_subs   (cadr env-subs)) ;;Previously made substitutions on this run
           ;; Update the constraint according to our current substitutions
           (constraint (multi-substitute-constraint constraint_ prev_subs))
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
                             (substitution:make left right all-ids)
                             #f))
                 ;;Aggregate the new substitution (if any) with exisiting ones
                 (subs (add-substitution prev_subs newSub))
                 ;;New environment after new substitution and type restriction
                 (newEnv-temp
                  (if (or (eq? ctype *equals*) (eq? ctype *requires*))
                      (update-variable
                       (substitute-into-environment environment newSub)
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
                 `(,(constraint:make 'b *equals* (type:make-boolean)))
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
                                 (multi-substitute-constraint c (cadr env-subs)))
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
           `(,(constraint:make 'b *equals* (type:make-boolean))
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
                 `(,(constraint:make 'b *equals* (type:make-boolean))
                   ,(constraint:make 'a *equals* 'b)))
|#
