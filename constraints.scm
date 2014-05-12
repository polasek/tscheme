;;; Analyze constraints to generate typing information

;; A placeholder for type failure. Will probably have additional arguments later.
(define (report-failure msg)
  (error msg))

;;Takes two finite sets of the same type
(define (intersect-finite-sets setA setB)
  (make-finite-set%
    (general-sort (filter (lambda (e) (memv e (cdr setB))) (cdr setA)))))

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
         (if (not (= (length typeA) (length typeB)))
             (report-failure
               "Cannot unify: procedures have different number of arguments")
             typeA)) ;;Simply return the first one.
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
  (cond ((type? v) v)
        ((symbol? v) v)
        ((or (tv-ret? v) (tv-arg? v))
         (let* ((proc-list (type:procedure (lookup-variable env (tv-proc-name v))))
                (k (if (tv-ret? v)
                       0
                       (+ 1 (tv-arg-num v)))))
           (cond ((eq? proc-list *all*) #f)
                 ((eq? proc-list *none*) (report-failure "Type error detected"))
                 ((and (list? proc-list) (< k (length proc-list)))
                  (lookup-proc-variable env (list-ref proc-list k)))
                 (else (report-failure "Type error detected")))))
        (else (begin (pp v)
                     (report-failure "Programmer error. I suck.")))))

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

(define (substitute-constraint old new constraint)
  (let* ((left (constraint:left constraint))
	 (ctype (constraint:relation constraint))
                (right (constraint:right constraint)))
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
	  (else constraint))))

(define (substitute-constraints old new constraints)
  (map (lambda (c) (substitute-constraint old new c)) constraints))

(define (multi-substitute-constraint subs constraint)
  (if (null? subs)
      constraint
      (multi-substitute-constraint
       (cdr subs)
       (substitute-constraint (caar subs) (cadar subs) constraint))))

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
	      (list (car a)
		    (fold-left (lambda (v b)
				 (if (eq? (car b) v) 
				     ;;If there is something substituting for our
				     ;;result later, we might as well do it
				     (cadr b) ;;straight away.
				     v))
			       (cadr a)
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
       (substitute-into-environment environment (caar subs) (cadar subs))
       (cdr subs))))

#|
(compose-substitutions '((a b) (c d) (e f)) '((g h) (i j) (k l)))
;Value 17: ((a b) (c d) (e f) (g h) (i j) (k l))
(compose-substitutions '((a b) (c d) (e f)) '((b h) (c j) (f l)))
;Value 19: ((a h) (c d) (e l) (b h) (f l))
(compose-substitutions '((a b) (c d) (e f)) '((a h) (c j) (e l)))
;Value 20: ((a b) (c d) (e f))
|#

;;Changes: enforce-constraint takes a single constraint and the environment
;;and returns a list of substitutions.

;; First argument is a pair of environment and current substitutions,
;; the second one is a constraint which hasn't yet been substituted into.
(define (enforce-constraint env-subs constraint_)
  (let* ((environment (car env-subs))
	 (prev_subs   (cadr env-subs)) ;;Previously made substitutions on this run
	 ;; Update the constraint according to our current substitutions
	 (constraint (multi-substitute-constraint prev_subs constraint_))
	 (left  (lookup-proc-variable environment (constraint:left constraint)))
         (right (lookup-proc-variable environment (constraint:right constraint)))
         (ctype (constraint:relation constraint)))
    (if (or (not left) (not right) (eqv? left right))
	;;Two of the same variable, or no info about arg/ret -> do nothing
	(list environment prev_subs)
        ;; Otherwise left is a type variable, right is a type var or a type
        (let* ((tA (lookup-variable environment left))
               (tB (if (symbol? right)
                       (lookup-variable environment right)
                       right))
               (newType (intersect tA tB))
               (newConstraints
		(if (or (not (eq? ctype *permits*))
			(for-all? type:accessors-proc
				  (lambda (acc) (eq? (acc newType) *none*))))
		    ;;We are either not dealing with permits, or the permits
		    ;;requires a check on procedure arguments.
		    (map
		     (lambda (l-and-r)
		       (constraint:make
			(car l-and-r) ctype (cadr l-and-r)))
		     (collect-proc-types tA tB))
		    '()))
	       ;;A new substitution
	       (newSub (if (and (eq? ctype *equals*) (symbol? right))
			   `((,left ,right))
			   '()))
	       ;;Aggregate the new substitution (if any) with exisiting ones
	       (subs (compose-substitutions prev_subs newSub))
	       ;;New environment after new substitution and type restriction
	       (newEnvironment
		(if (or (eq? ctype *equals*) (eq? ctype *requires*))
		    (update-variable
		     (multi-substitute-into-environment environment newSub)
		     left
		     newType)		    
		    environment)))
          (if (type:empty? newType)
              (report-failure
                "There is no possible type for this variable")
	      ;;Process the newly generated constraints (they won't be
	      ;; kept for running to convergence, once processed,
	      ;; these generated constraints are thrown away).
	      (fold-left enforce-constraint
			 (list newEnvironment subs)
			 newConstraints))))))

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
(define (enforce-all-constraints constraints)
  (let until-fixation ((constraints constraints)
		       (environment base-environment))
    (let* ((env-subs (fold-left enforce-constraint
				(list environment '()) constraints)))
      (if (tscheme:equal? environment (car env-subs))
	  '*SUCCESS* ;; This could perhaps return the mapping or other values
	  (until-fixation (map (lambda (c) (multi-substitute-constraint (cadr env-subs) c))
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

(pp (map record->list
         (cadr (enforce-all-constraints
                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b))))))

(pp (map record->list
         (cadr (enforce-all-constraints
                (get-constraints-for prestest-success)))))

(enforce-all-constraints (get-constraints-for prestest-fail))
(enforce-all-constraints (get-constraints-for prestest-success))

(print-recursive (get-constraints-for test1))

(enforce-all-constraints
                 `(,(constraint:make 'b *equals* type:make-boolean)
                   ,(constraint:make 'a *equals* 'b)))
|#
