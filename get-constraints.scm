;;;; Constraint generator and code analyzer

;;; Set up global variables

(define *the-constraints* (make-constraint-table))
(define *query-map* '())


;;; Reads in an expression.  Returns a table of constraints generated and a
;;; mapping of query names to type variables.
(define (get-constraints-for expr)
  (fluid-let ((*the-constraints* (make-constraint-table))
              (*query-map* (qmap:make))
              (**type-var-counter** 0))
    (tscheme:process-expr expr *base-cvmap*)
    (list *the-constraints* *query-map*)))


;;; Analyzer.  Generates constraints for the given expression, unifies them,
;;; and returns a result.  If a contradiction was found, the result is a list
;;; of the form (failure tv constraints), where tv is a type variable and
;;; constraints is a list of constraints which produced the contradiction.  If
;;; no contradiction is found, the return value is of the form (qmap ((tv type
;;; constraints) ...)), where each type variable tv appears along with its
;;; deduced type and the constraints used to deduce that type.
(define (tscheme:analyze expr)
  (call-with-current-continuation
    (lambda (remote-exit)
      (let* ((constraints&qmap (get-constraints-for expr))
             (constraint-table (car constraints&qmap))
             (constraint-list (constraint-table->datum-list constraint-table))
             (qmap (cadr constraints&qmap)))
        (define fail-continuation
          (lambda (tv ids)
            (remote-exit
              (list 'failure tv (map (lambda (id)
                                       (fetch-constraint constraint-table id))
                                     (finite-set-elts ids))))))
        
        (let ((result (enforce-all-constraints constraint-list
                                               fail-continuation)))
         (let ((tv->type&reason-ids (car result))
               (all-constraints (cadr result)))
           (let ((tv->type&reasons
                   (map
                     (lambda (p)
                       (let ((tv (car p))
                             (type (caadr p))
                             (reason-ids (finite-set-elts (cadadr p))))
                         (list tv
                               type
                               (map (lambda (id)
                                      (fetch-constraint constraint-table id))
                                    reason-ids))))
                     tv->type&reason-ids)))
             (list qmap tv->type&reasons))))))))



;;; The workhorse for constraint generation is the following generic operation,
;;; which takes in an expression and a cvmap (the current mapping of code
;;; variables to type variables), and produces a tv&cvmap (where the tv is the
;;; type variable representing the value produced, and the cvmap is an update
;;; of the old cvmap which accounts for the changes made by our code snippet).
(define tscheme:process-expr
  (make-generic-operator 2
                         'tscheme:process-expr
                         (lambda (expr cvmap)
                           (tscheme:process-application expr cvmap))))


;; To process a query, just process the item and then add a binding to the
;; query map
(define (tscheme:process-query expr cvmap)
  (let ((qname (query-name expr))
        (qitem (query-item expr)))
    (let ((tv&cvmap (tscheme:process-expr qitem cvmap)))
     (add-to-qmap qname (tv&cvmap:tv tv&cvmap))
     tv&cvmap)))

(defhandler tscheme:process-expr tscheme:process-query query?)


;; Self-quoting expressions produce a fresh type variable which "equals" a
;; singleton type.
(define (tscheme:process-self-quoting expr cvmap)
  (let ((tv (fresh)))
   (add-constraint (constraint:make-equal tv
                                          (singleton expr)
                                          expr))
   (tv&cvmap:make tv cvmap)))

(defhandler tscheme:process-expr tscheme:process-self-quoting self-quoting?)

;; Quoted expressions are also bound to singletons.  But we don't recurse into
;; the quoted object because, well, it's quoted.
(define (tscheme:process-quote expr cvmap)
  (let ((tv (fresh))
        (contents (quoted-expr-contents expr)))
   (add-constraint
     (constraint:make-equal
       tv
       ;; In our implementation, the "pair" field of a type cannot be a finite
       ;; set
       (if (pair? contents)
         (type:make-pair)
         (singleton contents))
       expr))
   (tv&cvmap:make tv cvmap)))

(defhandler tscheme:process-expr tscheme:process-quote quote?)


;;; Handler for lambda expressions
(define (tscheme:process-lambda expr cvmap)
  (let ((lambda-tv (fresh-procvar)) ; points to the procedure object created by
                                    ; the lambda
        (ret-tv (fresh-retvar))   ; the return type of the procedure
        (arg-tvs '())             ; the argument types of the procedure
        (outer-cvmap cvmap)    ; cvmap of the scope outside the lambda, will
                               ; not be mutated
        (inner-cvmap cvmap))   ; cvmap of the scope inside the lambda, will be
                               ; mutated to incorporate the formal parameters
                               ; of the lambda, as well as any internal
                               ; definitions (the pointer to inner-cvmap, not
                               ; the object itself, is mutated).
    ;; Build up arg-tvs and bind the formal parameters in inner-cvmap
    (let lp ((remaining-args (lambda-arglist expr)))
     (if (null? remaining-args)
       #!unspecific
       (let ((arg-cv (car remaining-args))
             (arg-tv (fresh-argvar)))
         (set! arg-tvs (append arg-tvs
                               (list arg-tv)))
         (set! inner-cvmap (cvmap:bind inner-cvmap arg-cv arg-tv))
         (lp (cdr remaining-args)))))

    ;; Let the unifier know what the argument and return variables of this
    ;; procedure are
    (add-constraint
      (constraint:make-equal lambda-tv
                             (type:make-procedure ret-tv arg-tvs)
                             expr))

    ;; Recurse into the body, and say what we can about the return type of the
    ;; last expression (again, we assume that the body is wrapped in a begin)
    (add-constraint
      (constraint:make-require
        ret-tv
        (tv&cvmap:tv (tscheme:process-expr (lambda-body expr) inner-cvmap))
        expr))

    ;; We use the outer cvmap because we are returning to the scope outside the
    ;; body of our lambda.
    (tv&cvmap:make lambda-tv outer-cvmap)))
  
(defhandler tscheme:process-expr tscheme:process-lambda lambda?)


;;; Handler for begins
(define (tscheme:process-begin expr cvmap)
  ;; Process everything in sequence, and return the tv of the last expression
  (let lp ((exprs (begin-seq expr))
           (next-cvmap cvmap)
           (return-tv 'theres-been-a-mistake))
    (if (null? exprs)
      ;; Exit condition is satisfied
      (tv&cvmap:make return-tv next-cvmap)
      ;; Otherwise there are more expressions to process
      (let ((tv&cvmap
              (tscheme:process-expr (car exprs) next-cvmap)))
       (lp (cdr exprs)
           (tv&cvmap:cvmap tv&cvmap)
           ;; doesn't actually matter what I put here
           (tv&cvmap:tv tv&cvmap))))))
  
(defhandler tscheme:process-expr tscheme:process-begin begin?)


;;; Handler for defines
(define (tscheme:process-define expr cvmap)
  (let ((tv&cvmap (tscheme:process-expr (define-rhs expr) cvmap)))
   (tv&cvmap:make
     (tv&cvmap:tv tv&cvmap) ; shouldn't matter what we put here, since the
                            ; return value of a define is unspecified
     (cvmap:bind (tv&cvmap:cvmap tv&cvmap)
                 (define-lhs expr)
                 (tv&cvmap:tv tv&cvmap)))))

(defhandler tscheme:process-expr tscheme:process-define define?)


;;; Handler for variables
(define (tscheme:process-variable expr cvmap)
  (let ((tv (cvmap:lookup expr cvmap)))
   (tv&cvmap:make (if tv
                    tv
                    (error "Unknown code variable" expr))
                  cvmap)))

(defhandler tscheme:process-expr tscheme:process-variable variable?)


;;; Handler for ifs
;;; 
;;; Outline of the logic:
;;; We store the constraints generated by the predicate, but we do not store
;;; the constraints generated by the consequent or the alternative.  The reason
;;; we even pay attention to the consequent and alternative is that they might
;;; contain queries.
(define (tscheme:process-if expr cvmap)
  (let* ((pred-expr (if-predicate expr))
         (consequent (if-consequent expr))
         (alternative (if-alternative expr))
         (pred-tv&cvmap (tscheme:process-expr pred-expr cvmap))
         (pred-cvmap (tv&cvmap:cvmap pred-tv&cvmap)))
    ;; Suppress modifications to *the-constraints* within the extents of the
    ;; consequent and the alternative.  So we only care about side-effects
    ;; here, and only those side-effects that modify the query table.
    (fluid-let ((*the-constraints* (make-constraint-table)))
      (tscheme:process-expr consequent pred-cvmap)
      (tscheme:process-expr alternative pred-cvmap))

    ;; We allocate a fresh type variable for the return of the if.  We assume
    ;; there are no defines in the consequent and the alternative, so we can
    ;; return the cvmap given to us by the predicate.
    ;;
    ;; In order to let the unifier know that this type variable even exists, we
    ;; generate a dummy constraint (return-tv requires top).
    (let ((return-tv (fresh-branchvar)))
     (add-constraint
       (constraint:make-require return-tv
                                type:top
                                expr))
     (tv&cvmap:make return-tv pred-cvmap))))

(defhandler tscheme:process-expr tscheme:process-if if?)


;;; Handler for applications (i.e., combinations).
;;;
;;; Outline of the logic:
;;; Operator permits procedure.
;;; Allocate fresh variable r for the return of this application.
;;; Then r requires (ret operator).
;;; Also, each time we substitute an expression x for an argument (arg operator
;;; i), we have two possibilities:
;;; If x is a variable, then x requires (arg operator i).
;;; Otherwise, (arg operator i) permits x.
(define (tscheme:process-application expr cvmap)
  (let* ((operator-tv&cvmap
           (tscheme:process-expr (application-operator expr) cvmap))
         (operator-tv (tv&cvmap:tv operator-tv&cvmap))
         (return-tv (fresh)))
    ;; Operator must permit procedure
    (add-constraint
      (constraint:make-permit operator-tv
                              (type:make-any-procedure)
                              expr))
    ;; The value produced will always be an element of the return type of
    ;; operator
    (add-constraint
      (constraint:make-require return-tv
                               (return-type operator-tv)
                               expr))
    ;; The order in which we evaluate the arguments is unspecified, but we do
    ;; need to keep track of updates to the cvmap throughout this process.
    (let lp ((i 0)
             (remaining-args (application-arglist expr))
             (next-cvmap (tv&cvmap:cvmap operator-tv&cvmap)))
      (if (null? remaining-args)
        (tv&cvmap:make return-tv next-cvmap)
        (let* ((arg-value-expr (car remaining-args))
               (arg-value-tv&cvmap (tscheme:process-expr arg-value-expr next-cvmap))
               (arg-value-tv (tv&cvmap:tv arg-value-tv&cvmap))
               (arg-value-cvmap (tv&cvmap:cvmap arg-value-tv&cvmap))
               (arg-tv (arg-of operator-tv i)))
          (add-constraint
            ;; For variables passed as arguments, we use require; for
            ;; everything else, we use permit
            (if (variable? arg-value-expr)
              (constraint:make-require arg-value-tv
                                       arg-tv
                                       expr)
              (constraint:make-permit arg-tv
                                      arg-value-tv
                                      expr)))
          (lp (+ i 1)
              (cdr remaining-args)
              arg-value-cvmap))))))


#|
(define test103
  '(begin
     (define x 4)
     (define y
       (if (query my_x x)
         "a"
         #\e))
     (+ y 7)
     (string-append y "e")))

(define analysis (tscheme:analyze test103))


(define test104
  '(begin
     (define x 4)
     (define y
       (if (query my_x x)
         "a"
         #\e))
     (string-append y "e")))

(define analysis (tscheme:analyze test104))

(car analysis)

(print-recursive
  (assoc 'x1 (cadr analysis)))
|#
