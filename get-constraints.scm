
;;; Temporary stuff, just so I can get print-outs
(define (singleton:make x)
  (type:finite-set x))
(define *procedure* type:make-any-procedure)
(define (tscheme:make-proc-type ret-tv arg-tvs)
  (type:make-procedure ret-tv arg-tvs))

;;; Setup

(define *the-constraints* '())

(define *base-cvmap*
  '((+ plus)
    (string-append string-append)
    (- minus)
    (|#!unspecific| unspecific)))

(define *query-map* '())

;;; This is what we'll use to process pieces of code
(define (get-constraints-for expr)
  (fluid-let ((*the-constraints* '())
              (*query-map* (qmap:make))
              (**type-var-counter** 0))
    (tscheme:process-expr expr *base-cvmap*)
    (list *the-constraints* *query-map*)))

;;; For testing purposes
(define (capture-constraints thunk)
  (fluid-let ((*the-constraints* '())
              (*query-map* (qmap:make))
              (**type-var-counter** 0))
    (thunk)
    *the-constraints*))

(define (add-constraint constraint)
  (set! *the-constraints* (cons constraint *the-constraints*)))

;;; Often we will want to pass around two pieces of information: a type
;;; variable, and a mapping of code variables to type variables.  Typically, tv
;;; will be the type variable representing the piece of code we have just
;;; processed, and cvmap will contain the mapping of code variables to type
;;; variables in the current scope.
(define-record-type tv&cvmap
    (tv&cvmap:make tv cvmap)
    tv&cvmap?
    (tv     tv&cvmap:tv     tv&cvmap:set-tv!)
    (cvmap  tv&cvmap:cvmap  tv&cvmap:set-cvmap!))

;;; Abstractions for the data structure mapping code variables to type
;;; variables
(define (cvmap:make)
  *base-cvmap*)

(define (cvmap:bind cvmap key val)
  (cons `(,key ,val) cvmap))

(define (cvmap:lookup key cvmap)
  (let ((record (assoc key cvmap)))
   (if record
     (cadr record)
     (begin
       (display "Warning: failed to look up key in cvmap: ")
       (pp key)
       #f))))

;;; Abstractions for query map (later, should refactor so that the same
;;; structure is used for cvmap and this)
(define (qmap:make)
  '())

(define (qmap:bind cvmap key val)
  (cons `(,key ,val) cvmap))

(define (add-to-qmap key val)
  (set! *query-map* (qmap:bind *query-map* key val)))

;;; Helpers for dispatching on the type of code fragment we see

(define (tagged-list? expr tag)
  (and (list? expr)
       (not (null? expr))
       (symbol? (car expr))
       (eq? (car expr) tag)))

(define (query? expr)
  (tagged-list? expr 'query))

(define (query-name expr)
  (cadr expr))

(define (query-item expr)
  (caddr expr))

(define (self-quoting? expr)
  (or (number? expr)
      (string? expr)
      (char? expr)))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-arglist expr)
  (cadr expr))

(define (lambda-body expr)
  ;; We assume the body of a lambda is already wrapped in a begin
  (caddr expr))

(define (quote? expr)
  (tagged-list? expr 'quote))

(define (quoted-expr-contents expr)
  (cadr expr))

(define (begin? expr)
  (tagged-list? expr 'begin))

;; Returns the sequence of expressions in a begin
(define (begin-seq expr)
  (cdr expr))

(define (define? expr)
  (tagged-list? expr 'define))

(define (define-lhs expr)
  (cadr expr))

(define (define-rhs expr)
  (caddr expr))

(define (if? expr)
  (tagged-list? expr 'if))

(define (if-predicate expr)
  (cadr expr))

(define (if-consequent expr)
  (caddr expr))

(define (if-alternative expr)
  (cadddr expr))

(define (variable? expr)
  (symbol? expr))

(define (application-operator expr)
  (if (or (not (list? expr))
          (null? expr))
    (error "Bad application expression" expr)
    (car expr)))

(define (application-arglist expr)
  (cdr expr))

;;; The workhorse for constraint generation is the following generic operation,
;;; which takes in an expression and a cvmap (the current mapping of code
;;; variables to type variables), and produces a tv&cvmap (where the tv is the
;;; type variable representing the value produced, and the cvmap is an update
;;; of the old cvmap which accounts for the updates made by our code snippet).
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

(define (tscheme:process-self-quoting expr cvmap)
  (let ((tv (fresh)))
   (add-constraint (constraint:make-equal tv
                                          (singleton:make expr)
                                          expr))
   (tv&cvmap:make tv cvmap)))

#|
(define x (tscheme:process-self-quoting 3 (cvmap:make)))
|#

(defhandler tscheme:process-expr tscheme:process-self-quoting self-quoting?)

(define (tscheme:process-quote expr cvmap)
  (let ((tv (fresh))
        (contents (quoted-expr-contents expr)))
   (add-constraint
     (constraint:make-equal
       tv
       ;; In our implementation, pairs cannot contain finite sets
       (if (pair? contents)
         type:make-pair
         (singleton:make contents))
       expr))
   (tv&cvmap:make tv cvmap)))

#|
(let ((x (capture-constraints
           (lambda ()
             (tscheme:process-quote '(quote (hello goodbye)) (cvmap:make)))))
      (y (capture-constraints
           (lambda ()
             ;; lol macros within macros
             (tscheme:process-quote ''a (cvmap:make))))))
  (pp (constraint:right (car x)))
  (newline)
  (pp (constraint:right (car y))))

#[type 2]
(boolean none)
(number none)
(char none)
(string none)
(symbol none)
(pair all)
(procedure none)

#[type 3]
(boolean none)
(number none)
(char none)
(string none)
(symbol (finite-set a))
(pair none)
(procedure none)
|#

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
                             (tscheme:make-proc-type ret-tv arg-tvs)
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
     (define-lhs expr)
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
    ;; Suppress modifications to the-constraints within the extents of the
    ;; consequent and the alternative.  So we only care about side-effects
    ;; here, and only those side-effects that modify the query table.
    (fluid-let ((*the-constraints* *the-constraints*))
      (tscheme:process-expr consequent pred-cvmap)
      (tscheme:process-expr alternative pred-cvmap))

    ;; Fresh type variable for the return of the if.  We assume there are no
    ;; defines in the consequent and the alternative, so we can return the
    ;; cvmap given to us by the predicate.
    ;;
    ;; In order to let the unifier know that this type variable even exists, we
    ;; generate a dummy constraint (tv requires top).
    (let ((return-tv (fresh-branchvar)))
     (add-constraint
       (constraint:make-require return-tv type:top))
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
    ;; Operator must be a procedure
    (add-constraint
      (constraint:make-permit operator-tv
                              *procedure*
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
(print-recursive `(a b (,(constraint:make-require 1 2) (d)) e))
|#

#|
(define test1
  '(lambda (x y)
     (begin
       (+ x 5)
       (string-append y "a b"))))

(print-recursive (get-constraints-for test1))

;;; The key thing to note here is that the code variable corresponding to x in
;;; (- x 1) is the same one as in (+ x 1), but is different from the one in
;;; (string-append x "a").
(define test2
  '(begin
     (define x 3)
     (+ x 1)
     (lambda (x)
       (string-append x "a"))
     (- x 1)))

(print-recursive (get-constraints-for test2))

;;; Applying anonymous lambdas
(define test3
  '((lambda (x y) (begin (+ x y))) 3 4))

(print-recursive (get-constraints-for test3))

;;; Complicated argument expressions.  And again, the scoping of x is done
;;; correctly.
(define test4
  '(begin
     (define x 3)
     (+ (begin
          9
          ((lambda (x y)
            5)))
        (begin
          (+ 7)
          x))))

(print-recursive (get-constraints-for test4))

;;; Note the asymmetry: when we process (+ x (+ 1 2)), we get the constraints
;;; that x requires (arg + 0), but (arg + 1) permits (ret +).  The current
;;; logic is such that variables which are passed as arguments generate require
;;; relations, but any other thing (e.g., a combination) passed as an argument
;;; generates a permit relation.
(define test5
  '(begin
     (define x 3)
     (+ x (- 1 2))))

(print-recursive (get-constraints-for test5))

;;; Assignments within arguments
(define test6
  '(begin
     (+ (begin
          (define x 3)
          1)
        2)
     x))

(print-recursive (get-constraints-for test6))

|#


