;;;; Supporting code for constraint generator

;;; There are several data structures which are basically just key-value
;;; stores, but are used for different things.  I'll give them separate
;;; abstractions, just so that I get some more flexibility in the choice of
;;; implementation.  For example, in a typical use case there will be many
;;; constraints, and the user will want to be able to look them up efficiently
;;; but never look at them all at once; thus a hash table is appropriate for
;;; the constraint table.  There will typically be only a few queries, and the
;;; user will often want to look at all the query results, so an association
;;; list is best suited to the qmap.  The cvmap is more complicated; see
;;; cvmap.scm for the full explanation there.


;;; "Constraint tables" -- a key-value store supporting insertion and lookup.
;;; Warns when you overwrite something, throws error when you try to look up a
;;; key that doesn't exist.  This data structure is intended to hold
;;; constraints keyed by id, so there are helpers for inserting constraints
;;; into the table.

(define (make-constraint-table)
  (let ((constraint-table (make-eqv-hash-table)))
   ;; Pre-populate the table with base constraints
   (for-each (lambda (constraint)
               (add-constraint-to constraint-table constraint))
             base-constraints)
   constraint-table))

(define (fetch-constraint constraint-table id)
  (define not-found (list 'not-found))
  
  (let ((constraint (hash-table/get constraint-table id not-found)))
   (if (eq? constraint not-found)
     (error "Reference to unknown constraint" id))
   constraint))

;; Creates a flat list of values
(define (constraint-table->datum-list constraint-table)
  (hash-table/datum-list constraint-table))

(define (add-constraint-to constraint-table constraint)
  (define not-found (list 'not-found))

  (define (insert-into-table key value)
    (let ((old-entry (hash-table/get constraint-table key not-found)))
     (if (not (eq? old-entry not-found))
       (display "Warning: overwriting a constraint\n"))
     (hash-table/put! constraint-table key value)))

  (for-each (lambda (id)
              (insert-into-table id constraint))
            (finite-set-elts (constraint:ids constraint))))

;;; Since we'll be keeping a global constraint table
(define (add-constraint constraint)
  (add-constraint-to *the-constraints* constraint))

;;; Base constraints are true regardless of user code.  We could of course make
;;; a very long list of base constraints, if the system needed to be put to
;;; serious use.
(define base-constraints
  (let ((rmake (lambda (left right id)
                 (constraint:make-with-ids
                   left *requires* right (finite-set id)))))
    (list (rmake 'number (type:make-number) -1)
          (rmake 'string (type:make-string) -2)
          (rmake 'plus   (type:make-procedure 'number  '(number number)) -3)
          (rmake 'minus  (type:make-procedure 'number  '(number number)) -4)
          (rmake 'times  (type:make-procedure 'number  '(number number)) -5)
          (rmake 'divide (type:make-procedure 'number  '(number number)) -6)
          (rmake 'integer-equal?
                 (type:make-procedure 'boolean '(number number)) -7)
          (rmake 'integer-less?
                 (type:make-procedure 'boolean '(number number)) -8)
          (rmake 'integer-greater?
                 (type:make-procedure 'boolean '(number number)) -9)
          (rmake 'string-append
                 (type:make-procedure 'string  '(string string)) -10))))


;;; "Query map" -- a simple human readable key-value store.  We don't need
;;; anything high-powered like a hash table.

(define (qmap:make)
  '())

(define (qmap:bind cvmap key val)
  (cons `(,key ,val) cvmap))

;;; Since we'll be keeping a global query map
(define (add-to-qmap key val)
  (set! *query-map* (qmap:bind *query-map* key val)))

;;; "Code variable map" -- the cvmap data structure is pretty complicated
(load "cvmap")


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


;;; Helpers for dispatching on the type of code fragment we read in

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
      (char? expr)
      (boolean? expr)))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-arglist expr)
  (cadr expr))

(define (lambda-body expr)
  ;; We assume the body of a lambda is already wrapped in a begin (see
  ;; rewrite.scm)
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


;;; For added clarity
(define (singleton x)
  (type:finite-set x))
