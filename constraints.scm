;;;; Core definitions of types and functions for working with constraints.

;; Fresh type variables
(define **type-var-counter** 0)
(define (fresh)
  (set! **type-var-counter** (+ **type-var-counter** 1))
  (var:make (symbol-append 'x **type-var-counter**)))

(define (unify constraints)
  (error 'TODO))

;; Primitive type
(define-record-type type
    (type:make name)
    type?
    (name  type:name))

;; Type variable
(define-record-type type-variable
    (var:make name)
    type-variable?
    (name  var:name))

(define-record-type constraint
    (constraint:make left relation right)
    constraint?
    (left      constraint:left)
    (relation  constraint:relation)
    (right     constraint:right))

(define *requires* 'REQUIRES)
(define *permits*  'PERMITS)

(define *boolean*   (type:make 'boolean))
(define *number*    (type:make 'number))
(define *char*      (type:make 'char))
(define *string*    (type:make 'string))
(define *symbol*    (type:make 'symbol))
(define *pair*      (type:make 'pair))
(define *procedure* (type:make 'procedure))

(define (constraint:make-require left right)
  (constraint:make left *requires* right))

(define (constraint:make-permit left right)
  (constraint:make left *permits* right))

