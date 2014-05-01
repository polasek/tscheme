;;;; Core definitions of types and functions for working with constraints.

;;; Data structure definitions

;; Primitive type
(define-record-type type
    (type:make name)
    type?
    (name  type:name))

;; Singleton type
(define-record-type singleton
    (singleton:make val)
    singleton?
    (val  singleton:val))

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

(define *equals*   'EQUALS)
(define *requires* 'REQUIRES)
(define *permits*  'PERMITS)

(define (equality? constraint)
  (and (constraint? constraint)
       (eq? (constraint:relation constraint) *equals*)))

(define (requirement? constraint)
  (and (constraint? constraint)
       (eq? (constraint:relation constraint) *requires*)))

(define (permission? constraint)
  (and (constraint? constraint)
       (eq? (constraint:relation constraint) *permits*)))

(define *boolean*   (type:make 'boolean))
(define *number*    (type:make 'number))
(define *char*      (type:make 'char))
(define *string*    (type:make 'string))
(define *symbol*    (type:make 'symbol))
(define *pair*      (type:make 'pair))
(define *procedure* (type:make 'procedure))

(define (type:boolean?   t) (and (type? t) (eq? t *boolean*)))
(define (type:number?    t) (and (type? t) (eq? t *number*)))
(define (type:char?      t) (and (type? t) (eq? t *char*)))
(define (type:string?    t) (and (type? t) (eq? t *string*)))
(define (type:pair?      t) (and (type? t) (eq? t *pair*)))
(define (type:procedure? t) (and (type? t) (eq? t *procedure*)))

;;; Methods to construct instances

;; Fresh type variables
(define **type-var-counter** 0)
(define (fresh #!optional prefix)
  (set! **type-var-counter** (+ **type-var-counter** 1))
  (var:make (symbol-append (if (equal? prefix #!default)
                             'x
                             prefix)
                           **type-var-counter**)))

(define (fresh-argvar) (fresh 'argvar))
(define (fresh-retvar) (fresh 'retvar))
(define (fresh-procvar) (fresh 'proc))

(define (constraint:make-require left right)
  (constraint:make left *requires* right))

(define (constraint:make-permit left right)
  (constraint:make left *permits* right))

(define (constraint:make-equal left right)
  (constraint:make left *equals* right))
