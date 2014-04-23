
(define (unify constraints)
  (error 'TODO))

(define-record-type type
    (type:make name)
    type?
    (name  type:name))

(define-record-type variable
    (var:make name)
    variable?
    (name  var:name))

(define-record-type argument
    (arg:make name number)
    argument?
    (name    arg:name)
    (number  arg:number))

(define-record-type constraint
    (constraint:make left relation right)
    constraint?
    (left      constraint:left)
    (relation  constraint:relation)
    (right     constraint:right))

(define *requires* 'REQUIRES)
(define *permits*  'PERMITS)

(define *boolean* (type:make 'boolean))
(define *number*  (type:make 'number))
(define *char*    (type:make 'char))
(define *string*  (type:make 'string))
(define *symbol*  (type:make 'symbol))
(define *pair*    (type:make 'pair))

(define (constraint:make-require left right)
  (constraint:make left *requires* right))

(define (constraint:make-permit left right)
  (constraint:make left *permits* right))

