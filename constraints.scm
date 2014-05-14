
;;; Description of the extra fields:
;;; * identifiers: a finite-set of numbers that serve as names for the
;;; constraint
;;; * usercode: the code that produced this constraint
;;; * left-annotation: snippet of user code (or some human-readable
;;; abbreviation thereof) representing left
;;; * right-annotation: same as left-annotation, but for right
(define-record-type constraint
    (constraint:make% left relation right identifiers
                      usercode left-annotation right-annotation)
    constraint?
    (left              constraint:left)
    (relation          constraint:relation)
    (right             constraint:right)
    (identifiers       constraint:ids     constraint:set-ids!)
    (usercode          constraint:usercode)
    (left-annotation   constraint:left-annot)
    (right-annotation  constraint:right-annot))

;;; Relation types

(define *equals*   'EQUALS)
(define *requires* 'REQUIRES)
(define *permits*  'PERMITS)

(define (equality? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *equals*)))

(define (requirement? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *requires*)))

(define (permission? constraint)
  (and (constraint? constraint)
       (eqv? (constraint:relation constraint) *permits*)))

;;; Constraint ids, making constraints

(define **constraint-counter** 0)

(define (new-constraint-id)
  (set! **constraint-counter** (+ **constraint-counter** 1))
  **constraint-counter**)

(define (constraint:make-with-ids left relation right identifiers
                         #!optional usercode left-annot right-annot)
  (constraint:make% left relation right
                    identifiers
                    usercode left-annot right-annot))

(define (constraint:make left relation right
                         #!optional usercode left-annot right-annot)
  (constraint:make-with-ids left relation right
                            (finite-set (new-constraint-id))
                            usercode left-annot right-annot))

(define (constraint:make-equal left right
                               #!optional usercode left-annot right-annot)
  (constraint:make left *equals* right
                   usercode left-annot right-annot))

(define (constraint:make-require left right
                               #!optional usercode left-annot right-annot)
  (constraint:make left *requires* right
                   usercode left-annot right-annot))

(define (constraint:make-permit left right
                               #!optional usercode left-annot right-annot)
  (constraint:make left *permits* right
                   usercode left-annot right-annot))

