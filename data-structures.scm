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

(define-record-type type
  (type:make boolean number char string symbol pair procedure)
  type?
  (boolean    type:boolean)
  (number     type:number)
  (char       type:char)
  (string     type:string)
  (symbol     type:symbol)
  (pair       type:pair)
  (procedure  type:procedure))

;;A generic map over any record type
;;Dangerous and order-dependent, but compact
(define (record-map ordered-accessors)
  (lambda (f . types)
    (apply type:make
           (map (lambda (acc)
                  (apply f (map acc types))) ordered-accessors))))

(define (record-tagged-map ordered-accessors)
  (lambda (f . types)
    (apply type:make
           (map (lambda (acc)
                  (apply f (cons acc (map acc types)))) ordered-accessors))))

(define type:accessors
  (list type:boolean type:number type:char type:string
        type:symbol  type:pair   type:procedure))

(define type:accessors-proc
  (list type:boolean type:number type:char type:string
        type:symbol  type:pair))

(define (type:empty? type)
  (for-all?
    (map (lambda (acc) (acc type)) type:accessors)
    (lambda (t) (eq? t *none*))))

(define type:map (record-map type:accessors))
(define type:tagged-map (record-tagged-map type:accessors))

;;I think the easiest way to have an equality over records is to define
;;a dynamic dispatch procedure over record and other types.
;;To distinguish this from standard scheme equality, we use tscheme:eq?
(define tscheme:equal?
  (make-generic-operator 2
                         'tscheme:equal?
                         equal?))
;;lists
(defhandler tscheme:equal? (lambda (l1 l2)
                             (or (and (null? l1) (null? l2))
                                 (and (pair? l1) (pair? l2)
                                      (tscheme:equal? (car l1) (car l2))
                                      (tscheme:equal? (cdr l1) (cdr l2)))))
  list? list?)

;;A generic defhandler for any record type. Could be implemented a tad
;;more efficiently (terminating when failure is detected), but that
;;sort of optimization is not really needed here.
(define (defhandler-tscheme:equal? accessors member?)
  (defhandler tscheme:equal? (lambda (t1 t2)                           
                               (for-all?
                                (map tscheme:equal?                               
                                     (map (lambda (acc) (acc t1)) accessors)
                                     (map (lambda (acc) (acc t2)) accessors))
                                identity))
    member? member?))

;;Define equal for the type record.
(defhandler-tscheme:equal? type:accessors type?)


#|
(pp (type:tagged-map list type:top (type:make-boolean)))
;; #[type 21]
;; (boolean (boolean all all))
;; (number (number all none))
;; (char (char all none))
;; (string (string all none))
;; (symbol (symbol all none))
;; (pair (pair all none))
;; (procedure (procedure all none))

(map type:boolean (list type:top (type:make-boolean)))
|#

(define *none* 'none)
(define *all*  'all)

(define (type:none? obj) (eqv? obj *none*))
(define (type:all?  obj) (eqv? obj *all*))

(define (make-finite-set% elts)
  (if (null? elts)
      *none*
      (cons 'finite-set elts)))

;; in other words,
(define (finite-set . elts)
  (make-finite-set% elts))

(define (type:finite-set? obj)
  (and (list? obj)
       (not (null? obj))
       (eqv? (car obj) 'finite-set)))

(define (finite-set-elts fs)
  (if (eq? fs *none*)
    '()
    (cdr fs)))

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

(define type:empty        (type:make *none* *none* *none* *none* *none* *none* *none*))
(define type:top          (type:make *all*  *all*  *all*  *all*  *all*  *all*  *all*))
(define (type:make-boolean) (type:make *all*  *none* *none* *none* *none* *none* *none*))
(define (type:make-number)  (type:make *none* *all*  *none* *none* *none* *none* *none*))
(define (type:make-char)    (type:make *none* *none* *all*  *none* *none* *none* *none*))
(define (type:make-string)  (type:make *none* *none* *none* *all*  *none* *none* *none*))
(define (type:make-symbol)  (type:make *none* *none* *none* *none* *all*  *none* *none*))
(define (type:make-pair)    (type:make *none* *none* *none* *none* *none* *all*  *none*))

(define (type:make-procedure ret-tv arg-tvs)
  (type:make *none* *none* *none* *none* *none* *none* (cons ret-tv arg-tvs)))

(define (type:make-any-procedure)
  (type:make *none* *none* *none* *none* *none* *none* *all*))

;;Creates a new type with elts (not necessarily of the same type) as finite-sets in the
;;appropriate fields of the type record
;;I don't think we ever want pairs or procedures to be parts of finite-list
(define type:predicates
  (list boolean? number? char? string? symbol? (lambda (x) #f) (lambda (x) #f)))
(define (type:finite-set . elts) 
  (if (not (null? (fold-right ;;Check that elements are of the correct types
                    (lambda (type-pred lst)
                      (list-transform-negative lst type-pred))
                    elts
                    type:predicates)))
    (error "Provided list contains elements that cannot be part of a finite set")
    (apply type:make ;The method itself; for each type, filter out the
           (map      ;corresponging elements, sort and deduplicate them.
            (lambda (type-pred)              
              (make-finite-set%
               (dedup (general-sort (list-transform-positive elts type-pred)))))
            type:predicates))))
#|
(pp (type:finite-set 'a 'b 9 'a 1 2 3 3 2 1 #f 32 1 2 3))
(pp (type:finite-set "a b"))
;; #[type 65]
;; (boolean (finite-set #f))
;; (number (finite-set 1 2 3 9 32))
;; (char none)
;; (string none)
;; (symbol (finite-set a b))
;; (pair none)
;; (procedure none)
|#

;;; Fresh type variables
(define **type-var-counter** 0)
(define (fresh #!optional prefix)
  (set! **type-var-counter** (+ **type-var-counter** 1))
  (symbol-append (if (equal? prefix #!default) 'x prefix)
                 **type-var-counter**))

(define (fresh-argvar) (fresh 'argvar))
(define (fresh-retvar) (fresh 'retvar))
(define (fresh-procvar) (fresh 'proc))
(define (fresh-branchvar) (fresh 'branch))

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


;;; Abstractions for dealing with ret and arg references
(define (return-type tv)
  (list 'ret tv))

(define (arg-of tv num)
  (list 'arg tv num))

(define (tv-ret? v) (and (pair? v) (eqv? (car v) 'ret)))
(define (tv-arg? v) (and (pair? v) (eqv? (car v) 'arg)))
(define (tv-proc-name v) (cadr v))
(define (tv-arg-num v) (caddr v))

(define (proc:get-ret proc) (car proc))
(define (proc:get-arg proc n) (list-ref proc (+ n 1)))

