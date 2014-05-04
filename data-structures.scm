(define-record-type constraint
    (constraint:make left relation right)
    constraint?
    (left      constraint:left)
    (relation  constraint:relation)
    (right     constraint:right))

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

(define (record-equal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (record-equal? (car a) (car b))
              (record-equal? (cdr b) (cdr b))))
        ((and (constraint? a) (constraint? b))
         (and (record-equal? (constraint:left     a) (constraint:left b))
              (record-equal? (constraint:relation a) (constraint:relation b))
              (record-equal? (constraint:right    a) (constraint:right b))))
        ((and (type? a) (type? b))
         (and (record-equal? (type:boolean   a) (type:boolean b))
              (record-equal? (type:number    a) (type:number b))
              (record-equal? (type:char      a) (type:char b))
              (record-equal? (type:string    a) (type:string b))
              (record-equal? (type:symbol    a) (type:symbol b))
              (record-equal? (type:pair      a) (type:pair b))
              (record-equal? (type:procedure a) (type:procedure b))))
        (else (eqv? a b))))


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

(define (type:empty? type)
  (for-all?
    (map (lambda (acc) (acc type)) type:accessors)
    (lambda (t) (eq? t *none*))))

(define type:map (record-map type:accessors))
(define type:tagged-map (record-tagged-map type:accessors))

#|
(pp (type:tagged-map list type:top type:make-boolean))
;; #[type 21]
;; (boolean (boolean all all))
;; (number (number all none))
;; (char (char all none))
;; (string (string all none))
;; (symbol (symbol all none))
;; (pair (pair all none))
;; (procedure (procedure all none))

(map type:boolean (list type:top type:make-boolean))
|#

(define *none* 'none)
(define *all*  'all)

(define (type:none? obj) (eqv? obj *none*))
(define (type:all?  obj) (eqv? obj *all*))

(define (make-finite-set% elts)
  (if (null? elts)
      *none*
      (cons 'finite-set elts)))

(define (type:finite-set? ob)
  (and (list? obj)
       (not (null? obj))
       (eqv? (car obj) 'finite-set)))

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
(define type:make-boolean (type:make *all*  *none* *none* *none* *none* *none* *none*))
(define type:make-number  (type:make *none* *all*  *none* *none* *none* *none* *none*))
(define type:make-char    (type:make *none* *none* *all*  *none* *none* *none* *none*))
(define type:make-string  (type:make *none* *none* *none* *all*  *none* *none* *none*))
(define type:make-symbol  (type:make *none* *none* *none* *none* *all*  *none* *none*))
(define type:make-pair    (type:make *none* *none* *none* *none* *none* *all*  *none*))

(define (type:make-procedure ret-tv arg-tvs)
  (type:make *none* *none* *none* *none* *none* *none* (cons ret-tv arg-tvs)))

(define type:make-any-procedure
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

(define (constraint:make-equal left right)
  (constraint:make left *equals* right))

(define (constraint:make-require left right)
  (constraint:make left *requires* right))

(define (constraint:make-permit left right)
  (constraint:make left *permits* right))


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

