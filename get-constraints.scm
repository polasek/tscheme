(load "ghelper")
(load "constraints")

(define *procedure* 'procedure)
;; TODO have this actually be the right thing
(define (make-require typeref1 typeref2)
  (list typeref1 'requires typeref2))
(define (make-permit typeref1 typeref2)
  (list typeref1 'permits typeref2))

;;; Maybe lambdas should be rewritten with their internal names attached, e.g.
;;; (lambda_name1 (arg1 arg2) (begin body))
(define (lambda? expr)
  (and (list? expr)
       (symbol? (car expr))
       (let ((first (symbol->string (car expr))))
        (and (>= (string-length first) 7)
             (equal? (string-head first 7) "lambda_")))))

(define (lambda-expr-name expr)
  (string->symbol (string-tail (symbol->string (car expr)) 7)))

(define type-produced
  (make-generic-operator 1 'type-produced
                         (lambda (expr)
                           (type-produced-by-application expr))))

(define (make-singleton-type x)
  `(type one-of (,x)))

(define (make-named-lambda-type expr)
  `(type procedure ,(lambda-expr-name expr)))

(defhandler type-produced make-singleton-type number?)
(defhandler type-produced make-singleton-type string?)
(defhandler type-produced make-named-lambda-type lambda?)

(define (begin? expr)
  (and (list? expr)
       (> (length expr) 1)
       (eq? (car expr) 'begin)))

(define (type-produced-by-begin expr)
  (type-produced (last expr)))

(defhandler type-produced type-produced-by-begin begin?)

(define (type-produced-by-application expr)
  (if (symbol? (car expr))
    (return-type (car expr))
    (return-type (type-produced (car expr)))))

(define (analyze-application expr)
  (let ((operator-expr (car expr))
        (arg-exprs (cdr expr)))
    (let ((operator-type (type-produced operator-expr))
          (arg-types (map type-produced arg-exprs)))
      (let ((basic-constraints
              (apply list
                     (make-require operator-type *procedure*)
                     (let lp ((i 1)
                              (remaining arg-types)
                              (constraints '()))
                       (if (null? remaining)
                         constraints
                         (lp (+ i 1)
                             (cdr remaining)
                             (cons
                               (make-permit (arg-of operator-type i)
                                             (car remaining))
                               constraints)))))))
        ;; Now recurse into operator-expr, each argument-expr, and body-expr,
        ;; and take out constraints from there
        
        basic-constraints))))

(define (type? x)
  (and (list? x)
       (eq? (car x) 'type)))

(define (procedure-type? x)
  (and (type? x)
       (eq? (cadr x) 'procedure)))

(define (procedure-name x)
  (caddr x))

(define (return-type typeref)
  (if (type? typeref)
    (if (procedure-type? typeref)
      (list 'ret (procedure-name typeref))
      (error "Tried to get return type of something other than a procedure..."))
    ;; typeref is a reference to another typeref
    (list 'ret typeref)))

(define (arg-of typeref num)
  (if (procedure-type? typeref)
    (list 'arg (procedure-name typeref) num)
    (list 'arg typeref num)))
