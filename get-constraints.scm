(load "ghelper")
(load "constraints")

;;; Temporary stuff, just so I can get print-outs
(define (singleton:make x)
  (list 'singleton x))
(define *procedure* 'procedure)
(define (tscheme:make-proc-type ret-tv arg-tvs)
  (cons 'procedure: (cons ret-tv arg-tvs)))

;;; Setup
(define *the-constraints* '())

(define *base-cvmap*
  '((+ plus)
    (string-append string-append)
    (- minus)))

(define (get-constraints-for expr)
  (fluid-let ((*the-constraints* '())
              (**type-var-counter** 0))
    (tscheme:process-expr expr *base-cvmap*)
    *the-constraints*))

(define (add-constraint constraint)
  (set! *the-constraints* (cons constraint *the-constraints*)))

(define-record-type tv&cvmap
    (tv&cvmap:make tv cvmap)
    tv&cvmap?
    (tv     tv&cvmap:tv     tv&cvmap:set-tv!)
    (cvmap  tv&cvmap:cvmap  tv&cvmap:set-cvmap!))

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



;;; Maybe lambdas should be rewritten with their internal names attached, e.g.
;;; (lambda_name1 (arg1 arg2) (begin body))
;(define (lambda? expr)
;  (and (list? expr)
;       (symbol? (car expr))
;       (let ((first (symbol->string (car expr))))
;        (and (>= (string-length first) 7)
;             (equal? (string-head first 7) "lambda_")))))
;
;(define (lambda-expr-name expr)
;  (string->symbol (string-tail (symbol->string (car expr)) 7)))

(define (tagged-list? expr tag)
  (and (list? expr)
       (not (null? expr))
       (symbol? (car expr))
       (eq? (car expr) tag)))

(define (lambda? expr)
  (tagged-list? expr 'lambda))

(define (lambda-arglist expr)
  (cadr expr))

(define (lambda-body expr)
  ;; We assume the body of lambdas is already wrapped in a begin
  (caddr expr))

(define (quote? expr)
  (tagged-list? expr 'quote))

(define (quoted-expr-contents expr)
  (cadr expr))

(define (begin? expr)
  (tagged-list? expr 'begin))

(define (begin-seq expr)
  (cdr expr))

(define (define? expr)
  (tagged-list? expr 'define))

(define (define-lhs expr)
  (cadr expr))

(define (define-rhs expr)
  (caddr expr))

(define (variable? expr)
  (symbol? expr))

(define (application-operator expr)
  (if (or (not (list? expr))
          (null? expr))
    (error "Bad application expression" expr)
    (car expr)))

(define (application-arglist expr)
  (cdr expr))

(define tscheme:process-expr
  (make-generic-operator 2
                         'tscheme:process-expr
                         (lambda (expr cvmap)
                           (tscheme:process-application expr cvmap))))

(define (tscheme:process-self-quoting expr cvmap)
  (let ((tv (fresh)))
   (add-constraint (constraint:make-equal tv
                                          (singleton:make expr)))
   (tv&cvmap:make tv cvmap)))

#|
(define x (tscheme:process-self-quoting 3 '()))

|#

(defhandler tscheme:process-expr tscheme:process-self-quoting number?)
(defhandler tscheme:process-expr tscheme:process-self-quoting string?)

(define (tscheme:process-lambda expr cvmap)
  (let ((lambda-tv (fresh-procvar))
        (ret-tv (fresh-retvar))
        (arg-tvs '())
        (outer-cvmap cvmap)    ; outer-cvmap will not be mutated
        (inner-cvmap cvmap))   ; inner-cvmap will get mutated (the pointer, not
                               ; the object itself)
    ;; Crappy way to do this, but I can't think of a better one right now
    (let lp ((remaining-args (lambda-arglist expr)))
     (if (null? remaining-args)
       #!unspecific
       (let ((arg-cv (car remaining-args))
             (arg-tv (fresh-argvar)))
         (set! arg-tvs (append (list arg-tv)
                               arg-tvs))
         (set! inner-cvmap (cvmap:bind inner-cvmap arg-cv arg-tv))
         (lp (cdr remaining-args)))))

     (add-constraint
       (constraint:make-equal lambda-tv
                              (tscheme:make-proc-type ret-tv arg-tvs)))

    ;; Recurse into the body, and say what we can about the return type
    (add-constraint
      (constraint:make-require
        ret-tv
        (tv&cvmap:tv (tscheme:process-expr (lambda-body expr) inner-cvmap))))

    ;; We use the outer cvmap because the bindings within the body of that
    ;; lambda are not relevant to code outside the body
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

(define (tscheme:process-define expr cvmap)
  (let ((tv&cvmap (tscheme:process-expr (define-rhs expr) cvmap)))
   (tv&cvmap:make
     (define-lhs expr)
     (cvmap:bind (tv&cvmap:cvmap tv&cvmap)
                 (define-lhs expr)
                 (tv&cvmap:tv tv&cvmap)))))

(defhandler tscheme:process-expr tscheme:process-define define?)

(define (tscheme:process-variable expr cvmap)
  (let ((tv (cvmap:lookup expr cvmap)))
   (tv&cvmap:make (if tv
                    tv
                    (error "Unknown code variable" expr))
                  cvmap)))

(defhandler tscheme:process-expr tscheme:process-variable variable?)

;;; Plan:
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
    (add-constraint
      (constraint:make-permit operator-tv *procedure*))
    (add-constraint
      (constraint:make-require return-tv (return-type operator-tv)))
    ;; The order in which we evaluate the arguments is unspecified, but we do
    ;; need to keep track of the cvmap throughout this process
    (let lp ((i 0)
             (remaining-args (application-arglist expr))
             (next-cvmap (tv&cvmap:cvmap operator-tv&cvmap)))
      (if (null? remaining-args)
        (tv&cvmap:make return-tv next-cvmap)
        (let* ((arg-value-expr (car remaining-args))
               (arg-value-tv&cvmap (tscheme:process-expr arg-value-expr cvmap))
               (arg-value-tv (tv&cvmap:tv arg-value-tv&cvmap))
               (arg-value-cvmap (tv&cvmap:cvmap arg-value-tv&cvmap))
               (arg-tv (arg-of operator-tv i)))
          (add-constraint
            (if (variable? arg-value-expr)
              (constraint:make-require arg-value-tv
                                       arg-tv)
              (constraint:make-permit arg-tv
                                      arg-value-tv)))
          (lp (+ i 1)
              (cdr remaining-args)
              arg-value-cvmap))))))
              

(define (return-type tv)
  (list 'ret tv))

(define (arg-of tv num)
  (list 'arg tv num))

(define (display-constraint constraint)
  (display "CONSTRAINT(")
  (display (constraint:left constraint))
  (display " ")
  (display (constraint:relation constraint))
  (display " ")
  (display (constraint:right constraint))
  (display ")"))

(define (print-constraint constraint)
  (display-constraint constraint)
  (newline))

#|
(print-constraint (constraint:make-require 'a 'b))
|#

(define (print-recursive x)
  (define (print-recursive-with-prefix y prefix)
    (cond ((list? y)
           (let ((new-prefix (string-append "   " prefix)))
            (display prefix)
            (display "(")
            (newline)
            (for-each (lambda (elt)
                        (print-recursive-with-prefix elt new-prefix))
                      y)
            (display prefix)
            (display ")")
            (newline)))
          ((constraint? y) (display prefix) (print-constraint y))
          (else (display prefix) (write-line y))))
  (print-recursive-with-prefix x ""))

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

|#
