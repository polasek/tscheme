(load "ghelper")
(load "constraints")

(define *the-constraints* '())

(define (add-constraint constraint)
  (set! *the-constraints* (cons constraint *the-constraints*)))

(define-record-type tv&cvmap
    (tv&cvmap:make tv cvmap)
    tv&cvmap?
    (tv     tv&cvmap:tv     tv&cvmap:set-tv!)
    (cvmap  tv&cvmap:cvmap  tv&cvmap:set-cvmap!))

(define (cvmap:make)
  '())

(define (cvmap:bind cvmap key val)
  (cons `(,key ,val) cvmap))

(define (cvmap:lookup cvmap key)
  (assoc key cvmap))



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
  symbol? expr)

(define (application-operator expr)
  (if (or (not (list? expr))
          (null? expr))
    (error "Bad application expression" expr)
    (car expr)))

(define (application-arglist expr)
  (cdr expr))

(define tscheme:process-expr
  (make-generic-operator 2
                         'type-produced
                         (lambda (expr cvmap)
                           (tscheme:process-application expr cvmap))))

(define (tscheme:process-self-quoting expr cvmap)
  (let ((tv (fresh)))
   (add-constraint (constraint:make-equal tv
                                          (singleton:make expr)))
   (tv&cvmap:make tv cvmap)))

(defhandler tscheme:process-expr tscheme:process-self-quoting number?)
(defhandler tscheme:process-expr tscheme:process-self-quoting string?)

(define (tscheme:process-lambda expr cvmap)
  (let ((lambda-tv (fresh-procvar))
        (ret-tv (fresh-retvar))
        (arg-tvs '())
        (inner-cvmap cvmap))
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
    (tv&cvmap:make tv inner-cvmap)))
  
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
  (let ((tv&cvmap (tscheme:process-expr (define-rhs expr))))
   (tv&cvmap:bind (define-lhs expr)
                  (tv&cvmap:tv tv&cvmap))))

(defhandler tscheme:process-expr tscheme:process-define define?)

(define (tscheme:process-variable expr cvmap)
  (let ((tv (cvmap:lookup expr)))
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
           (tscheme:process-expr (application-operator expr)))
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
            (if (variable? arg)
              (constraint:make-require arg-value-tv
                                       arg-tv)
              (constraint:make-permit arg-tv
                                      arg-value-tv)))
          (lp (+ i 1)
              (cdr remaining-args)
              (arg-value-cvmap)))))))
              

(define (return-type tv)
  (list 'ret tv))

(define (arg-of tv num)
  (list 'arg typeref num))
