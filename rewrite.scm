;;;; Rewriter
;;; Translates a reasonable subset of Scheme into the subset accepted by
;;; get-constraints
;;;
;;; Warning: This is boring code!
;;;
;;; Note that we cannot safely reuse the boilerplate abstractions from
;;; get-constraints.scm, since those assume that the code has already been
;;; rewritten.  For example, here rw:lambda-body is defined as cddr, but in
;;; get-constraints lambda-body is defined as caddr, since we assume the bodies
;;; of lambdas are wrappen in a begin upon rewrite.

;;; Helpers for dispatching on the type of code fragment we see

(define (rw:query-name expr)
  (cadr expr))

(define (rw:query-item expr)
  (caddr expr))

(define (rw:make-query name item)
  (list 'query name item))

(define (rw:lambda? expr)
  (tagged-list? expr 'lambda))

(define (rw:lambda-arglist expr)
  (cadr expr))

(define (rw:lambda-body expr)
  (cddr expr))

(define (rw:make-lambda arglist body)
  (list 'lambda arglist body))

(define (rw:sequence->begin exprs)
  (cons 'begin exprs))

(define (rw:begin->sequence expr)
  (cdr expr))

(define (rw:define-lhs expr)
  (cadr expr))

(define (rw:define-rhs expr)
  (caddr expr))

(define (rw:define-tail expr)
  (cddr expr))

(define (rw:make-define lhs rhs)
  (list 'define lhs rhs))

(define (rw:application-operator expr)
  (car expr))

(define (rw:application-operands expr)
  (cdr expr))

(define (rw:make-application operator operands)
  (cons operator operands))

(define rw:rewrite
  (make-generic-operator 1
                         'rw:rewrite
                         (lambda (expr)
                           (rw:rewrite-application expr))))

(define (rw:rewrite-query expr)
  (rw:make-query
    (rw:query-name expr)
    (rw:rewrite (rw:query-item expr))))

(defhandler rw:rewrite rw:rewrite-query query?)

(define (rw:rewrite-self-quoting expr)
  expr)

(defhandler rw:rewrite rw:rewrite-self-quoting self-quoting?)

(define (rw:rewrite-quote expr)
  expr)

(defhandler rw:rewrite rw:rewrite-quote quote?)

(define (rw:rewrite-begin expr)
  (rw:sequence->begin
    (map rw:rewrite (rw:begin->sequence expr))))

(defhandler rw:rewrite rw:rewrite-begin begin?)

(define (rw:rewrite-lambda expr)
  (let ((arglist (rw:lambda-arglist expr))
        (body (rw:lambda-body expr)))
    (if (not (list? arglist))
      (error
        "We only support lambda expressions with a fixed number of arguments"))
    (rw:make-lambda arglist
                    (sequence->begin
                      (map rw:rewrite body)))))

(defhandler rw:rewrite rw:rewrite-lambda lambda?)

(define (rw:rewrite-define expr)
  (let ((lhs (rw:define-lhs expr)))
    (if (list? lhs)
      (let ((opname (car lhs))
            (arglist (cdr lhs)))
        (rw:make-define
          opname
          (rw:make-lambda arglist
                       (rw:rewrite
                         (rw:sequence->begin (rw:define-tail expr))))))
      (rw:make-define lhs
                      (rw:rewrite (rw:define-rhs expr))))))

(defhandler rw:rewrite rw:rewrite-define define?)

(define (rw:rewrite-variable expr)
  expr)

(defhandler rw:rewrite rw:rewrite-variable variable?)

(define (rw:rewrite-application expr)
  (rw:make-application
    (rw:rewrite (rw:application-operator expr))
    (map rw:rewrite (rw:application-operands expr))))
  


