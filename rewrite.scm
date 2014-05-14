;;;; Rewriter
;;; Translates a reasonable subset of Scheme into the subset accepted by
;;; get-constraints
;;;
;;; Warning: This is boring code!
;;;
;;; Note that the abstractions here are conceptually distinct from those used
;;; by get-constraints, because the input language of the rewriter (a subset of
;;; Scheme) is conceptually distinct from the output language (a smaller subset
;;; of Scheme).  There are concrete places where this shows up, e.g.,
;;; rw:lambda-body is cddr, bin in get-constraints lambda-body is caddr, since
;;; in the output language, all bodies of lambdas are wrapped in a begin.
;;;
;;; So, some amount of redundant code here is appropriate, especially if we
;;; want to allow the author of the rewriter to not always worry that the
;;; author of get-constraints has changed something.


;;; Helpers / abstractions for dispatching on the type of code fragment we see

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

(define (rw:make-if pred consequent alternative)
  (list 'if pred consequent alternative))

(define (cond? expr)
  (tagged-list? expr 'cond))

(define (let? expr)
  (tagged-list? expr 'let))

;; We don't supported named let
(define (let-bindings expr)
  (cadr expr))

(define (let-body expr)
  (cddr expr))


;;; I tried to write my own version of cond->if, but it wasn't as nice as the
;;; one in ps04
(define (rw:cond->if cond-expr)
  (define (clauses cndl) (cdr cndl))
  (define (no-clauses? clauses) (null? clauses))
  (define (first-clause clauses) (car clauses))
  (define (rest-clauses clauses) (cdr clauses))
  (define (else-clause? clause) (eq? (predicate clause) 'else))
  (define (predicate clause) (car clause))
  (define (actions clause)

  (rw:sequence->begin (cdr clause)))
  (define (expand clauses)
    (cond ((no-clauses? clauses)
           '|#!unspecific|)
	  ((else-clause? (car clauses))
	   (if (no-clauses? (cdr clauses))
	       (actions (car clauses))
	       (error "else clause isn't last -- INTERP" cond-expr)))
	  (else
	   (rw:make-if (predicate (car clauses))
                       (actions (car clauses))
                       (expand (cdr clauses))))))
  (expand (clauses cond-expr)))

;;; Generic operation for rewriting pieces of code
(define rw:rewrite
  (make-generic-operator 1
                         'rw:rewrite
                         (lambda (expr)
                           (rw:rewrite-application expr))))

;;; The handlers for rewriting are very simple.  Usually they do little more
;;; than recurse into subexpressions.

(define (rw:rewrite-query expr)
  (rw:make-query
    (rw:query-name expr)
    (rw:rewrite (rw:query-item expr))))

(defhandler rw:rewrite rw:rewrite-query query?)

(define (rw:rewrite-self-quoting expr)
  expr)

(defhandler rw:rewrite rw:rewrite-self-quoting self-quoting?)

;; Quotes should not get rewritten
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

(define (rw:rewrite-cond expr)
  (rw:rewrite (rw:cond->if expr)))

(defhandler rw:rewrite rw:rewrite-cond cond?)

(define (rw:rewrite-let expr)
  (let ((bindings (let-bindings expr))
        (body (let-body expr)))
    (rw:make-application
      (rw:make-lambda
        (map car bindings)
        (rw:sequence->begin body))
      (map (lambda (binding)
             (rw:rewrite (cadr binding)))
           bindings))))

(defhandler rw:rewrite rw:rewrite-let let?)

(define (rw:rewrite-application expr)
  (rw:make-application
    (rw:rewrite (rw:application-operator expr))
    (map rw:rewrite (rw:application-operands expr))))
  


