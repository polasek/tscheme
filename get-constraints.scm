;;; Maybe lambdas should be rewritten with their internal names attached, e.g.
;;; (lambda_name1 (arg1 arg2) (begin body))
(define (lambda? expr)
  (and (list? expr)
       (symbol? (car expr))
       (let ((first (symbol->string (car expr))))
        (and (>= (string-length first) 7)
             (equal? (string-head first 7) "lambda_")))))

(define (analyze-application expr)
  (let ((operator-expr (car expr))
        (arg-exprs (cadr expr))
        (body (caddr expr)))
    (let ((operator-type (type-produced operator-expr))
          (arg-types (map type-produced arg-exprs))
          (body-type (type-produced body)))
      (let ((basic-constraints
              (apply list
                     (make-require operator-type *procedure*)
                     (make-require (return-type operator) body-type)
                     (let lp ((i 1)
                              (remaining arg-types)
                              (constraints '()))
                       (if (null? remaining)
                         constraints
                         (lp (+ i 1)
                             (cdr arg-types)
                             (cons
                               (make-require (arg-of operator 1)
                                             (type-produced (car arg-types)))
                               constraints)))))))
        ;; Now recurse into operator-expr, each argument-expr, and body-expr,
        ;; and take out constraints from there
             
