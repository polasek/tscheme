(load "load-analyze")

(define analyze-type
  (make-generic-operator 2 'analyze-type
                         (lambda (exp env)
                           (cond ((application? exp)
				  (analyze-application exp env))
				 (else
				  (error "Unknown expression type"
					 exp env))))))

(defhandler analyze-type (lambda (x env) (list 'string  env)) string?)
(defhandler analyze-type (lambda (x env) (list 'number  env)) number?)
(defhandler analyze-type (lambda (x env) (list 'boolean env))
            (lambda (exp) (or (eq? exp #t) (eq? exp #f))))

(define (analyze-type-variable exp env)
