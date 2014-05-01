(load "constraints")

(define (substitute old new constraints env)
  (let lp ((constraints constraints))
    (if (null? constraints) '()
      (let* ((c (car constraints))
             (new-c (cond ((equal? (constraint:left c) old)
                           (constraint:make
                             new (constraint:relation c) (constraint:right c)))
                          ((equal? (constraint:right c) old)
                           (constraint:make
                             (constraint:left c) (constraint:relation c) new))
                          (else c))))
        (if (check-constraint new-c)
            (cons new-c (lp (cdr constraints)))
            (lp (cdr constraints)))))))

(define (check-constraint c)
  (if (not (constraint? c)) #f
    #t)) ; TODO

(define (printable-type v)
  (cond ((type? v) (list 'type (type:name v)))
        ((singleton? v) (list 'singleton (singleton:val v)))
        ((type-variable? v) (list 'type-var (var:name v)))
        (else 'bad-value)))

(define (print-constraint c)
  (cond ((pair? c) (begin
                     (print-constraint (car c))
                     (print-constraint (cdr c))))
        ((constraint? c) (pp (list
                               (printable-type (constraint:left c))
                               (constraint:relation c)
                               (printable-type (constraint:right c)))))
        ((null? c) (newline))
        (else (pp 'not-a-constraint))))

(define (test)
  (let* ((x (fresh))
         (y (fresh))
         (z (fresh))
         (cs (list (constraint:make-require x *string*)
                   (constraint:make-equal x y)
                   (constraint:make-require z x)
                   (constraint:make-permit  y x)
                   (constraint:make-permit *number* z)
                   (constraint:make-require y *symbol*))))
    (pp 'before)
    (print-constraint cs)
    (pp 'after)
    (print-constraint (merge-types x y cs))))

(test)
