;;; Helpers for printing things in a human-readable format

(define (display-constraint constraint)
  (display "CONSTRAINT[")
  (write (finite-set-elts (constraint:ids constraint)))
  (display "](")
  (write (constraint:left constraint))
  (display " ")
  (display (constraint:relation constraint))
  (display " ")
  (let ((right (constraint:right constraint)))
   (if (type? right)
     (print-type right)
     (write right)))
  (display ")"))

(define (print-constraint constraint)
  (display-constraint constraint)
  (newline))

#|
(print-constraint (constraint:make-require 'a 'b))
|#

(define (print-type t)
  (write
    (filter (lambda (elt)
              (not (and
                     (list? elt)
                     (pair? (cdr elt))
                     (eq? (cadr elt) *none*))))
              (cdr (record->list t)))))

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
          ((type? y) (display prefix) (print-type y))
          (else (display prefix) (write-line y))))
  (print-recursive-with-prefix x ""))
