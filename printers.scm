;;; Helpers for printing things in a human-readable format

;; Constraints with negative ids are built-in
(define (built-in-constraint? item)
  (and (constraint? item)
       (every negative? (finite-set-elts (constraint:ids item)))))

(define (not-built-in-constraint? item)
  (not (built-in-constraint? item)))


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
  
  (display ")")
  
  (let ((left-annot (constraint:left-annot constraint))
        (right-annot (constraint:right-annot constraint))
        (usercode (constraint:usercode constraint)))
    (if (not (default-object? left-annot))
      (begin
        (display " LEFT ")
        (write left-annot)))

    (if (not (default-object? right-annot))
      (begin
        (display " RIGHT ")
        (write right-annot)))
    
    (if (not (default-object? usercode))
      (begin
        (display " FROM CODE ")
        (write usercode)))))

(define (print-constraint constraint)
  (display-constraint constraint)
  (newline))


(define (print-type t)
  (write
    (filter (lambda (elt)
              (not (and
                     (list? elt)
                     (pair? (cdr elt))
                     (eq? (cadr elt) *none*))))
              (cdr (record->list t)))))

(define (print-substitution s)
  (write (sub:old s))
  (display " -> ")
  (write (sub:new s))
  (display " ")
  (write (finite-set-elts (sub:ids s)))
  (newline))

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
          ((type? y) (display prefix) (print-type y) (newline))
          (else (display prefix) (write-line y))))
  (print-recursive-with-prefix x ""))


