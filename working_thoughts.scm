(f 1 2)

;;; As a side effect, produces some constraints:
;;; f REQUIRES procedure
;;; (arg f 1) PERMITS (singleton-type 1) ; {1}
;;; (arg f 2) PERMITS (singleton-type 2)

;;; Also produces a return Type_reference
;;; (return-type f)

;;; This is not a constraint per se, but if this block of
;;; code got embedded in some bigger block of code, e.g.

(g (f 1 2))

;;; Then we would get the constraint
;;; (arg g 1) PERMITS (return-type f)
;;;
;;; (When seeing "(return-type f)", the unifier will have to
;;; figure out what the unique identifier of the lambda
;;; referred to by f was.)
;;;
;;; "How do we deal with return types internally?"
;;; 
;;; They are specified by the type of the procedure.  So, e.g.,
;;; the Type procedure contains all procedures, and the type
;;; (procedure-returning t) contains all procedures
;;; guaranteed to return an element of t.  Then, for
;;; example, (procedure-returning t1) intersect
;;; (procedure-returning t2) = (procedure-returning (t1
;;; intersect t2)).
;;;
;;; This allows nesting for free: you get types like
;;; 
;;; (procedure-returning (procedure-returning number))
;;; 
;;; at no extra complexity cost.
;;;
;;; First milestone: exclude conditionals
;;; Include: defines, lambdas, applications.
