;;;; "Cvmap" -- A mapping of code variables to type variables.
;;; 
;;; This data structure is actually rather complicated.  Basically, it is a
;;; key-value store whose keys are variable names in the user's code and whose
;;; values are the type variables bound to them in the current scope of the
;;; user's code.  (tscheme:process-expr scans through the user code, and
;;; "current" refers to the current position of that scan.)
;;; 
;;; However, if we attempt to look up a code variable that is not bound, the
;;; behavior must be to wait for that code variable to be bound in the current
;;; or an enclosing scope.  To see why, consider the following code:
;;; 
;;; (define (f)
;;;   x)
;;; (define x 3)
;;; 
;;; Our constraint generator ought to know that x within the body of f will
;;; always be 3.  This is also enables our constraint generator to reason about
;;; recursive and mutually recursive procedures.
;;;
;;; The implementation strategy is as follows.  The underlying data structure
;;; of the cvmap is an association list.  When we look up a code variable (say
;;; cv) that is not bound, we allocate a fresh type variable (say fresh-tv) and
;;; mutate the cvmap by adding a "waiting marker" indicating that fresh-tv
;;; should be made EQUALS to whatever cv ends up getting bound to.  The waiting
;;; marker is itself an association (i.e., a two-element list), but this is
;;; just a hack.  The waiting marker contains the name of the code variable
;;; (cv), as well as a list of type variables that should be made EQUALS to
;;; whatever cv ends up getting bound to.  Initially this list consists of just
;;; fresh-tv, but if another unsuccessful lookup occurs, the second fresh type
;;; variable will just be tacked on to the list rather than generating a new
;;; waiting marker.
;;;
;;; When a code variable is bound to a type variable, we also check whether
;;; there is a waiting marker for that code variable above the most recent
;;; scope barrier (see the next paragraph).  If there is, we generate an EQUALS
;;; constraint for each type variable waiting to be identified with whatever cv
;;; gets bound to.
;;; 
;;; To handle scope and shadowing correctly, the cvmap must also contain
;;; information about scope.  When the constraint generator enters the body of
;;; a lambda expression, we expect it to push a "scope barrier" onto the cvmap.
;;; When it exits the body, it pops off the scope barrier.  At that point, all
;;; bindings that happened after the scope barrier was pushed are forgotten,
;;; but the waiting markers accumulated are remembered, and are merged into the
;;; waiting markers one level below.  This way, waiting markers propagate only
;;; downwards, and therefore fragments such as the following are handled
;;; correctly:
;;;
;;; (begin
;;;   (define (f) x)
;;;   (define x "a")
;;;   (lambda (x)
;;;     (+ x 3))
;;;   (string-append x "b"))
;;;
;;; The lookup and bind operations on cvmaps sometimes mutate them, but the
;;; only things that get added or removed by mutation are waiting markers.
;;; Thus, we can view cvmaps abstractly as mappings from code variable names to
;;; type variables (as long as we ignore waiting markers, which can be seen as
;;; some sort of under-the-hood thing).  The bind and lookup operations have
;;; the side effect of sometimes generating constraints.

(define (cvmap:make)
  *base-cvmap*)

;;; Just as with base-constraints, this catalog would need to be extended for
;;; use in production
(define *base-cvmap*
  '((+ plus)
    (- minus)
    (* times)
    (/ divide)
    (= integer-equal?)
    (< integer-less?)
    (> integer-greater?)
    (string-append string-append)
    (|#!unspecific| unspecific)))


;;; Binds code variable cv to type variable tv.  If there are type variables
;;; waiting to know the binding of cv (and their waiting-for records are above
;;; the most recent scope barrier, in order to handle shadowing), then we
;;; generate an EQUALS constraint equating all these waiting type variables to
;;; tv as well.  Once that's done, we delete the waiting-for marker.
(define (cvmap:bind cvmap cv tv)
  (let ((waiting-for (cvmap:find-waiting-marker-for-cv-above-barrier
                       cvmap cv)))
   (if waiting-for
     (begin
       (for-each (lambda (waiting-tv)
                   (add-constraint
                     (constraint:make-equal tv
                                            waiting-tv
                                            #!default
                                            `(delayed-binding-of ,cv))))
                 (cadr waiting-for))
       (cvmap:remove-waiting-marker-for-cv-above-barrier! cv cvmap))))

  (cons `(,cv ,tv) cvmap))


;;; Looks up the code variable cv in cvmap.  If no binding is found, allocates
;;; a fresh type variable and inserts a waiting marker indicating that the
;;; fresh type variable should be EQUALS to whatever cv ends up getting bound
;;; to.  Either way, returns a type variable.
(define (cvmap:lookup cv cvmap)
  (let ((result (assoc cv cvmap)))
   (if result
     (cadr result)
     (let ((fresh-tv (fresh)))
       (cvmap:wait-for! cvmap cv fresh-tv)
       fresh-tv))))

;;; Mutates the given list by consing on the given element
(define (cons! elt lst)
  (if (null? lst)
    (error "cons! doesn't work on the null list" elt))
  (let ((copy (list-copy lst)))
   (set-cdr! lst copy)
   (set-car! lst elt)))


;;; Scope barriers

(define (cvmap:make-scope-barrier)
  '(scope-barrier scope-barrier))

(define (cvmap:scope-barrier? thing)
  (equal? thing '(scope-barrier scope-barrier)))

(define (cvmap:push-scope-barrier cvmap)
  (cons (cvmap:make-scope-barrier) cvmap))

;;; Pops off a scope barrier, forgets about all the bindings above it, but does
;;; remember the waiting markers accumulated above it.  Merges them into the
;;; waiting markers one level below.
(define (cvmap:pop-scope-barrier cvmap)
  (define waiting-markers `(,(cvmap:make-sentinel-waiting-marker)))

  (define (merge-waiting-markers-into new-cvmap)
    (for-each (lambda (waiting-marker)
                (let ((cv (cadar waiting-marker)))
                 (for-each (lambda (tv)
                             (cvmap:wait-for! new-cvmap cv tv))
                           (cadr waiting-marker))))
              waiting-markers)
    new-cvmap)

  (let lp ((remaining cvmap))
   (cond ((null? remaining)
          (error "cvmap: tried to pop a scope barrier at the bottom scope"))
         ;; exit condition
         ((cvmap:scope-barrier? (car remaining))
          ;; Here (cdr remaining) is the stuff after the scope barrier
          (merge-waiting-markers-into (cdr remaining)))
         ((cvmap:waiting-marker? (car remaining))
          (cons! (car remaining) waiting-markers)
          (lp (cdr remaining)))
         (else
           (lp (cdr remaining))))))


;;; Waiting markers
;;;
;;; A waiting marker has the format ((waiting-for cv) (tv ...)).  It indicates
;;; that, once cv gets bound, each of the tv's should be made EQUALS to the
;;; type variable to which cv has been bound.  This format was chosen because
;;; it fits into an association list, and cvmaps are association lists.

(define (cvmap:waiting-marker-for? item cv)
  (and (list? item)
       (equal? (car item) `(waiting-for ,cv))))

(define (cvmap:waiting-marker? item)
  (and (list? item)
       (list? (car item))
       (eq? (caar item) 'waiting-for)))

(define (cvmap:make-waiting-marker-for cv tv)
  `((waiting-for ,cv) (,tv)))

(define (cvmap:add-tv-to-waiting-marker! tv waiting-marker)
  (set-car! (cdr waiting-marker)
            (cons tv (cadr waiting-marker))))

(define (cvmap:make-sentinel-waiting-marker)
  '((waiting-for sentinel) (sentinel)))

(define (cvmap:sentinel-waiting-marker? item)
  (and (cvmap:waiting-marker? item)
       (equal? (car item) '(waiting-for sentinel))))


;;; Inserting and removing waiting markers from cvmaps

;;; Mutates cvmap by adding a waiting marker.  Or, if a waiting marker for cv
;;; already exists above the most recent scope barrier, merges in the new tv's
;;; to the existing waiting marker.
(define (cvmap:wait-for! cvmap cv tv)
  (let ((waiting-marker (cvmap:find-waiting-marker-for-cv-above-barrier
                          cvmap cv)))
    (if waiting-marker
      (cvmap:add-tv-to-waiting-marker! tv waiting-marker)
      (cons! (cvmap:make-waiting-marker-for cv tv)
             cvmap))))

;;; Searches for a waiting marker for cv above the most recent scope barrier.
;;; Returns #f if none exists.
(define (cvmap:find-waiting-marker-for-cv-above-barrier cvmap cv)
  (let lp ((remaining cvmap))
   (cond ((or (null? remaining)
              (cvmap:scope-barrier? (car remaining)))
          #f)
         ((cvmap:waiting-marker-for? (car remaining) cv)
          (car remaining))
         (else
           (lp (cdr remaining))))))

;;; Removes the first (and, in our imagined use case, only) waiting marker for
;;; cv that occurs above the most recent scope barrier.  Actually, by "removes"
;;; we mean "replaces with a dummy sentinel waiting marker."
(define (cvmap:remove-waiting-marker-for-cv-above-barrier! cv cvmap)
  (let lp ((remaining cvmap))
   (cond
     ((or (null? remaining)
          (cvmap:scope-barrier? (car remaining)))
      (error
        "Could not find waiting marker above the barrier (wanted to delete)"))
     ((cvmap:waiting-marker-for? (car remaining) cv)
      ;; Replace (car remaining) with a sentinel waiting marker
      (set-car! remaining (cvmap:make-sentinel-waiting-marker)))
     (else
       (lp (cdr remaining))))))



