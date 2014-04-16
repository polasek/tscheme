;;;;                Monads in Scheme

;;; Monads are a set of elegant design patterns
;;; for imposing predictable order in a pure
;;; functional program.  (CPS is another way to
;;; accomplish this goal.)  One may need to impose
;;; a predictable order to operate I/O or to
;;; simulate state with a functional "store"
;;; carried around in the program.

;;; Monads also provide plumbing for carrying the
;;; store in a way that does not impose itself on
;;; the user's code, so that the result looks very
;;; much like a stateful program.

;;; A traditional example is to produce a binary
;;; tree isomorphic to a given one, but with the
;;; leaves replaced by a sequence of numbers in a
;;; left-to-right depth-first order.  Here I
;;; investigate various ways to do this, starting
;;; with the most traditional use of assignment on
;;; a free variable.

(define make-tree cons)
(define left car)
(define right cdr)
(define (terminal? x) (not (pair? x)))

;;; Statefully, with a free variable.  This is the
;;; "natural" way to do the job in an imperative
;;; language.

(define (enumerate-tree tree)
  (let ((n 0))
    (define (walk tree)
      (if (terminal? tree)
	  (let ((old-n n))
	    (set! n (+ n 1))
	    old-n)
	  (let* ((lt (walk (left tree))) 
		 (rt (walk (right tree))))
	    (make-tree lt rt))))
    (walk tree)))

#|
(enumerate-tree '(a (b . c) . d))
;Value: (0 (1 . 2) . 3)
|#

;;; Alternatively, one can use an auxiliary data
;;; structure to carry the state.  This requires
;;; constructing and destructuring at each step.
;;; (This can be prettified with pattern-matching
;;; destructuring.)

(define (enumerate-tree tree)
  (define (walk tree n)
    (if (terminal? tree)
	(cons n (+ n 1))
	(let* ((ltn (walk (left tree) n))
	       (lt (car ltn)) (ln (cdr ltn))
	       (rtn (walk (right tree) ln))
	       (rt (car rtn)) (rn (cdr rtn)))
	  (cons (make-tree lt rt) rn))))
  (car (walk tree 0)))

#|
(enumerate-tree '(a (b . c) . d))
;Value: (0 (1 . 2) . 3)
|#

;;; GJS's natural style, using continuation
;;; passing to eliminate the extra data structure.

(define (enumerate-tree tree)
  (define (walk tree n cont)
    (if (terminal? tree)
	(cont n (+ n 1))
	(walk (left tree)
	      n
	      (lambda (lt n)
		(walk (right tree)
		      n
		      (lambda (rt n)
			(cont (make-tree lt rt)
			      n)))))))
  (walk tree 0 (lambda (tree n) tree)))

#|
(enumerate-tree '(a (b . c) . d))
;Value: (0 (1 . 2) . 3)
|#

;;; Notice that we must carry around that ugly
;;; "n", the state that was so neatly packaged in
;;; the imperative version.  Is there some way we
;;; can retain the pure functional form but pipe
;;; it around, without having it clutter up our
;;; algorithm?  The answer is yes.  We set up a
;;; monad.  Let's first do it with the auxiliary
;;; data structure.

(define (return val)
  (lambda (state)
    (cons val state)))

(define (pipe first then)		;bind
  (lambda (state)
    (let* ((fvs (first state))
	   (first-val (car fvs))
	   (next-state (cdr fvs)))
      ((then first-val) next-state))))

(define (get)
  (lambda (state)
    (cons state state)))

(define (set new-state)
  (lambda (old-state)
    (cons #f new-state)))

(define (sequentially first then)
  (lambda (state)
    (let* ((fvs (first state))
	   (first-val (car fvs))
	   (next-state (cdr fvs)))
      (then next-state))))

(define (run c initial-state)
  (let* ((cvs (c initial-state))
	 (c-val (car cvs))
	 (next-state (cdr cvs)))
    c-val))

;;; And here is the enumerator using this
;;; plumbing.  Notice that we have gotten rid of
;;; that annoying intrusion of the state into the
;;; non-terminal case of the walker.

(define (enumerate-tree tree)
  (define (walk tree)
    (if (terminal? tree)
	(pipe (get)
	      (lambda (n)
		(sequentially (set (+ n 1))
			      (return n))))
	(pipe (walk (left tree))
	      (lambda (lt)
		(pipe (walk (right tree))
		      (lambda (rt)
			(return
			 (make-tree lt rt))))))))
  (run (walk tree) 0))

(enumerate-tree '(a (b . c) . d))
;Value: (0 (1 . 2) . 3)

;;; Alternatively, we can get rid of the explicit
;;; construction of the value and state, using a
;;; continuation function:

(define (return val)
  (lambda (state)
    (lambda (cont)
      (cont val state))))

(define (pipe first then)		;bind
  (lambda (state)
    ((first state)
     (lambda (first-val next-state)
       ((then first-val) next-state)))))

(define (get)
  (lambda (state)
    (lambda (cont)
      (cont state state))))

(define (set new-state)
  (lambda (old-state)
    (lambda (cont)
      (cont #f new-state))))

(define (sequentially first then)
  (lambda (state)
    ((first state)
     (lambda (first-val next-state)
       (then next-state)))))

(define (run c initial-state)
  ((c initial-state)
   (lambda (val next-state) val)))
