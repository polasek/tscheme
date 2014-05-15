;;;; Examples
;;;
;;; Time to show off what our analyzer can do!

(define test1-should-fail
  '(begin
     (define x 4)
     (define y
       (if (query my_x x)
         "a"
         #\e))
     (+ y 7)
     (string-append y "e")))

(define analysis (tscheme:analyze test1-should-fail))

(car analysis)
;Value: failure

(print-recursive analysis)
;(
;   failure
;   branch4
;   (
;      CONSTRAINT[(-2)](string requires ((string all)))
;      CONSTRAINT[(17)](branch4 requires ((boolean all) (number all) (char all) (string all) (symbol all) (pair all) (procedure all))) FROM CODE (if (query my_x x) "a" #\e)
;      CONSTRAINT[(20)](branch4 requires (arg plus 0)) FROM CODE (+ y 7)
;      CONSTRAINT[(25)](branch4 requires (arg string-append 0)) FROM CODE (string-append y "e")
;   )
;)


(define test2-should-succeed
  '(begin
     (define x 4)
     (define y
       (if (query my_x x)
         "a"
         #\e))
     (string-append y "e")))

(define analysis (tscheme:analyze test2-should-succeed))

(car analysis)
;Value: ((my_x x1))

(query-lookup 'my_x analysis)
;(
;   x1
;   ((number (finite-set 4)))
;   (
;      CONSTRAINT[(28)](x1 equals ((number (finite-set 4)))) FROM CODE 4
;   )
;)


(define fact-test-should-succeed
  (rw:rewrite
    '(define (fact n)
       (if (< n 2)
           n
           (* n (fact (- n 1)))))))

;; Show rewriting
(pp fact-test-should-succeed)
;(define fact
;  (lambda (n)
;    (begin (if (< n 2) n (* n (fact (- n 1)))))))

(define analysis (tscheme:analyze fact-test-should-succeed))

analysis
; Gives a bunch of deductions


(define fact-test-should-fail
  (rw:rewrite
    '(define (fact n)
       (if (< n "2")
           n
           (* n (fact (- n 1)))))))

;; Show rewriting
(pp fact-test-should-fail)
;(define fact
;  (lambda (n)
;    (begin (if (< n "2") n (* n (fact (- n 1)))))))

(define analysis (tscheme:analyze fact-test-should-fail))

(car analysis)
;Value: failure


(define delayed-binding-test-succeed
  '(begin
     (define f (lambda () (query x x)))
     (define x 3)))

(define analysis (tscheme:analyze delayed-binding-test-succeed))

(query-lookup 'x analysis)
;(
;   x3
;   ((number (finite-set 3)))
;   (
;      CONSTRAINT[(3)](x4 equals ((number (finite-set 3)))) FROM CODE 3
;      CONSTRAINT[(4)](x4 equals x3) LEFT (delayed-binding-of x)
;   )
;)


(define shadowing-test-succeed
  '(begin
     (define g (lambda () (begin x)))
     (lambda (x)
       (+ x 5))
     (define x "a")
     (string-append (g) "e")))

(define analysis (tscheme:analyze shadowing-test-succeed))

analysis
; (() (a bunch of deductions))


;; This is pretty subtle.  Because x within g ends up getting bound to the
;; memory location that gets filled with "a", having (+ x 1) inside the body of
;; g is a problem, assuming the programmer expects to be able to ever run g.
(define shadowing-test-fail
  '(begin
     (define g (lambda () (+ x 1)))
     (lambda (x)
       (+ x 5))
     (define x "a")
     (string-append (g) "e")))

(define analysis (tscheme:analyze shadowing-test-fail))

analysis
; (failure x4 ([constraint] ...))


(define mutual-recursion-test-succeed
  '(begin
     (define f (lambda (x)
                 (begin
                   (g "a")
                   (+ x 7))))
     (define g (lambda (y)
                 (begin
                   (f 4))))))

(define analysis (tscheme:analyze mutual-recursion-test-succeed))
;; Succeeds

(define mutual-recursion-test-fail
  '(begin
     (define f (lambda (x)
                 (begin
                   (g "a")
                   (+ x 7))))
     (define g (lambda (y)
                 (begin
                   (f y))))))

(define analysis (tscheme:analyze mutual-recursion-test-fail))
;; Failure: g has to able to accept "a", so f has to be able to accept "a", but
;; it doesn't

