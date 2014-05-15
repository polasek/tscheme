
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


