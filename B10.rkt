#lang racket



;For exercise 1 the make-hydra procedure should probably be used the same as make-account procedure
; when new heads appear perhaps just use append to add the new head, maybe use a list of pairs with a taggedlist of 'original head or 'grown head
; this to be able to differenciate in the growth process of the heads depending on where we cut
; or a tagged list with a number to know what to subtract n with, so if a head with the n-1 heads is slain then that head needs to be tagged with 1 or '1


;exercise 1
(define (makeHydra n)
  (if (<= n 0)
      '()
      (cons n (makeHydra (- n 1)))))

;alternative method
(define (make-hydra n)
  (for/list ([x (reverse (range 1 (+ 1 n)))])x))

(make-hydra 5)
(makeHydra 5)




;exercise 4

(define (funcList n)
  (for/list ([i (range n)]) ; makes  a list where the range is 0 to n-1
    (lambda (x) (+ x i)))) ;each element is basically a function where it waits for the second argument


((list-ref (funcList 5) 0) 7) ; Output: 7
((list-ref (funcList 5) 2) 7) ; Output: 9


;exericse 5

(define (diffPair xs ys)
  (for/list ([x xs] [y ys]
                    #:when (not (= x y)))
    (cons x y)))
(diffPair '(1 2 3 4 5) '(1 2 9 4 5))

(define (inBoth xs ys)
  (for/list ([x xs]
             #:when (for/list ([y ys] #:when (= x y)) y))
    x))


(inBoth '(1 2 3 4 5) '(4 2 9 1 3))




;possible solutions for exercise 5 a
; just iterate through both lists and compare the car of each if they are eq?, if yes add it to acc if not then do cdr without adding it to acc

;Possible solutions for exercise 5 b
; sort the lists first then have to conditions one that checks if it is greater than and one if it is equal
; if the element on the right is greater than the element on the left list then the element on the left skip one step forward, otherwise if it is equal add it
; to the new list of what is in common
