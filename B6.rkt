#lang racket

;Aufgabe 1

;Accumlate can do it for nested lists compared to foldr which cannot
(define (mycount mynode)
  (if (not(null? mynode))
      1
      0))
(define (mysum mynode)
  mynode)


(define (accumulate-tree tree term op init)
  (cond ((null? tree) init)
        ((not (pair? tree)) (term tree))
        (else (op (accumulate-tree (car tree) term op init)
                  (accumulate-tree (cdr tree) term op init)))))


;(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))


#|
(define (make-tree list)
  (define (count)
    (accumulate-tree list (lambda (list) (if (not(null? list))
                                             1
                                             0)) + 0))
  (define (sum)
    (accumulate-tree list (lambda (list) (car list)) + 0))

  (define (dispatch m)
    (cond ((eq? m 'count) count)
          ((eq? m 'sum) sum)
          (else (error "nah wrong command" m))))
  dispatch)
  |#




;(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;(count t) ;expected 7
;(sum t) ;expected 28

;(define A (make-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))



;Aufgabe 2
; try using the accumlate-tree and remove term and instead of op use append
;

(define (leaflist tree)
  (cond ((empty? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (leaflist(car tree))
              (leaflist (cdr tree))))))
  
(define mytree (list (list 1 2) (list 3 4)))
(leaflist (list mytree mytree))

;alternative way for aufgabe 2

(define (fringe tree)
  (accumulate-tree tree (lambda (x) (list x)) append null))




;Aufgabe 3
#|
(define (mymap p xs)
  (foldr (lambda (x y) (cons (p x) y)) null xs))

(define (myappend xs ys)
  (foldr cons ys xs))

(define (mylength xs)
  (foldr (ambda (x acc)(+ 1 acc)) 0 xs))
  |#



;Aufgabe 4
#|
(define (mean xs [accumulator 0])
  (if (empty? xs)
      accumulator
      (mean (rest xs) (+ accumulator (car xs))))
  (/ accumulator (length xs)))

|#



(define (mean2 xs [accumulator 0] [listLength 0])
  (if (empty? xs)
      (if (= listLength 0)
          0
          (/ accumulator listLength))
      (mean2 (rest xs) (+ accumulator (car xs)) (+ listLength 1))))

(mean2 '(1 2 3))
  


(define mylist '(1 2 -100 -999 1000))
(define test-data1 '( -999 10 20 -5 15 25 -999))
(define test-data2 '(10 -999 10 10 -999))
(define test-data3 '(0 0 0 -999 0 0 -999))
(define test-data4 '(5 10 -999 15 20 25 -999))


(displayln "rainfall here")

(mean2 (for/list ([x '(1 2 -100 -999 1000)]
                  #:break (= -999 x)
                  #:when (> x 0))
           
           
  x))



;Aufgabe 5

(define (mylength xs)
  (match xs
    ['() 0]
    [(cons head tail) (+ 1 (mylength tail))]))



(define (mylengthTail xs [accumulator 0])
  (match xs
    ['() accumulator]
    [(cons head tail) (mylengthTail tail (+ accumulator 1))]))





(displayln "I am here")
(mylengthTail '(1 2 3 4 5 6) )



(define (myfilter xs proc)
  (match xs
    ['() '()]
    [(cons head tail) (if (proc head)
                          (cons head (myfilter tail proc))
                          (myfilter tail proc))]))


(myfilter '(1 2 3 4 5 6) even?)
(myfilter '(0 1 2 3 4 5 6) null?)







