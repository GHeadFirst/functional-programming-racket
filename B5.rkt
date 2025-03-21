#lang racket

;Aufgabe 1

#|
(define (checkProp predicate xs)

  (cond
    [(null? xs) (xs)]
    [(even? (car xs)) (checkProp even? (cons (#t) (cons xs)))]
    [else (checkProp even? (cons #f (cons xs)))]))
 

(define (checkProp predicate xs)

  (define (checkProp-helper predicate xs oldList)
    (cond
      [(null? xs) (xs)]
      [(even? (car xs)) (checkProp-helper even? (cons #t (rest car (xs))))]
      [else ]))
  (checkProp-helper even? xs xs))
 |#

(displayln "")
(displayln "Start of Aufgabe 1")
(displayln "")

(define (checkProp2 predicate xs)

  (if (null? xs)
      xs
      (cons (if (predicate (car xs))
                #t
                #f)
            (checkProp2 predicate (rest xs)))))

(define (checkProp3 predicate xs)
  (if (null? xs)
      xs
      (cons (predicate (car xs))
            (checkProp3 predicate (cdr xs)))))
  

(define (checkPropOneLine predicate xs) ;can be done with map too
  (map predicate xs))

;Test cases Aufgabe 1
(checkProp2 even? (list 1 2 3 4)) ; expected '(#f #t #f #t)
(checkProp3 even? (list 1 2 3 4)) ; expected '(#f #t #f #t)
(checkPropOneLine even? (list 1 2 3 4)) ; expected '(#f #t #f #t)

(displayln "")
(displayln "End of Aufgabe1")
(displayln "")

(displayln "")
(displayln "Did not do Aufgabe 2")
(displayln "")


;Aufgabe 2

;Maybe try using fold here with the procedure instead of +
;try maybe foldr instead of foldl
;try fold two times

;required methods
(define (square x)
  (* x x))
(define (cube x)
  (* x x x))


; copied from Proseminar
(define el (list))

(define (doAll proc-1 it)
  (if (null? proc-1)
      el
      (append el (list ((car proc-1) it))
              (doAll (cdr proc-1) it))))

; copied from Proseminar
(define (doAll3 ops x)
  (if (null? ops)
      null
      (cons ((car ops) x) (doAll3 (cdr ops) x))))

; copied from Proseminar
(define (doAll2 list item)
  (map (lambda (x) (x item)) list))

;my attempt

#|
(define (doAll myProcedures items)
  (if (or (null? myProcedures) (null? items))
      '()
      (cons ((car myProcedures) (car items))
            (doAll (rest myProcedures) (rest items)))))

;(define (doAll2 myProcedures items)


(define (doAll2 myProcedures items)
  (if (or (null? myProcedures) (empty? myProcedures))
      items
      (void))
  (if (list? items)
      
      (if (or (null? myProcedures) (null? items))
          '()
          (map (car myProcedures) items))
      (cons ((car myProcedures) items)
            (doAll2 (rest myProcedures) items))))
  
|#

;(doAll2 (list sqrt square cube) 4)
;(doAll2 (list length car cdr) (list 1 2 3))

(displayln "")
(displayln "End of Aufgabe 2")
(displayln "")



(displayln "")
(displayln "Start of Aufgabe 3 teil A")
(displayln "")

;Aufgabe 3
;Part A
(define (toCelsius items)
  (map (lambda (item)
         (*(/ 5 9) (- item 32)))
       items))
  
(toCelsius (list -40 32 50)) ;expected (-40 0 10)

(displayln "")
(displayln "End of Aufgabe 3 teil A")
(displayln "")


(displayln "")
(displayln "Start of Aufgabe 3 teil B")
(displayln "")

;Part B

(define (myand a b)
  (and a b))


(define (checkAll items procedure)
  (foldr myand #t (map procedure items)))

(define (checkAll2 items procedure)
  (foldr (lambda (x y)
           (if (and x y) ; my if statement is not needed
               #t
               #f)) #t
                    (map procedure items)))

;general solution to the problem
(define (checkAll4 items procedure)
  (foldr equal? (procedure (car items)) (map procedure items)))



;Other way to implement
(define (checkAll3 p list)
  (foldr (lambda (x y) (and x y)) #t (map p list)))

(checkAll (list 1 2 3 4) even?)
(checkAll (list 2 4 6 8) even?)
(checkAll2 (list 1 2 3 4) even?)
(checkAll2 (list 2 4 6 8) even?)
(displayln "checkAll4 tests")
(checkAll4 (list 2 4 6 8) even?) ;expected true
(checkAll4 (list 1 2 3 4) even?) ;expected false

 
(displayln "End of Augabe 3")


(displayln "")
(displayln "Start of Aufgabe 4 Teil A")
(displayln "")

;Aufgabe 4


;Part A

(define (greaterThan20 x)
  (if (> x 20)
      #t
      #f))
(define (numberToOne x)
  (expt x 0))

(define (countEvenGreater20 values)
  (foldr + 0 (map numberToOne (filter greaterThan20 values))))


(define (oneLineGreaterThan20 values)
  (foldr + 0 (map (lambda (x)
                    (expt x 0)) (filter (lambda (x)
                                          (if (> x 20)
                                              #t
                                              #f)) values))))

(countEvenGreater20 (list 1 4 22 24 26 33 -44)) ;expected 4 because there are 4 numbers greater than 20
(countEvenGreater20 (list 1 2 3 4 5 6 7)) ; expected 0
(oneLineGreaterThan20 (list 1 4 22 24 26 33 -44)) ;expected 4 because there are 4 numbers greater than 20
(oneLineGreaterThan20 (list 1 2 3 4 5 6 7)) ; expected 0

;Part B

(displayln "")
(displayln "Start of Aufgabe 4 teil B")
(displayln "")

(define (sumEvenGreater20 values)
  (foldr + 0 (filter (lambda (x)
                       (if (and (> x 20) (even? x))
                           #t
                           #f)) values)))

(sumEvenGreater20 (list 1 4 22 24 26 33 -44)) ;expected 72

(displayln "")
(displayln "End Of Augabe 4")
(displayln "")

;Aufgabe 05 Trying to have a list of the cube of numbers from 10 to 100 that are even

(displayln "")
(displayln "Start of Aufgabe 5")
(displayln "")

;(range 10 101 1 )
(filter even? (map cube (range 10 101 1)))

;With lambda expression
(filter even? (map (lambda (x)
  (* x x x)) (range 10 101 1)))














