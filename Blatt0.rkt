;s1103032 Albakri Fares
#lang racket

#|(define c (circle 10))
(define r (rectangle 10 20))

(define (square n)
; A semi-colon starts a line comment
;The expression below is the function body.
(filled-rectangle n n))
|#

;Aufgabe A2 C
#|
Racket has two distinguished constants to represent boolean values:
#t for true and #f for false. Uppercase #T and #F are parsed
 as the same values, but the lowercase forms are preferred.

The boolean? procedure recognizes the two boolean constants.
In the result of a test expression for if, cond, and, or, etc.
, however, any value other than #f counts as true.

Examples:

    > (= 2 (+ 1 1))

    #t
    > (boolean? #t)

    #t
    > (boolean? #f)

    #t
    > (boolean? "no")

    #f
    > (if "no" 1 0)

    1
|#

;Aufgabe A3 answer is 43
(+ (* 2 (+ 11 9) -4 5) ; gives out -77
 23)
(+ 23 (* 2 (- (+ 11 9) 4) 5)) ; gives out 183
(define x (+ 11 9))
(+ 23 (* 2 x -4 5)) ; gives out -777
(+  (* 2 -4 5 (+ 11 9)) 23) ; gives out -777
(define y (= 2))
(define z (* -4 5))
(+ (* (* 2 (+ 11 9)) (* -4 5) ) 23 )

(+ (- (* 2 (+ 11 9)) (* 4 5) )    23) ; correct one

;aufgabe 4
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(fact 5)

;aufgabe 5
(define (isEven m)
  (if (= (modulo m 2) 0)
      #t
      #f))
  
    
      
      