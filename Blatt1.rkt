;s1103032 Fares Albakri
#lang racket
;A1
(define a 3) ;a = 3

(define b (+ a 1)) ; b = a + 1 => b = 3 + 1 => 4

(+ a b (* a b)) ; a + b + (a * b ) => 3 + 4 + (3 * 4 ) => 19

(= a b) ;#f if (a ==b)

(if (and (> b a) (< b (* a b)))
    b 
    a)

#|
 if ( (b > a) && (b < (a *b)) {
  System.out.print(b)
} else System.out.print(a)
Console will print out: a => 4
|#

/

;(-4) ; gives out -4 as a number (false)

(- 4) ; subtracts 4 from nothing, gives out positive 4 (false)

-4 ;gives out -4 since it is already as simple as possible (cannot be
; broken down further

- 4 ;results in an error as it is a function call without () parenthesis (false)

(displayln "")

(displayln "exercise A1 other side")
(displayln "")

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))




#|
if (a == 4) System.out.print(6)
else if (b == 4) System.out.print(6 + 7 + a)
else System.out.print(25)
Console should give out 16
|#

 (+ 2 (if (> b a) b a)) ; basically a teritary operator 2 + (b > a) ? b : a 
; adds 2 and b => 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
; (a + 1) * b => 16

((if (< a b) + -) a b)
; a (a < b) ? + : - b => a + b => 7

(displayln "")

(displayln "exercise A2")
(displayln "")
;A2

(define (sign num)
  (cond ((> num 0) 1)
        ((= num 0) 0)
        ((< num 0) -1)))

;Test cases for exercise A2
(sign -2)
(sign -1)
(sign 0)
(sign 1)
(sign 2)


(displayln "")

(displayln "exercise A3")
(displayln "")

;A3
(define (square x) ; helper function
  (* x x))

(define (sumSquareBigger num1 num2 num3)
  (define largerNumber1 0)
  (define largerNumber2 0) 
  (if (and (>= num2 num1) (>= num2 num3))
    (begin
      (set! largerNumber1 num2)
      (if (>= num3 num1)
          (set! largerNumber2 num3)
          (set! largerNumber2 num1)))
    (begin
      (if (and (>= num3 num1) (>= num3 num2))
          (begin
            (set! largerNumber1 num3)
            (if (>= num1 num2)
                (set! largerNumber2 num1)
                (set! largerNumber2 num2)))
          (begin
            (set! largerNumber1 num1)
            (if (>= num2 num3)
                (set! largerNumber2 num2)
                (set! largerNumber2 num3))))))


  
(+ (square largerNumber1) (square largerNumber2)))

;Test Cases
(sumSquareBigger 5 12 8) ; expected result 208
(sumSquareBigger 100 50 75) ;expected result 15625
(sumSquareBigger 10 20 30) ;expected result 1300
(sumSquareBigger 15 15 10) ;expected result 450
(sumSquareBigger 0 -5 -10)

#|(define (sumSquareBigger num1 num2 num3)
  (define largerNumber1 0)
  (define largerNumber2 0) 
   ( if (and (>= num1 num2) (>= num1 num3))
     (set! largerNumber1 num1)
     ((if (>= num2 num3)
          (set! largerNumber2 num2)
          (set! largerNumber2 num3))))
  (+ (square largerNumber1) (square largerNumber2))
  )|#
     
  

(displayln "")

(displayln "exercise A4")
(displayln "")

;A4
  ;a
  (define (areaRect length width)
    ( if (and (>= length 0) (>= width 0)) 
    (* length width)
    (displayln "not a valid width or length")))
  ;b
  (define (circumferenceCircle radius)
    (if (>= radius 0)
    (* 2 pi radius)
    (displayln "not a valid radius")))

;Test cases
(areaRect 3 4) ;12
(areaRect 5 5) ;25
(areaRect 0 10) ;0
(areaRect -3 4) ; invalid

(circumferenceCircle 1) ; 6.28...
(circumferenceCircle 5) ; 31.4...
(circumferenceCircle 0) ;0 
(circumferenceCircle -3) ; invalid

;A5
(define (mynot x)
  (cond
    [(equal? x #f) #t] ;checks if x is equal to #f which is always false therefore the conditon never evaulates to true
    [else #f]))


;test cases
(mynot #f) ;#t
(mynot 5) ;#f
(mynot #t) ;#f

#|(define (mynot x)
  (cond
    [(equals? x x) (#f)]
    [else #t]))

; second attempt
  
  (define (mynot2 x)
  (if (equal? x #f)
      (#t)
      (#f)))
  |#

;A5 part b
(cond
  [(and (= 1 1) (> 1 2)) "With this and, both conditions have to be evaulated 1 == 1 && 1 > 2 and both need to evaluate to true for this to print"]
  [(and (= 1 1) (= 2 2)) "this will evaluate since both conditions are true"])

(if (or (= 1 1) (= 2 3))
     "this will short circuit and 2 == 3 will not even be evaluated since with or only one condition needs to  be true"
     ("false statement"))
(if (or (> 1 2) (> 2 3))
    "since both evaluate to false, this will not print"
    ("false"))
  