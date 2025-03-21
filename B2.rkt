#lang racket
(require racket/trace)

;Aufgabe 1
#|
(define (squareroot x)
  (if (good-enough? guess x)
      guess
      (squareroot (improve guess x)
                x)))
(define (improve guess x)
  (average guess (/ x guess)))

(define (cube x)
  (cuberoot 1.0 x))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))
|#

(define (cuberoot x)
  (define (improve y)
    (/ (+ (/ x (* y y)) (* 2 y)) 3))
  
  (define (good-enough? y)
    (< (abs (- (* y y y) x)) 0.001))
  
  (define (cuberoot-iter y)
    (if (good-enough? y)
        y
        (cuberoot-iter (improve y))))
  
  (cuberoot-iter 1.0))

"-- Aufgabe 1 test cases --"
(displayln "")
; Test cases
(cuberoot 27) ; expected 3
(cuberoot 8) ; expected 2
 


;Aufgabe 2


;Aufgabe 3
(define (powerCloseTo b n)
  (define (smallestPowerGreaterThanN b n e)
    (if (> (expt b e) n)
        e
        (smallestPowerGreaterThanN b n (+ e 1))))
  
  (smallestPowerGreaterThanN b n 1))
(displayln "")
"-- Aufgabe 3 test cases --"
(displayln "")
;test cases 
(powerCloseTo 2 10) ; expected  4 (since 2^4 = 16 > 10)
(powerCloseTo 3 20) ;expected  3 (since 3^3 = 27 > 20)
(powerCloseTo 10 50) ;expected  2 (since 10^3 = 100 > 500)


;Aufgabe 4
(define (myif predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))
#| The reason is Lazy evaluation, as both are evaluated,
  therefore the newton example keeps on repeating similar to an
endless loop
|#

;aufgabe 5 (taken from  PS)

#|
(define (fib n)
  (fib-iter 1 n))
(define (fib-iter current n)
  (if (or (= n 1) (= n 0))
      current
      (fib-iter (* current n) (- n 1))))
|#

(define (fib n)
  (define (good-enough? count)
    (= count 0))
  (define (fib-iter a b count)
    (trace fib-iter)
    (if (good-enough? count)
        a
        (fib-iter b ( + a b) (- count 1))))
  (fib-iter 0 1 n))



