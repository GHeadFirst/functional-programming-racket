; G1_B8_Albakri_s1103032
#lang racket


(require racket/stream)
;Procedures stolen from Lectures
;special-form!
(define-syntax-rule (s-delay exp)
  (λ() exp))

(define (s-force delayedObject)
  (delayedObject))

(define empty-s 'S-EMPTY-STREAM)     ;;a symbol that represents the empty stream (could also be defined as null (as in the MIT Scheme implementation [SICP]))

(define (s-empty? s)
  (eq? s empty-s))

;special-form!
(define-syntax-rule (s-cons a b)
  (cons a (s-delay b))) 

(define (s-first s)
  (car s))

(define (s-rest s)
  (s-force (cdr s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;list-ref
(define (s-ref s n)
  (if (= n 0)
      (s-first s)
      (s-ref (s-rest s) (- n 1))))

;map
(define (s-map proc s)
  (if (s-empty? s)
      empty-s
      (s-cons (proc (s-first s)) (s-map proc (s-rest s)))))

;filter
(define (s-filter p s)
  (cond ((s-empty? s) empty-s)
        ((p (s-first s))
         (s-cons (s-first s)
               (s-filter p (s-rest s))))
        (else (s-filter p (s-rest s)))))


;range (enumerate)
(define (s-range low high)
  (if (>= low high)
      empty-s
      (s-cons
       low
       (s-range (+ low 1) high))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (s-display s)
  (if (s-empty? s)
      ""
      (~a (s-first s) "," (s-display (s-rest s)))))

(define (s-display-limit s limit)
  (if (or (= limit 0) (s-empty? s))
      "...?"
      (~a (s-first s) "," (s-display-limit (s-rest s) (- limit 1)))))

(define (s2list stream)
  (if (s-empty? stream)
         '()
         (cons (s-first stream) (s2list (s-rest stream)))))


           




;Aufgabe 1



(define (mySum s)
  (define (myHelperFunction s sum)
    (s-cons sum (myHelperFunction (s-rest s) (+ sum (s-first s)))))
  (myHelperFunction (s-rest s) (s-first s)))

(define (mySum2 s)
  (define (helper-function s previous-element)
    (if (s-empty? s)
        empty-s
        (let ((current-element (s-first s)))
          (s-cons (+ previous-element current-element)
                  (helper-function (s-rest s) (+ previous-element current-element))))))
  (helper-function s 0))

(define (naturals n)
  (s-cons n (naturals (+ n 1))))


(displayln "I am here")
(define naturalsWithout0 (naturals 1))

(mySum2 naturalsWithout0)
(s-display-limit (mySum2 naturalsWithout0) 5)

(s-display-limit (mySum naturalsWithout0) 5)



;Aufgabe 2

#|
2. Use beta reduction to evaluate (λx.λy.λz. - (+ x y) z) 1 2 3
Also write down the intermediate steps.

λx. x + 1 in racket is (lambda (x) (+ x 1))

|#

;(λx.λy.λz. - (+ x y) z) 1 2 3
; will be evaluated as follows
;(λ1.λy.λz. - (+ 1 y) z) 2 3
;(λ1.λ2.λz. - (+ 1 2) z) 3
;(λ1.λ2.λ3. - (+ 1 2) 3)
; - (3) 3)
; - 3 3
; 0




#|
3. Given the following lambda expression: (λx.λy.(λx. + x x) y) 5 m

a) As a first step: perform a useful alpha conversion.
b) As a second step: Show that its normal form is + m m
|#


;Part A
;(λx.λy.(λx. + x x) y) 5 m
;Renaming of the inner inner lambda function to lambda z
;(λx.λy.(λz. + z z) y) 5 m

;Part B
;(λx.λy.(λz. + z z) y) 5 m
;(λ5.λy.(λz. + z z) y) m
;(λy.(λz. + z z) y) m
;(λm.(λz. + z z) m)
;(λz. + m m)
;(+ m m)


;Aufgabe 4
(define (g x y)
  (define (f x) (* x 2))
  (- x (f y)))

;(λx.λy. - x (λf.(f y))(* f 2)
;(λx.λy. - x (λf. (* f 2)) y) 7 3 

(g 7 3)

(lambda (x y)
  (define (f x)
    (* x 2))
  (- x (f y)))

((lambda (x y)
  (define (f x)
    (* x 2))
  (- x (f y))) 7 3)

;(λx.λy. - x (λf.(f y) ))(* f 2)
; (λx.λf. - x (λf.(f y) ))(* f 2) 7 3
; (λ7.λf. - 7 (λf.(f y) ))(* f 2) 3
; (λ7.λ3. - 7 (* 3 2)
;- 7 6
;1


; or this one
;;(λx.λy. - x (λf. (* f 2)) y) 7 3
;(λ7.λy. - 7 (λf. (* f 2)) y) 3
;(λy. - 7 (λf. (* f 2)) y) 3
;(λ3. - 7 (λf. (* f 2)) 3)
;(λ3. - 7 (λf. (* f 2)) 3
; - 7 (λ3. (* 3 2)) 3
; - 7 6
;1

;Aufgabe 5
;;;;; Currying

(define (add2 x y)
  (+ x y))

(define (add3 x y z)
  (+ x y z))

(define curried_add3 (curry add3))
(define curried_add3_2 (curry add3 1 2))
(curried_add3_2 6)



(define (curried-addition a)
  (lambda (b)
    (lambda (c)
      (+ a b c))))

(define myCurry
  (lambda (n)
    (lambda (x)
      (lambda (y)
        (+ x y n)))))

(((myCurry 2) 5) 6)

(define sum6From3
  (lambda (n)
    (lambda (x)
      (lambda (y)
        (+ x y n)))))

(((sum6From3 1) 2)3)

(define myPartialApplication
  (lambda (n x)
    (lambda (y)
      (+ x y n))))

((myPartialApplication 1 2) 3)












;trying to reference the previous stream doesn't work because the previous element in s
; is not the same stream that we want to refer to
#|
(define (mySum3 s)
  (define (helper-function s [myn 1])
    (s-cons (+ s (s-ref s (- myn 1))) (helper-function (s-rest s) (+ 1 myn))))
  (helper-function s 1))
|#


;one way is to make a list and then rev the list and get access to the first element


#|
;one way is to store our previous stream or last element of our stream in a previous element variable in a helper function
(define (mySum2 s)
  (define myPrevious (s-first s))

  (define (helper-function s [myPrevious])
    (

     |#








#|
(define (mySum s)
  (s-cons (+ s) (mySum s)))
(define (mySum2 s)
  (s-cons s (mySum (+ s s))))


(define (mySum3 s)
  (define (myHelper-Function s [mypreviousNumber])
    (let ((s (+ mypreviousNumber)))
      (if (good-enough? currentGuess x epsilon)
          currentGuess
        (helper-nextGuessFunction (s-rest myStream))))))
    

;(s-display-limit (mySum naturals) 5)

(s-display-limit naturals 5)


|#

#|

(define (mySum s)
  (define (helper-function s [mylist '()])
    (s-cons (+ s (s-first (reverse mylist))) (mySum (s-rest s))))
  (helper-function s (s2list s)))

|#

#|
(define (mySum2 s)
  (define (helper-function s previous-element)
    (if (s-empty? s)
        empty-s
        (let ((current-element (s-first s)))
          (s-cons (+ previous-element current-element)
                  (helper-function (s-rest s) (+ previous-element current-element))))))
  (helper-function s 0))


 


|#








