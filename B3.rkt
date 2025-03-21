#lang racket

;Aufgabe 1

;Required methods for Aufgabe 1
(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (root x)
  (sqrt x))

; Part A
(define (min-fx-gx fx gx x)
  (define outputOfFunctionF (fx x))
  (define outputOfFunctionG (gx x))
  (min outputOfFunctionF outputOfFunctionG))

;test cases for aufgabe 1 part A

(displayln "")
(displayln "Test cases for aufabe 1 part a")
(displayln "")

(min-fx-gx square cube -1) ;expected output: -1
(min-fx-gx square cube 2) ;expected output: 4
(min-fx-gx square cube 3) ;expected output: 9

(displayln "")
(displayln "End of Aufgabe 1 part a")
(displayln "")

;Aufgabe 1 part B
(define (combine-fx-gx ourFunction fx gx x)
  (define outputOfFunctionF (fx x))
  (define outputOfFunctionG (gx x))
  (ourFunction outputOfFunctionF outputOfFunctionG))


(displayln "")
(displayln "Test cases for aufabe 1 part b")
(displayln "")

(combine-fx-gx min square cube -1) ;expected output -1
(combine-fx-gx min square cube 2)  ;expected output 4
(combine-fx-gx min square cube 3)  ;expected output 9

(combine-fx-gx max square cube -1) ;expected output 1
(combine-fx-gx max square cube 2)  ; expected output 8
(combine-fx-gx max square cube 3)  ; expected output 27

(combine-fx-gx max sqrt square 5) ; expected output 25
(combine-fx-gx max sqrt square 10) ; expected output 100

(combine-fx-gx min sqrt square 4) ; expected output 2
(combine-fx-gx min sqrt square 16) ; expected output 4


(displayln "")
(displayln "End of Aufgabe 1 part b")
(displayln "")


; Aufgabe 2

(displayln "")
(displayln "Start of Aufgabe2")
(displayln "")


(define (f g)
  (g 5))

#| What I think will happen

(f +) will simply evaulaute (+ 5)

(f square) This will work the same way as in Aufgabe 1, because
square will be treated as a first class citizen and 5 will be squared


(f (lambda (x) (* x (+ x 2)))) here we have a lamda function
(procedure with no name) which takes the argument x and then does
the addition of x by 2 then multiplies x+2 by x, however x's value
won't be updated and will multly x+2 by x( the same x as in the argument
not  the updated x after 2 was added to it)

So in this case we will get (lambda (* x (+ x 2)) 5) so our argument
for our lambda function is our 5
which evaluates to 5 + 2 = 7, and then 7 * 5, which is equal to 35

(f f) it is basically a function with itself as a function parameter
it will call itself once and then it will be (f 5)
which will evaluate to (5 5) which is not a procedure and will give an error



|#


(f +)
(f square)
(f (lambda (x) (* x (+ x 2))))
;(f f) Will cause an error

(displayln "")
(displayln "End of Aufgabe2")
(displayln "")



(displayln "")
(displayln "Start of Aufgabe 3")
(displayln "")

;Aufgabe 3
 ;f(n) = n if n<3 and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3

(define (fr n)
  (if (< n 3)
      n
      (+ (fr (- n 1)) (* 2 (fr (- n 2))) (* 3 (fr (- n 3))))))


; does not work correctly

#|(define (fi n)
  (define (fi-iter sum counter max-count)
  (if (> 3 n)
      n
      (fi-iter (* (- n 1) counter)
               (+ counter 1)
               max-count)))
  (fi-iter 1 1 n))
|#

; Corrected code for part b
(define (fi n)
  (define (fi-iter a b c count)
    (if ( = count n)
        a
        (fi-iter (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (add1 count))))
    ;(trace fi-iter)
  (fi-iter 2 1 0 2))






(fr 2)
(fr 3)
(fr 4)
(fr 5)
(fr 6)

;(fi 2)
;(fi 3)
;(fi 4)
;(fi 5)
;(fi 6)


(displayln "")
(displayln "End of Aufgabe 3")
(displayln "")


(displayln "")
(displayln "Start of Aufgabe 4")
(displayln "")
;Aufgabe 4

;Teil A
(define (twice proc)
  (lambda (x)
    (proc (proc x))))

(displayln "")
(displayln "Test of Aufgabe 4 part a")
((twice square) 4) ;the reason to write it with double brackets like this
; is because Racket evaluates (twice square) first and returns a new procedure
;this new procedure is what evaluates the square of 4 twice


;Aufgabe 4 Teil b
;Needed for part b
(define (inc x)
  (+ x 1)) 

(define (comp proc1 proc2)
  (lambda (x)
    (proc1 (proc2 x))))

(displayln "")
(displayln "Test of Aufgabe 4 part b")

((comp cube inc) 2)
((comp inc cube) 2)


(displayln "")
(displayln "End of Aufgabe 4 ")
(displayln "")

;Aufgabe 5
;Procedures taken from the lecture
(define (mycons x y)
  (define (dispatch m)
    (cond [(= m 0) x]
          [(= m 1) y]
          [else (error "Argument not 0 or 1 -- in mycons" m)]))
  dispatch)

(define (mycar z) (z 0))
(define (mycdr z) (z 1))

;seems completely wrong, missing some details on how to properly represent imaginary numbers
(define (add-complex c1 c2)
  (define a1 (mycar c1)) ;a stands for the real part of the imaginary number
  (define b1 (mycdr c1)) ;b stands for the imaginary part
  (define a2 (mycar c2))
  (define b2 (mycdr c2))
  (mycons (+ a1 a2)
          (+ b1 b2)))




