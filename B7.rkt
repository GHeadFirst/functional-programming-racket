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



;Testing the functions above
(define mystream
  (s-cons 5
          (s-cons 2
                  (s-cons 3
                          (s-cons 4 empty-s)))))



;Aufgabe 1
#|
(define (s-length2 mystream)
  (
|#
(define (s-length mystream [accumulator 0])
  (if (s-empty? mystream)
      accumulator
      (s-length (s-rest mystream) (+ 1 accumulator))))




;(define (s-length2 mystream)
 ; (s-map + (s-map (lambda (x) (expt x 0)) mystream)))
; need foldr for the above procedure


(define (s-length3 mystream)
   (define (turningStreamToList s)
     (if (s-empty? s)
         '()
         (cons (s-first s) (turningStreamToList (s-rest s)))))
  (length (turningStreamToList mystream)))
(s-length3 mystream)
(s-length mystream)


(define mytemps
  (s-cons 50
          (s-cons 54
                  (s-cons 45
                          (s-cons 70 empty-s)))))





(define (toCelsius-stream stream)
  (s-map (lambda (x) (* x (/ 5 9))) (s-map (lambda (x) (- x 32)) stream)))
;mytemps.s-map(x-32).s.map(x* (5/9))

(toCelsius-stream mytemps)
(define myceltemp (toCelsius-stream mytemps))
(s-first myceltemp)
(s-first (s-rest myceltemp))
(s-display myceltemp)

;Aufgabe 2
#|
(define (list2s list)
  (cond [(null? list) (s-cons list)]
        [(not (pair? list)) (

  )]))
|#



(define (list2s list)
  (cond
    [(null? list) empty-s]
    [else (s-cons (car list) (list2s (cdr list)))]))
  

(define (s2list stream)
  (if (s-empty? stream)
         '()
         (cons (s-first stream) (s2list (s-rest stream)))))

(list2s (list 1 2 3 4 5))
(define mylisttostream (list2s (list 1 2 3 4 5)))
(s-first mylisttostream)
(s-first (s-rest mylisttostream))
(s-first (s-rest  (s-rest mylisttostream)))




  
;Aufgabe 3

(define (powersOf2 x)
  (s-cons x (powersOf2 (* x 2))))
(powersOf2 64)
(s-first (powersOf2 64))
(s-first (s-rest (powersOf2 64)))
(s-first (s-rest  (s-rest (powersOf2 64))))



;Aufgabe 4
(define s1
  (s-cons 1
          (s-cons 2
                  (s-cons 3
                          (s-cons 4 empty-s)))))

(define s2
  (s-cons 1
          (s-cons 2
                  (s-cons 3
                          (s-cons 4 empty-s)))))

(define (s-add s1 s2)
  (s-cons (+ (s-first s1) (s-first s2)) (s-add (s-rest s1) (s-rest s2))))

(define myAddedStreams (s-add s1 s2))
(s-first myAddedStreams)
(s-first (s-rest myAddedStreams))
(s-first (s-rest  (s-rest myAddedStreams)))
;(s-display (s-add s1 s2))

;(define (s-add s1 s2)
 ; (s-cons (+ (s-first s1) (s-first s2))))


;Aufgabe 5

#| Method from lecture
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (s-cons 1.0 (s-map (lambda (guess) (improve guess x)) guesses)))
  guesses)

(s-display-limit (sqrt-stream 2) 20)

|#

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;Part A
(define (sqrt-stream x [previous-guess 1.0])
  (s-cons previous-guess (sqrt-stream x (improve previous-guess x))))

(sqrt-stream 2)
(s-first (sqrt-stream 2))
(s-first (s-rest (sqrt-stream 2)))
(s-first (s-rest  (s-rest (sqrt-stream 2))))

                 

; Part B

(define (square x)
  (* x x))


(displayln "Aufgabe 5 Part B")

(define (good-enough2? x y epsilon)
  (<= (abs (- x y)) epsilon))


(define (good-enough? guess x epsilon)
  (<= (abs (- (square guess) x)) epsilon))


(define (sqrt x epsilon)
  (define (helper-nextGuessFunction myStream)
    (let ((currentGuess (s-first myStream)))
    (if (good-enough? currentGuess x epsilon)
        currentGuess
        (helper-nextGuessFunction (s-rest myStream)))))
  (helper-nextGuessFunction (sqrt-stream x)))


#|
(define (sqrt x epsilon)
  (define (helper-nextGuessFunction myStream)
    (let ((currentGuess (s-first myStream)))
      (if (good-enough? x currentGuess epsilon)
          currentGuess
          (helper-nextGuessFunction (s-rest myStream)))))
  (helper-nextGuessFunction (sqrt-stream x)))
|#  
  
  
(sqrt 2 0.1)
(sqrt 2 0.001)
  







