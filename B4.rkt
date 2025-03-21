#lang racket
(require racket/trace)
 


;Stolen from slides
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (rest items)))))

(define (list-ref items n)
  (if (= n 0)
      (first items)
      (list-ref (rest items) (- n 1 ))))


;Aufgabe 1


;Aufgabe 1 Teil a
(define (mylast xs)
  (if (null? (rest xs))
         (first xs)
         (mylast (rest xs))))

;Alternative more efficient way
(define (mylast2 xs)
  (car (reverse xs)))
(mylast2 (list 1 2 3))


;Test Cases for Aufgabe 1 Teil A

(displayln "")
(displayln "Test Cases for Aufgabe 1 Teil A")
(displayln "")

(define mylist (list 1 2 3 4 5))
(mylast mylist) ; Should give out 5

;Aufgabe 1 Teil b

(define (tailLength xs)
  
  (define (tailLength-helper xs counter)
    (if (or (empty? xs) (null?  xs))
        counter
        (tailLength-helper (cdr xs) (+ counter 1))))
  (tailLength-helper xs 0))

(displayln "")
(displayln "Test Cases for Aufgabe 1 Teil B")
(displayln "")

(tailLength (list)) ; Should be 0
    
(tailLength (list 1 2 3)) ; should give out 2

;Aufgabe 2

(define (getFromIdx idx xs)
  (if (= idx 1)
      (rest xs)
      (getFromIdx (- idx 1) (rest xs))))

(define (getFromIdx2 idx xs) ;this method works better
  (cond
    [(= idx 0) xs]
    [else (getFromIdx2 (- idx 1) (rest xs))]))

(displayln "")
(displayln "Test Cases for Aufgabe 2")
(displayln "")

(getFromIdx 1 (list 0 1 2 3 4 5 6)) ; Should give out 1 2 3 4 5 6
(getFromIdx 2 (list 0 1 2 3 4 5 6)) ; Should give out 2 3 4 5 6
(displayln "")
(displayln "works with index 0 getFromIdx2")
(displayln "")
(getFromIdx2 1 (list 0 1 2 3 4 5 6)) ; Should give out 1 2 3 4 5 6
(getFromIdx2 2 (list 0 1 2 3 4 5 6)) ; Should give out 2 3 4 5 6
(getFromIdx2 0 (list 0 1 2 3 4 5 6)) ; Should give out 1 2 3 4 5 6




;Aufgabe 3
(define (combine op xs ys) ; did not do

  (define (combine-helper op xs ys length oldList)
  (cond
    [(= length 0) (oldList)]
    [(> length 0) (combine-helper op (rest xs) (rest ys) (- length 1) (append oldList (op (car xs) (car ys))))]
    ))
  (combine-helper op xs ys (tailLength xs) (list)))

(displayln "")
(displayln "Test Cases for Aufgabe 3")
(displayln "")

;(combine + (list 1 2 3) (list 4 5 6))





;Aufgabe 4

(define (isSame xs ys)

  (cond
    [(and (null? xs) (null? ys)) #t]
    [(not (= (tailLength xs) (tailLength ys))) #f]
    [(or (and (null? xs) (not (null? ys))) (and (null? ys) (not (null? xs)))) #f ]
    [(= (car xs) (car ys)) (isSame (rest xs) (rest ys)) ]
    [else #f]))

(displayln "")
(displayln "Test Cases for Aufgabe 4")
(displayln "")

(isSame (list 1 2) (list 1)) ;expected #f
(displayln "break") 
(isSame (list 1 2) (list 2 1)) ;expected #f
(displayln "break") 
(isSame (list 1 2) (list 1 2)) ;expected #t



;aufgabe 5
;It is bad because it has a runtime of O(n²)
; The reason is because we are doing a recursive call too often, which basically
;means we are running in n time and n-1 n-2 n-3 n-4 and so on however that can be summed
; to n² run time. So we are basically listing and comparing the array too often, due to the recursive calls



  

; Write a help function where your first element is your smallest
; My basis fall is to check if it is empty when not then my first element is the smallest
  ;then recursive list with min and compare with car of rest

(define (minbad xs)
  (if (null? xs)
      (displayln "null")
      (void))
  (define (min-helper xs smallestElement)
    (cond
      [(null? (rest xs)) smallestElement]
      [(< smallestElement (car (rest xs))) (min-helper (rest xs) smallestElement)]
      [else (min-helper (rest xs) (car (rest xs)))]))
  (min-helper xs (car xs)))

; The idea behind is to basically always assume my first element is my min,
;Then I see if the car of the rest is smaller or bigger
; if it is smaller, then I simply pass it into the next argument
; Otherwise if it is bigger I still assume my first was the smallest until
; the end of my list


(displayln "")
(displayln "Test Cases for aufgabe 5")
(displayln "")

(define array1 '(5 3 8 2 7 1)) ;expected 1
(define array2 '(-10 -4 -7 -3 -8 -1)) ;expected -10
(define array3 '(10 -5 3 -2 0 7)); expected -5

(define array5 '(42)) ;expected 42
(define array6 (list))

(minbad array1)
(minbad array2)
(minbad array3)
(minbad array5)
;(minbad array6) ;expected null but returns an error



(define (minbad2 xs)
(cond [(null? (cdr xs)) (car xs)]
[(< (car xs) (minbad (cdr xs))) (car xs) ]
[else (minbad (cdr xs))]))


;corrected minbad more efficient
(define (minbad3 xs [m (car xs)])
  (if (null? xs)
      m
      ((cdr xs) (minbad3 (car xs) m))))

(define (min2 a b) (if (< a b) a b))
(define (mintr xs [m (car xs) ])
  (if (null? xs)
      m
      (mintr (cdr xs) (min2 (car xs) m))))

(define test-list (range 1 100000)) ; generates a really long list
(define test-list2 (range 1 5000000))

(time (minbad2 test-list)) ; shows the time in cpu time and real time for minbad2 (which is what is in the exercises
; cpu time: 53 real time: 53 gc time: 12
(time (minbad test-list)) ; improved procedure cpu time:
; 30 real time: 30 gc time: 8
(time (void(minbad2 test-list2)))
(time (void(minbad test-list2)))
;(time (void(minbad3 test-list2)))
(displayln "")
(displayln "Teacher's function")
(time (void(mintr test-list2)))
(time (void(mintr test-list2)))
(trace minbad2)
(trace mintr)




  

;(minbad2 array6) 


;Failed attempts
#|
(define (mylast-tail xs)
  (define counter (length xs))
  (define (mylast-helper xs counter)
    (cond
      [(empty? xs) '()]
      [(= counter 0)])))
|#


#|
(define (isSame xs ys)
  (define arrayLengthxs (length xs))
  (define arrayLengthys (length ys))
  (define currentIndex 0)
  (define (isSame-helper xs ys currentIndex)
    (cond
    [(not (equals? arrayLengthxs arrayLengthys)) #f]
    [(= (list-ref xs currentIndex) (list-ref ys currentIndex)) (isSame-helper xs ys (+ currentIndex 1))]
    [else #f])))



(isSame (list 1 2) (list 1))
|#

#|
(define (combine op xs ys)
  (define arrayLength (length xs))
  (define counter 0)
  (define myNewList (list))
  (define (combine-helper op xs ys arrayLength counter)
    (define leftElement (list-ref xs counter))
    (define rightElement (list-ref ys counter))
    (if (< counter arrayLength)
        ((op leftElement rightElement) (combine-helper op xs ys arrayLength (+ counter 1)))
        (void))))


(combine + (list 1 2 3) (list 4 5 6))
|#
  #|
  (if  (null? (first xs))
      (void)
      ((op (first xs) (first ys)) (combine op (rest xs) (rest ys)))))
|#




  #|
  (define currentIndex 0)
  (define lengthOfArray (length xs))
  (define (combinationStep op xs ys lengthOfArray currentIndex)
    (if (not (= lengthOfArray currentIndex))
        ((op (list-ref xs currentIndex) (list-ref ys currentIndex)) (combinationStep op xs ys lengthOfArray (+ currentIndex 1)))
        (void))))
  |#



    
        
  
    
  
  
