#lang racket



;Functions from demo10

(define (mc-eval exp [env null])      ;input: an expression and an (maybe empty) environment; goal: evaluate the expression in the environment
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value exp env))  ;to resolve variables and primitive functions
        ((pair? exp) (mc-apply (mc-eval (car exp) env) (list-of-values (cdr exp) env))) ;application: (operation operand1 operand2..)
        (else (error "Unknown expression type -- EVAL" exp))))


(define (mc-apply procedure arguments)     ;input: a procedure and its arguments; goal: apply the procedure to its arguments
  (cond ((tagged-list? procedure 'primitive) (apply-primitive-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))




(define (lookup-variable-value var env)       ;input: a variable and an environment (i.e., a list of pairs); goal: get the value for this variable (since we have only primitive procedures, only one scope is required)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(define (list-of-values exps env)             ;input: a list of expressions and an environment; goal: turn the list of expressions into a list of values (by evaluating each expression in the environment) 
  (if (null? exps)
      '()
      (cons (mc-eval (car exps) env) (list-of-values (cdr exps) env))))

(define (tagged-list? exp tag)                ;input: an expression (a list that might start with tag) and a tag; goal: determine if the expression is a list that starts whose first element is tag
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (apply-primitive-procedure proc args)  ;input a procedure {e.g., '(primitive #<procedure:*>)} and arguments {e.g., '(1 2) }; goal: apply the procedure to the arguments
  (apply-in-underlying-racket (car (cdr proc)) args))

(define apply-in-underlying-racket apply) ;redirect to implementation language


;Interpreter2
;step 2: compound 

;can handle also compound procedures (added lambda)
;models the environment model (with multiple frames) -> changed lookup and environment

(define (mc-eval2 exp [env null])
  (cond ((number? exp) exp)
        ((symbol? exp) (lookup-variable-value2 exp env)) 
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((pair? exp) (mc-apply2 (mc-eval2 (car exp) env) (list-of-values2 (cdr exp) env))) 
        (else (error "Unknown expression type -- EVAL" exp))))

(define (mc-apply2 procedure arguments)
  (cond ((tagged-list?2 procedure 'primitive) (apply-primitive-procedure procedure arguments))
        ((tagged-list?2 procedure 'procedure) (apply-compound-procedure procedure arguments))
        (else (error "Unknown procedure type -- APPLY" procedure))))





(define (list-of-values2 exps env)
  (if (null? exps)
      '()
      (cons (mc-eval2 (car exps) env) (list-of-values2 (cdr exps) env))))

(define (tagged-list?2 exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (apply-compound-procedure proc args)
  (mc-eval (caddr proc) (extend-environment (cadr proc) args (cadddr proc))))



#| ;version with explained parameters
(define (apply-compound-procedure proc args)
  (define body (car (cdr (cdr proc))))
  (define formalPs (car (cdr proc)))
  (define oldEnv (car (cdr (cdr (cdr proc)))))
  (mc-eval body (extend-environment formalPs args oldEnv)))
|#

(define (lambda? exp) (tagged-list? exp 'lambda)) ;;;for compound procedures (lambda)

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (lambda-parameters exp) (car (cdr exp)))       ;(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (car (cdr (cdr exp))))       ;(define (lambda-body exp) (caddr exp))

(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (make-frame vars vals)
  (cond ((and (null? vars) (null? vals)) '())
        ((null? vars) (error "too many arguments supplied" vals))
        ((null? vals) (error "too few arguments supplied" vars))
        (else (cons (cons (car vars) (car vals))
                    (make-frame (cdr vars) (cdr vals))))))



;Aufgabe 1

; Some basic tests
(mc-eval 5) ; will give 5 back because of this piece of code (cond ((number? exp) exp) from (mc-eval)
(number? '5)

; At the start our enviroment is always assumed to be null unless an enviroment is given
; An enviroment is essentially a list of pairs, where each pair has a certain value
;assigned

;Therefore because of a null enviroment anything other than numbers will not be recognized
; unless it is defined in our enviroment for example
;(mc-eval 'b) ; will give a error of unbound variable

;But if we assigned an enviroment where it is defined like
(define myEnv1 (list (cons 'b 50)))
(mc-eval 'b myEnv1) ; will return 50 because we gave an enviroment where it was already defined

; the same applies for our primitive procedures
; procedures like + - / * are already defined in our enviroment but the symbolic
; equivalent of + - / * are not therefore
(mc-eval (+ 1 1)) ; will return 2 but not because 1 + 1 was evaluated by what we defined
; but rather by racket itself since + is already defined for the proper test we need
; the symbol '+

;(mc-eval '(+ 2 3)) ; in this case our enviroment hasn't defined our addition variable
; Therefore we need to do that and this is where mc-apply comes into play
; because what procedures are, is essentialay defined in our enviroment as a pair,
; where it tells us if it is a primitive procedure or a more complex procedure
; This also allows us to make our own syntax where we can use any name we want to
; use addition I can even set it to the symbol '* which is confusing but doable
(define myAddition (list (cons 'addition (list 'primitive +))))
(define myAppend (append myAddition myEnv1)) ; I can use append to expand my enviroment
(define myEnv2 (list (cons 'addition (list 'primitive +)) (cons 'b 50)))

(define e2 (list (cons '+ (list 'primitive +)) (cons 'a  100)))

(mc-eval (list 'addition 2 3) myAppend) ;proc => (primitive #<procedure:+>) args => (2 3) when (apply-primitive-procedure is called)


(define myWeirdEnv (list (cons '* (cons 'primitive (cons + null))) (cons 'a 100)))

(mc-eval (list '* 2 1) myWeirdEnv) ; if we did actual multiplciation we would get 2
;(mc-eval (list 2 'addition 1) myAddition) ; the issue we encounter here, is that it assumes that
; the first element in our list will be our procedure but in this case it isn't

; another way to create or extend our enviorment is this
(define extendedEnv (cons (cons 'c 30) myEnv2))

(mc-eval 'c extendedEnv) ; returns 30 which is what I added to the exisiting env
(mc-eval 'b extendedEnv) ; returns 50 from the old env
; How (extendedEnv) looks now '((c . 30) (addition primitive #<procedure:+>) (b . 50)) 


;Aufgabe 2
; The idea, is to basically use mc-apply on my lst make a local state where
; I check each time to see if the element in my list returns true for the pred
; Try maybe making a new list where the pred is the first element in my lst
; and gets applied to the rest of the args
; and then


(define (findFirst1 pred lst [noMatch "No match found"])
  (cond
      [(empty? lst) noMatch]
      [(pred (car lst)) (car lst)]
      [else (findFirst1 pred (rest lst))]))

(define (findFirst pred lst [noMatch "No match found"])
  (define (myHelper lst)
    (cond
      [(empty? lst) noMatch]
      [(pred (car lst)) (car lst)]
      [else (myHelper (rest lst))]))
  (myHelper lst))

(displayln "here")
(findFirst1 even? '(1 3 5 6 7 9))


;Aufgabe 3


(define (lookup-variable-value30 var env)       ;input: a variable and an environment (i.e., a list of pairs); goal: get the value for this variable (since we have only primitive procedures, only one scope is required)
      (define val (assq var env))
      (if (eq? val false)
          (error "unbound variable" var)
          (cdr val)))

(define (lookFor x lst)
  (eq? (car lst) x))

(define (mypredicate list variable) ; might have to change the = to eq? because it is so in assq implemented or to equal?
  (if (= (variable (car list)))
      (car list)
      (mypredicate (rest list) variable)))

(define (lookup-variable-value1 variable env)
  (define elementSearch (findFirst (lambda (x) (eq? (car x) variable)) env))
  (if (eq? elementSearch #f)
      (displayln "unbound variable" variable)
      (cdr elementSearch)))

(define (lookup-variable-value2 var env) ;lookup with multiple frames: env is a list of environments (we check the left most environment first)
  (cond ((null? env) (error "unbound variable" var))
        (else
         (define binding (findFirst (lambda (x) (eq? (car x) var)) env))
         (if (eq? binding false)
             (lookup-variable-value var (cdr env))
             (cdr binding)))))


(lookup-variable-value1 'b myEnv2)
(displayln "lookup-2")
(lookup-variable-value2 'b myEnv2)

;(define (lookup-variable-value2 variable env))


;Aufgabe 4

;One way is to implement it in mc-eval
; another way is to implement it in taggedlist? with equal? procedure 'and and so on





;Aufgabe 5
(define (g) 
  (g))

;(mc-eval (g) null)

;(mc-eval ((lambda (x) (x)) (lambda (x) (x))) null)


;(mc-eval ((lambda (z) (z z)) (lambda (z) (z z))) null) ;taken from lecture notes hat keinen Normal form
;(mc-eval2 ((lambda (z) (z z)) (lambda (z) (z z))) null)


