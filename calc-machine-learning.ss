#lang racket

(define tolerance 0.01)

; (hyp x th)
; 3-parameter linear hypothesis function; takes lists (vectors) x and th
; Assumes length(x) = length(th)
(define (hyp x th)
  (cond ((empty? x) 0)
        ((empty? th) 0)
        (else (+ (* (car x) (car th)) (hyp (cdr x) (cdr th))))))

; (err y hyp)
; Error function; between a hypothesis value and a training example
(define (err y hyp)
  (* 1/2 (- y hyp) (- y hyp)))

; (desc x th fn)
; Gradient descent function; minimizes the difference between y and hyp (parametrized by th)
(define (desc th x y hyp)
  
  )

  
  