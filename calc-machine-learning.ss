#lang racket

(define tolerance 10)

; (hyp x th)
; 3-parameter linear hypothesis function; takes lists (vectors) x and th
; Assumes length(x) = length(th)
(define (hyp x th)
  (cond ((empty? x) 0)
        ((empty? th) 0)
        (else (+ (* (car x) (car th)) (hyp (cdr x) (cdr th))))))

; (lms-update x th y hyp)
; Updates th using gradient descent to minimize the error between y and (hyp x th)
(define (lms-update X TH y hyp)
  (define alpha 0.001)
  (define (lms-help x th) ; x, th subsets of X, TH; xi is first element of subset x
    (cond ((empty? th) '())
          ((empty? x) '())
          (else (cons (+ (car th) (* (- y (hyp X TH)) alpha (car x))) (lms-help (cdr x) (cdr th))))))
  
  (lms-help X TH))
  
; (desc X TH y hyp)
; Repeats the LMS Update Rule for th until convergence
(define (desc X TH y hyp)
  (cond ((< (abs (- y (hyp X TH))) tolerance) TH)
        (else
         (desc X (lms-update X TH y hyp) y hyp))))