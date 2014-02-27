#lang racket

(define tolerance 0.1)
(define learning-rate 0.0001)

; (hyp x th)
; 3-parameter linear hypothesis function; takes lists (vectors) x and th
; Assumes length(x) = length(th)
(define (hyp x th)
  (cond ((empty? x) 0)
        ((empty? th) 0)
        (else (+ (* (car x) (car th)) (hyp (cdr x) (cdr th))))))

; (lms-prod-term X th y x)
; Calculates difference times the value of x
(define (lms-prod-term X TH y x)
  (* (- y (hyp X TH)) x))

; (lms-update x th y hyp)
; Updates th using gradient descent to minimize the error between y and (hyp x th)
(define (lms-update X TH y hyp)
  (define (lms-help x th) ; x, th subsets of X, TH; xi is first element of subset x
    (cond ((empty? th) '())
          ((empty? x) '())
          (else (cons (+ (car th) (* (lms-prod-term X TH y (car x)) learning-rate)) (lms-help (cdr x) (cdr th))))))
  (lms-help X TH))
  
; (desc X TH y hyp)
; Repeats the LMS Update Rule for th until convergence
(define (desc X TH y hyp)
  (cond ((< (abs (- y (hyp X TH))) tolerance) TH)
        (else (desc X (lms-update X TH y hyp) y hyp))))

; (batch-lms-update x th y hyp)
; Need to call lms-prod-term for each training example
; Keep track of state with var j (i.e. which th we're updating at the moment)
(define (batch-lms-update X-TR TH Y-TR hyp)
  ; For each th-j, we need to consider the entire set of training examples (X-TR)
  
  (define (sum-tr x-tr y-tr j) ; x-tr, y-tr are subsets of X-TR, Y-TR
    (+ (lms-prod-term (car x-tr) TH (car y-tr) (nth j (car x-tr))) (sum-tr (cdr x-tr) (cdr y-tr) j)))
  
  (define (lms-help j)
    (cond ((>= j (x length)) '())
          (else (cons (+ (nth j TH) (* (sum-tr X-TR Y-TR j) learning-rate)) (lms-help (+ j 1))))))
  
  (lms-help X TH 1))
  
