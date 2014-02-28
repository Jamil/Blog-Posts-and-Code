#lang scheme

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
  (define (sum-tr x-tr y-tr j TH) ; x-tr, y-tr are subsets of X-TR, Y-TR
    (cond ((empty? x-tr) 0)
          ((empty? y-tr) 0)
          (else 
    (+ (lms-prod-term (car x-tr) TH (car y-tr) (list-ref (car x-tr) j)) (sum-tr (cdr x-tr) (cdr y-tr) j TH)))))
  
  (define (lms-help j)
    (cond ((>= j (length TH)) '())
          (else (cons (+ (list-ref TH j) (* (sum-tr X-TR Y-TR j TH) learning-rate)) (lms-help (+ j 1))))))
  
  (lms-help 0))

; Initiates the batch gradient descent procedure; terminates when the cumulative error is below the tolerance
(define (batch-desc X-TR TH Y-TR hyp)
  (define (sum-errs j)
    (cond ((>= j (length X-TR)) 0)
          ((>= j (length Y-TR)) 0)
          (else
           (+ (abs (- (list-ref Y-TR j) (hyp (list-ref X-TR j) TH))) (sum-errs (+ 1 j))))))
  
  (cond ((< (sum-errs 0) tolerance) TH)
        (else (batch-desc X-TR (batch-lms-update X-TR TH Y-TR hyp) Y-TR hyp))))
