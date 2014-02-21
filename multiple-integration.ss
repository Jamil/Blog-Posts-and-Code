#lang scheme

(define (make-sum a1 a2) 
  (list '+ a1 a2))
(define (make-product m1 m2) 
  (list '* m1 m2))
(define (make-exponent e1 e2)
  (list '** e1 e2))

(define (term-var-integrate expr var)
  (define (term-var term) (cadr term))
  (define (term-exp term) (caddr term))
  (define (dependent? expr) ; test for dependence on integrating variable
    (cond ((empty? expr) #f)
          ((eq? (term-var (car expr)) var) #t)
          (else
           (dependent? (list-tail expr 1)))))
  (define (coeff expr) ; determine the divisor of the coefficient
    (cond ((empty? expr) 0)
          ((eq? (term-var (car expr)) var)
           (+ (+ (term-exp (car expr)) 1) (coeff (cdr expr))))
          (else
           (coeff (list-tail expr 1)))))
  (define (product-terms expr) ; determine each product term after integrationq
    (cond ((empty? expr) empty)
          ((eq? (term-var (car expr)) var)
           (cons (make-exponent var (+ (term-exp (car expr)) 1)) (product-terms (cdr expr))))
          (else
           (cons (car expr) (product-terms (list-tail expr 1))))))
  (cond ((empty? expr) empty)
        ((dependent? (list-tail expr 2))
         (cons '* (cons (/ (cadr expr) (coeff (list-tail expr 2))) (product-terms (list-tail expr 2)))))
        (else empty)))

(define (integrate expr var-list)
  (define (var-integrate expr var)
    (cond ((empty? expr) empty)
        (else (cons 
               (term-var-integrate (car expr) var)
               (var-integrate (cdr expr) var)))))
  (cond ((empty? var-list) expr)
        (else
         (display expr)(display var-list)(display "\n")
         (integrate (var-integrate expr (car var-list)) (cdr var-list)))))
