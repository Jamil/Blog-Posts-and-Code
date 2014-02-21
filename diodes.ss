#lang scheme

(define (find-Vt temp)
  (define boltzmann 1.38e-23) ; J/K
  (define e-charge 1.60e-19) ; Coulombs/e-
  (/ (* boltzmann (+ temp 273)) e-charge))

(define (kvl-current Vdd R Vd)
  (display (/ (- Vdd Vd) R))
  (display "--curr\n")
  (/ (- Vdd Vd) R))

(define (log10 x)
  (/ (log x) (log 10)))

(define (diode-voltage Vt Vd-prev Id-prev Id-curr)
  (+ Vd-prev (* 2.3 Vt (log10 (/ Id-curr Id-prev))))) 
  
(define (solve-iter Vt Vdd R Vd-prev Id-prev)
  (display (diode-voltage Vt Vd-prev Id-prev (kvl-current Vdd R Vd-prev)))
  (display "--V\n")
  (cond ((< (abs (- (diode-voltage Vt Vd-prev Id-prev (kvl-current Vdd R Vd-prev)) Vd-prev)) 0.0001) Vd-prev)
        (else 
         (solve-iter Vt Vdd R (diode-voltage Vt Vd-prev Id-prev (kvl-current Vdd R Vd-prev)) (kvl-current Vdd R Vd-prev))) ))

(define (solve Vdd R Vd-ref Id-ref temp) 
  (solve-iter (find-Vt temp) Vdd R Vd-ref Id-ref)
  )