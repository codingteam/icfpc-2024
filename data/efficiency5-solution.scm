#lang racket

; so this checks if n is a Mersenne number more than N
(define (f1 n primep powerp)
    (if (and (> n 1000000) (and (primep n) (powerp (+ n 1)))) n (f1 (+ n 1) primep powerp))
)

; return true if x is an exact power of 2, false otherwise
(define (is-power-of-2 x)
    (if (= x 1) true (if (= (modulo x 2) 1) false (is-power-of-2 (quotient x 2))))
)

; when applied with x = 2, this checks if y is a prime number
(define (is-prime x y)
    (if (= x y) true (if (= (modulo y x) 0) false (is-prime (+ x 1) y)))
)

((lambda (v6)
   ((lambda (v7)
      (f1 2 v6 v7))
    is-power-of-2))
 (lambda (v5)
   (is-prime 2 v5)))
