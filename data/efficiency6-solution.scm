#lang racket

(define (f1 v4)
    (if (and (> v4 30) (v6 (v7 v4))) v4 (f1 (+ v4 1)))
)

; fibonaci numbers
(define (fib n)
    (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))
)

; check if n is prime
(define (is-prime v4 v5)
    (if (= v4 v5) true (if (= (modulo v5 v4) 0) false (is-prime (+ v4 1))))
)

((lambda (v6)
   ((lambda (v7)
      (f1 2 v6 v7))
    fib))
 (lambda (v5)
   (is-prime 2 v5)))
