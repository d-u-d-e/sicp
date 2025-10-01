#lang sicp

; ex 1.32
; the accumulate procedure generalizes sum and product

(define (accumulate combiner null term a next b)
  (cond ((> a b) null)
        (else (combiner (term a) (accumulate combiner null term (next a) next b)))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (prod term a next b)
  (accumulate * 1.0 term a next b))

(define (sum-integers a b)
  (sum identity a inc b))

; 1 + 2 + ... + 6
(sum-integers 3 6)

; compute wallis product
(define (wallis-term n)
  (define (square x) (* x x))
  (let ((double (* 2 n)))
    (/ (* double (+ double 2)) (square (+ double 1)))))

(prod wallis-term 1 inc 100)
