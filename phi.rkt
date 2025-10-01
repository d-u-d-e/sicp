#lang sicp

; ex 1.33
; filtered-accumulate can be used to compute Euler's phi

(define (filtered-accumulate combiner null term a next b pred)
  (cond ((> a b) null)
        ((pred a) (combiner
                   (term a)
                   (filtered-accumulate combiner null term (next a) next b pred)))
        (else (filtered-accumulate combiner null term (next a) next b pred))))


(define (gcd a b)
  (cond ((= b 0) a)
        (else (gcd b (remainder a b)))))

(define (phi n)
  (define (unit _) 1)
  (define (is_relatively_prime a) (= (gcd a n) 1))
  (filtered-accumulate + 0 unit 1 inc n is_relatively_prime))

; some phi values
(phi 6)
(phi 13)
(phi 17)
(phi 107)
(phi 148)