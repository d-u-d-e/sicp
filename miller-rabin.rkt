#lang sicp

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (let* ((x (expmod base (/ exp 2) m)) (y (square x)))
           (if
            (and (= y 1) (not (= x 1)) (not (= x (- m 1))))
            0
            (remainder y m))))
        (else (remainder (* base (expmod base (- exp 1) m)) m))))

(define (mr-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((mr-test n) (fast-prime? n (- times 1)))
        (else false)))

; these are prime
(fast-prime? 79 5)
(fast-prime? 83 5)
(fast-prime? 89 5)
(fast-prime? 337 5)
(fast-prime? 347 5)
(fast-prime? 349 5)

; next are composite
(fast-prime? 93 5)
(fast-prime? 1000 5)
(fast-prime? 2500 5)
(fast-prime? 3872 5)