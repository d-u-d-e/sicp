#lang sicp

(define (exp b n)
  (define (iter-exp a n b)
    (define (even n)
      (= (remainder n 2) 0))

    (cond ((= 1 n) (* a b))
          ((even n) (iter-exp a (/ n 2) (* b b)))
          (else (iter-exp (* a b) (/ (- n 1) 2) (* b b)))))

  (if (= 0 n)
      1
      (iter-exp 1 n b)))

(exp 5 2)