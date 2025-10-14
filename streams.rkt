#lang sicp

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low (stream-enumerate-interval (+ low 1) high))))

(define (stream-take stream n)
  (if (> n 0)
      (cons (stream-car stream) (stream-take (stream-cdr stream) (- n 1)))
      nil))

;(define s (stream-enumerate-interval 1 1000))
;(define a (stream-take s 3))
;(for-each (lambda (x) (display x) (newline)) a)

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))


(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))


(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))


(define (display-line x) (newline) (display x))
(define (display-stream s)
  (stream-for-each display-line s))


; ex 3.51
(define (show x)
  (display-line x) x)

(define x
  (stream-map show (stream-enumerate-interval 0 10)))

; stream-ref prints all integers up to 5 and then returns 5
(stream-ref x 5)
; stream-ref here won't reprint all integers up to 5 because we already forced
; a part of the stream; as before it prints 7 twice
(stream-ref x 7)


; ex 3.52
(define sum 0)
(define (acc x) (set! sum (+ x sum)) sum)
(define seq (stream-map acc (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter
           (lambda (x) (= (remainder x 5) 0))
           seq))
(stream-ref y 7)
(display-stream z)


; eratosthenes' sieve
(define (divisible? x y) (= (remainder x y) 0))
(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x) (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)


; another way
(define (square x) (* x x))
(define (cube x) (* (* x x) x))

(define primes-2
  (cons-stream 2 (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes-2))

(stream-ref primes-2 50)


; computing sqrt 2 using successive approximations
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess) (sqrt-improve guess x))
                             guesses)))
  guesses)

(for-each display-line (stream-take (sqrt-stream 2) 8))
(newline)

; generating all pairs of integers (i, j)
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))


(define integers (integers-starting-from 1))

(for-each display-line (stream-take (pairs integers integers) 16))
(newline)


; ex 3.70: ordered pairs
(define (interleave-weighted s1 s2 w)
  ; (stream-car sx) is a pair (i, j)
  (cond ((stream-null? s1) s2)
        ((< (w (stream-car s1))
            (w (stream-car s2)))
         (cons-stream (stream-car s1) (interleave-weighted (stream-cdr s1) s2 w)))
        (else (cons-stream (stream-car s2) (interleave-weighted s1 (stream-cdr s2) w)))))

(define (pairs-weighted s t w)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs-weighted (stream-cdr s) (stream-cdr t) w)
    w)))

(for-each display-line (stream-take (pairs-weighted
                                     integers
                                     integers
                                     (lambda (p) (+ (car p) (cadr p))))
                                    16))
(newline)


; ex 3.71: ramanujan numbers
(define (w p) (+ (cube (car p)) (cube (cadr p))))
(define r (pairs-weighted integers integers w))

(define (search s n)
  (define (helper s n r)
    (let ((p1 (stream-car s)) (p2 (stream-car (stream-cdr s))))
      (cond
        ((= 0 n) '())
        ((= (w p1) (w p2)) (cons (w p1) (helper (stream-cdr s) (- n 1) r)))
        (else (helper (stream-cdr s) n r)))))
  (helper s n '()))

(for-each display-line (search r 6))
(newline)