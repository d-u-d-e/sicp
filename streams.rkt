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

(define s (stream-enumerate-interval 1 1000))
(define a (stream-take s 3))
(for-each (lambda (x) (display x) (newline)) a)
