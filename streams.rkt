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


; ex 3.51
(define (show x)
  (display x) (newline) x)

(define x
  (stream-map show (stream-enumerate-interval 0 10)))

; stream-ref prints all integers up to 5 and then returns 5
(stream-ref x 5)
; stream-ref here won't reprint all integers up to 5 because we already forced
; a part of the stream; as before it prints 7 twice
(stream-ref x 7)
