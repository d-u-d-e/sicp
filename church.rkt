#lang sicp

; ex 2.6
; church numerals

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (plus-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define (plus n m)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

(define three (plus one two))

; #t
(= ((three inc) 0) 3)

; #t
(= (((plus-1 two) inc) 0) 3)

; #t
(= (((plus zero three) inc) 0) 3)