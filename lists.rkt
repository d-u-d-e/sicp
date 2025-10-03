#lang sicp

; get the n-th element of a list
(define (list-ref l n)
  (if (= 0 n)
      (car l)
      (list-ref (cdr l) (- n 1))))

(list-ref (list 1 2 3 4) 2)

; ex 2.18
; reverse a list
(define (reverse l)
  (define (helper l acc)
    (if (null? l)
        acc
        (helper (cdr l) (cons (car l) acc))))
  (helper l nil))

(reverse (list 1 2 3 4))

; ex 2.23
; for-each
(define (for-each proc l)
  (cond
    ((null? l) nil)
    (else (proc (car l)) (for-each proc (cdr l)))))

(for-each
 (lambda (x) (display x) (newline))
 (list 7 4 5 6))

; ex 2.32
; the power set of a list of elements
(define (subsets l)
  (if (null? l)
      (list nil)
      (let ((rest (subsets (cdr l))))
        (append rest (map (lambda (x)
                            (cons (car l) x))
                          rest)))))
(subsets (list 1 2 3 4))