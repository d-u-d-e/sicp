#lang sicp

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

(define (fold-right op initial sequence)
  (cond ((null? sequence) initial)
        (else (op (car sequence) (fold-right op initial (cdr sequence))))))

; 1 + 2 + ... 5
(fold-left + 0 (list 1 2 3 4 5))

(define (append seq1 seq2)
  (fold-right cons seq2 seq1))

; [1 2 3 4 5] + [6 7]
(append (list 1 2 3 4 5) (list 6 7))

; ex 2.39
; reverse as a function of fold-left or fold-righ

(define (reverse seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(define (reverse-2 seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(equal? (reverse (list 2 3 4 5))
        (reverse-2 (list 2 3 4 5)))
