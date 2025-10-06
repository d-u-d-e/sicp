#lang sicp

; ex 3.2
; how many times does a function gets called? f takes just one argument
(define (make-monitored f)
  (let ((counter 0))
    (define (dispatch arg)
      (cond ((eq? arg 'how-many-calls?) counter)
            ((eq? arg 'reset-count) (set! counter 0))
            (else
             (begin
               (set! counter (+ counter 1))
               (f arg)))))
    dispatch))

(define s (make-monitored sqrt))
(s 100)
(s 1000)
(define t (make-monitored (lambda (x) (* x x))))
(t 5)
(equal? 2 (s 'how-many-calls?)) ; #t
(s 'reset-count)
(equal? 0 (s 'how-many-calls?)) ; #t
(equal? 1 (t 'how-many-calls?)) ; #t