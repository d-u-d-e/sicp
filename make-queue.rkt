#lang racket

; ex 3.22
(define (make-queue)
  (let ((front-ptr '()) (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (insert-queue i)
      (let ((new-pair (mcons i '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-mcdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))))
    (define (front-queue)
      (if (empty-queue?) (error "front called with empty queue!")
          (mcar front-ptr)))
    (define (delete-queue)
      (cond ((empty-queue?)
             (error "delete called with empty queue!"))
            (else (set! front-ptr (mcdr front-ptr)))))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) (empty-queue?))
            ((eq? m 'insert-queue!) insert-queue)
            ((eq? m 'front-queue) (front-queue))
            ((eq? m 'delete-queue!) (delete-queue))
            (else (error "invalid operation"))))
    dispatch))

(define (empty-queue? q) (q 'empty-queue?))
(define (insert-queue! q item) ((q 'insert-queue!) item))
(define (front-queue q) (q 'front-queue))
(define (delete-queue! q) (q 'delete-queue!))

(provide make-queue)
(provide empty-queue?)
(provide insert-queue!)
(provide front-queue)
(provide delete-queue!)

; (define q (make-queue))
; (empty-queue? q) ; #t
; (insert-queue! q 'a)
; (insert-queue! q 'b)
; (insert-queue! q 'c)
; (delete-queue! q)
; (delete-queue! q)
; (eq? 'c (front-queue q)) ; #t
; (insert-queue! q 'd)
; (eq? 'c (front-queue q)) ; #t
; (delete-queue! q)
; (eq? 'd (front-queue q)) ; #t
; (delete-queue! q)
; (empty-queue? q) ; #t
