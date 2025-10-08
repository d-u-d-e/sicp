#lang racket

; this is a discrete simulation of digital circuits
; we use lang racket in order to import modules
(require "make-queue.rkt")

; the agenda holds the action to be run when a signal on a wire changes
(define (make-time-segment time queue) (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))
(define (make-agenda) (mcons 0 '()))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments) (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments)) action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr! segments (mcons
                                   (make-new-time-segment time action)
                                   rest))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments! agenda (mcons (make-new-time-segment time action) segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (when (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "agenda is empty")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; wires
(define (call-each procs)
  (if (null? procs)
      'done
      (begin
        ((mcar procs))
        (call-each (mcdr procs)))))

(define (make-wire)
  (let ((signal-value 0) (action-procs '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin
            (set! signal-value new-value)
            (call-each action-procs))
          'done))
    (define (accept-action-proc! proc)
      (set! action-procs (mcons proc action-procs))
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-proc!)
            (else (error "unknown operation"))))
    dispatch))

(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action) ((wire 'add-action!) action))
(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; logical functions
(define (logical-not s)
  (cond ((= s 0) 1)
        (else 0)))

(define (logical-and a1 a2)
  (cond ((or (= a1 0) (= a2 0)) 0)
        (else 1)))

(define (logical-or a1 a2)
  (cond ((or (= a1 1) (= a2 1)) 1)
        (else 0)))

; logic gates
(define (inverter in out)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal in))))
      (after-delay inverter-delay
                   (lambda () (set-signal! out new-value)))))
  (add-action! in invert-input)
  'ok)

(define (and-gate in1 in2 out)
  (define (and-action)
    (let ((new-value (logical-and (get-signal in1) (get-signal in2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! out new-value)))))
  (add-action! in1 and-action)
  (add-action! in2 and-action)
  'ok)

(define (or-gate in1 in2 out)
  (define (or-action)
    (let ((new-value (logical-or (get-signal in1) (get-signal in2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! out new-value)))))
  (add-action! in1 or-action)
  (add-action! in2 or-action)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

; simulation
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "   new value = ")
                 (display (get-signal wire))
                 (newline))))

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)
(propagate)

(set-signal! input-2 1)
(propagate)