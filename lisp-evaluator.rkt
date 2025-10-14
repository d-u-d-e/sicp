#lang sicp

; representing expressions

; a primitive lisp expression evaluates to itself
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

; variables are represented by symbols
(define (variable? exp) (symbol? exp))

; quotations have the form (quote <text of quote>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; assignments have the form (set! <var> <value>)
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; definitions have the form (define <var> <value>) or 
; (define (<var> <param1> ... <paramn>) <body>) which is sugar for
; (define <var> (lambda (<param1> ... <paramn>) <body>))
