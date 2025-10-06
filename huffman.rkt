#lang sicp

; helpers
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? obj)
  (eq? (car obj) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons
               (car set)
               (adjoin-set x (cdr set))))))

(define (has-symbol x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (has-symbol x (cdr set)))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

; encoder
(define (encode message tree)
  (define (encode-symbol symbol tree)
    (cond ((leaf? tree)
           (if (not (equal? symbol (symbol-leaf tree)))
               (error "invalid symbol")
               '()))
          ((has-symbol
            symbol
            (symbols (car tree))) (cons 0 (encode-symbol symbol (car tree)))) ;left
          (else (cons 1 (encode-symbol symbol (cadr tree)))))) ;right
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree) (encode (cdr message) tree))))

; decoder
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "invalid bit"))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)
              ))))
  (decode-1 bits tree))

; huffman's algorithm
(define (generate-huffman-tree pairs)
  (define (successive-merge set)
    (if (> (length set) 1)
        (let ((merged (make-code-tree (car set) (cadr set))))
          (successive-merge (adjoin-set merged (cddr set))))
        (car set)))
  (successive-merge (make-leaf-set pairs)))


; tests
; a sample tree
;       .
;    A     .
;       B     .
;          D     C
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define message (list 'A 'D 'A 'B 'B 'C 'A))
(equal? (decode (encode message sample-tree) sample-tree) message)

; the same example above
(define sample-pairs
  (list (list 'A 4) (list 'B 2) (list 'C 1) (list 'D 1)))
(define huffman-tree (generate-huffman-tree sample-pairs))
(equal? (decode (encode message huffman-tree) sample-tree) message)
(equal? (decode (encode message sample-tree) huffman-tree) message)

; ex 2.70
(define pairs
  (list (list 'A 2) (list 'GET 2) (list 'SHA 3) (list 'WAH 1) (list 'BOOM 1)
        (list 'JOB 2) (list 'NA 16) (list 'YIP 9))
  )

(set! huffman-tree (generate-huffman-tree pairs))
(set! message (list 'GET 'A 'JOB
                    'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
                    'GET 'A 'JOB
                    'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA
                    'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP
                    'SHA 'BOOM))

(display "Bits used with huffman encoding: ") (length (encode message huffman-tree))
(display "Bits used with a 3-bit fixed length code: ") (* 3 (length message))