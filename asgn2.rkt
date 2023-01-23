#lang typed/racket

(require typed/rackunit)

;2.3.3

(define (total-profit [num : Real]) : Real
  (- (* num 5) (+ 20 (* 0.5 num))))

(check-= (total-profit 5) 2.5 0 "does not work")
(check-= (total-profit 2) -11 0 "does not work")
(check-= (total-profit 100) 430 0 "does not work")

;3.3.3

(define (area-cylinder [radius : Real] [height : Real]) : Real
  (+ (* 2 (* pi (* radius radius))) (* (* pi (* 2 radius)) height)))

(check-= (area-cylinder 3 4) 131.94 0.1 "wrong")

;2.2

(define-type Writer (U Pen Pencil))
(struct Pen ([ink-volume : Real] [how-full : Real]) #:transparent)
(struct Pencil ([length : Real]) #:transparent)

(define (how-far-to-write [wri : Writer]) : Real
  (match wri
  [(Pen ink full) (* 150 (* ink full))]
  [(Pencil len) (* 56 len)]))

(check-= (how-far-to-write (Pen 2 0.5)) 150 0 "wrong")
(check-= (how-far-to-write (Pencil 15)) 840 0 "wrong")

;2.3

(define-type Polynomial (U Linear Quadratic))
(struct Linear ([A : Real] [B : Real]) #:transparent)
(struct Quadratic ([A : Real] [B : Real] [C : Real]) #:transparent)

(define (interp [p : Polynomial] [x : Real]) : Real
  (match p
  [(Linear A B) (+ (* A x) B)]
  [(Quadratic A B C) (+ (+ (* A (* x x)) (* B x)) C)]))

(check-= (interp (Linear 5 3) 2) 13 0 "wrong")
(check-= (interp (Quadratic 5 3 1) 2) 27 0 "wrong")

;2.4

(define (derivative [p : Polynomial]) : Polynomial
  (match p
    [(Linear A B) (Linear 0 A)]
    [(Quadratic A B C) (Linear (* A 2) B)]))

(check-equal? (derivative (Linear 2 3)) (Linear 0 2))
(check-equal? (derivative (Quadratic 2 3 4)) (Linear 4 3))

;2.5

(define-type BTree (U Leaf Node))
(struct Leaf ([val : Real]) #:transparent)
(struct Node ([right : BTree] [left : BTree]) #:transparent)

(Node (Node (Node (Leaf 5) (Node (Leaf 2) (Leaf 1))) (Node (Leaf 3) (Leaf 4))) (Leaf 6))
(Node (Leaf 1) (Leaf 2))
(Leaf 0)

;2.6

