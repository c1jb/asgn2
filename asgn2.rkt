#lang typed/racket

(require typed/rackunit)

;2.3.3

(define profit-mult 5)
(define cost-const 20)
(define cost-per 0.5)

(define (total-profit [num : Real]) : Real
  (- (* num profit-mult) (+ cost-const (* cost-per num))))

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

(define pen-dist 150)
(define pencil-dist 56)

(define (how-far-to-write [wri : Writer]) : Real
  (match wri
  [(Pen ink full) (* pen-dist (* ink full))]
  [(Pencil len) (* pencil-dist len)]))

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
(struct Leaf ([val : Symbol]) #:transparent)
(struct Node ([right : BTree] [left : BTree]) #:transparent)

(Node (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e))) (Leaf 'f))
(Node (Leaf 'a) (Leaf 'b))
(Leaf 'a)

;2.6

(define (zz-tree [tree : BTree]) : BTree
  (match tree
    [(Leaf v) (Leaf 'zz)]
    [(Node r l) (Node (zz-tree r) (zz-tree l))]))

(check-equal? (zz-tree (Node (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'c) (Leaf 'e))) (Leaf 'f))) (Node (Node (Node (Leaf 'zz) (Node (Leaf 'zz) (Leaf 'zz))) (Node (Leaf 'zz) (Leaf 'zz))) (Leaf 'zz)))

;2.7

(define (min-depth [tree : BTree]) : Real
  (match tree
    [(Leaf v) 0]
    [(Node r l) (cond
                  [(> (min-depth r) (- (min-depth l) 1))  (+ 1 (min-depth l))]
                  [else (+ 1 (min-depth r))])]))

(check-equal? (min-depth (Leaf 'a)) 0)
(check-equal? (min-depth (Node (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e))) (Node (Leaf 'f) (Leaf 'g)))) 2)

;2.8

(define (contains? [tree : BTree] [v : Any]) : Boolean
  (match tree
    [(Leaf val) (equal? val v)]
    [(Node r l) (or (contains? r v) (contains? l v))]))

(check-equal? (contains? (Node (Node (Node (Leaf 'b) (Node (Leaf 'c) (Leaf 'a))) (Node (Leaf 'e) (Leaf 'f))) (Node (Leaf 'g) (Leaf 'h))) 'a) #t)
(check-equal? (contains? (Node (Node (Node (Leaf 'a) (Node (Leaf 'b) (Leaf 'c))) (Node (Leaf 'd) (Leaf 'e))) (Node (Leaf 'f) (Leaf 'g))) 'h) #f)

;2.9

(define (subst [orig : BTree] [sym : Symbol] [rep : BTree]) : BTree
  (match orig
    [(Leaf val) (cond
                [(equal? val sym) rep]
                [else orig])]
    [(Node r l) (Node (subst r sym rep) (subst l sym rep))]))

(check-equal? (subst (Node (Node (Leaf 'ab) (Leaf 'zz)) (Leaf 'ab)) 'ab (Node (Leaf 'yy) (Leaf 'yy))) (Node (Node (Node (Leaf 'yy) (Leaf 'yy)) (Leaf 'zz)) (Node (Leaf 'yy) (Leaf 'yy))))
(check-equal? (subst (Leaf 'ab) 'ab (Leaf 'zz)) (Leaf 'zz))
