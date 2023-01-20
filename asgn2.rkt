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

