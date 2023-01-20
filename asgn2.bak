#lang typed/racket

(require typed/rackunit)

(define (total-profit [num : Real]) : Real
  (- (* num 5) (+ 20 (* 0.5 num))))

(check-= (total-profit 5) 2.5 0 "does not work")
(check-= (total-profit 2) -11 0 "does not work")
(check-= (total-profit 100) 430 0 "does not work")