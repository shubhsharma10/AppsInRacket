#lang racket
(require "Interfaces.rkt")

(provide limit-dragged-particle-coord)

;; limit-dragged-particle-coord : Real Real Real NonNegInt -> Real
;; GIVEN: current coordinate, delta, offset to which it was dragged and maximum
;; for given coordinate
;; RETURNS: new coordinate
;; STRATEGY: Combine simpler function
(define (limit-dragged-particle-coord coord delta offset max)
  (local
    ((define newC (- delta offset)))
    (if
     (or (> newC max) (< newC 0))
     coord
     newC)))
