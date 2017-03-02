#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "metatoy.rkt")
(require "throbber.rkt")
(require "clock.rkt")
(require "politician.rkt")

(check-location "09" "metatoy.rkt")
(check-location "09" "q1.rkt")
(check-location "09" "clock.rkt")
(check-location "09" "throbber.rkt")
(check-location "09" "politician.rkt")
(check-location "09" "interfaces.rkt")

(provide run
         make-metatoy
         make-throbber
         make-clock
         make-politician
         Metatoy<%>
         Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; run : PosNum -> Metatoy
; GIVEN: a frame rate, in secs/tick
; EFFECT: creates a Metatoy with no toys in it, and runs it using big-bang
; at the given frame rate.
; RETURNS: the final state of the Metatoy.
; STRATEGY: combine simpler functions
; EXAMPLES: See test cases

(define (run rate)
  (big-bang (make-metatoy empty)
            (on-tick
             (lambda (mt) (send mt after-tick))
             rate)
            (on-draw
             (lambda (mt) (send mt to-scene)))
            (on-key
             (lambda (mt kev)
               (send mt after-key-event kev)))
            (on-mouse
             (lambda (mt mx my mev)
               (send mt after-mouse-event mx my mev)))))

(define TOY-X 250)
	(define TOY-Y 300)
;	(define (toy-moved-correctly? toy mx my)
;	  (local
;	   ((define TOY-AFTER-MOVE (send toy after-move mx my))
;	    (define TOY-AFTER-MOVE-AND-TICK (send TOY-AFTER-MOVE after-tick)))))
	   
	(define POLITICIAN (make-politician TOY-X TOY-Y))
	(define INITIAL-METATOY (make-metatoy '()))
    (define P1 (send POLITICIAN after-move 100 100))
(define P2 (send P1 after-tick))
