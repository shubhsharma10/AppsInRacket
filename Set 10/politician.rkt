#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "WidgetWorks.rkt")

(provide make-politician
         Politician%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Politician class

;; Constructor template for Politician%:
;; (new Politician% [x Int][y Int]
;;                [face-change? Boolean][mx Int][my Int])
;; the last 3 arguments are optional
;; Interpretation: An object of class Politician% represents a politician.

(define Politician%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one politician to
    ;; the next.
    
    ; the x and y position of the center of the politician    
    (init-field x y)
    
    ; boolean to change the politician image on repel
    (init-field [face-change? true])
    
    ; the position of the mouse coordinates 
    (init-field [saved-mx (/ CANVAS-WIDTH 2)] [saved-my ZERO])
    
    ; image for displaying the first politician-Donald.    
    (field [DONALD (bitmap "Donald.png")])
    
    ; image for displaying the second politician-Hillary.    
    (field [HILLARY (bitmap "Hillary.png")])
    
    ; Minimum distance between politcian image and mouse coordinates
    (field [REPEL-DISTANCE 75])
    
    ;Move towards constatns
    (field [MOVE-TOWARDS-X1 7] [MOVE-TOWARDS-X2 8])

    ;Repel Coordinates
    (field [REPEL-X1 4] [REPEL-X2 3])
    
    (super-new)
    
    ;; ------------------------------------------------------------------------
    ;; Interface required functions
    ;; ------------------------------------------------------------------------
    
    
    ;; after-tick : -> Void
    ;; GIVEN: No arguments
    ;; STRATEGY: cases on the distance of the mouse from the image.
    ;; EFFECT: If distance more than REPEL-DISTANCE then jump back else move
    ;; towards the mouse
    ;; EXAMPLES: See test cases    
    (define/public (after-tick )
      (if (> (distance-from-mouse) REPEL-DISTANCE)
          (politician-towards-mouse)
          (politician-repel saved-mx saved-my)))    
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent
    ;; EFFECT: No effect
    ;; DETAILS: a Politician ignores key events
    ;; EXAMPLES: See test cases    
    (define/public (after-key-event kev)
      this)      
    
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of a button-down event
    ;; EFFECT: object of the class Politician
    ;; as it should be after a button-down event.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-down mx my)
      (after-move mx my))
    
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: object of the class Politician
    ;; as it should be after a button-up event.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-up mx my)
      (after-move mx my))
    
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: object of the class politician
    ;; as it should be after a drag event.
    ;; EXAMPLES: See test cases    
    (define/public (after-drag mx my)
      (after-move mx my))
    
    
    ;; after-move: Int Int -> Void
    ;; Given: coordinate of the mouse
    ;; EFFECT: Change politician's (x,y) coordinates such that it jumps back
    ;; if distance from mouse is less than REPEL-DISTANCE else save mouse
    ;; coordinates
    ;; STRATEGY: Use case on distance from mouse.
    ;; EXAMPLES: See test cases    
    (define/public (after-move mx my)
      (if (> (distance-from-given-mouse mx my) REPEL-DISTANCE)
          (begin
            (set! saved-mx mx)
            (set! saved-my my))
          (politician-repel mx my)))    
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this politician painted
    ;; on it.
    ;; STRATEGY: Cases on whether face-change? is true or not
    ;; EXAMPLES: See test cases    
    (define/public (add-to-scene scene)
      (if (equal? face-change? true)
          (place-image DONALD x y scene)
          (place-image HILLARY x y scene)))
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: the x position of the center of the toy
    ;; EXAMPLES: See test cases
    (define/public  (toy-x)
      (exact-round x))
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: the y position of the center of the toy
    ;; EXAMPLES: See test cases
    (define/public  (toy-y)
      (exact-round y))
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: Current distance to the mouse
    ;; EXAMPLES: See test cases
    (define/public (toy-data)
      (distance-from-mouse))
    
    ;; -------------------------------------------------------------------------
    ;; Non-Interface required functions:
    ;; -------------------------------------------------------------------------
    
    ;; politician-towards-mouse : -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: update politician's (x,y) coordinats such that it is moving
    ;; towards the mouse in a straight line after each tick.
    ;; EXAMPLES: See test cases    
    (define (politician-towards-mouse)
      (begin
        (set! x (exact-round(/ (+  (* MOVE-TOWARDS-X1 x) saved-mx)
                               MOVE-TOWARDS-X2)))
        (set! y (exact-round(/ (+  (* MOVE-TOWARDS-X1 y) saved-my)
                               MOVE-TOWARDS-X2)))))
    
    
    ;; politician-repel : -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: update politician's (x,y) coordinats such that it is moving away
    ;; from the mouse in a straight line.
    ;; EXAMPLES: See test cases    
    (define (politician-repel mx my)
      (begin
        (set! x (exact-round(- (* REPEL-X1 x) (* REPEL-X2 mx))))
        (set! y (exact-round(- (* REPEL-X1 y) (* REPEL-X2 my))))
        (set! saved-mx mx)
        (set! saved-my my)
        (set! face-change? (not face-change?))))
    
    
    ;; distance-from-mouse : -> Int
    ;; GIVEN: No arguments
    ;; RETURNS: the distance of the centre of the image and the mouse coordinae
    ;; in the next tick
    ;; STRATEGY: combine simpler function.
    ;; EXAMPLES: See test cases    
    (define (distance-from-mouse)
      (exact-round (calculate-distance
                    x
                    y
                    saved-mx
                    saved-my)))
    
    
    ;; distance-from-given-mouse : Int Int -> Int
    ;; GIVEN: (x,y) coordinates of given mouse
    ;; RETURNS: the distance of the centre of the image and the mouse coordinae
    ;; in the next tick
    ;; STRATEGY: combine simpler function.
    ;; EXAMPLES: See test cases    
    (define (distance-from-given-mouse mx my)
      (exact-round (calculate-distance
                    x
                    y
                    mx
                    my)))
    
    
    ;; calculate-distance : Int Int Int Int -> NonNegReal
    ;; GIVEN: No arguments
    ;; RETURNS: the distance of the centre of the image and the mouse coordinae
    ;; in the next tick
    ;; STRATEGY: combine simpler function.
    ;; EXAMPLES: See test cases    
    (define (calculate-distance x y mx my)
      (sqrt (+ (sqr(- mx x)) (sqr(- my y)))))
    
    ))


;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position.
;; EXAMPLES: See test cases
(define (make-politician x y)
  (new Politician% [x x] [y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;TESTS:

(begin-for-test
  (local
    ((define P0 (make-politician 50 50)))
    (send P0 after-tick) 
    (check-equal? (send P0 toy-x) 75
                  "Toy-x is equal to 75.")
    (check-equal? (send P0 toy-y) 44
                  "Toy-y is equal to 44.")
    (send P0 after-key-event "p")
    (check-equal? (send P0 toy-x) 75
                  "Toy-x is equal to 75.")
   ))

(begin-for-test
  (local
    ((define P1 (make-politician 100 100))
     (define P1-in-scene (place-image (bitmap "Donald.png")
                                 100 100 EMPTY-CANVAS)))
    (check-equal? (send P1 add-to-scene EMPTY-CANVAS) P1-in-scene
                  "Politician is added to scene.")
    (check-equal? (send P1 toy-data) 180
                  "Toy-data is 180.")
    (send P1 after-move 80 80)
    (send P1 after-tick)
    (check-equal? (send P1 add-to-scene EMPTY-CANVAS)
                  (place-image (bitmap "Hillary.png")
                                 (send P1 toy-x)
                                 (send P1 toy-y)
                                 EMPTY-CANVAS)
                  "Politician is added to the canvas")
    (check-equal? (send P1 toy-data) 99
                  "The toy-data is 99")
    (send P1 after-tick)
    (send P1 after-tick)
    (send P1 after-tick)
    (check-equal? (send P1 toy-data) 300
                  "The toy-data is 300")
    ))

(begin-for-test
  (local
    ((define P2 (make-politician 100 100)))
     (send P2 after-move 15 15)
     (check-equal? (send P2 toy-x) 100
                   "The x coordinate is 100")
     ))

(begin-for-test
  (local
    ((define P2 (make-politician 100 100)))
     (send P2 after-drag 15 15)
     (check-equal? (send P2 toy-x) 100
                   "The x coordinate is 100")
     (send P2 after-button-down 15 15)
     (check-equal? (send P2 toy-x) 100
                   "The x coordinate is 100")
     (send P2 after-button-up 15 15)
     (check-equal? (send P2 toy-x) 100
                   "The x coordinate is 100")
     ))
     

