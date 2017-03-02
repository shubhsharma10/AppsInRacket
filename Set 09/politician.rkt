#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

(provide make-politician)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define REPEL-DISTANCE 75)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
    (init-field [saved-mx (/ CANVAS-WIDTH 2)] [saved-my 0])
    
    ; image for displaying the first politician.    
    (field [POLITICIAN1 (bitmap "Donald.png")])
    
    ; image for displaying the second politician.    
    (field [POLITICIAN2 (bitmap "Hillary.png")])
    
    (super-new)
    
    
    ;; after-tick : -> Widget
    ;; GIVEN: No arguments
    ;; RETURNS: A widget which is a toy object of the class politician
    ;; as it should be after a tick.
    ;; STRATEGY: cases on the distance of the mouse from the image.
    ;; EXAMPLES: See test cases
    
    (define/public (after-tick )
      (if (> (distance-from-mouse) REPEL-DISTANCE)
          (politician-towards-mouse)
          (politician-repel saved-mx saved-my)))
    
    ;; politician-towards-mouse : -> Politician
    ;; GIVEN: No arguments
    ;; RETURNS: A toy object of the class politician moving towards
    ;; the mouse in a straight line after each tick.
    ;; STRATEGY: Use template of politician.
    ;; EXAMPLES: See test cases
    
    (define (politician-towards-mouse)
      (new Politician%
           [x (exact-round(/ (+  (* 7 x) saved-mx) 8))]
           [y (exact-round(/ (+  (* 7 y) saved-my) 8))]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [face-change? face-change?]))
    
    ;; politician-repel : -> Politician
    ;; GIVEN: No arguments
    ;; RETURNS: A toy object of the class politician moving away
    ;; from the mouse in a straight line.
    ;; STRATEGY: Use template of politician.
    ;; EXAMPLES: See test cases
    
    (define (politician-repel mx my)
      (new Politician%
           [x (exact-round(- (* 4 x) (* 3 mx)))]
           [y (exact-round(- (* 4 y) (* 3 my)))]
           [saved-mx mx]
           [saved-my my]
           [face-change? (not face-change?)]))
    
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
    
    ;; after-key-event : KeyEvent -> Widget
    ;; GIVEN: a keyevent
    ;; RETURNS: A widget which is a toy object of the class Politician
    ;; as it should be after a key event.
    ;; DETAILS: a Politician ignores key events
    ;; EXAMPLES: See test cases
    
    (define/public (after-key-event kev)
      this)      
    
    
    ;; after-button-down : Int Int -> Widget
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A widget which is a toy object of the class Politician
    ;; as it should be after a button-down event.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-down mx my)
      (after-move mx my))
    
    ;; after-button-up : Int Int -> Widget
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A widget which is a toy object of the class Politician
    ;; as it should be after a button-up event.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-up mx my)
      (after-move mx my))
    
    ;; after-drag : Int Int -> Widget
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a drag event.
    ;; STRATEGY: Use template of Politician.
    ;; EXAMPLES: See test cases
    
    (define/public (after-drag mx my)
      (after-move mx my))
    
    ;;  after-move: Int Int -> Toy
    ;;  Given: coordinate of the mouse
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    ;; STRATEGY: Use template of Politician.
    ;; EXAMPLES: See test cases
    
    (define/public (after-move mx my)
      (if (> (distance-from-given-mouse mx my) REPEL-DISTANCE)
          (new Politician%
               [x x]
               [y y]
               [saved-mx mx]
               [saved-my my]
               [face-change? face-change?])
          (politician-repel mx my)))
    
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this politician painted
    ;; on it.
    ;; STRATEGY: combine simpler functions
    ;; EXAMPLES: See test cases
    
    (define/public (add-to-scene scene)
      (if (equal? face-change? true)
          (place-image POLITICIAN1 x y scene)
          (place-image POLITICIAN2 x y scene)))
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: the x position of the center of the toy
    ;; EXAMPLES: See test cases
    (define/public  (toy-x)
      x)
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: the y position of the center of the toy
    ;; EXAMPLES: See test cases
    (define/public  (toy-y)
      y)
    
    ;; -> Int
    ;; GIVEN:no arguments
    ;; RETURNS: Current distance to the mouse
    ;; EXAMPLES: See test cases
    (define/public (toy-data)
      (distance-from-mouse))
    
    ))


;; make-politician : PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a politician at the given position.
;; STRATEGY: Use template for .
;; EXAMPLES: See test cases

(define (make-politician x y)
  (new Politician% [x x] [y y]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants for TESTS:

(define P0 (make-politician 50 50))
(define P1 (send P0 after-tick))
(define P2 (make-politician 100 100))
(define P2-after-move (send P2 after-move 80 80))
(define P4 (send P2-after-move after-tick))
(define P5 (send (send (send P4 after-tick) after-tick) after-tick))
(define P2-in-scene (place-image (bitmap "Donald.png")
                                 100 100 EMPTY-CANVAS))
(define P4-in-scene (place-image (bitmap "Hillary.png")
                                 (send P4 toy-x)
                                 (send P4 toy-y)
                                 EMPTY-CANVAS))
(define P3 (send P2 after-tick))

;; TESTS:

(begin-for-test
  (check-equal?
   (send P1 toy-x) 75
   "toy-x should be 75")
  (check-equal? (send P1 toy-y) 44
                "toy-y should be 44")
  (check-equal? (send P3 toy-y) 88
                "toy-y should be 88")
  (check-equal? (send P3 toy-y) 88
                "toy-y should be 88")
  (check-equal? (send P2 toy-data) 180
                "toy-data should be 180")
  (check-equal? (send P4 toy-data) 99 "toy-data should be 99")
  (check-equal? (send P5 toy-data) 300 "toy-data should be 300")
  (check-equal? (send P4 add-to-scene EMPTY-CANVAS)
                P4-in-scene "P4 in canvas")
  (check-equal? (send P2 add-to-scene EMPTY-CANVAS) P2-in-scene "P2 in canvas")
  (check-equal? (send (send P2 after-move 15 15) toy-x)
                100 "toy-x should be 100")
  (check-equal? (send (send P2 after-drag 15 15) toy-x) 100
                "toy-x should be 100")
  (check-equal? (send P1 after-key-event "p") P1)
  (check-equal? (send (send P2 after-button-down 15 15) toy-x) 100
                "toy-x should be 100")
  (check-equal? (send (send P2 after-button-up 15 15) toy-x) 100)
  "toy-x should be 100")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
