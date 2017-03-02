#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "sblock.rkt")
(require "interfaces2.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-cubefactory)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-cubefactory : Container -> CubeFactory
;; GIVEN: a world
;; RETURNS: an object of class CubeFactory% containing container
;; EXAMPLES: See test cases
(define (make-cubefactory world)
  (new Cubefactory% [w world]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Cubefactory% class

;; It implements SWidget<%> interface.

;; Constructor template for Cubefactory%
;; (new Cubefactory% [w Container%])
;; Interpretation: An object of class Cubefactory% creates SWidget and adds
;; them to container.

(define Cubefactory%
  (class* object% (SWidget<%>)
    
    ;; the init-fields are the values that may vary from one SBlock to
    ;; the next.
    
    ;; container to which sblocks will be added
    (init-field w)
    
    ;; saved coordinates of mouse
    (init-field [saved-mx (/ CANVAS-WIDTH 2)] [saved-my (/ CANVAS-HEIGHT 2)])
    
    ;; all sblocks present in the container
    (field [cubes empty])
    
    (super-new)
    
    ;; -------------------------------------------------------------------------
    ;; Interface required methods
    ;; -------------------------------------------------------------------------
    
    ;; after-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: No effect
    ;; DETAILS: Cubefactory ignores this event
    
    (define/public (after-tick) this)
    
    
    ;; after-button-down: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of mouse
    ;; EFFECT: Stores mouse coordinates in saved-mx, saved-my
    ;; EXAMPLE: See tests
    
    (define/public (after-button-down mx my)      
      (begin
        (set! saved-mx mx)
        (set! saved-my my)))
    
    
    ;; after-button-up: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of mouse
    ;; EFFECT: Stores mouse coordinates in saved-mx, saved-my
    ;; EXAMPLE: See tests
    
    (define/public (after-button-up mx my)
      (begin
        (set! saved-mx mx)
        (set! saved-my my)))
    
    
    ;; after-drag: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: No effect
    ;; DETAILS: Cubefactory ignores this event
    ;; EXAMPLE:See tests
    
    (define/public (after-drag mx my)
      this)
    
    ;; add-to-scene: Scene -> Scene
    ;; EFFECT: No effect
    ;; RETURNS: Scene as given with no change
    ;; DETAILS: Cubefactory ignores this event
    ;; EXAMPLE:See tests
    
    (define/public (add-to-scene s) s)
    
    
    ;; after-move: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: No effect
    ;; DETAILS: Cubefactory ignores this event
    ;; EXAMPLE:See tests
    
    (define/public (after-move mx my)
      this)
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: A keyevent
    ;; EFFECT: Creates sblock if "b" key is pressed and adds it to container
    ;; also add all sblocks to each sblock
    ;; STRATEGY: Cases on Kev
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev "b")
         (local
           ((define new-cube (make-block saved-mx saved-my empty)))
           (begin
             (set! cubes (cons new-cube cubes))
             (send w add-stateful-widget new-cube)
             (update-all-cubes)))]))
    
    ;; -------------------------------------------------------------------------
    ;; Non-Interface required methods
    ;; -------------------------------------------------------------------------
    
    
    ;; update-all-cubes -> Void
    ;; EFFECT: Adds all sblock present to each sblock's all-cubes list
    ;; STRATEGY: Use for-each HOF on cubes
    ;; EXAMPLE: See tests
    (define (update-all-cubes)
      (for-each
       (lambda (cb) (populate-for-a-cube cb)) cubes))
    
    
    ;; populate-for-a-cube: SBlock% -> Void
    ;; EFFECT: Adds all sblock present to given sblock's all-cubes list
    ;; STRATEGY: Use for-each HOF on cubes
    ;; EXAMPLE: See tests
    (define (populate-for-a-cube cube)
      (for-each
       (lambda (cb) (send cube add-to-all-cubes cb)) cubes))

    ))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS:

(begin-for-test
  (local
    ((define W (container-init 500 600))
     (define CF1 (make-cubefactory W))
     (define s (place-image (square 20 "outline" "green") 100 100
                            (empty-scene 500 600))))
    (send CF1 after-key-event "b")
    (check-equal? (length (get-field cubes CF1)) 1
                  "The length should be 1")
    (check-equal? (get-field saved-mx CF1) 300
                  "The cube is created at the initial saved-mx")
    (send CF1 after-button-down 100 100)
    (send CF1 after-key-event "b")
    (check-equal? (send CF1 add-to-scene s) s
                  "Scene is displayed.")
    (send CF1 after-key-event "b")
    (check-equal? (length (get-field cubes CF1)) 3
                  "The length should be 3")
    (check-equal? (get-field saved-mx CF1) 100
                  "The cube is created at 100")
    (send CF1 after-tick)
    (check-equal? (length (get-field cubes CF1)) 3
                  "The length should be 3")
    (send CF1 after-drag 150 150)
    (send CF1 after-button-up 150 150)
    (send CF1 after-key-event "b")
    (check-equal? (length (get-field cubes CF1)) 4
                  "The length should be 4")
    (check-equal? (get-field saved-mx CF1) 150
                  "The cube is created at 150")
    (send CF1 after-button-up 150 150)
    (send CF1 after-move 160 160)
    (check-equal? (get-field saved-mx CF1) 150
                  "The saved-mx is at 150")
    ))
    
