#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

(provide make-clock
         new-clock-state
         Clock%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define RED "red")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for Clock%:
;; (new Clock% [x Int][y Int][t time]
;;                [selected? Boolean][mx Int][my Int])
;; the last 3 arguments are optional
;; Interpretation: An object of class Clock% represents a clock.

(define Clock%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one clock to
    ;; the next.
    
    ; the x and y position of the center of the clock
    (init-field x y)
    
    ; t identifies current tick value
    (init-field t)
    
    ; is the clock selected? Default is false.
    (init-field [selected? false])
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; image for displaying the clock when it is selected.
    
    (field [CLOCK-IMG (rectangle 60 40 OUTLINE-CIRCLE BLUE)])
    
    (super-new)
    
    ;; after-tick : -> Widget
    ;; GIVEN: No arguments
    ;; RETURNS: A widget which is a toy object of the class Clock
    ;; as it should be after a tick.
    ;; STRATEGY: use template for clock.
    ;; EXAMPLES: See test cases
    
    (define/public (after-tick)
      (new Clock%
           [x x]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [t (+ t 1)]))
    
    ;; after-key-event : KeyEvent -> Widget
    ;; GIVEN: a keyevent
    ;; RETURNS: A widget which is a toy object of the class Clock
    ;; as it should be after a key event.
    ;; DETAILS: a Clock ignores key events
    ;; EXAMPLES: See test cases
    
    (define/public (after-key-event kev)
      this)      
    
    ;; after-button-down : Int Int -> Widget
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A widget which is a toy object of the class Clock
    ;; as it should be after a button-down event.
    ;; STRATEGY: Cases on whether the event is in the clock
    ;; If the clock is not selected, then select it.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (new Clock%
               [x x][y y]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)]
               [t t])
          this))
    
    ;; after-button-up : Int Int -> Widget
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A widget which is a toy object of the class Clock
    ;; as it should be after a button-up event.
    ;; STRATEGY: Cases on whether the event is in the Clock.
    ;; If the clock is selected, then unselect it.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-up mx my)
      (if (in-clock? mx my)
          (new Clock%
               [x x][y y]
               [selected? false]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [t t])
          this))   
    
    ;; after-drag : Int Int -> Widget
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A widget which is a toy object of the class Clock
    ;; as it should be after a drag event.
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    ;; EXAMPLES: See test cases
    
    (define/public (after-drag mx my)
      (if selected?
          (new Clock%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [t t])
          this))   
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    ;; STRATEGY:combine simpler functions.
    ;; EXAMPLES: See test cases
    
    (define/public (add-to-scene scene)
      (place-image CLOCK-IMG x y (place-text scene)))
    
    ;; place-text : Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: A scene like the given one, but with current tick painted on
    ;; the clock
    ;; STRATEGY:combine simpler functions.
    ;; EXAMPLES: See test cases
    
    (define/public (place-text scene)
      (place-image (text(number->string t) 10 RED) x y scene))
    
    ;; in-clock? : Int Int -> Boolean
    ;; GIVEN: location of the mouse coordinates
    ;; RETURNS: true iff the location is inside the clock.
    ;; STRATEGY: combine simper functions
    ;; EXAMPLES: See test cases
    
    (define (in-clock? other-x other-y)
      (and
       (<= 
        (- x 30)
        other-x
        (+ x 30))
       (<= 
        (- y 15)
        other-y
        (+ y 15))))
    
    
    ;;  after-move: Int Int -> Toy
    ;;  Given: coordinate of the mouse
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    ;; EXAMPLES: See test cases
    
    (define/public  (after-move mx my)
      this)
    
    ;; toy-x: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the x position of the center of the toy
    ;; EXAMPLES: See test cases
    
    (define/public  (toy-x)
      x)
    
    ;; toy-y: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: the y position of the center of the toy
    ;; EXAMPLES: See test cases
    
    (define/public  (toy-y)
      y)
    
    ;; toy-data: -> Int
    ;; GIVEN: no arguments
    ;; RETURNS: Current time of the throbber
    ;; EXAMPLES: See test cases
    
    (define/public (toy-data)
      t)
    
    ;; -> Boolean
    ;; RETURNS: True iff clock is selected
    ;; EXAMPLES: See test cases
    (define/public (for-test:selected?) selected?)
    
    ))

;; make-clock: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; STRATEGY:combine simpler functions
;; EXAMPLES: See test cases

(define (make-clock x y)
  (new-clock-state x y 0))

;; new-clock-state: PosInt PosInt PosInt -> Toy
;; GIVEN: an x a y position and time t
;; RETURNS: an object representing a clock at the given position.
;; STRATEGY: Use template for throbber.
;; EXAMPLES: See test cases

(define (new-clock-state x y t)
  (new Clock% [x x][y y] [t t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constants for tests
(define C0 (make-clock 50 50))
(define C1 (send C0 after-tick))
(define C2 (send C0 after-button-down 45 45))
(define C3 (send C2 after-button-up 45 45))
(define C4 (send C2 after-drag 46 46))
(define C5 (send C2 after-move 40 40))
(define C6 (send C2 after-key-event "p"))
(define C7 (send C0 after-button-down 5 5))
(define C8 (send C0 after-button-up 5 5))
(define C9 (send C0 after-drag 5 5))
(define TEXT (place-image (text "1" 10 RED) 50 50 EMPTY-CANVAS))
(define CLOCK (place-image
               (rectangle 60 40 OUTLINE-CIRCLE BLUE) 50 50 TEXT))

;; TESTS:
(begin-for-test
  (check-equal? (send C1 toy-data) 1)
  (check-equal? (send C2 for-test:selected?) #t)
  (check-equal? (send C3 for-test:selected?) #f)
  (check-equal? (send C7 for-test:selected?) #f)
  (check-equal? (send C8 for-test:selected?) #f)
  (check-equal? (send C4 toy-x) 51)
  (check-equal? (send C5 toy-x) 50)
  (check-equal? (send C6 toy-x) 50)
  (check-equal? (send C9 toy-y) 50)
  (check-equal? (send C1 place-text EMPTY-CANVAS) TEXT)
  (check-equal? (send C1 add-to-scene EMPTY-CANVAS) CLOCK))

