#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "WidgetWorks.rkt")

(provide make-clock
         Clock%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define RED "red")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for Clock%:
;; (new Clock% [x Int][y Int][t time]
;;             [selected? Boolean][saved-mx Int][saved-my Int])
;; the last 4 arguments are optional
;; Interpretation: An object of class Clock% represents a clock.

(define Clock%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one clock to
    ;; the next.
    
    ; the x and y position of the center of the clock
    (init-field x y)
    
    ; t identifies current tick value
    (init-field [t ZERO])
    
    ; is the clock selected? Default is false.
    (init-field [selected? false])
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the clock, relative to the
    ;; clock's center.  Else any value.
    
    (init-field [saved-mx ZERO] [saved-my ZERO])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; Height and Width for clock
    (field [CLOCK-HEIGHT 40] [CLOCK-WIDTH 60])
    
    ; image for displaying the clock when it is selected.    
    (field [CLOCK-IMG (rectangle CLOCK-WIDTH CLOCK-HEIGHT OUTLINE-CIRCLE BLUE)])
    
    ; number by which time needs to be increment in each tick
    (field [TICK-INCREMENT 1])
    
    ; Font size for text displayed
    (field [FONT-SIZE 10])
    
    (super-new)
    
    
    ;; -------------------------------------------------------------------------
    ;; Interface required functions
    ;; -------------------------------------------------------------------------
    
    
    ;; after-tick : -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Tick incremented by TICK-INCREMENT
    ;; EXAMPLES: See test cases    
    (define/public (after-tick)
      (begin
        (set! t (+ t TICK-INCREMENT))))
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent
    ;; EFFECT: No effect
    ;; DETAILS: a Clock ignores key events
    ;; EXAMPLES: See test cases    
    (define/public (after-key-event kev)
      this)
    
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of a button-down event
    ;; EFFECT: Store difference between mouse and center if
    ;; mouse is in clock
    ;; STRATEGY: Cases on whether the mouse is in the clock
    ;; If the clock is not selected, then select it.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-down mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: Set selected? to false if mouse is in clock
    ;; STRATEGY: Cases on whether the event is in the Clock.
    ;; If the clock is selected, then unselect it.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-up mx my)
      (if (in-clock? mx my)
          (begin
            (set! selected? false))
          this))
    
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: Update coordinate of clock using delta stored in saved-mx,
    ;; saved-my if it is selected
    ;; STRATEGY: Cases on whether the clock is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    ;; EXAMPLES: See test cases    
    (define/public (after-drag mx my)
      (if selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (set! selected? true))
          this))   
    
    
    ;; add-to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this clock painted
    ;; on it.
    ;; STRATEGY: Combine simpler functions.
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
      (place-image (text(number->string t) FONT-SIZE RED) x y scene))
    
    
    ;; after-move: Int Int -> Void
    ;; GIVEN: coordinate of the mouse
    ;; EFFECT: No effect
    ;; DETAILS: Clock ignores mouse move event
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
    
    
    ;; -------------------------------------------------------------------------
    ;; Non-Interface required functions
    ;; -------------------------------------------------------------------------
    
    ;; in-clock? : Int Int -> Boolean
    ;; GIVEN: location of the mouse coordinates
    ;; RETURNS: true iff the location is inside the clock.
    ;; STRATEGY: combine simper functions
    ;; EXAMPLES: See test cases    
    (define (in-clock? other-x other-y)
      (and
       (<= (- x (/ CLOCK-WIDTH 2))  other-x (+ x (/ CLOCK-WIDTH 2)))
       (<= (- y (/ CLOCK-HEIGHT 2)) other-y (+ y (/ CLOCK-HEIGHT 2)))))
    
    ))

;; make-clock: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a clock at the given position.
;; EXAMPLES: See test cases

(define(make-clock x y)
  (new Clock% [x x][y y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests:

(begin-for-test
  (local
    ((define C0 (make-clock 50 50))
     (define TEXT (place-image (text "1" 10 RED) 50 50 EMPTY-CANVAS))
     (define CLOCK (place-image
                    (rectangle 60 40 OUTLINE-CIRCLE BLUE) 50 50 TEXT)))
    (send C0 after-tick)
    (check-equal? (send C0 toy-data) 1
                  "The toy-data is 1")
    (check-equal? (send C0 place-text EMPTY-CANVAS) TEXT
                  "Image is placed on the canvas.")
    (check-equal? (send C0 add-to-scene EMPTY-CANVAS) CLOCK
                  "Image is placed on the canvas.")
    (send C0 after-button-down 45 45)
    (check-equal? (get-field selected? C0) #t
                  "Selected is true")
    (send C0 after-drag 46 46)
    (check-equal? (send C0 toy-x) 51
                  "The x coordinate of the toy is 51.")
    (check-equal? (send C0 toy-y) 51
                  "The y coordinate of the toy is 51.")
    (send C0 after-key-event "p")
    (check-equal? (send C0 toy-x) 51
                  "The x coordinate of the toy is 51")
    (send C0 after-button-up 60 60)
    (check-equal? (send C0 for-test:selected?) #f
                  "The selected field is false")
    (send C0 after-button-down 120 140)
    (check-equal? (get-field selected? C0) #f
                  "The selected field is true")
    (send C0 after-drag 150 160)
    (check-equal? (send C0 toy-x) 51
                  "The x coordinate of the toy is 51") 
    (send C0 after-button-up 120 140)
    (check-equal? (get-field selected? C0) #f
                  "Selected is true")
    (send C0 after-move 140 160)
    (check-equal? (send C0 toy-x) 51
                  "The x corodinate of the toy is 51")    
    ))


