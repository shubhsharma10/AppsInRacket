#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")

(provide make-throbber
         Throbber%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define GREEN "green")
(define MAX-RADIUS 20)
(define MIN-RADIUS 5)
(define CHANGE-VALUE 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for Throbber%:
;; (new Throbber% [x Int][y Int]
;;                [selected? Boolean][mx Int][my Int][r Int]
;;                [inc? Boolean])
;; the last 5 arguments are optional
;; Interpretation: An object of class Throbber% represents a throbber.

(define Throbber%
  (class* object% (Toy<%>)
    
    ;; the init-fields are the values that may vary from one throbber to
    ;; the next.
    
    ; the x and y position of the center of the throbber
    (init-field x y) 
    
    ; is the throbber selected? Default is false.
    (init-field [selected? false])
    
    ;; if the throbber is selected, the position of
    ;; the last button-down event inside the throbber, relative to the
    ;; throbber's center.  Else any value.
    
    (init-field [saved-mx 0] [saved-my 0])  
    ; the throbber's radius
    
    (init-field [r MIN-RADIUS])
    
    ;determines if the throbbers radius should increase or decrease.
    (init-field [inc? true])
    
    ;; private data for objects of this class.
    ;; these can depend on the init-fields.
    
    ; image for displaying the helicopter when it is selected.
    (field [THROB-IMG-SELECTED (circle r OUTLINE-CIRCLE GREEN)])
    
    ; image for displaying the helicopter when it is selected.
    (field [THROB-IMG-UNSELECTED (circle r SOLID-CIRCLE GREEN)])
    
    
    (super-new)
    
    ;; after-tick : -> Widget
    ;; GIVEN: No arguments
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a tick.
    ;; STRATEGY: use template for throbber.
    ;; EXAMPLES: See test cases
    
    (define/public (after-tick)
      (new Throbber%
           [x x]
           [y y]
           [selected? selected?]
           [saved-mx saved-mx]
           [saved-my saved-my]
           [r (change-radius)]
           [inc? (change-inc)]))
    
    ;; change-radius : -> Int
    ;; GIVEN: no argument. 
    ;; RETURNS: the new radius of the throbber, as it should be after a tick
    ;; If true radius should increase or else decrease
    ;; STRATEGY: Cases on inc? and radius
    ;; EXAMPLES: See test cases
    
    (define (change-radius)
      (cond
        [(increment-radius) (+ r CHANGE-VALUE)]
        [(decrement-radius) (- r CHANGE-VALUE)]))

    ;; increment-radius: -> Boolean
    ;; GIVEN: no argument.
    ;; RETURNS: true if inc is true and the radius is less than 20.Or
    ;; inc is false and the radius is equal to 5.Else return false
    ;; after a tick
    ;; STRATEGY: Combine simpler functions.
    ;; EXAMPLES: See test cases

    (define (increment-radius)
      (or
       (and (equal? inc? true) (< r MAX-RADIUS))
       (and (equal? inc? false) (equal? r MIN-RADIUS))))

    ;; decrement-radius: -> Boolean
    ;; GIVEN: no argument.
    ;; RETURNS: true if inc is false and radius is greater than equal to 6.
    ;; Or inc is true and the radius is equal to 20. Else return false
    ;; STRATEGY: Combine simpler functions.
    ;; EXAMPLES: See test cases

    (define (decrement-radius)
      (or (and (equal? inc? false) (> r MIN-RADIUS))
          (and (equal? inc? true) (equal? r MAX-RADIUS))))
    
    ;; change-inc : -> Boolean
    ;; GIVEN: no argument.
    ;; RETURNS: a boolean true if the radius increase till 20 or else false
    ;; till the radius decreases till 5
    ;; STRATEGY: Cases on inc? and radius
    ;; EXAMPLES: See test cases
    
    (define (change-inc)
      (cond
        [(or (and (= r MAX-RADIUS) (equal? inc? true))
             (and (= r MIN-RADIUS) (equal? inc? false)))
         (not inc?)]
        [else inc?]))
    
    ;; after-key-event : KeyEvent -> Widget
    ;; GIVEN: a keyevent
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a key event.
    ;; DETAILS: a throbber ignores key events
    ;; EXAMPLES: See test cases
    
    (define/public (after-key-event kev)
      this)      
    
    ;; after-button-down : Int Int -> Widget
    ;; GIVEN: the location of a button-down event
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a button-down event.
    ;; STRATEGY: Cases on whether the event is in the throbber
    ;; If the throbber is not selected, then select it.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-down mx my)
      (if (in-throb? mx my)
          (new Throbber%
               [x x][y y]
               [selected? true]
               [saved-mx (- mx x)]
               [saved-my (- my y)]
               [r r]
               [inc? inc?])
          this))
    
    ;; after-button-up : Int Int -> Widget
    ;; GIVEN: the location of a button-up event
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a button-up event.
    ;; STRATEGY: Cases on whether the event is in the throbber.
    ;; If the throbber is selected, then unselect it.
    ;; EXAMPLES: See test cases
    
    (define/public (after-button-up mx my)
      (if (in-throb? mx my)
          (new Throbber%
               [x x][y y]
               [selected? false]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [r r]
               [inc? inc?])
          this))   
    
    ;; after-drag : Int Int -> Widget
    ;; GIVEN: the location of a drag event
    ;; RETURNS: A widget which is a toy object of the class throbber
    ;; as it should be after a drag event.
    ;; STRATEGY: Cases on whether the throbber is selected.
    ;; If it is selected, move it so that the vector from the center to
    ;; the drag event is equal to (mx, my)
    ;; EXAMPLES: See test cases
    
    (define/public (after-drag mx my)
      (if selected?
          (new Throbber%
               [x (- mx saved-mx)]
               [y (- my saved-my)]
               [selected? true]
               [saved-mx saved-mx]
               [saved-my saved-my]
               [r r]
               [inc? inc?])
          this))   
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    ;; STRATEGY: combine simpler functions
    ;; EXAMPLES: See test cases
    
    (define/public (add-to-scene scene)
      (if (equal? selected? true)
          (place-image THROB-IMG-SELECTED x y scene)
          (place-image THROB-IMG-UNSELECTED x y scene)))
    
    ;; in-throb? : Int Int -> Boolean
    ;; GIVEN: location of the mouse coordinates
    ;; RETURNS: true iff the location is inside the throbber.
    ;; STRATEGY: combine simper functions
    ;; EXAMPLES: See test cases
    
    (define (in-throb? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    
    
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
    ;; RETURNS: Current radius of the throbber
    ;; EXAMPLES: See test cases
    
    (define/public (toy-data)
      r)
    
    ;; test methods, to probe the throbber selected?.
    ;; -> Boolean
    (define/public (for-test:selected?)
      selected?)
    
    ;; -> (list Int Int Boolean)
    ;; RETURNS: Throbber's x,y coordinate, radius and whether it is selected
    (define/public (for-test:get-list)
      (list x y r selected?))
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-throbber: PosInt PosInt -> Toy
;; GIVEN: an x and a y position
;; RETURNS: an object representing a throbber at the given position.
;; STRATEGY: Use template for throbber.
;; EXAMPLES: See test cases

(define (make-throbber x y)
  (new Throbber% [x x][y y]))

;; simulate-ticks: Throbber Int -> Throbber
;; GIVEN: a throbber and number of ticks which need to be simulated
;; RETURNS: Throbber after n ticks
;; HALTING-MEASURE: n
;; STRATEGY: recur on n
;; EXAMPLES: See test cases

(define (simulate-ticks tb n)
  (cond
    [(equal? n 0) tb]
    [else (simulate-ticks (send tb after-tick) (- n 1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS :

(define T1 (make-throbber 50 50))
(define T2 (send T1 after-tick))
(define T3 (send T1 after-button-down 55 50))
(define T4 (send T3 after-button-up 55 50))
(define T5 (send (simulate-ticks T1 15) after-tick))
(define T12 (send T5 after-tick))
(define T6 (send T3 after-drag 46 46))
(define T7 (send T3 after-move 40 40))
(define T8 (send T3 after-key-event "p"))
(define T9 (send T1 after-button-down 5 5))
(define T10 (send T1 after-button-up 5 5))
(define T11 (send T1 after-drag 5 5))
(define T13 (simulate-ticks T5 14))
(define T14 (send T13 after-tick))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:

(begin-for-test
  (check-equal? (send T1 toy-data) 5)
  (check-equal? (send T2 toy-data) 6)
  (check-equal? (send T3 for-test:selected?) #t)
  (check-equal? (send T4 for-test:selected?) #f)
  (check-equal? (send T5 toy-data) 19)
  (check-equal? (send T12 toy-data) 18)
  (check-equal? (send T13 toy-data) 5)
  (check-equal? (send T14 toy-data) 6)
  (check-equal? (send T6 for-test:get-list)
                (list 41 46 5 #t))
  (check-equal? (send T7 for-test:get-list)
                (list 50 50 5 #t))
  (check-equal? (send T8 for-test:get-list)
                (list 50 50 5 #t))
  (check-equal? (send T9 for-test:get-list)
                (list 50 50 5 #f))
  (check-equal? (send T10 for-test:get-list)
                (list 50 50 5 #f))
  (check-equal? (send T4 add-to-scene EMPTY-CANVAS)
                (place-image (circle (send T4 toy-data) SOLID-CIRCLE GREEN)
                             (send T4 toy-x)
                             (send T4 toy-y)
                             EMPTY-CANVAS))
  (check-equal? (send T3 add-to-scene EMPTY-CANVAS)
                (place-image (circle (send T3 toy-data) OUTLINE-CIRCLE GREEN)
                             (send T3 toy-x)
                             (send T3 toy-y)
                             EMPTY-CANVAS)))