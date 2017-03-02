#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "WidgetWorks.rkt")

(provide make-throbber
         Throbber%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define GREEN "green")
(define CHANGE-VALUE 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constructor template for Throbber%:
;; (new Throbber% [x Int][y Int]
;;                [selected? Boolean][saved-mx Int][saved-my Int])
;; the last 3 arguments are optional
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
    
    (init-field [saved-mx ZERO] [saved-my ZERO])  
    
    
    (super-new)
    
    ; Maximum and minimum radius for throbber
    (field [MAX-RADIUS 20] [MIN-RADIUS 5])
    
    ; the throbber's radius    
    (field [r MIN-RADIUS])
    
    ;determines if the throbbers radius should increase or decrease.
    (field [inc? true])
    
    ;; ------------------------------------------------------------------------
    ;; Interface required functions:
    ;; ------------------------------------------------------------------------
    
    
    ;; after-tick : -> Void
    ;; EFFECT: Updates radius and inc? after-tick
    ;; STRATEGY: Combine simple functions
    ;; EXAMPLES: See test cases    
    (define/public (after-tick)
      (begin
        (set! r (change-radius))
        (set! inc? (change-inc))))    
    
    
    ;; after-key-event : KeyEvent -> Void
    ;; GIVEN: a keyevent
    ;; EFFECT: No effect
    ;; DETAILS: a throbber ignores key events
    ;; EXAMPLES: See test cases    
    (define/public (after-key-event kev)
      this)
    
    
    ;; after-button-down : Int Int -> Void
    ;; GIVEN: the location of a button-down event
    ;; EFFECT: Store delta in saved-mx, saved-my if mouse is in throbber
    ;; STRATEGY: Cases on whether the event is in the throbber
    ;; If the throbber is not selected, then select it.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-down mx my)
      (if (in-throb? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    
    ;; after-button-up : Int Int -> Void
    ;; GIVEN: the location of a button-up event
    ;; EFFECT: Set selected? to false if mouse is in throbber
    ;; STRATEGY: Cases on whether the event is in the throbber.
    ;; If the throbber is selected, then unselect it.
    ;; EXAMPLES: See test cases    
    (define/public (after-button-up mx my)
      (if (in-throb? mx my)
          (begin
            (set! selected? false))
          this))
    
    
    ;; after-drag : Int Int -> Void
    ;; GIVEN: the location of a drag event
    ;; EFFECT: Update (x,y) coordinates with new coordinates calculated
    ;; from delta stored in saved-mx, saved-my
    ;; STRATEGY: Cases on whether the throbber is selected.
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
    
    
    ;; to-scene : Scene -> Scene
    ;; GIVEN: a scene 
    ;; RETURNS: a scene like the given one, but with this throbber painted
    ;; on it.
    ;; EXAMPLES: See test cases    
    (define/public (add-to-scene scene)
      (if (equal? selected? true)
          (place-image (circle r OUTLINE-CIRCLE GREEN) x y scene)
          (place-image (circle r SOLID-CIRCLE GREEN) x y scene)))
    
    
    ;; after-move: Int Int -> Void
    ;; GIVEN: coordinate of the mouse
    ;; EFFECT: No effect
    ;; DETAILS: Throbber ignores mouse-move event
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
    
    ;; ------------------------------------------------------------------------
    ;; Non-Interface required functions:
    ;; ------------------------------------------------------------------------
    
    ;; change-radius : -> Int
    ;; RETURNS: the new radius of the throbber, as it should be after a tick
    ;; either increase or decrease
    ;; STRATEGY: Cases on whether increment is needed or not
    ;; EXAMPLES: See test cases    
    (define (change-radius)
      (cond
        [(increment?) (+ r CHANGE-VALUE)]
        [else (- r CHANGE-VALUE)]))
    
    
    ;; increment?: -> Boolean
    ;; GIVEN: no argument.
    ;; RETURNS: true if inc is true and the radius is less than 20
    ;; inc is false and the radius is equal to 5.Else return false
    ;; STRATEGY: Using cases on inc?
    ;; EXAMPLES: See test cases
    (define (increment?)
      (or
       (and inc? (< r MAX-RADIUS))
       (and (not inc?) (equal? r MIN-RADIUS))))
    
    
    ;; change-inc : -> Boolean
    ;; GIVEN: no argument.
    ;; RETURNS: a boolean true if the radius increase till MAX-RADIUS or else
    ;; false
    ;; till the radius decreases till MIN-RADIUS
    ;; STRATEGY: Cases on inc? and radius
    ;; EXAMPLES: See test cases    
    (define (change-inc)
      (cond
        [(or (and (= r MAX-RADIUS) inc?)
             (and (= r MIN-RADIUS) (not inc?)))
         (not inc?)]
        [else inc?]))
    
    
    ;; in-throb? : Int Int -> Boolean
    ;; GIVEN: location of the mouse coordinates
    ;; RETURNS: true iff the location is inside the throbber.
    ;; STRATEGY: combine simper functions
    ;; EXAMPLES: See test cases    
    (define (in-throb? other-x other-y)
      (<= (+ (sqr (- x other-x)) (sqr (- y other-y)))
          (sqr r)))
    
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
    [else
     (begin (send tb after-tick))
     (simulate-ticks tb (- n 1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS :

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "The toy-data is 5")
    
    (send T1 after-tick)
    
    (check-equal? (send T1 toy-data) 6
                  "The toy-data is 6")
    
    (send T1 after-button-down 80 80)
    
    (check-equal? (get-field selected? T1) #f
                  "Selected is false") 
    ))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-button-down 55 50)
    
    (check-equal? (get-field selected? T1) #t
                  "Selected is true")    
    
    (send T1 after-button-up 55 50)
    
    (check-equal? (get-field selected? T1) #f
                  "Selected is false")
    
    (send T1 after-drag 80 80)
    
    (check-equal? (get-field selected? T1) #f
                  "Selected is false")))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-button-down 55 50)
    
    (check-equal? (get-field selected? T1) #t
                  "Selected is true")
    
    (send T1 after-drag 46 46)
    
    (check-equal? (list (get-field x T1) (get-field y T1) (get-field r T1))
                  (list 41 46 5)
                  "x-cordinate y-cordinate and radius of Throbber")))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-button-down 55 50)
    
    (check-equal? (get-field selected? T1) #t
                  "Selected is true")
    
    (check-equal? (send T1 add-to-scene EMPTY-CANVAS)
                  (place-image (circle (send T1 toy-data) OUTLINE-CIRCLE GREEN)
                               (send T1 toy-x)
                               (send T1 toy-y)
                               EMPTY-CANVAS)
                  
                  (send T1 after-move 40 40))
    
    (check-equal? (list (get-field x T1) (get-field y T1) (get-field r T1))
                  (list 50 50 5)
                  "x-cordinate y-cordinate and radius of Throbber")))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-button-down 55 50)
    
    (check-equal? (get-field selected? T1) #t
                  "Selected is true")
    
    (send T1 after-key-event "p")
    
    (check-equal? (list (get-field x T1) (get-field y T1) (get-field r T1))
                  (list 50 50 5)
                   "x-cordinate y-cordinate and radius of Throbber")))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-button-up 5 5)
    
    (check-equal? (list (get-field x T1) (get-field y T1) (get-field r T1))
                  (list 50 50 5)
                   "x-cordinate y-cordinate and radius of Throbber")
    
    (check-equal? (send T1 add-to-scene EMPTY-CANVAS)
                  (place-image (circle (send T1 toy-data) SOLID-CIRCLE GREEN)
                               (send T1 toy-x)
                               (send T1 toy-y)
                               EMPTY-CANVAS))))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send (simulate-ticks T1 15) after-tick)
    
    (check-equal? (send T1 toy-data) 19
                  "Toy-data is 19")
    
    (send T1 after-tick)
    
    (check-equal? (send T1 toy-data) 18)
    "Toy-data is 19"))

(begin-for-test
  (local
    ((define T1 (make-throbber 50 50)))
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (simulate-ticks T1 14)
    (send T1 after-tick)
    
    (check-equal? (send T1 toy-data) 20
                  "Toy-data is 20")
    
    (send T1 after-tick)
    
    (check-equal? (send T1 toy-data) 19
                  "Toy-data is 19")
    
    (simulate-ticks T1 14)
    
    (check-equal? (send T1 toy-data) 5
                  "Toy-data is 5")
    
    (send T1 after-tick)
    
    (check-equal? (send T1 toy-data) 6
                  "Toy-data is 6")))