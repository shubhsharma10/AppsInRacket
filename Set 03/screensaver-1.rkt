;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-1  
;; A screensaver with two circles which move around the canvas
;; The user can pause/unpause movement of circles with space bar

;; start with (screensaver 1)

(require rackunit)
(require "extras.rkt")
(check-location "03" "screensaver-1.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
  screensaver
  initial-world
  world-after-tick
  world-after-key-event
  new-circle
  world-circ1
  world-circ2
  world-paused?
  circ-x
  circ-y
  circ-vx
  circ-vy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SCREENSAVER FUNCTION

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world
(define (screensaver speed)
  (big-bang (initial-world speed)
            (on-tick world-after-tick speed)
            (on-draw world-to-scene)
            (on-key world-after-key-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; dimensions of the canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)

;; radius of the circle
(define CIRCLE-RADIUS 40)

(define ZERO 0)

;; Font size of the text displayed at center of circle
(define FONT-SIZE 10)

;; Colors for circle boundary or text at center of circle
(define BLUE-COLOR "blue")
(define RED-COLOR "red")

;; Radius of the circle displayed when mouse is on canvas
(define POINTER-RADIUS 5)

;; Circle type of the the circle
(define OUTLINE-CIRCLE "outline")

;; Key used to pause/run simulation of circles
(define PAUSE-KEY " ")

;; Allowed mouse events
(define BUTTON-DOWN-EVENT "button-down")
(define BUTTON-UP-EVENT "button-up")
(define DRAG-EVENT "drag")

;; Empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

(define-struct new-circ(x y vx vy))
;; A circle is a (make-new-circ NonNegInt NonNegInt Int Int)
;; x and y are (x,y) coordinates of center of the circle, unit is in pixels
;; vx and vy are velocity of circle in x and y coordinate
;; respectively, unit is in pixels/ticks

;; TEMPLATE:
;; new-circ-fn : Circle -> ??
;; (define (new-circ-fn w)
;;   (... (new-circ-x w) (new-circ-y w)
;;        (new-circ-vx w) (new-circ-vy w))

(define-struct world-state(circ1 circ2 paused?))
;; A circle is a (make-world-state Circle Circle Boolean)
;; circ1 and circ2 two circle present in world, whose data type is new-circ
;; paused? is a boolean tells whether simulation is paused or not

;; TEMPLATE:
;; world-state-fn : WorldState -> ??
;; (define (world-state-fn w)
;;   (... (world-state-circ1 w) (world-state-circ2 w)
;;        (world-state-paused? w))

;; Example of Circles
(define circle1 (make-new-circ 50 60 12 12))
(define circle2 (make-new-circ 60 70 15 15))
;; Example of WorldState
(define unpaused-world (make-world-state circle1 circle2 #true))
(define paused-world   (make-world-state circle1 circle2 #false))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLE:
;; (initial-world 10) = (make-world-state (new-circle 200 100 -12 20)
;;                                        (new-circle 200 200 23 -14) #true))
;; STRATEGY: Using WorldState constructor template
(define (initial-world input)
  ( make-world-state
    (new-circle 200 100 -12 20)
    (new-circle 200 200 23 -14)
    #true))

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLE:
;; (new-circle 200 100 -12 20) = (make-new-circle 200 100 -12 20)
;; STRATEGY: Using constructor tmeplate of circle
(define (new-circle x y vx vy)
  (make-new-circ x y vx vy))

;; world-circ1 : WorldState -> Circle
;; RETURNS: Circle 1 present in WorldState
;; EXAMPLE:
;; (world-circ1 (world-state circle1 circle2 #true)) = circle1
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-circ1 ws)
  (world-state-circ1 ws))

;; world-circ2 : WorldState -> Circle
;; RETURNS: Circle 2 present in WorldState
;; EXAMPLE:
;; (world-circ2 (world-state circle1 circle2 #true)) = circle2
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-circ2 ws)
  (world-state-circ2 ws))

;; world-paused? : WorldState -> Boolean
;; GIVEN: World state
;; RETURNS: whether simulation is paused or not
;; EXAMPLE:
;; (world-paused? (world-state circle1 circle2 #true)) = #true
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-paused? ws)
  (world-state-paused? ws))

;; circ-x : Circle -> NonNegInt
;; RETURNS: x coordinate of center of the circle
;; EXAMPLE:
;;(circ-x (new-circle 60 50 10 -20)) = 60
;; STRATEGY: Using observer template of Circle on input
(define (circ-x input)
  (new-circ-x input))

;; circ-y : Circle -> NonNegInt
;; RETURNS: y coordinate of center of the circle
;; EXAMPLE:
;;(circ-y (new-circle 60 50 10 -20)) = 50
;; STRATEGY: Using observer template of Circle on input
(define (circ-y input)
  (new-circ-y input))

;; circ-vx : Circle -> Int
;; RETURNS: velocity of the circle in x direction
;; EXAMPLE:
;;(circ-vx (new-circle 60 50 10 -20)) = 10
;; STRATEGY: Using observer template of Circle on input
(define (circ-vx input)
  (new-circ-vx input))

;; circ-vy : Circle -> Int
;; RETURNS: velocity of the circle in y direction
;; EXAMPLE:
;;(circ-vy (new-circle 60 50 10 -20)) = -20
;; STRATEGY: Using observer template of Circle on input
(define (circ-vy input)
  (new-circ-vy input))

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLE:
;; (world-after-tick (make-world-state (new-circle 30 100 -20 40) (new-circle 80 240 -20 40) #false))
;;    (make-world-state (new-circle 40 140 20 40) (new-circle 60 260 -20 -40) #false)
;; STRATEGY: Using Conditional statement
(define (world-after-tick ws)
  (if (world-paused? ws)
      ws
   (make-world-state
                     (circ-after-tick (world-circ1 ws))
                     (circ-after-tick (world-circ2 ws))
                     #false)))

;; world-to-scene : WorldState -> Scene
;; RETURNS: a Scene that portrays two circles with their velocities as string on center          
;; STRATEGY: Use template for WorldState on w
(define (world-to-scene ws)
  (place-circ1-with-text
    (world-circ1 ws) (place-circ2-with-text (world-circ2 ws))))

;; place-circ1: Circle Image -> Image
;; RETURNS: Image which contains Circle
;; EXAMPLE:
;; (place-circ1 circle1 EMPTY-CANVAS) =
;; (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
;;              (circ-x circle1) (circ-y circle1) EMPTY-CANVAS
;; STRATEGY: Combine simple functions
(define (place-circ1 input container)
  (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
               (new-circ-x input) (new-circ-y input) container))

;; place-circ1-with-text : Circle Image -> Image
;; RETURNS: Image which contains string representation of velocity of Circle
;; EXAMPLE:
;; (place-circ1-with-text circle1 EMPTY-CANVAS) =
;; (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
;;              (circ-x circle1) (circ-y circle1) (place-circ1 circle1 EMPTY-CANVAS))
;; STRATEGY: Combine simple functions
(define (place-circ1-with-text input container)
  (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
               (new-circ-x input) (new-circ-y input) (place-circ1 input container)))

;; circ-velocity-in-string : Circle -> String
;; RETURNS: String representaiton of velocity of circle
;; EXAMPLE:
;; (circ-velocity-in-string (new-circle 50 60 -10 -20)) = "(-10,-20)"
;; STRATEGY: Combine simple functions
(define (circ-velocity-in-string input-circle)
  (string-append  "("
                  (number->string (new-circ-vx input-circle))
                  ","
                  (number->string (new-circ-vy input-circle))
                  ")"))

;; place-circ2: Circle -> Image
;; RETURNS: Image which contains circle in a canvas
;; EXAMPLE:
;; (place-circ2 circle2) =
;; (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
;;              (circ-x circle2) (circ-y circle2) EMPTY-CANVAS))
;; STRATEGY: Combine simple functions
(define (place-circ2 input)
  (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
               (new-circ-x input) (new-circ-y input) EMPTY-CANVAS))

;; place-circ2-with-text: Circle -> Image
;; RETURNS: Image which contains circle and text
;; EXAMPLE:
;; (place-circ2-with-text circle2 EMPTY-CANVAS) =
;; (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
;;              (circ-x circle2) (circ-y circle2) (place-circ2 circle2 EMPTY-CANVAS))
;; STRATEGY: Combine simple functions
(define (place-circ2-with-text input)
  (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
               (new-circ-x input) (new-circ-y input) (place-circ2 input)))

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLE:
;; (world-after-key-event
;;     (world-state (new-circle 50 60 -10 -20) (new-circle 70 90 12 20) #false)
;;      PAUSE-KEY) =  (world-state (new-circle 50 60 -10 -20) (new-circle 70 90 12 20) #true)
;; STRATEGY: Combine simple funcitons
(define (world-after-key-event ws key)
  (if(pause-key-event? key)
     (world-with-pause-toggled ws)
     ws))


;; world-with-pause-toggled : WorldState -> WorldState
;; RETURNS: World state with pause boolean toggled
;; EXAMPLE:
;; (world-with-pause-toggled (world-state (new-circle 50 60 -10 -20) (new-circle 70 90 12 20) #false))
;; = (world-state (new-circle 50 60 -10 -20) (new-circle 70 90 12 20) #true)
;; STRATEGY: Using constructor template of WorldState on ws
(define (world-with-pause-toggled ws)
  (make-world-state
   (world-state-circ1 ws)
   (world-state-circ2 ws)
   (not (world-state-paused? ws))))
  
;; pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; EXAMPLE: (pause-key-event? PAUSE-KEY) = #true
;; RETURNS: true iff the KeyEvent represents a pause instruction
(define (pause-key-event? key)
  (key=? key PAUSE-KEY))

;; circ-after-tick : Circle -> Circle
;; GIVEN: Circle
;; RETURNS: Circle state that should follow the given circle 1 state
;; after a tick.
;; EXAMPLE:
;; (circ-handls-lhs (new-circle 10 60 20 40)) = (new-circle 40 100 -20 40)
;; STRATEGY: Using conditional statements
(define (circ-after-tick circle)
  (cond
    [(and (<= (circ-exceeds-lhs circle) ZERO) (<= (circ-exceeds-top circle) ZERO))
     (circ-handle-lhs-top circle)]
    [(and (<= (circ-exceeds-lhs circle) ZERO) (<= (circ-exceeds-bottom circle) ZERO))
     (circ-handle-lhs-bottom circle)]
    [(and (<= (circ-exceeds-rhs circle) ZERO) (<= (circ-exceeds-top circle) ZERO))
     (circ-handle-rhs-top circle)]
    [(and (<= (circ-exceeds-rhs circle) ZERO) (<= (circ-exceeds-bottom circle) ZERO))
     (circ-handle-rhs-bottom circle)]
    [(<= (circ-exceeds-lhs circle) ZERO)                              (circ-handle-lhs circle)]
    [(<= (circ-exceeds-rhs circle) ZERO)                              (circ-handle-rhs circle)]
    [(<= (circ-exceeds-top circle) ZERO)                              (circ-handle-top circle)]
    [(<= (circ-exceeds-bottom circle) ZERO)                           (circ-handle-bottom circle)]
    [else    (new-circle (possible-circ-x circle) (possible-circ-y circle)
                               (new-circ-vx circle) (new-circ-vy circle))]))

;; circ-handle-lhs : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds on lhs side
;; EXAMPLE:
;; (circ-handls-lhs (new-circle 10 60 20 40)) = (new-circle 40 100 -20 40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-lhs circle)
  (new-circle CIRCLE-RADIUS
                    (+ (new-circ-y circle) (new-circ-vy circle))
                    (- (new-circ-vx circle))
                    ( new-circ-vy circle)))

;; circ-handle-rhs : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceed on rhs side
;; EXAMPLE:
;; (circ-handle-rhs (new-circle 390 60 20 40)) = (new-circle 360 100 -20 40)
;; STRATEGY: Using costructor template of Circle on circle input
(define (circ-handle-rhs circle)
  (new-circle (+ (possible-circ-x circle) (circ-exceeds-rhs circle))
                    (+ (new-circ-y circle) (new-circ-vy circle))
                    (- (new-circ-vx circle))
                    ( new-circ-vy circle)))

;; circ-handle-top : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds on top
;; EXAMPLE:
;; (circ-handle-top (new-circle 60 10 20 40)) = (new-circle 80 40 20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-top circle)
  (new-circle (+ (new-circ-x circle) (new-circ-vx circle))
                    CIRCLE-RADIUS
                    (new-circ-vx circle)
                    (- (new-circ-vy circle))))

;; circ-handle-bottom : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds boundary on bottom
;; EXMAPLE:
;; (circ-handle-botton (new-circle 60 280 20 40)) = (new-circle 80 260 20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-bottom circle)
  (new-circle (+ (new-circ-x circle) (new-circ-vx circle))
                    (+ (possible-circ-y circle) (circ-exceeds-bottom circle))
                    (new-circ-vx circle)
                    (- (new-circ-vy circle))))

;; circ-handle-lhs-top : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds boundary on top left corner
;; EXAMPLE:
;; (circ-handle-lhs-top (new-circle 10 10 20 40)) = (new-circle 40 40 -20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-lhs-top circle)
  (new-circle (new-circ-x (circ-handle-lhs circle))
                    (new-circ-y (circ-handle-top circle))
                    (new-circ-vx (circ-handle-lhs circle))
                    (new-circ-vy (circ-handle-top circle))))

;; circ-handle-rhs-top : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds boundary on top right corner
;; EXAMPLE:
;; (circ-handle-rhs-top (new-circle 390 10 20 40)) = (new-circle 360 40 -20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-rhs-top circle)
  (new-circle (new-circ-x (circ-handle-rhs circle))
                    (new-circ-y (circ-handle-top circle))
                    (new-circ-vx (circ-handle-rhs circle))
                    (new-circ-vy (circ-handle-top circle))))

;; circ-handle-lhs-bottom : Circle -> Circle
;; RETURNS: circle with new x,y position when it exceeds boundary on top right corner
;; EXAMPLE:
;; (circ-handle-lhs-bottom (new-circle 10 280 20 40)) = (new-circle 40 260 -20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-lhs-bottom circle)
  (new-circle (new-circ-x (circ-handle-lhs circle))
                    (new-circ-y (circ-handle-bottom circle))
                    (new-circ-vx (circ-handle-lhs circle))
                    (new-circ-vy (circ-handle-bottom circle))))

;; circ-handle-rhs-bottom : Circle -> Circle
;; RETURNS: circle with new x,y position and velocities when it exceeds boundary on
;; bottom right corner
;; EXAMPLE:
;; (circ-handle-rhs-bottom (new-circle 390 280 20 40)) = (new-circle 360 260 -20 -40)
;; STRATEGY: Using constructor template of Circle on circle input
(define (circ-handle-rhs-bottom circle)
  (new-circle (new-circ-x (circ-handle-rhs circle))
                    (new-circ-y (circ-handle-bottom circle))
                    (new-circ-vx (circ-handle-rhs circle))
                    (new-circ-vy (circ-handle-bottom circle))))

;; possible-circ-x : Circle -> Int
;; Returns : possible value for x-coordinate of Circle
;; EXAMPLE:
;; (possible-circ-x (make-new-circ 390 270 10 20)) = 400
;; STRATEGY: Combine simple function
(define (possible-circ-x circle)
  (+ (circ-x circle) (circ-vx circle)))

;; possible-circ-y : Circle -> Int
;; Returns : possible value for y-coordinate of Circle
;; EXAMPLE:
;; (possible-circ-y (make-new-circ 390 270 10 20)) = 290
;; STRATEGY: Combine simple function
(define (possible-circ-y circle)
  (+ (circ-y circle) (circ-vy circle)))

;; circ-exceeds-lhs : Circle -> Int
;; Returns : how much circle exceeds canvas in x-direction(left side)
;; EXAMPLE:
;; (circ-exceeds-lhs (make-new-circ 10 270 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-lhs circle)
  (- (abs (possible-circ-x circle)) CIRCLE-RADIUS ))

;; circ-exceeds-rhs : Circle -> Int
;; RETURNS : how much circle exceeds canvas in x-direction(right side)
;; EXAMPLE:
;; (circ-exceeds-rhs (make-new-circ 390 270 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-rhs circle)
  (- CANVAS-WIDTH (+ (possible-circ-x circle) CIRCLE-RADIUS) ))

;; circ-exceeds-top : Circle -> Int
;; RETURNS : how much circle exceeds canvas in y-direction(top)
;; EXAMPLE:
;; (circ-exceeds-top (make-new-circ 100 10 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-top circle)
  (- (abs (possible-circ-y circle)) CIRCLE-RADIUS ))

;; circ-exceeds-bottom : Circle -> Int
;; RETURNS : how much circle exceeds canvas in y-direction(bottom)
;; EXAMPLE:
;; (circ-exceeds-bottom (make-new-circ 100 270 10 20)) = -10
;; STRATEGY: Combine simple function
(define (circ-exceeds-bottom circle)
  (- CANVAS-HEIGHT (+ (possible-circ-y circle) CIRCLE-RADIUS) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS
(begin-for-test
  (check-equal?
   (world-after-tick (make-world-state (new-circle 30 50 -20 -40) (new-circle 370 240 20 40) #false))
    (make-world-state (new-circle 40 40 20 40) (new-circle 360 260 -20 -40) #false)
    "x,y coordinate of Circle1 should change to 40,40 and Circle2 360,260")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 370 50 20 -40) (new-circle 30 240 -20 40) #false))
    (make-world-state (new-circle 360 40 -20 40) (new-circle 40 260 +20 -40) #false)
    "x,y coordinate of Circle1 should change to 40,40 and Circle2 360,260")

 (check-equal?
   (world-after-tick (make-world-state (new-circle 100 50 -20 -40) (new-circle 370 180 20 40) #false))
    (make-world-state (new-circle 80 40 -20 40) (new-circle 360 220 -20 40) #false)
    "x,y coordinate of Circle1 should change to 80,40 and Circle2 360,220")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 30 100 -20 40) (new-circle 80 240 -20 40) #false))
    (make-world-state (new-circle 40 140 20 40) (new-circle 60 260 -20 -40) #false)
    "x,y coordinate of Circle1 should change to 40,140 and Circle2 60,260")

  (check-equal?
   (world-after-tick (initial-world 10))
    (make-world-state (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #true)
    "There should be no change in state of circles as simulation was paused")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 90 100 -20 40) (new-circle 160 180 20 -40) #false))
   (make-world-state (new-circle 70 140 -20 40) (new-circle 180 140 20 -40) #false)
   "x,y coordinate of Circle1 should change to 70,140 and Circle2 180,140 with no change in velocity")
  
  (check-equal?
   (world-to-scene (initial-world 30))
    (place-circ1-with-text (new-circle 200 100 -12 20)
                           (place-circ2-with-text (new-circle 200 200 23 -14)))
    "There should be no change in state of circles as simulation was paused")
  
  (check-equal?
   (world-after-key-event (initial-world 30) PAUSE-KEY)
    (make-world-state (new-circle 200 100 -12 20) (new-circle 200 200 23 -14) #false)
    "As space key was pressed, only change is in paused state variable")

  (check-equal?
   (world-after-key-event (initial-world 30) "p")
    (initial-world 30)
    "Space key was not pressed, so no change in world state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;