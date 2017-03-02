;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-2
;; A screensaver with two circles which move around the canvas
;; The user can pause/unpause movement of circles with space bar
;; User also has option to drag the circles around with mouse

;; start with (screensaver 1)

(require rackunit)
(require "extras.rkt")
(check-location "03" "screensaver-2.rkt")

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
  circ-vy
  world-after-mouse-event
  circ-after-mouse-event
  circ-selected?)

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
            (on-key world-after-key-event)
            (on-mouse world-after-mouse-event)))

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
(define SOLID-CIRCLE "solid")
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

(define-struct new-circ(x y vx vy selected? lx ly))
;; A circle is a (make-new-circ NonNegInt NonNegInt Int Int Boolean NonNegInt NonNegInt)
;; x and y are (x,y) coordinates of center of the circle, units is in pixels
;; vx and vy are velocity of circle in x and y coordinate respectively, units is in pixel/ticks
;; selected? is a boolean which has information about whether circle is selected or not
;; lx and ly are (x,y) coordinates of point where last time mouse was present, in pixel units

;; TEMPLATE:
;; new-circ-fn : Circle -> ??
;; (define (new-circ-fn w)
;;   (... (new-circ-x w) (new-circ-y w)
;;        (new-circ-vx w) (new-circ-vy w) (new-circ-selected? w)
;;        (new-circ-lx w) (new-circ-ly w)))

(define-struct world-state(circ1 circ2 pointer-x pointer-y paused? mouse-down?))
;; A WorldState is a (make-world-state Circle Circle Int Int Boolean Boolean)
;; circ1 and circ2 two circle present in world, whose data type is new-circ
;; pointer-x and pointer-y are x,y coordinates of the circle which is drawn
;; when mouse down or drag even is done on canvas, units is in pixels
;; paused? is a boolean which tells whether simulation is paused or not
;; mouse-down? is a boolean which tells whether mouse is pressed/ being dragged or not

;; TEMPLATE:
;; world-state-fn : WorldState -> ??
;; (define (world-state-fn w)
;;   (... (world-state-circ1 w) (world-state-circ2 w)
;;        (world-state-pointer-x w) (world-state-pointer-y w)
;;        (world-state-paused? w)(world-state-mouse-down? w)))

;; Example of Circles
(define circle1 (make-new-circ 50 60 12 12 #false 0 0))
(define circle2 (make-new-circ 60 70 15 15 #false 0 0))
;; Example of WorldState, for testing
(define unpaused-world (make-world-state circle1 circle2 0 0 #true #false))
(define paused-world   (make-world-state circle1 circle2 0 0 #false #false))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLE:
;; (initial-world 10) = (make-world-state (new-circle 200 100 -12 20)
;;                                        (new-circle 200 200 23 -14)
;;                                         0 0 #true #false))
;; STRATEGY: Using WorldState constructor template
(define (initial-world input)
  ( make-world-state
    (new-circle 200 100 -12 20)
    (new-circle 200 200 23 -14)
    0 0
    #true
    #false))

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a unselected circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLE:
;; (new-circle 200 100 -12 20) = (make-new-circ 200 100 -12 20 #false 0 0)
;; STRATEGY: Using constructor tmeplate of circle
(define (new-circle x y vx vy)
  (make-new-circ x y vx vy #false 0 0))

;; world-circ1 : WorldState -> Circle
;; GIVEN: World state
;; RETURNS: Circle1 present in WorldState
;; EXAMPLE:
;;(world-circ1 (world-state circle1 circle2 0 0 #true #false)) = circle1
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-circ1 ws)
  (world-state-circ1 ws))

;; world-circ2 : WorldState -> Circle
;; GIVEN: World state
;; RETURNS: Circle2 present in WorldState
;; EXAMPLE:
;;(world-circ2 (world-state circle1 circle2 0 0 #true #false)) = circle2
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-circ2 ws)
  (world-state-circ2 ws))

;; world-paused? : WorldState -> Boolean
;; GIVEN: World state
;; RETURNS: whether simulation is paused or not
;; EXAMPLE:
;; (world-paused? (world-state circle1 circle2 0 0 #true #false)) = #true
;; STRATEGY: Using observer template of WorldState on ws input
(define (world-paused? ws)
  (world-state-paused? ws))

;; circ-x : Circle -> NonNegInt
;; GIVEN: Circle
;; RETURNS: x coordinate of center of the circle
;; EXAMPLE:
;;(circ-x (new-circle 60 50 10 -20)) = 60
;; STRATEGY: Using observer template of Circle on input
(define (circ-x input)
  (new-circ-x input))

;; circ-y : Circle -> NonNegInt
;; GIVEN: Circle
;; RETURNS: y coordinate of center of the circle
;; EXAMPLE:
;;(circ-x (new-circle 60 50 10 -20)) = 50
;; STRATEGY: Using observer template of Circle on input
(define (circ-y input)
  (new-circ-y input))

;; circ-vx : Circle -> Int
;; GIVEN: Circle
;; RETURNS: velocity of the circle in x direction
;; EXAMPLE:
;;(circ-x (new-circle 60 50 10 -20)) = 10
;; STRATEGY: Using observer template of Circle on input
(define (circ-vx input)
  (new-circ-vx input))

;; circ-vy : Circle -> Int
;; GIVEN: Circle
;; RETURNS: velocity of the circle in y direction
;; EXAMPLE:
;;(circ-x (new-circle 60 50 10 -20)) = -20
;; STRATEGY: Using observer template of Circle on input
(define (circ-vy input)
  (new-circ-vy input))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLE:
;; (world-after-tick (make-world-state (new-circle 30 100 -20 40)
;;                                     (new-circle 80 240 -20 40)
;;                                      0 0 #false #false))
;;    (make-world-state (new-circle 40 140 20 40)
;;                      (new-circle 60 260 -20 -40)
;;                      0 0 #false #false)
;; STRATEGY: Using conditional statement
(define (world-after-tick ws)
  (if (world-paused? ws)
      ws
   (make-world-state
                     (circ-after-tick (world-circ1 ws))
                     (circ-after-tick (world-circ2 ws))
                     (world-state-pointer-x ws) (world-state-pointer-y ws)
                     #false
                     (world-state-mouse-down? ws))))

;; world-after-mouse-event
;; WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; STRATEGY: use observer template for WorldState on ws
(define (world-after-mouse-event ws mx my mev)
  (make-world-state
     (circ-after-mouse-event (world-circ1 ws) mx my mev)
     (circ-after-mouse-event (world-circ2 ws) mx my mev)
     (pointer-x-after-mouse-event mx mev)
     (pointer-y-after-mouse-event my mev)
     (world-state-paused? ws)
     (mouse-down-after-mouse-event? ws mev)))

;; mouse-down-mouse-event? : WorldState MouseEvent -> Boolean
;; GIVEN: world state and mouse event
;; RETURNS: true iff button-down or drag even occurs
(define (mouse-down-after-mouse-event? ws mev)
  (cond
    [(or (mouse=? mev BUTTON-DOWN-EVENT) (mouse=? mev DRAG-EVENT)) #true]
    [else #false]))

;; pointer-x-after-mouse-event : Int MouseEvent -> Int
;; GIVEN: the x coordinate of a mouse event and mouse event
;; RETURNS: return x coordinate of mouse event iff button-down or drag event occurs
(define (pointer-x-after-mouse-event mx mev)
 (cond
    [(or (mouse=? mev BUTTON-DOWN-EVENT) (mouse=? mev DRAG-EVENT)) mx]
    [else ZERO]))
  
;; pointer-y-after-mouse-event : Int MouseEvent -> Int
;; GIVEN: the y coordinate of a mouse event and mouse event
;; RETURNS: return y coordinate of mouse event iff button-down or drag event occurs
(define (pointer-y-after-mouse-event my mev)
 (cond
    [(or (mouse=? mev BUTTON-DOWN-EVENT) (mouse=? mev DRAG-EVENT)) my]
    [else ZERO]))

;; circ-after-mouse-event :  Circle Int Int MouseEvent -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the circle that should follow the given circle after
;; the given mouse event
;; STRATEGY: Cases on MouseEvent
(define (circ-after-mouse-event circ mx my mev)
   (cond
     [(mouse=? mev BUTTON-DOWN-EVENT) (circ-after-button-down circ mx my)]
     [(mouse=? mev DRAG-EVENT) (circ-after-drag circ mx my)]
     [(mouse=? mev BUTTON-UP-EVENT) (circ-after-button-up circ mx my)]
     [else circ]))

;; circ-after-button-down : Circle Int Int -> Circle
;; GIVEN: A circle and x,y coordinate of location on button down of mouse
;; RETURNS: Circle following a button-down at given location
;; STRATEGY: Use template for Circle on circ
(define (circ-after-button-down circ mx my)
  ( if (in-circle? circ mx my)
       (make-new-circ (new-circ-x circ) (new-circ-y circ)
                   (new-circ-vx circ) (new-circ-vy circ)
                   #true mx my)
     circ))

;; circ-after-drag : Circle Int Int -> Circle
;; GIVEN: A circle and x,y coordinate of location on which mouse drag is happening
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circle on circ
(define (circ-after-drag circ mx my)
  (if (circ-selected? circ)
      (make-new-circ (new-x-coord circ mx my) (new-y-coord circ mx my)
                  (new-circ-vx circ) (new-circ-vy circ) #true
                  mx my)
      circ))

;; circ-after-button-up : Circle Int Int -> Circle
;; RETURNS: the circ following a button-up at the given location
;; STRATEGY: Use template for Circle on circ
(define (circ-after-button-up circ mx my)
  (if (circ-selected? circ)
      (make-new-circ (new-circ-x circ) (new-circ-y circ)
                   (new-circ-vx circ) (new-circ-vy circ)
                   #false 0 0)   
      circ))

;; new-y-coord : Circle Int Int -> Int
;; RETURNS: new y coordinate of center of the circle
;; logic followed is that old center,old mouse down location,
;; current mouse location and new center forms a parallelogram,
;; where diagonals bisect each other
;; EXAMPLE:
;; (new-y-coord (make-new-circ 100 100 20 24 #true 100 80) 80 80)
;; = 100
;; STRATEGY: Combine simple function
(define (new-y-coord circ mx my)
  (- (+ (new-circ-y circ) my) (new-circ-ly circ)))

;; new-x-coord : Circle Int Int -> Int
;; RETURNS: new x coordinate of center of the circle
;; logic followed is that old center,old mouse down location,
;; current mouse location and new center forms a parallelogram,
;; where diagonals bisect each other
;; EXAMPLE:
;; (new-x-coord (make-new-circ 100 100 20 24 #true 100 80) 80 80)
;; = 80
;; STRATEGY: Combine simple function
(define (new-x-coord circ mx my)
  (- (+ (new-circ-x circ) mx) (new-circ-lx circ)))
    

;; in-circle? : Circle Int Int -> Circle
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given Circle.
;; EXAMPLE:
;; (in-circle? (make-new-circ 100 100 20 24 #true 80 90) 100 80)
;; = #true
;; STRATEGY: Use template for Circle on circ
(define (in-circle? circ mx my)
  (<= (sqrt ( + (* (- (new-circ-x circ) mx) (- (new-circ-x circ) mx))
                (* (- (new-circ-y circ) my) (- (new-circ-y circ) my))))
      CIRCLE-RADIUS))

;; circ-selected? : Circle -> Boolean
;; RETURNS: true iff the given circle is selected.
;; EXAMPLE:
;; (circ-selected? (make-new-circ 100 100 20 24 #true 100 80))
;; = #true
;; STRATEGY: Combine simple function
(define (circ-selected? circ)
  (new-circ-selected? circ))

;; world-to-scene : WorldState -> Scene
;; RETURNS: a Scene that portrays two circles or with two circles and one circle
;; to identify mouse location on canvas
;; STRATEGY: Use template for WorldState on w
(define (world-to-scene ws)
  (if(world-state-mouse-down? ws)
      (place-pointer-in-scene ws  (place-circles-in-scene ws))
  (place-circles-in-scene ws)))

;; place-circles-in-scene : WorldState -> Scene
;; RETURNS: a scene with two circles
;; STRATEGY: Combine simple funciton
(define (place-circles-in-scene ws)
 (place-circ1-with-text
    (world-state-circ1 ws)
    (place-circ2-with-text
      (world-state-circ2 ws))))

;; place-pointer-in-scene : WorldState Scene -> Scene
;; RETURNS: Scene with circle which idenntifies mouse location on canvas
;; STRATEGY: Combine simple function
(define (place-pointer-in-scene ws container)
  (place-image (circle POINTER-RADIUS SOLID-CIRCLE RED-COLOR)
                (world-state-pointer-x ws) (world-state-pointer-y ws) container))

;; place-circ1 : Circle Scene -> Scene
;; RETURNS: Scene with Circ1
;; EXAMPLE:
;; (place-circ1 circle1 EMPTY-CANVAS) =
;; (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
;;              (circ-x circle1) (circ-y circle1) EMPTY-CANVAS
;; STRATEGY: Combine simple function
(define (place-circ1 input container)
  (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE (get-color input))
               (new-circ-x input) (new-circ-y input) container))

;; get-color : Circle -> Color
;; RETURNS: Color depending on circle is selected or not
;; EXAMPLE:
;; (get-color (make-new-circ 100 100 20 24 #true 100 80))
;; = RED-COLOR
;; STRATEGY: Combine simple function
(define (get-color circ)
  (if (circ-selected? circ)
      RED-COLOR
   BLUE-COLOR))

;; place-circ1-with-text: Circle Scene -> Scene
;; RETURNS: Scene with Circle1 having it's velocity as string at center
;; EXAMPLE:
;; (place-circ1-with-text circle1 EMPTY-CANVAS) =
;; (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
;;              (circ-x circle1) (circ-y circle1) (place-circ1 circle1 EMPTY-CANVAS))
;; STRATEGY: Combine simple function
(define (place-circ1-with-text input container)
  (place-image (text (circ-velocity-in-string input) FONT-SIZE BLUE-COLOR)
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
  (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE (get-color input))
               (new-circ-x input) (new-circ-y input) EMPTY-CANVAS))

;; place-circ2-with-text: Circle -> Image
;; RETURNS: Image which contains circle and text
;; EXAMPLE:
;; (place-circ2-with-text circle2 EMPTY-CANVAS) =
;; (place-image (text (circ-velocity-in-string input) FONT-SIZE COLOR)
;;              (circ-x circle2) (circ-y circle2) (place-circ2 circle2 EMPTY-CANVAS))
;; STRATEGY: Combine simple functions
(define (place-circ2-with-text input)
  (place-image (text (circ-velocity-in-string input) FONT-SIZE BLUE-COLOR)
               (new-circ-x input) (new-circ-y input) (place-circ2 input)))

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLE:
;; (world-after-key-event
;;     (world-state (new-circle 50 60 -10 -20) (new-circle 70 90 12 20) 0 0 #false #false )
;;      PAUSE-KEY) =  (world-state (new-circle 50 60 -10 -20)
;;                   (new-circle 70 90 12 20) 0 0 #true #false)
;; STRATEGY: Combine simple funcitons
(define (world-after-key-event ws key)
  (if(pause-key-event? key)
     (world-with-pause-toggled ws)
     ws))

;; world-with-pause-toggled : WorldState -> WorldState
;; RETURNS: World state with pause boolean toggled
;; EXAMPLE:
;; (world-with-pause-toggled (world-state
;;                             (new-circle 50 60 -10 -20)
;;                             (new-circle 70 90 12 20)
;;                             0 0 #false #false))
;; = (world-state (new-circle 50 60 -10 -20)
;;                (new-circle 70 90 12 20)
;;                 0 0 #true #false)
;; STRATEGY: Using constructor template of WorldState on ws
(define (world-with-pause-toggled ws)
  (make-world-state
   (world-state-circ1 ws)
   (world-state-circ2 ws)
   (world-state-pointer-x ws)
   (world-state-pointer-y ws)
   (not (world-state-paused? ws))
   (world-state-mouse-down? ws)))
  
;; help function for key event
;; pause-key-event? : KeyEvent -> Boolean
;; GIVEN: a KeyEvent
;; EXAMPLE
;; (pause-key-event (world-state (new-circle 50 60 -10 -20)
;;                (new-circle 70 90 12 20)
;;                 0 0 #false #false)PAUSE-KEY)
;; = (world-state (new-circle 50 60 -10 -20)
;;                (new-circle 70 90 12 20)
;;                 0 0 #true #false)
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
    [(circ-selected? circle) circle]
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
;; (possible-circ-x (new-circle 390 270 10 20)) = 400
;; STRATEGY: Combine simple function
(define (possible-circ-x circle)
  (+ (circ-x circle) (circ-vx circle)))

;; possible-circ-y : Circle -> Int
;; Returns : possible value for y-coordinate of Circle
;; EXAMPLE:
;; (possible-circ-y (new-circle 390 270 10 20)) = 290
;; STRATEGY: Combine simple function
(define (possible-circ-y circle)
  (+ (circ-y circle) (circ-vy circle)))

;; circ-exceeds-lhs : Circle -> Int
;; Returns : how much circle exceeds canvas in x-direction(left side)
;; EXAMPLE:
;; (circ-exceeds-lhs (new-circle 10 270 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-lhs circle)
  (- (abs (possible-circ-x circle)) CIRCLE-RADIUS ))

;; circ-exceeds-rhs : Circle -> Int
;; Returns : how much circle exceeds canvas in x-direction(right side)
;; EXAMPLE:
;; (circ-exceeds-rhs (new-circle 390 270 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-rhs circle)
  (- CANVAS-WIDTH (+ (possible-circ-x circle) CIRCLE-RADIUS) ))

;; circ-exceeds-top : Circle -> Int
;; Returns : how much circle exceeds canvas in y-direction(top)
;; EXAMPLE:
;; (circ-exceeds-top (new-circle 100 10 10 20)) = -30
;; STRATEGY: Combine simple function
(define (circ-exceeds-top circle)
  (- (abs (possible-circ-y circle)) CIRCLE-RADIUS ))

;; circ-exceeds-bottom : Circle -> Int
;; Returns : how much circle exceeds canvas in y-direction(bottom)
;; EXAMPLE:
;; (circ-exceeds-bottom (new-circle 100 270 10 20)) = -10
;; STRATEGY: Combine simple function
(define (circ-exceeds-bottom circle)
  (- CANVAS-HEIGHT (+ (possible-circ-y circle) CIRCLE-RADIUS) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS

(define world-with-one-circle-selected
     (make-world-state (make-new-circ 80 100 -20 40 #true 80 80) (new-circle 160 180 20 -40)
                     80 80 #true #true))

(begin-for-test
  (check-equal?
   (world-after-tick (make-world-state (new-circle 30 50 -20 -40) (new-circle 370 240 20 40)
                                       0 0 #false #false))
    (make-world-state (new-circle 40 40 20 40) (new-circle 360 260 -20 -40)
                       0 0 #false #false)
    "x,y coordinate of Circle1 should change to 40,40 and Circle2 360,260")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 370 50 20 -40) (new-circle 30 240 -20 40)
                                       0 0 #false #false))
    (make-world-state (new-circle 360 40 -20 40) (new-circle 40 260 +20 -40)
                       0 0 #false #false)
    "x,y coordinate of Circle1 should change to 40,40 and Circle2 360,260")

 (check-equal?
   (world-after-tick (make-world-state (new-circle 100 50 -20 -40) (new-circle 370 180 20 40)
                                       0 0 #false #false))
    (make-world-state (new-circle 80 40 -20 40) (new-circle 360 220 -20 40)
                      0 0 #false #false)
    "x,y coordinate of Circle1 should change to 80,40 and Circle2 360,220")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 30 100 -20 40) (new-circle 80 240 -20 40)
                                       0 0 #false #false))
    (make-world-state (new-circle 40 140 20 40) (new-circle 60 260 -20 -40)
                      0 0 #false #false)
    "x,y coordinate of Circle1 should change to 40,140 and Circle2 60,260")

  (check-equal?
   (world-after-tick (initial-world 10))
    (make-world-state (new-circle 200 100 -12 20) (new-circle 200 200 23 -14)
                      0 0 #true #false)
    "There should be no change in state of circles as simulation was paused")

  (check-equal?
   (world-after-tick (make-world-state (new-circle 90 100 -20 40) (new-circle 160 180 20 -40)
                                       0 0 #false #false))
   (make-world-state (new-circle 70 140 -20 40) (new-circle 180 140 20 -40)
                     0 0 #false #false)
   "x,y coordinate of Circle1 should change to 70,140 and Circle2 180,140 with no change in velocity")

  (check-equal?
   (world-after-tick (make-world-state (make-new-circ 90 100 -20 40 #true 0 0)
                                       (new-circle 160 180 20 -40)
                                       0 0 #false #false))
   (make-world-state (make-new-circ 90 100 -20 40 #true 0 0) (new-circle 180 140 20 -40)
                     0 0 #false #false)
   "x,y coordinate of Circle2 should change to 180,140 with no change in velocity and Circle1 won't
    move because it is selected")

  (check-equal?
   (world-after-mouse-event (make-world-state (new-circle 90 100 -20 40)
                                              (new-circle 160 180 20 -40)
                                              0 0 #false #false)
                                       20 40 BUTTON-DOWN-EVENT)
   (make-world-state (new-circle 90 100 -20 40) (new-circle 160 180 20 -40)
                     20 40 #false #true)
   "x,y coordinate of Circle1 should change to 70,140 and Circle2 180,140 with no change in velocity
    WorldState also have (x,y) coordinate of button down")

  (check-equal?
   (world-after-mouse-event (make-world-state (new-circle 100 100 -20 40)
                                              (new-circle 160 180 20 -40)
                                              0 0 #true #false)
                                       100 80 BUTTON-DOWN-EVENT)
   (make-world-state (make-new-circ 100 100 -20 40 #true 100 80) (new-circle 160 180 20 -40)
                     100 80 #true #true)
   "x,y coordinate of Circle1 and Circle2 shouldn't change, however Circle1 will become selected")

  (check-equal?
   (world-after-mouse-event (make-world-state (make-new-circ 100 100 -20 40 #true 100 80)
                                              (new-circle 160 180 20 -40)
                                              100 80 #true #true)
                                       80 80 DRAG-EVENT)
                            world-with-one-circle-selected
   "x,y coordinate of Circle1 should change to 80,100 and Circle2 shouldn't change")

  (check-equal?
   (world-after-mouse-event (make-world-state (new-circle 100 100 -20 40)
                                              (new-circle 160 180 20 -40)
                                              0 0 #true #false)
                                       80 80 "enter")
   (make-world-state (new-circle 100 100 -20 40)
                     (new-circle 160 180 20 -40)
                     0 0 #true #false)
   "x,y coordinate of Circle1 and Circle2 shouldn't change as mouse-enter event is not handled")

  (check-equal?
   (world-after-mouse-event (make-world-state (make-new-circ 80 100 -20 40 #true 80 80)
                                              (new-circle 160 180 20 -40)
                                              100 80 #true #true)
                                       80 80 BUTTON-UP-EVENT)
   (make-world-state (make-new-circ 80 100 -20 40 #false 0 0) (new-circle 160 180 20 -40)
                     0 0 #true #false)
   "x,y coordinate of Circle1 and Circle2 shouldn't change, also Circle1 will become unselected")
  
  (check-equal?
   (world-to-scene (initial-world 30))
    (place-circ1-with-text (new-circle 200 100 -12 20)
                           (place-circ2-with-text (new-circle 200 200 23 -14)))
    "There should be no change in state of circles as simulation was paused")

  (check-equal?
   (world-to-scene world-with-one-circle-selected)
   (place-image (circle POINTER-RADIUS SOLID-CIRCLE RED-COLOR)
                (world-state-pointer-x world-with-one-circle-selected)
                (world-state-pointer-y world-with-one-circle-selected)
                (place-circles-in-scene world-with-one-circle-selected))
   "Scene should have two circles, out of them one is selected and has pointer")
  
  (check-equal?
   (world-after-key-event (initial-world 30) PAUSE-KEY)
    (make-world-state (new-circle 200 100 -12 20) (new-circle 200 200 23 -14)
                      0 0 #false #false)
    "As space key was pressed, only change is in paused state variable")

  (check-equal?
   (world-after-key-event (initial-world 30) "p")
    (initial-world 30)
    "Space key was not pressed, so no change in world state"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
