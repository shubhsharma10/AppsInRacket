;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname screensaver-3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; screensaver-3
;; A screensaver where user can create multiple circles
;; and have pen down on any selected circle which will result in
;; leaving ticks at canvas

;; start with (screensaver 1)

(require rackunit)
(require "extras.rkt")
(check-location "04" "screensaver-3.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 screensaver
 initial-world
 world-after-tick
 world-after-key-event
 new-circle
 world-paused?
 circ-x
 circ-y
 circ-vx
 circ-vy
 world-after-mouse-event
 circ-selected?
 world-circles
 circle-after-key-event
 circle-pen-down?)

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

;; constant related to canvas
(define CANVAS-WIDTH 400)
(define CANVAS-HEIGHT 300)
(define CANVAS-CENTER-X (/ CANVAS-WIDTH 2))
(define CANVAS-CENTER-Y (/ CANVAS-HEIGHT 2))

;; constant related to circle
(define CIRCLE-RADIUS 40)

(define ZERO 0)

;; Constants related to circle
(define BLUE-COLOR "blue")
(define RED-COLOR "red")
(define FONT-SIZE 10)

;; Constants for pen
(define PEN-RADIUS 1)
(define PEN-CIRCLE-TYPE "solid")
(define PEN-CIRCLE-COLOR "black")

;; Radius of the circle displayed when mouse is on canvas
(define POINTER-RADIUS 5)

;; Circle type of the the circle
(define SOLID-CIRCLE "solid")
(define OUTLINE-CIRCLE "outline")

;; Keyboard input constants
(define PAUSE " ")
(define PEN-DROP "d")
(define PEN-UP "u")
(define PEN-ERASE "e")
(define NEW-CIRCLE "n")
(define VX-INC "right")
(define VX-DEC "left")
(define VY-INC "down")
(define VY-DEC "up")

;; Allowed mouse events
(define BUTTON-DOWN-EVENT "button-down")
(define BUTTON-UP-EVENT "button-up")
(define DRAG-EVENT "drag")

(define INC "inc")
(define DEC "dec")
(define UP "up")
(define DOWN "down")
(define V-CHANGE 2)
(define V-INC V-CHANGE)
(define V-DEC (- V-CHANGE))

;; Empty canvas
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATA DEFINITIONS

;; a PenPosition is one of
;; -- "up"
;; -- "down"
;; INTERPRETATION:self-evident

;; TEMPLATE
;; pen-position-fn : PenPosition -> ??
#|
(define (pen-position-fn input)
  (cond
    [(string=? input UP) ...]
    [(string=? input DOWN) ...]))
|#

;; a ChangeString is one of
;; -- "inc"
;; -- "dec"
;; INTERPRETATION:self-evident

;; TEMPLATE
;; change-string-fn : ChangeString -> ??
#|
(define (change-string-fn input)
  (cond
    [(string=? input INC) ...]
    [(string=? input DEC) ...]))
|#

;; a KeyEvent is one of
;; -- "n"
;; -- "d"
;; -- "u"
;; -- "e"
;; -- "up"
;; -- "down"
;; -- "right"
;; -- "left"
;; -- " "
;; INTERPRETATION:
;; "n" - Pressing n key on keyboard will create a new circle with center at the
;;       center of canvas with (0,0) velocity
;; "d" - Pressing d key on keyboard will drop a pen down on selected circle at it's center
;; "u" - Pressing u key on keyboard will lift the pen up from selected circle
;; "e" - Pressing e key on keyboard will erase all marks made by circle
;; "up" - Pressing up key on keyboard will decrease velocity by 2 pixels/tick in y direction
;; "down" - Pressing down key on keyboard will increase velocity
;;          by 2 pixels/tick in y direction
;; "right" - Pressing right key on keyboard will increase velocity
;;           by 2 pixels/tick in x direction
;; "left" - Pressing left key on keyboard will decrease velocity by 2 pixels/tick in x direction
;; " " - Pressing space key on keyboard will result in either starting or stopping simulation

;; TEMPLATE
;; key-event-fn : KeyEvent -> ??
#|
(define (key-event-fn input)
  (cond
    [(string=? input NEW-CIRCLE) ...]
    [(string=? input PEN-DROP) ...]
    [(string=? input PEN-UP) ...]
    [(string=? input PEN-ERASE) ...]
    [(string=? input VY-DEC) ...]
    [(string=? input VY-INC) ...]
    [(string=? input VX-DEC) ...]
    [(string=? input VX-INC) ...]
    [(string=? input PAUSE) ...]))
|#

(define-struct dot(x y))
;; A Dot is a (make-dot NonNegInt NonNegInt)
;; x and y are (x,y) coordinates of center of the dots, units is in pixel

;; TEMPLATE:
;; dot-fn : Dot -> ??
;; (define (dot input)
;;   (.. (dot-x input) (dot-y input)))

;; ListOfDot

;; ListOfDot(LOD) is either
;; -- empty
;; -- (cons Dot LOD)

;; TEMPLATE:
;; lod-fn : LOD -> ??
;; (define (lod-fn lod)
;;   (cond
;;     [(empty? lod)...]
;;     [ else (...
;;               (dot-fn (first lod))
;;               (lod-fn (rest lod)))]))


(define-struct new-circ(x y vx vy selected? lx ly lod pen-down?))
;; A circle is a (make-new-circ NonNegInt NonNegInt Int Int Boolean
;;                              NonNegInt NonNegInt ListOfDot Boolean)
;; x and y are (x,y) coordinates of center of the circle, units is in pixels
;; vx and vy are velocity of circle in x and y coordinate respectively, units is in pixel/ticks
;; selected? is a boolean which has information about whether circle is selected or not
;; lx and ly are (x,y) coordinates of point where last time mouse was present, in pixel units
;; lod is list of dots drawn by circle when circle is selected and "d" key is pressed
;; pen-down? tells whether pen is down on circle or not

;; TEMPLATE:
;; new-circ-fn : Circle -> ??
;; (define (new-circ-fn w)
;;   (... (new-circ-x w) (new-circ-y w)
;;        (new-circ-vx w) (new-circ-vy w) (new-circ-selected? w)
;;        (new-circ-lx w) (new-circ-ly w) (new-circ-lod w)
;;        (new-circ-pen-down? w)))

;; ListOfCircle

;; A ListOfCircle (LOC) is either
;; -- empty
;; -- (cons Circle LOC)

;; TEMPLATE:
;; loc-fn : LOC -> ??
;; (define (loc-fn loc)
;;   (cond
;;     [(empty? loc)...]
;;     [ else (...
;;               (circle-fn (first loc))
;;               (loc-fn (rest loc)))]))



(define-struct world-state (circle-list pointer-x pointer-y paused? mouse-down?))
;; A WorldState is a (make-world-state ListOfCircle Int Int Boolean Boolean)
;; circle-list is a list of circles which are created in world state dynamically
;; pointer-x and pointer-y are x,y coordinates of the circle which is drawn
;; when mouse down or drag even is done on canvas, units is in pixels
;; paused? is a boolean which tells whether simulation is paused or not
;; mouse-down? is a boolean which tells whether mouse is pressed/ being dragged or not

;; TEMPLATE:
;; world-state-fn : WorldState -> ??
;; (define (world-state-fn w)
;;   (... (world-state-circle-list w)
;;        (world-state-pointer-x w) (world-state-pointer-y w)
;;        (world-state-paused? w)(world-state-mouse-down? w)))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; PROVIDE FUNCTIONS:

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;;          canvas with no circles
;; EXAMPLE:
;; (initial-world 10) = (make-world-state empty
;;                                         0 0 #true #false))
;; STRATEGY: Using WorldState constructor template
(define (initial-world input)
  (make-world-state
   empty
   ZERO ZERO
   #true
   #false))

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a unselected circle centered at (x,y), which will travel with
;; velocity (vx, vy).
;; EXAMPLE:
;; (new-circle 50 60 12 12) = circle1
;; STRATEGY: Using constructor tmeplate of circle
(define (new-circle x y vx vy)
  (make-new-circ x y vx vy #false 0 0 empty #false))

;; world-paused? : WorldState -> Boolean
;; GIVEN: World state
;; RETURNS: whether simulation is paused or not
;; EXAMPLE:
;; (world-paused? paused-world) = #true
;; (world-paused? unpaused-world) = #false
;; STRATEGY: Using observer template of WorldState on ws
(define (world-paused? ws)
  (world-state-paused? ws))

;; circ-x : Circle -> NonNegInt
;; GIVEN: Circle
;; RETURNS: x coordinate of center of the circle
;; EXAMPLE:
;; (circ-x circle1) = 50
;; STRATEGY: Using observer template of Circle on input
(define (circ-x input)
  (new-circ-x input))

;; circ-y : Circle -> NonNegInt
;; GIVEN: Circle
;; RETURNS: y coordinate of center of the circle
;; EXAMPLE:
;; (circ-y circle1) = 60
;; STRATEGY: Using observer template of Circle on input
(define (circ-y input)
  (new-circ-y input))

;; circ-vx : Circle -> Int
;; GIVEN: Circle
;; RETURNS: velocity of the circle in x direction
;; EXAMPLE:
;; (circ-vx circle1) = 12
;; STRATEGY: Using observer template of Circle on input
(define (circ-vx input)
  (new-circ-vx input))

;; circ-vy : Circle -> Int
;; GIVEN: Circle
;; RETURNS: velocity of the circle in y direction
;; EXAMPLE:
;; (circ-vy circle1) = 12
;; STRATEGY: Using observer template of Circle on input
(define (circ-vy input)
  (new-circ-vy input))

;; world-circles : WorldState -> ListOfCircle
;; RETURNS: the specified attribute of the WorldState
;; which is ListOfCircle
;; EXAMPLE:
;; (world-circles paused-world) = loc1
;; STRATEGY: Using template of WorlsState on ws
(define (world-circles ws)
  (world-state-circle-list ws))

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
;; EXAMPLE:
;; (circle-pen-down? circle1) = #false
;; STRATEGY: Using template of Circle on circle
(define (circle-pen-down? circle)
  (new-circ-pen-down? circle))

;; world-after-tick : WorldState -> WorldState
;; GIVEN: world state
;; RETURNS: the world state that should follow the given world state
;; after a tick.
;; EXAMPLE:
;; (world-after-tick unpaused-world)= unpaused-world-after-tick
;; (world-after-tick paused-world) = paused-world
;; STRATEGY: Using template on world
(define (world-after-tick ws)
  (if (world-paused? ws)
      ws
      (make-world-state
       (circles-after-tick (world-state-circle-list ws))
       (world-state-pointer-x ws) (world-state-pointer-y ws)
       #false
       (world-state-mouse-down? ws))))

;; world-after-mouse-event: WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A WorldState, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.
;; EXAMPLE:
;; (world-after-mouse-event unpaused-world 40 60 BUTTON-DOWN-EVENT)
;;                      = unpaused-world-after-mouse-down
;; STRATEGY: Use template for WorldState on ws
(define (world-after-mouse-event ws mx my mev)
  (make-world-state
   (circle-list-after-mouse-event (world-circles ws) mx my mev)
   (pointer-x-after-mouse-event mx mev)
   (pointer-y-after-mouse-event my mev)
   (world-state-paused? ws)
   (mouse-down-after-mouse-event? ws mev)))

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent
;; EXAMPLE:
;; (world-after-key-event unpaused-world PAUSE) = paused-world
;; (world-after-key-event paused-world NEW-CIRCLE) = paused-world-after-n
;; STRATEGY: Combine simple funcitons
(define (world-after-key-event ws key)
  (cond
    [(key=? key NEW-CIRCLE)      (add-circle ws)]
    [(key=? key PAUSE)           (world-with-pause-toggled ws)]
    [ else   (make-world-state
              (circle-list-after-key-event (world-circles ws) key)
              (world-state-pointer-x ws)
              (world-state-pointer-y ws)
              (world-state-paused? ws)
              (world-state-mouse-down? ws))]))

;; circ-selected? : Circle -> Boolean
;; RETURNS: true iff the given circle is selected.
;; EXAMPLE:
;; (circ-selected? (make-new-circ 100 100 20 24 #true 100 80 empty #false))
;; = #true
;; STRATEGY: Combine simple function
(define (circ-selected? circ)
  (new-circ-selected? circ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KeyEvent helper functions

;; circle-list-after-key-event : ListOfCircle KeyEvent -> ListOfCircle
;; GIVEN: ListOfCircle and key event
;; RETURNS: list of circles following key event
;; EXAMPLE:
;; (circle-list-after-key-event loc1 VY-INC) = loc1-after-vy-inc
;; STRATEGY: Use template of ListOfCircle on loc
;; HALTING-MEASURE: (length loc)
(define (circle-list-after-key-event loc key)
  (cond
    [(empty? loc) empty]
    [else (cons (circle-after-key-event (first loc) key)
                (circle-list-after-key-event (rest loc) key))]))

;; circle-after-key-event : Circle KeyEvent -> Circle
;; GIVEN: Circle and key event
;; RETURNS: Circle following key event
;; EXAMPLE:
;; (circle-after-key-event circle1 VY-DEC) = circle1-after-vy-dec
;; STRATEGY: Use KeyEvent template on key
(define (circle-after-key-event circle key)
  (cond
    [(not (circ-selected? circle)) circle]
    [(key=? key VY-DEC)          (change-circle-vy circle DEC)]
    [(key=? key VY-INC)          (change-circle-vy circle INC)]
    [(key=? key VX-INC)          (change-circle-vx circle INC)]
    [(key=? key VX-DEC)          (change-circle-vx circle DEC)]
    [(key=? key PEN-DROP)        (set-pen-to-circle circle DOWN)]
    [(key=? key PEN-UP)          (set-pen-to-circle circle UP)]
    [(key=? key PEN-ERASE)       (erase-dots-from-circle circle)]
    [else                        circle]))

;; change-circle-vx : Circle ChangeString -> Circle
;; GIVEN: Circle and ChangeString
;; RETURNS: Circle with either vx increased or decreased by 2 pixels/tick
;; EXAMPLE:
;; (change-circle-vx circle1 INC) = circle-after-vx-inc
;; STRATEGY: Combine simple functions
(define (change-circle-vx circle change)
  (make-new-circ (circ-x circle) (circ-y circle)
                 (+ (circ-vx circle) (get-aboslute-value change))
                 (circ-vy circle) (circ-selected? circle)
                 (new-circ-lx circle) (new-circ-ly circle)
                 (new-circ-lod circle) (circle-pen-down? circle)))

;; change-circle-vy : Circle ChangeString -> Circle
;; GIVEN: Circle and ChangeString
;; RETURNS: Circle with either vy increased or decreased by 2 pixels/tick
;; EXAMPLE:
;; (change-circle-vy circle1 INC) = circle-after-vy-inc
;; STRATEGY: Combine simple functions
(define (change-circle-vy circle change)
  (make-new-circ (circ-x circle) (circ-y circle)
                 (circ-vx circle)
                 (+ (circ-vy circle) (get-aboslute-value change))
                 (circ-selected? circle)
                 (new-circ-lx circle) (new-circ-ly circle)
                 (new-circ-lod circle) (circle-pen-down? circle)))

;; get-aboslute-value : ChangeString -> Int
;; GIVEN: ChangeString to identify whether need to decrease or increase
;; RETURNS: Integer value either V-INC or V-DEC
;; EXAMPLE:
;; (get-absolute-value DEC) = V-DEC
;; STRATEGY: Use template of ChangeString on change
(define (get-aboslute-value change)
  (if(string=? change INC)
     V-INC
     V-DEC))

;; add-circle : WorldState -> WorldState
;; GIVEN: WorldState
;; RETURNS: WorldState with a new circle centered at center of canvas
;; EXAMPLE:
;; (add-circle paused-world) = paused-world-after-n
;; STRATEGY: Using template of WorldState on ws
(define (add-circle ws)
  (make-world-state (create-circle (world-circles ws))
                    (world-state-pointer-x ws)
                    (world-state-pointer-y ws)
                    (world-state-paused? ws)
                    (world-state-mouse-down? ws)))

;; create-circle : ListOfCircle -> ListOfCircle
;; GIVEN: List of circles
;; RETURNS: List of circles with a new circle at center of canvas
;; EXAMPLE:
;; (create-circle loc1) = loc1-after-n
;; STRATEGY: Combine simple functions
(define (create-circle loc)
  (cons (make-new-circ CANVAS-CENTER-X CANVAS-CENTER-Y 0 0 #false 0 0 empty #false)
        loc))

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
   (world-state-circle-list ws)
   (world-state-pointer-x ws)
   (world-state-pointer-y ws)
   (not (world-state-paused? ws))
   (world-state-mouse-down? ws)))

;; set-pen-to-circle : Circle PenPosition -> Circle
;; GIVEN: Circle and pen position
;; RETURNS: Circle with pen-down? set
;; EXAMPLE:
;; (set-pen-to-circle selected-circle1 DOWN) = selected-circle1-with-pen-down
;; (set-pen-to-circle selected-circle1-with-pen-down UP) = selected-circle1-with-pen-up
;; STRATEGY: Using template of Circle on circle
(define (set-pen-to-circle circle pen-position)
  (make-new-circ (circ-x circle) (circ-y circle)
                 (circ-vx circle) (circ-vy circle)
                 (circ-selected? circle) (new-circ-lx circle)
                 (new-circ-ly circle) (new-circ-lod circle)
                 (get-pen-position pen-position)))

;; get-pen-position: PenPosition -> Boolean
;; GIVEN: PenPosition
;; RETURNS: true iff if pen position is down
;; STRATEGY: Using if-else
(define (get-pen-position pen-position)
  (if(string=? pen-position DOWN)
     #true
     #false))

;; erase-dots-from-circle : Circle -> Circle
;; GIVEN: Circle
;; RETURNS: Circle with empty ListOfDot
;; STRATEGY: Using template of Circle on circle
(define (erase-dots-from-circle circle)
  (make-new-circ (circ-x circle) (circ-y circle)
                 (circ-vx circle) (circ-vy circle)
                 (circ-selected? circle) (new-circ-lx circle)
                 (new-circ-ly circle) empty
                 (circle-pen-down? circle)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tick helper functions

;; circles-after-tick : ListOfCircle -> ListOfCircle
;; GIVEN: ListOfCircle
;; RETURNS: list of circles following tick
;; STRATEGY: Using ListOfCircle template on loc
;; HALTING-MEASURE: (length loc)
(define (circles-after-tick loc)
  (cond
    [(empty? loc) empty]
    [ else (cons (circ-after-tick (first loc))
                 (circles-after-tick (rest loc)))]))


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
    [else (make-new-circ (move-in-x circle) (move-in-y circle)
                         (change-vx circle) (change-vy circle)
                         (new-circ-selected? circle)
                         (new-circ-lx circle) (new-circ-ly circle)
                         (change-list-of-dots circle)
                         (circle-pen-down? circle))]))

;; change-list-of-dots Circle -> ListOfDot
(define (change-list-of-dots circle)
  (if(circle-pen-down? circle)
     (cons (make-dot (circ-x circle) (circ-y circle))
           (new-circ-lod circle))
     (new-circ-lod circle)))

;; move-in-x : Circle -> NonNegInt
(define (move-in-x circle)
  (cond
    [(circ-exceeds-lhs? circle)  CIRCLE-RADIUS]
    [(circ-exceeds-rhs? circle) (- CANVAS-WIDTH CIRCLE-RADIUS)]
    [else (possible-circ-x circle)]))

;; move-in-y : Circle -> NonNegInt
(define (move-in-y circle)
  (cond
    [(circ-exceeds-top? circle) CIRCLE-RADIUS]
    [(circ-exceeds-bottom? circle) (- CANVAS-HEIGHT CIRCLE-RADIUS)]
    [else (possible-circ-y circle)]))

;; change-vx : Circle -> Int
(define (change-vx circle)
  (if (or (circ-exceeds-lhs? circle) (circ-exceeds-rhs? circle))
      (- (circ-vx circle))
      (circ-vx circle)))

;; change-vy : Circle -> Int
;; RETURNS
(define (change-vy circle)
  (if (or (circ-exceeds-top? circle) (circ-exceeds-bottom? circle))
      (- (circ-vy circle))
      (circ-vy circle)))

;; possible-circ-x : Circle -> Int
;; RETURNS : possible value for x-coordinate of Circle
;; EXAMPLE:
;; (possible-circ-x (new-circle 390 270 10 20)) = 400
;; STRATEGY: Combine simple function
(define (possible-circ-x circle)
  (+ (circ-x circle) (circ-vx circle)))

;; possible-circ-y : Circle -> Int
;; RETURNS : possible value for y-coordinate of Circle
;; EXAMPLE:
;; (possible-circ-y (new-circle 390 270 10 20)) = 290
;; STRATEGY: Combine simple function
(define (possible-circ-y circle)
  (+ (circ-y circle) (circ-vy circle)))

;; circ-exceeds-lhs? : Circle -> Boolean
;; RETURNS : does circle exceeds canvas in x-direction(left side)
;; EXAMPLE:
;; (circ-exceeds-lhs? (new-circ 10 270 10 20)) = #true
;; STRATEGY: Combine simple function
(define (circ-exceeds-lhs? circle)
  (<= (possible-circ-x circle) CIRCLE-RADIUS ))

;; circ-exceeds-rhs? : Circle -> Boolean
;; RETURNS : does circle exceeds canvas in x-direction(right side)
;; EXAMPLE:
;; (circ-exceeds-rhs? (new-circ 390 270 10 20)) = #true
;; STRATEGY: Combine simple function
(define (circ-exceeds-rhs? circle)
  (<= CANVAS-WIDTH (+ (possible-circ-x circle) CIRCLE-RADIUS) ))

;; circ-exceeds-top? : Circle -> Boolean
;; RETURNS : does circle exceeds canvas in y-direction(top)
;; EXAMPLE:
;; (circ-exceeds-top? (new-circ 100 10 10 20)) = #true
;; STRATEGY: Combine simple function
(define (circ-exceeds-top? circle)
  (<= (possible-circ-y circle) CIRCLE-RADIUS ))

;; circ-exceeds-bottom? : Circle -> Boolean
;; RETURNS : does circle exceeds canvas in y-direction(bottom)
;; EXAMPLE:
;; (circ-exceeds-bottom? (new-circ 100 270 10 20)) = #true
;; STRATEGY: Combine simple function
(define (circ-exceeds-bottom? circle)
  (<= CANVAS-HEIGHT (+ (possible-circ-y circle) CIRCLE-RADIUS) ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mouse event helper functions

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

;; circ-after-mouse-event :  WorldSrate Int Int MouseEvent -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the circle that should follow the given circle after
;; the given mouse event
;; STRATEGY: Cases on MouseEvent
(define (circle-list-after-mouse-event loc mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN-EVENT) (circle-list-after-button-down loc mx my)]
    [(mouse=? mev DRAG-EVENT) (circle-list-after-drag loc mx my)]
    [(mouse=? mev BUTTON-UP-EVENT) (circle-list-after-button-up loc mx my)]
    [else loc]))

;; circle-list-after-button-down : ListOfCircle Int Int -> ListOfCircle
;; GIVEN: A ListOfCircle and x,y coordinate of location on button down of mouse
;; RETURNS: ListOfCircle which has selected or unselected circles depending on whether
;;          mouse down was inside the circle or not
;; HALTING MEASURE length of list of circles
;; STRATEGY: Use template of ListOfCircle on loc
(define (circle-list-after-button-down loc mx my)
  (cond
    [(empty? loc) empty]
    [else (cons (circle-after-button-down (first loc) mx my)
                (circle-list-after-button-down (rest loc) mx my))]))

(define (circle-after-button-down circ mx my)
  ( if (in-circle? circ mx my)
       (make-new-circ (new-circ-x circ) (new-circ-y circ)
                      (new-circ-vx circ) (new-circ-vy circ)
                      #true mx my (new-circ-lod circ)
                      (circle-pen-down? circ))
       circ))

;; circle-list-after-drag : ListOfCircle Int Int -> ListOfCircle
;; GIVEN: A ListOfCircle and x,y coordinate of location on which mouse drag is happening
;; RETURNS: ListOfCircle which has circles with updated (x,y) center coordinate if drag was
;;          happening inside the circle
;; HALTING MEASURE length of list of circles
;; STRATEGY: Use template of ListOfCircle on loc
(define (circle-list-after-drag loc mx my)
  (cond
    [(empty? loc) empty]
    [else (cons (circ-after-drag (first loc) mx my)
                (circle-list-after-drag (rest loc) mx my))]))

;; circ-after-drag : Circle Int Int -> Circle
;; GIVEN: A circle and x,y coordinate of location on which mouse drag is happening
;; RETURNS: the circle following a drag at the given location
;; STRATEGY: Use template for Circle on circ
(define (circ-after-drag circ mx my)
  (if (circ-selected? circ)
      (make-new-circ (new-x-coord circ mx my) (new-y-coord circ mx my)
                     (new-circ-vx circ) (new-circ-vy circ) #true
                     mx my (new-circ-lod circ) (circle-pen-down? circ))
      circ))

;; circle-list-after-button-up : ListOfCircle Int Int -> ListOfCircle
;; GIVEN: A ListOfCircle and x,y coordinate of location on which mouse up happened
;; RETURNS: ListOfCircle which has circles with updated (x,y) center coordinate on mouse up
;; HALTING MEASURE: length of list of circles
;; STRATEGY: Use template of ListOfCircle on loc
(define (circle-list-after-button-up loc mx my)
  (cond
    [(empty? loc) empty]
    [else (cons (circ-after-button-up (first loc) mx my)
                (circle-list-after-button-up (rest loc) mx my))]))

;; circ-after-button-up : Circle Int Int -> Circle
;; RETURNS: the circ following a button-up at the given location
;; STRATEGY: Use template for Circle on circ
(define (circ-after-button-up circ mx my)
  (if (circ-selected? circ)
      (make-new-circ (new-circ-x circ) (new-circ-y circ)
                     (new-circ-vx circ) (new-circ-vy circ)
                     #false 0 0 (new-circ-lod circ) (circle-pen-down? circ))   
      circ))

;; new-y-coord : Circle Int Int -> Int
;; RETURNS: new y coordinate of center of the circle
;; logic followed is that old center,old mouse down location,
;; current mouse location and new center forms a parallelogram,
;; where diagonals bisect each other
;; EXAMPLE:
;; (new-y-coord (make-new-circ 100 100 20 24 #true 100 80) 80 80 empty #false)
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
;; (new-x-coord (make-new-circ 100 100 20 24 #true 100 80) 80 80 empty #false)
;; = 80
;; STRATEGY: Combine simple function
(define (new-x-coord circ mx my)
  (- (+ (new-circ-x circ) mx) (new-circ-lx circ)))


;; in-circle? : Circle Int Int -> Circle
;; RETURNS true iff the given coordinate is inside the bounding box of
;; the given Circle.
;; EXAMPLE:
;; (in-circle? (make-new-circ 100 100 20 24 #true 80 90) 100 80 empty #false)
;; = #true
;; STRATEGY: Use template for Circle on circ
(define (in-circle? circ mx my)
  (<= (sqrt ( + (* (- (new-circ-x circ) mx) (- (new-circ-x circ) mx))
                (* (- (new-circ-y circ) my) (- (new-circ-y circ) my))))
      CIRCLE-RADIUS))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scene helper functions

;; world-to-scene : WorldState -> Scene
;; RETURNS: a Scene that portrays two circles or with two circles and one circle
;; to identify mouse location on canvas
;; STRATEGY: Use template for WorldState on w
(define (world-to-scene ws)
  (if(world-state-mouse-down? ws)
     (place-pointer-in-scene ws  (place-circles-in-scene (world-circles ws)))
     (place-circles-in-scene (world-circles ws))))

;; place-circles-in-scene : ListOfCircle -> Scene
;; GIVEN: List of circles
;; RETURNS: Scene with circles and dots in it
;; STRATEGY: Using ListOfCircles template on loc
;; HALTING MEASURE: (length loc)
(define (place-circles-in-scene loc)
  (cond
    [(empty? loc) EMPTY-CANVAS]
    [ else (place-circle-with-dot-and-text (first loc)
                                           (place-circles-in-scene (rest loc)))]))

;; place-pointer-in-scene : WorldState Scene -> Scene
;; RETURNS: Scene with circle which idenntifies mouse location on canvas
;; STRATEGY: Combine simple function
(define (place-pointer-in-scene ws container)
  (place-image (circle POINTER-RADIUS SOLID-CIRCLE RED-COLOR)
               (world-state-pointer-x ws) (world-state-pointer-y ws) container))

;; place-circ-with-dot-and-text : Circle Scene -> Scene
;; RETURNS: Scene with circle having it's velocity as string at center
;;          and dot on center
;; STRATEGY: Combine simple functions
(define (place-circle-with-dot-and-text circle container)
  (place-dots-and-circle (new-circ-lod circle)
                         (place-circ-with-text circle container)))

;; place-dots-and-circle ListOfDot Scene -> Scene
;; RETURNS: Scene with dots of a circle
;; STRATEGY: using template of ListOfDots on lod
;; HALTING-MEASURE: (length lod)
(define (place-dots-and-circle lod container)
  (cond
    [(empty? lod) container]
    [ else (place-image (circle PEN-RADIUS PEN-CIRCLE-TYPE PEN-CIRCLE-COLOR)
                        (dot-x (first lod)) (dot-y (first lod))
                        (place-dots-and-circle (rest lod) container) )]))

;; place-circ-with-text: Circle Scene -> Scene
;; RETURNS: Scene with Circle having it's velocity as string at center
;; STRATEGY: Combine simple function
(define (place-circ-with-text circle container)
  (place-image (text (circ-velocity-in-string circle) FONT-SIZE BLUE-COLOR)
               (new-circ-x circle) (new-circ-y circle) (place-circ circle container)))


;; place-circ : Circle Scene -> Scene
;; RETURNS: Scene with Circ1
;; EXAMPLE:
;; (place-circ1 circle1 EMPTY-CANVAS) =
;; (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE COLOR)
;;              (circ-x circle1) (circ-y circle1) EMPTY-CANVAS
;; STRATEGY: Combine simple function
(define (place-circ input container)
  (place-image (circle CIRCLE-RADIUS OUTLINE-CIRCLE (get-color input))
               (new-circ-x input) (new-circ-y input) container))

;; get-color : Circle -> Color
;; RETURNS: Color depending on circle is selected or not
;; EXAMPLE:
;; (get-color (make-new-circ 100 100 20 24 #true 100 80 empty #false))
;; = RED-COLOR
;; STRATEGY: Combine simple function
(define (get-color circ)
  (if (circ-selected? circ)
      RED-COLOR
      BLUE-COLOR))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLE DEFINITIONS:

;; Example of Dots
(define dot1 (make-dot 50 60))

(define lod1
  (list dot1))

;; Example of Circles
(define circle1 (make-new-circ 50 60 12 12 #false 0 0 empty #false))

(define selected-circle1
  (make-new-circ 50 60 12 12 #true 0 0 empty #false))

(define selected-circle1-with-pen-down
  (make-new-circ 50 60 12 12 #true 0 0 empty #true))

(define selected-circle1-with-pen-down-and-dot
  (make-new-circ 50 60 12 12 #true 0 0 lod1 #true))

(define selected-circle1-with-pen-up
  (make-new-circ 50 60 12 12 #true 0 0 empty #false))

(define circle1-after-vy-inc
  (make-new-circ 50 60 12 14 #true 0 0 empty #false))

(define circle1-after-vy-dec
  (make-new-circ 50 60 12 10 #true 0 0 empty #false))

(define circle1-after-vx-inc
  (make-new-circ 50 60 14 12 #true 0 0 empty #false))

(define circle1-after-vx-dec
  (make-new-circ 50 60 10 12 #true 0 0 empty #false))

(define circle1-after-tick
  (make-new-circ 62 72 12 12 #false 0 0 empty #false))

(define circle1-after-mouse-down
  (make-new-circ 50 60 12 12 #true 40 60 empty #false))

(define loc1
  (list
   circle1))

(define loc1-with-selection
  (list
   selected-circle1))

(define loc1-after-vy-inc
  (list
   circle1-after-vy-inc))

(define loc2 (create-circle loc1))

(define loc1-after-tick
  (list
   circle1-after-tick))

(define loc1-after-mouse-down
  (list
   circle1-after-mouse-down))

;; Example of WorldState, for testing
(define paused-world (make-world-state loc1 0 0 #true #false))

(define paused-world-with-selected-loc1
  (make-world-state loc1-with-selection 0 0 #true #false))

(define paused-world-with-selected-loc1-with-pointer
  (make-world-state loc1-with-selection 90 90 #true #true))

(define paused-world-after-vy-inc
  (make-world-state loc1-after-vy-inc 0 0 #true #false))

(define unpaused-world
  (make-world-state loc1 0 0 #false #false))

(define paused-world-after-n
  (make-world-state loc2 0 0 #true #false))

(define unpaused-world-after-mouse-down
  (make-world-state loc1-after-mouse-down 40 60 #false #true))

(define unpaused-world-after-tick
  (make-world-state loc1-after-tick 0 0 #false #false))

;; TESTS

(begin-for-test
  (check-equal? (initial-world 10) (make-world-state empty 0 0 #true #false))
  
  (check-equal? (world-after-tick paused-world) paused-world)
  
  (check-equal? (world-after-tick unpaused-world) unpaused-world-after-tick)
  
  (check-equal? (circ-after-tick selected-circle1) selected-circle1)
  
  (check-equal? (world-after-mouse-event unpaused-world 40 60 BUTTON-DOWN-EVENT)
                unpaused-world-after-mouse-down)
  
  (check-equal? (world-after-key-event unpaused-world PAUSE) paused-world)
  
  (check-equal? (world-after-key-event paused-world NEW-CIRCLE) paused-world-after-n)
  
  (check-equal? (circle-list-after-key-event loc1-with-selection VY-INC)
                loc1-after-vy-inc)
  
  (check-equal? (circle-after-key-event selected-circle1 VY-DEC)
                circle1-after-vy-dec)
  
  (check-equal? (circle-after-key-event circle1 VY-DEC) circle1)
  
  (check-equal? (circle-after-key-event selected-circle1 "m")
                selected-circle1)
  
  (check-equal? (circle-after-key-event selected-circle1 VX-DEC)
                circle1-after-vx-dec)
  
  (check-equal? (circle-after-key-event selected-circle1 VX-INC)
                circle1-after-vx-inc)
  
  (check-equal? (circle-after-key-event selected-circle1 PEN-DROP)
                selected-circle1-with-pen-down)
  
  (check-equal? (circle-after-key-event selected-circle1-with-pen-down PEN-UP)
                selected-circle1-with-pen-up)
  
  (check-equal? (circle-after-key-event selected-circle1 PEN-ERASE) selected-circle1)
  
  (check-equal? (change-list-of-dots selected-circle1-with-pen-down)
                lod1)
  
  (check-equal? (mouse-down-after-mouse-event? paused-world "enter") #false)
  
  (check-equal? (pointer-x-after-mouse-event 10 "enter") ZERO)
  
  (check-equal? (pointer-y-after-mouse-event 10 "enter") ZERO)
  
  (check-equal? (circ-after-button-up circle1 10 20) circle1)
  
  (check-equal? (circle-after-button-down circle1 10 20) circle1)
  
  (check-equal? (circ-after-drag circle1 10 20) circle1)
  
  (check-equal? (circle-list-after-mouse-event loc1 10 20 "enter") loc1)
  
  (check-equal?
   (world-after-tick (make-world-state (list (new-circle 30 50 -20 -40))
                                       0 0 #false #false))
   (make-world-state (list (new-circle 40 40 20 40)) 0 0 #false #false)
   "x,y coordinate of Circle1 should change to 40,40")
  
  (check-equal?
   (world-after-tick (make-world-state (list  (new-circle 370 240 20 40))
                                       0 0 #false #false))
   (make-world-state (list (new-circle 360 260 -20 -40))
                     0 0 #false #false)
   "x,y coordinate of Circle1 should change to 360,260")
  
  (check-equal?
   (world-to-scene paused-world)
   (place-circ-with-text circle1
                         EMPTY-CANVAS)
   "There should be no change in state of circles as simulation was paused")
  
  (check-equal?
   (world-to-scene paused-world-with-selected-loc1)
   (place-circ-with-text selected-circle1
                         EMPTY-CANVAS)
   "There should be no change in state of circles as simulation was paused")
  
  (check-equal?
   (place-dots-and-circle lod1 EMPTY-CANVAS)
   (place-image (circle PEN-RADIUS PEN-CIRCLE-TYPE PEN-CIRCLE-COLOR)
                (dot-x (first lod1))
                (dot-y (first lod1))
                EMPTY-CANVAS)
   "Dot is not placed correctly in canvas")
  
  (check-equal?
   (world-after-key-event paused-world-with-selected-loc1 VY-INC)
   paused-world-after-vy-inc)
  
  (check-equal?
   (world-after-mouse-event (make-world-state (list(make-new-circ 100 100 -20 40
                                                                  #true 100 80
                                                                  empty #false))                             
                                              100 80 #true #true)
                            80 80 DRAG-EVENT)
   
   (make-world-state (list(make-new-circ 80 100 -20 40
                                         #true 80 80
                                         empty #false))
                     80 80 #true #true)
   "x,y coordinate of Circle1 should change to 80,100")
  
  (check-equal?
   (world-after-mouse-event (make-world-state (list (make-new-circ 80 100 -20 40
                                                                   #true 80 80
                                                                   empty #false))
                                              100 80 #true #true)
                            80 80 BUTTON-UP-EVENT)
   (make-world-state (list (make-new-circ 80 100 -20 40
                                          #false 0 0
                                          empty #false))
                     0 0 #true #false)
   "x,y coordinate of Circle1 shouldn't change, also Circle1 will become unselected")
  
  (check-equal?
   (world-to-scene paused-world-with-selected-loc1)
   (place-circles-in-scene loc1-with-selection)
   "Scene should have one circle and it is selected")
  
  (check-equal?
   (world-to-scene paused-world-with-selected-loc1-with-pointer)
   (place-image (circle POINTER-RADIUS SOLID-CIRCLE RED-COLOR)
                (world-state-pointer-x paused-world-with-selected-loc1-with-pointer)
                (world-state-pointer-y paused-world-with-selected-loc1-with-pointer)
                (place-circles-in-scene loc1-with-selection))
   "Scene should have one circle and it is selected with pointer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;