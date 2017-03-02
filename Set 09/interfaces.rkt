#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")

(provide CANVAS-WIDTH
         CANVAS-HEIGHT
         EMPTY-CANVAS
         SOLID-CIRCLE
         OUTLINE-CIRCLE
         World<%>
         Widget<%>
         Metatoy<%>
         Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define SOLID-CIRCLE "solid")
(define OUTLINE-CIRCLE "outline")

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACES

;; A World is an object of any class that implements World<%> interface.
;; big-bang will communicate with the world through the Metatoy<%>
;; interface.

(define World<%>
  (interface ()
    
    ; -> World
    ; GIVEN: no arguments
    ; RETURNS: the state of the world at the next tick.
    after-tick          
    
    ; Int Int MouseEvent-> World
    ; GIVEN: a location and a mouse-event
    ; RETURNS: the state of the world that should follow the
    ; given mouse event at the given location.
    after-mouse-event
    
    ; KeyEvent : KeyEvent -> World
    ; GIVEN: a key event
    ; RETURNS: the state of the world that should follow the
    ; given key event.
    after-key-event     
    
    ; -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene that depicts this World
    to-scene
    ))

;; A Widget is an object of any class that implements Widget<%> interface.

(define Widget<%>
  (interface ()
    
    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          
    
    ; Int Int -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    
    ; Int Int -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-up
    
    ; Int Int -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-drag
    
    ; KeyEvent : KeyEvent -> Widget
    ; GIVEN: a key event
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     
    
    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; A Metatoy is an object of any class that implements Metatoy<%>.

(define Metatoy<%>
  (interface 
      
      ;; the (World<%>) says that Metatoy<%> inherits from World<%>
      ;; This means that any class that implements Metatoy<%> must
      ;; implement all the methods from World<%> plus all the methods
      ;; defined here.
      
      (World<%>)
    
    ;; -> ListOfToy
    ;; GIVEN: no argument.
    ;; RETURNS: A list of toys.
    get-toys
    
    ))

;; A Toy is an object of any class that implements Toy<%>

(define Toy<%> 
  (interface
      
      ;; The interface Toy<%> inherits from the interface Widget<%>.
      ;; This means that any class that implements Toy<%> must implement
      ;; all the methods from Widget<%> plus all the methods defined here.
      (Widget<%>)
    
    
    ;;  Int Int -> Toy
    ;;  GIVEN: a location of the mouse coordinate
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move    
    
    ;; -> Int
    ;; GIVEN: a x-coordinate
    ;; RETURNS: the x position of the center of the toy
    toy-x
    
    ;; -> Int
    ;; GIVEN: a y-coordinate
    ;; RETURNS: the y position of the center of the toy
    toy-y
    
    ;; -> Int
    ;; GIVEN: no argument
    ;; RETURNS: some data related to the toy. The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data
    ))
