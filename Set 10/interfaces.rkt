#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(provide CANVAS-WIDTH
         CANVAS-HEIGHT
         EMPTY-CANVAS
         SOLID-CIRCLE
         OUTLINE-CIRCLE
         ZERO
         Metatoy<%>
         Toy<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define SOLID-CIRCLE "solid")
(define OUTLINE-CIRCLE "outline")
(define ZERO 0)

(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; A Metatoy is an object of any class that implements Metatoy<%>.

(define Metatoy<%>
  (interface 
      
      ;; the (World<%>) says that Metatoy<%> inherits from World<%>
      ;; This means that any class that implements Metatoy<%> must
      ;; implement all the methods from World<%> plus all the methods
      ;; defined here.
      
      (SWidget<%>)
    
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
      (SWidget<%>)
    
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
