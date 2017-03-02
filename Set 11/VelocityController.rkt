#lang racket

;; Displays as an outline rectangle with text showing the x,y
;; coordinate and velocity of the particle.

;; This controller manipulates the position of the particle in the container
;; based on the key events from the user.

;; the rectangle is draggable using the handler

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "model.rkt")
(require "Controller.rkt")
(require "MovementController.rkt")

(provide make-velocity-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-velocity-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A velocity controller for m
;; EXAMPLES: See test cases

(define (make-velocity-controller m)
  (new VelocityController% [model m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VelocityController is a sub class of MovementController% and inherits all
;; the methods from this class.MovementController% is a subclass of
;; Controller% and inherits all the methods from it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a VelocityController is a (new VelocityController% [model Model<%>]
;;                                      [x Real] [y Real]
;;                                      [width NonNegInt] [height NonNegInt]
;;                                      [particle-x Real] [particle-y Real]
;;                                      [particle-vx Real]
;;                                      [particle-vy Real]
;;                                      [half-width NonNegInt]
;;                                      [half-height NonNegInt]
;;                                      [saved-mx Real] [saved-my Real]
;;                                      [handler-selected? Boolean]
;;                                      [controller-selected? Boolean])
;; where the model represents the model with which the controller is binded
;; x,y are coordinates of center of the controller
;; width,height are width and height of the controller
;; particle-x, particle-y are cache for Controller to store center coordinate
;; of particle's
;; particle-vx, particle-vy are cache for Controller to store velocity of
;; particle.
;; half-width,half-height are half of width and height of controller
;; saved-mx,saved-my stores delta for when controller is dragged
;; handler-selected? identifies whether controller can be dragged or not
;; controller-selected? identifies whether particle can be dragged or not

;; Only model field is mandatory while creating object of this class, rest
;; fields are optional

(define VelocityController%
  (class* MovementController%
    ;; Methods needs to be implemented in these interfaces
    (MovementController<%>  ControllerHooks<%> MovementControllerHooks<%>)
    
    ;;the model to which this VelocityController belongs to.
    (inherit-field model)
    
    ;;the position of the center of this VelocityController
    (inherit-field x y)

    ;;the width and height of the velocity controller
    (inherit-field width height)

    ;;the half-width and half-height of the position controller
    (inherit-field half-width half-height)

    ;;Controller's cache for storing particle's (x,y) coordinate 
    (inherit-field particle-x particle-y)

    ;;velocity of the particle along the x and y directions
    (inherit-field particle-vx particle-vy)

    ;;true iff the controller is seletected or false
    (inherit-field controller-selected?)

    ;;saved-mx and saved-my for dragging
    (inherit-field saved-mx saved-my)
    
    ;;the value with which the position should be updated.
    (inherit-field update-factor)

    ;;font-size
    (inherit-field FONT-SIZE)
    
    ;; text
    (field [VELOCITY-TEXT "Arrow keys : Change velocity"])

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    (super-new)
    
    ;; at initialization, register this controller with the model
    (send model register this)

    ;; handle-arrow-up-key-event -> Void
    ;; EFFECT: Decreases velocity in y direction and updates model
    ;; STRATEGY: Using Set-Velocity template
    (define/override (handle-arrow-up-key-event)
      (send model execute-command
            (make-set-velocity 0 (validate-coord-for-velocity-controller
                                  particle-y particle-vy #f
                                  XY-CONTROLLER-HEIGHT
                                  update-factor))))

    ;; handle-arrow-down-key-event -> Void
    ;; EFFECT: Increases velocity in y direction and updates model
    ;; STRATEGY: Using Set-Velocity template
    (define/override (handle-arrow-down-key-event)
      (send model execute-command
            (make-set-velocity 0 (validate-coord-for-velocity-controller
                                  particle-y particle-vy #t
                                  XY-CONTROLLER-HEIGHT
                                  update-factor))))

    ;; handle-arrow-right-key-event -> Void
    ;; EFFECT: Increases velocity in x direction and updates model
    ;; STRATEGY: Using Set-Velocity template
    (define/override (handle-arrow-right-key-event)
      (send model execute-command
            (make-set-velocity (validate-coord-for-velocity-controller
                                particle-x particle-vx #t
                                XY-CONTROLLER-WIDTH
                                update-factor) 0)))

    ;; handle-arrow-left-key-event -> Void
    ;; EFFECT: Decreases velocity in x direction and updates model
    ;; STRATEGY: Using Set-Velocity template
    (define/override (handle-arrow-left-key-event)
      (send model execute-command
            (make-set-velocity (validate-coord-for-velocity-controller
                                particle-x particle-vx #f
                                XY-CONTROLLER-WIDTH
                                update-factor) 0)))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; data-image -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: image of this Velocity Controller with
    ;;          the x and y co-ordinates and velocities along x and
    ;;          y directions of the particle.
    ;; DETAILS: This is an override method as it is an abstract
    ;; method in the superclass.
    ;; STRATEGY: Combine simpler functions
    
    (define/override (data-image)
      (above
       (text VELOCITY-TEXT FONT-SIZE (send this controller-color))
       (send this get-position-text)
       (send this get-velocity-text)))

    ;; validate-coord-for-velocity-controller : Real Real Boolean NonNegInt Integer
    ;;                                        -> Real
    ;; GIVEN: current coordinate, old velocity, increasing or decreasing
    ;; maximum value allowed for this coordinate, factor by which to change velocity
    ;; RETURNS: delta to which velcoity can be changed
    (define (validate-coord-for-velocity-controller coord oldV inc? max
                                                    update-factor)
      (local
        ((define newC coord)
         (define dV 0))
        (if inc?
            (begin
              (set! dV (+ update-factor))
              (set! newC (+ coord (+ oldV update-factor))))
            (begin
              (set! dV (- update-factor))
              (set! newC (+ coord (- oldV update-factor)))))
        (if (or (< newC 0) (> newC max))
            0
            dV)))
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tests

(begin-for-test
  (local
    ((define m (make-model))
     (define p (make-velocity-controller m)))
    (send p after-button-down 5 5)
    (check-equal? (get-field controller-selected? p) #f
                  "The controller is selected.")
    (send p after-key-event "a")
    (check-equal? (get-field particle-vx p) 0
                  "The particle vx velocity is 0")
    (send p after-button-down 300 250)
    (check-equal? (get-field controller-selected? p) #t
                  "The controller is selected")
    (check-equal? (image-height (send p viewer-image)) 70
                  "The height is 70")
    (send p after-key-event UP)
    (check-equal? (get-field particle-vy p) -5
                  "The particle vy velocity is -5")
    (send p after-key-event DOWN)
    (check-equal? (get-field particle-vy p) 0
                  "The particle vy velocity is 0")
    (send p after-key-event RIGHT)
    (check-equal? (get-field particle-vx p) 5
                  "The particle vx velocity is 5")
    (send p after-key-event LEFT)
    (check-equal? (get-field particle-vx p) 0
                  "The particle vx velocity us 0")
    ))
