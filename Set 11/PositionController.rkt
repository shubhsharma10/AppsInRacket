#lang racket

;; Displays as an outline rectangle with text showing the x,y
;; coordinate and velocity of the particle.

;; This controller manipulates the position of the particle in the container
;; based on the key events from the user.

;; the rectangle is draggable using the handler
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 2htdp/image)
(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "model.rkt")
(require "Controller.rkt")
(require "MovementController.rkt")

(provide make-position-controller)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-position-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A position controller for m
;; EXAMPLES: See test cases

(define (make-position-controller m)
  (new PositionController% [model m]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PositionController is a sub class of MovementController% and inherits all
;; the methods from ;; this class. MovementController% is a subclass of
;; Controller% and inherits all the methods from it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a PositionController is a (new PositionController% [model Model<%>]
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

(define PositionController%
  (class* MovementController%
    ;; Methods needs to be implemented in these interfaces
    (MovementController<%>  ControllerHooks<%> MovementControllerHooks<%>)

    ;;the model to which this PositionController belongs to.
    (inherit-field model)
    
    ;;the position of the center of this PositionController
    (inherit-field x y)

    ;;the width and height of the position controller
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
    (field [POSITION-TEXT "Arrow keys : Change position"])

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (super-new)

    ;; at initialization, register this controller with the model

    (send model register this)

    ;; handle-arrow-up-key-event -> Void
    ;; EFFECT: Decreases position in y direction and updates model
    ;; STRATEGY: Using Set-Position template
    (define/override (handle-arrow-up-key-event)
      (send model execute-command
            (make-set-position particle-x
                               (- particle-y update-factor))))

    ;; handle-arrow-down-key-event -> Void
    ;; EFFECT: Increases position in y direction and updates model
    ;; STRATEGY: Using Set-Position template
    (define/override (handle-arrow-down-key-event)
      (send model execute-command
            (make-set-position particle-x
                               (+ particle-y update-factor))))

    ;; handle-arrow-right-key-event -> Void
    ;; EFFECT: Increases position in x direction and updates model
    ;; STRATEGY: Using Set-Position template
    (define/override (handle-arrow-right-key-event)
      (send model execute-command
            (make-set-position (+ particle-x update-factor)
                               particle-y)))

    ;; handle-arrow-left-key-event -> Void
    ;; EFFECT: Decreases position in x direction and updates model
    ;; STRATEGY: Using Set-Position template
    (define/override (handle-arrow-left-key-event)
      (send model execute-command
            (make-set-position (- particle-x update-factor)
                               particle-y))) 

   
    ;; data-image -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: image of this Position Controller with
    ;;          the x and y co-ordinates and velocities along x and
    ;;          y directions of the particle.
    ;; STRATEGY: Combine simpler functions
    (define/override (data-image)
      (above
       (text POSITION-TEXT FONT-SIZE (send this controller-color))
       (send this get-position-text)
       (send this get-velocity-text)))

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Tests

(begin-for-test
  (local
    ((define m (make-model))
     (define p (make-position-controller m)))
    (send p after-button-down 5 5)
    (check-equal? (get-field controller-selected? p) #f
                  "The controller is not selected")
    (send p after-button-down 300 250)
    (check-equal? (get-field controller-selected? p) #t
                  "The controller is selected")
    (check-equal? (image-height (send p viewer-image)) 70
                  "The height of the position controller is 70")
    (send p after-key-event UP)
    (check-equal? (get-field particle-y p) 45
                  "The particle-y is 45")
    (send p after-key-event DOWN)
    (check-equal? (get-field particle-y p) 50
                  "The particle-y is 50")
    (send p after-key-event RIGHT)
    (check-equal? (get-field particle-x p) 80
                  "The particle-x is 80")
    (send p after-key-event LEFT)
    (check-equal? (get-field particle-x p) 75
                  "The particle-x is 75")
    ))
