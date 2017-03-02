#lang racket

;; displays as an outline rectangle with particle moving in
;; both x and y direction
;; the rectangle is draggable.

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "model.rkt")
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "Controller.rkt")
(require "DisplayController.rkt")

(provide make-xycontroller
         XYController%)

;; make-xy-controller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A XY Controller for m
(define (make-xycontroller m)
  (new XYController% [model m]))


;; XY controller contains details about the motion of the particle along the
;; x-direction and y-direction. It inherits DisplayController% class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a XYController is a (new XYController% [model Model<%>]
;;                                      [x Real] [y Real]
;;                                      [width NonNegInt] [height NonNegInt]
;;                                      [particle-x Real] [particle-y Real]
;;                                      [particle-vx Real]
;;                                      [particle-vy Real]
;;                                      [half-width NonNegInt]
;;                                      [half-height NonNegInt]
;;                                      [saved-mx Real] [saved-my Real]
;;                                      [handler-selected? Boolean]
;;                                      [controller-selected? Boolean]
;;                                      [particle-saved-mx Real]
;;                                      [particle-saved-my Real])
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
;; particle-saved-mx,particle-saved-my stores delta for when particle is dragged
;; inside controller

;; Only model field is passed while creating object of this class, rest
;; fields are optional
(define XYController%
  (class* DisplayController%
    ;Implement methods provided in interfaces
    (Controller<%>  ControllerHooks<%> DisplayControllerHooks<%>)
    
    ; the model to which this XController belongs
    (inherit-field model)    
    ;; initial values of x, y (center of controller)
    (inherit-field x y)
    ; Width and height of the controller, this will be set by each controller
    ; sub-class
    (inherit-field width height)    
    ; Half width and height of controller
    (inherit-field half-width half-height)
    ; Controller's cache for storing particle's (x,y) coordinate
    (inherit-field particle-x particle-y)
    ; Fields to store delta when controller is being dragged
    (inherit-field saved-mx saved-my)
    ; Controller's cache for storing particle's velocity
    (inherit-field particle-vx particle-vy)
    ;; Fields to store delta when particle is being dragged
    (inherit-field particle-saved-mx particle-saved-my)
    
    (super-new [width (+ XY-CONTROLLER-WIDTH MARGIN)]
               [height (+ XY-CONTROLLER-HEIGHT MARGIN)] )
    
    ;; at initialization, register this controller with the model
    (send model register this)

    ;; handle-controller-selection : Real Real -> Void
    ;; GIVEN: given coordinate of mouse
    ;; EFFECT: drags particle in controller and sets position in model
    (define/override (handle-controller-selection mx my)
      (begin
        (set-field! particle-x this (limit-dragged-particle-coord
                                     particle-x
                                     mx
                                     particle-saved-mx
                                     XY-CONTROLLER-WIDTH))
        (set-field! particle-y this (limit-dragged-particle-coord
                                     particle-y
                                     my
                                     particle-saved-my
                                     XY-CONTROLLER-HEIGHT))
        (send model execute-command
              (make-set-position particle-x  particle-y))))

    ;; viewer-image: -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: returns an image having the handler, XYController and
    ;; the particle
    ;; STRATEGY: Combine simpler functions
    (define/override (viewer-image)
      (let ((the-particle (send this data-image))
            (the-controllor (send this rect-image
                                  XY-CONTROLLER-WIDTH
                                  XY-CONTROLLER-HEIGHT BLUE))
            (the-canvas (send this rect-image
                              width height BLACK))
            (the-handler (send this rect-image HANDLER-WIDTH HANDLER-HEIGHT
                               (send this handler-color))))
        (overlay 
         (place-image the-particle particle-x particle-y 
                      the-controllor)
         (overlay/align
          "left"
          "top"
          the-handler
          the-canvas)
         the-controllor)))
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS

(begin-for-test
  (local
    ((define m (make-model))
     (define xy (make-xycontroller m)))
    (check-equal? (image-height (send xy viewer-image)) 150
                  "The height of the xy-controller is 150")
    (check-equal? (image-width (send xy viewer-image)) 200
                  "The width of the xy-controller is 200")
    (send xy after-button-down 300 250)
    (check-equal? (get-field controller-selected? xy) #t
                  "The controlled is selected")
    (send xy after-drag 330 280)
    (check-equal? (get-field particle-x xy) 105
                  "The particle-x is 105")
    (check-equal? (get-field particle-y xy) 80
                  "The particle-y is 80")
    (send xy after-button-up 330 280)
    (send xy after-button-down 202 177)
    (check-equal? (get-field handler-selected? xy) #t
                  "The handler is selected")
    (send xy after-drag 203 178)
    (check-equal? (get-field x xy) 301
                  "The x-coordinate is 301")
    (check-equal? (get-field y xy) 251
                  "The y-coordinate is 251")
    (send xy after-button-up 206 228)
    (send xy after-button-down 405 250)
    (send xy after-drag 406 250)
    (check-equal? (get-field handler-selected? xy) #f
                  "The handler is selected")
    (check-equal? (get-field controller-selected? xy) #f
                  "The controller is selected.")
    ))
