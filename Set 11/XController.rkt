#lang racket

;; displays as an outline rectangle with particle moving only in
;; x direction
;; the rectangle is draggable

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "model.rkt")
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "DisplayController.rkt")
(require "Controller.rkt")

(provide make-xcontroller
         XController%)

;; make-xcontroller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A X Controller for m
(define (make-xcontroller m)
  (new XController% [model m]))


;; X controller contains details about the motion of the particle along the
;; x-direction. It inherits DisplayController% class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a XController is a (new XController% [model Model<%>]
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
(define XController%
  (class* DisplayController%
    ; Implement methods provided in interfaces
    (Controller<%> ControllerHooks<%> DisplayControllerHooks<%>)
    
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
    (inherit-field particle-x [particle-y half-height])
    ; Fields to store delta when controller is being dragged
    (inherit-field saved-mx saved-my)
    ; Controller's cache for storing particle's velocity
    (inherit-field particle-vx particle-vy)
    ; whether handler is selected or controller is selected
    (inherit-field controller-selected? handler-selected?)
    ;; Fields to store delta when particle is being dragged
    (inherit-field particle-saved-mx particle-saved-my)
    
    (super-new [width (+ XY-CONTROLLER-WIDTH MARGIN)] [height MARGIN])
    
    ;; at initialization, register this controller with the model
    (send model register this)

    ;; handle-controller-selection : Real Real -> Void
    ;; GIVEN: given coordinate of mouse
    ;; EFFECT: drags particle only in x direction in controller and
    ;; sets position in model
    (define/override (handle-controller-selection mx my)
      (begin
        (set-field! particle-x this (limit-dragged-particle-coord
                                     particle-x mx particle-saved-mx
                                     XY-CONTROLLER-WIDTH))
        (send model execute-command
              (make-set-x-position particle-x))))
    
    ;; viewer-image: -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: returns an image having the handler, XController and
    ;; the particle
    ;; STRATEGY: Combine simpler functions
    (define/override (viewer-image)
      (let ((particle (send this data-image))
            (controller (send this rect-image
                              XY-CONTROLLER-WIDTH
                              MARGIN BLUE))
            (canvas (send this rect-image width height BLACK))
            (handler (send this rect-image HANDLER-WIDTH HANDLER-HEIGHT
                           (send this handler-color))))
        (overlay 
         (place-image particle particle-x particle-y 
                      controller)
         controller
         (overlay/align
          "left"
          "top"
          handler
          canvas)
         )))    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS:

(begin-for-test
  (local
    ((define m (make-model))
     (define xc (make-xcontroller m)))
    (check-equal? (image-height (send xc viewer-image)) 50
                  "The height of the X-controller is 50")
    (check-equal? (image-width (send xc viewer-image)) 200
                  "The width of the X-controller is 200")
    (send xc after-button-down 300 250)
    (check-equal? (get-field controller-selected? xc) #t
                  "The controller is selected")
    (send xc after-drag 330 280)
    (check-equal? (get-field particle-x xc) 105
                  "The x-coordinate of the particle is 105")
    (send xc after-button-up 330 280)
    (send xc after-button-down 205 227)
    (check-equal? (get-field handler-selected? xc) #t
                  "The handler is selected")
    (send xc after-drag 206 228)
    (check-equal? (get-field x xc) 301
                  "The x-coordinate of the particle is 301")
    (check-equal? (get-field y xc) 251
                  "The x-coordinate of the particle is 251")
    (send xc after-button-up 206 228)
    (send xc after-button-down 405 250)
    (send xc after-drag 406 250)
    (check-equal? (get-field handler-selected? xc) #f
                  "The handler is selected")
    (check-equal? (get-field controller-selected? xc) #f
                  "The controller is selected")
    
    ))