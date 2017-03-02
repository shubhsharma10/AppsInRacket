#lang racket

;; displays as an outline rectangle with particle moving
;; only in Y-direction.
;; the rectangle is draggable

(require 2htdp/image)
(require 2htdp/universe)
(require rackunit)
(require "extras.rkt")
(require "model.rkt")
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "Controller.rkt")
(require "DisplayController.rkt")

(provide make-ycontroller
         YController%)

;; make-ycontroller : Model -> Controller
;; GIVEN: a model m
;; RETURNS: A Y Controller for m
(define (make-ycontroller m)
  (new YController% [model m]))


;; Y controller contains details about the motion of the particle along the
;; y-direction. It inherits DisplayController% class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a YController is a (new YController% [model Model<%>]
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
(define YController%
  (class* DisplayController%
    ;Implement methods provided in both interfaces
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
    (inherit-field [particle-x half-width] particle-y)
    ; Fields to store delta when controller is being dragged
    (inherit-field saved-mx saved-my)
    ; Controller's cache for storing particle's velocity
    (inherit-field particle-vx particle-vy)
    ; whether handler is selected or controller is selected
    (inherit-field controller-selected? handler-selected?)
    ;; Fields to store delta when particle is being dragged
    (inherit-field particle-saved-mx particle-saved-my)
    
    (super-new [width MARGIN] [height (+ XY-CONTROLLER-HEIGHT MARGIN)])
    
    ;; at initialization, register this controller with the model
    (send model register this)

    ;; handle-controller-selection : Real Real -> Void
    ;; GIVEN: given coordinate of mouse
    ;; EFFECT: drags particle only in y direction in controller and
    ;; sets position in model
    (define/override (handle-controller-selection mx my)
      (begin
        (set-field! particle-y this (limit-dragged-particle-coord
                                     particle-y my particle-saved-my
                                     XY-CONTROLLER-HEIGHT))
        (send model execute-command
              (make-set-y-position particle-y))))  
    
    ;; viewer-image: -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: returns an image having the handler, YController and
    ;; the particle
    ;; STRATEGY: Combine simpler functions
    
    (define/override (viewer-image)
      (let ((particle (send this data-image))
            (controllor (send this rect-image MARGIN
                              XY-CONTROLLER-HEIGHT BLUE))
            (canvas (send this rect-image width height BLACK))
            (handler (send this rect-image HANDLER-WIDTH HANDLER-HEIGHT
                           (send this handler-color))))
        (overlay
         (place-image particle particle-x particle-y 
                      controllor)
         controllor
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
     (define yc (make-ycontroller m)))
    (check-equal? (image-height (send yc viewer-image)) 150
                  "The height of the y-controller is 150")
    (check-equal? (image-width (send yc viewer-image)) 50
                  "The width of the y-controller is 50")
    (send yc after-button-down 300 250)
    (check-equal? (get-field controller-selected? yc) #t
                  "The controller is selected")
    (send yc after-drag 330 280)
    (check-equal? (get-field particle-y yc) 80
                  "The x-coordinate of the particle is 80")
    (send yc after-button-up 330 280)
    (send yc after-button-down 277 177)
    (check-equal? (get-field handler-selected? yc) #t
                  "The handler is selected")
    (send yc after-drag 278 178)
    (check-equal? (get-field x yc) 301
                  "The x-coordinate is 301")
    (check-equal? (get-field y yc) 251
                  "The y-coordinate is 251")
    (send yc after-button-up 278 178)
    (send yc after-button-down 405 250)
    (send yc after-drag 406 250)
    (check-equal? (get-field handler-selected? yc) #f
                  "The handler is selected")
    (check-equal? (get-field controller-selected? yc) #f
                  "The controller is selected")
    
    ))