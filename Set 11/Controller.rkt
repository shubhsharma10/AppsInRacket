#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Utility.rkt")

;; Controller.rkt
;;
;; This is the super class to all the other controllers.
;; This implements all the common functionalities on the existing controllers
;; which are:
;; -- PositionController
;; -- VelocityController
;; -- XController
;; -- YController
;; -- XYController
;; -- DisplayController
;; -- MovemenetController
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide Controller%
         ControllerHooks<%>)

;; CONSTANTS
(define INIT-X (/ CANVAS-WIDTH 2))
(define INIT-Y (/ CANVAS-HEIGHT 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a Controller% is a (new Controller% [model Model<%>]
;;                                      [x Real] [y Real]
;;                                      [width NonNegInt] [height NonNegInt]
;;                                      [particle-x Real] [particle-y Real]
;;                                      [particle-vx NonNegInt]
;;                                      [particle-vy NonNegInt]
;;                                      [half-width Integer]
;;                                      [half-height Integer]
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

;; Only model field is passed while creating object of this class, rest
;; fields are optional

;; Controller% class implements methods provided in Controller<%> interface.
;; This implements all the common functionalities on the existing controllers
;; which are:
;; -- PositionController
;; -- VelocityController
;; -- XController
;; -- YController
;; -- XYController
;; -- DisplayController
;; -- MovemenetController
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define Controller%
  (class* object%
    ; Interface whose methods are implemented
    (Controller<%>)   

    ;; model to which this controller belongs
    (init-field model)
    ;; initial values of x, y (center of controller)
    (init-field [x INIT-X] [y INIT-Y])
    ; Width and height of the controller, this will be set by each controller
    ; sub-class
    (init-field [width 0] [height 0])    
    ; Controller's cache for storing particle's (x,y) coordinate
    (init-field [particle-x x] [particle-y y])    
    ; Half width and height of controller
    (init-field [half-width  (/ width  2)] [half-height (/ height 2)])    
    ; Fields to store delta when controller is being dragged
    (init-field [saved-mx 0] [saved-my 0])    
    ; Controller's cache for storing particle's velocity
    (init-field [particle-vx 0] [particle-vy 0])
    ; whether handler is selected or controller is selected
    (init-field [handler-selected? #false] [controller-selected? #false])
    ; factor by which velocity or position is incremented/decremented
    (field [update-factor 5])
    
    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; returns a scene depending upon the implementation of this
    ;; function in the sub classes.
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (abstract data-image)

    ;; receive-signal : Signal -> Void
    ;; EFFECT: decodes signal and updates particle's (x,y) or velocity values
    ;; STRATEGY: Divide cases on sig
    (define/public (receive-signal sig)
      (cond
        [(report-position? sig)
         (set! particle-x (report-position-x sig))
         (set! particle-y (report-position-y sig))]
        [(report-velocity? sig)
         (set! particle-vx (report-velocity-vx sig))
         (set! particle-vy (report-velocity-vy sig))]))

    ;; after-button-down : Integer Integer -> Void
    ;; EFFECT: Controller's state after button down
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-button-down mx my)
      'trap)

    ;; after-button-up : Integer Integer -> Void
    ;; EFFECT: Controller's state after button up
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-button-up mx my)
      'trap)

    ;; after-drag : Integer Integer -> Void
    ;; EFFECT: Controller's state after after drag
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-drag mx my)
      'trap)

    ;; add-to-scene : Scene -> Scene
    ;; EFFECT: Creates visual representation of this controller
    ;; DETAILS: Implementation is left to sub class
    (define/public (add-to-scene scene)
      (place-image (viewer-image) x y scene))
     

    ;; after-key-event: KeyEvent -> Void
    ;; EFFECT: Controller's state after key event
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-key-event kev) 'trap)
    
    ;; after-move: Integer Integer -> Void
    ;; EFFECT: Controller's state after move
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-move mx my) 'trap)
    
    ;; after-tick: -> Void
    ;; EFFECT: Controller's state after tick
    ;; DETAILS: Implementation is left to sub class
    (define/public (after-tick) 'trap)

    ;; in-this?: Real Real -> Boolean
    ;; GIVEN: Coordinates of a point
    ;; RETURNS: True iff given coordinate is in this controller
    ;; STRATEGY: Combine simple functions
    (define/public (in-this? other-x other-y)
      (and
       (<= (- x half-width) other-x (+ x half-width))
       (<= (- y half-height) other-y (+ y half-height))))
    
    ;; controller-color: -> Color
    ;; GIVEN: No arguments
    ;; RETURNS: red if particle is selected otherwise black
    ;; STRATEGY: Cases on controller-selected?
    (define/public (controller-color)
      (if controller-selected? RED BLACK))
    
    ;; handler-color: -> Color
    ;; GIVEN: No arguments
    ;; RETURNS: red if handler is selected otherwise black
    ;; STRATEGY: Cases on handler-selected?
    (define/public (handler-color)
      (if handler-selected? RED BLACK))
    
    ;; viewer-image: -> Image
    ;; GIVEN: No arguments
    ;; RETURNS: returns an image having the handler, controller
    ;; and the data to represented in the controllor
    ;; STRATEGY: Combine simpler functions
    (define/public (viewer-image)
      (let ((the-data-image (data-image))
            (the-handler (rect-image HANDLER-HEIGHT HANDLER-WIDTH
                                     (handler-color)))
            (the-canvas (rect-image width height BLACK)))
        (overlay 
         the-data-image
         (overlay/align
          "left" "top"
          the-handler
          the-canvas))))    
    
    ;; rect-image: NonNegInt NonNegInt Color -> Image
    ;; GIVEN: width, height and a color
    ;; RETURNS: returns an image of a rectangle having the
    ;; given width,height and color in an outline mode.
    ;; STRATEGY: Combine simpler functions
    (define/public (rect-image w h color)
      (rectangle w h OUTLINE color))

    ;; in-draggable-section? : Real Real -> Boolean
    ;; RETURNS: True iff given coordinate is in drag handler
    ;; STRATEGY: Combine simpler functions
    (define/public (in-draggable-section? other-x other-y)
      (and
       (<= (- x half-width)
           other-x
           (+ (- x half-width) HANDLER-WIDTH))
       (<= (- y half-height)
           other-y
           (+ (- y half-height) HANDLER-HEIGHT))))
    
    ))

;; ControllerHooks<%> has methods which are abstract in Controller%
;; Each class which inherits from Controller% will implement this interface and
;; provide it's implementation for given methods
(define ControllerHooks<%>
  (interface ()

    ;; data-image: -> Image
    ;; RETURNS: A image depending upon implementation of subclass
    data-image

    ))