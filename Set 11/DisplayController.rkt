#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "Controller.rkt")

;; DisplayController.rkt
;;
;; This inherits Controller% class and controllers which display particle
;; inherits this class
;; This implements all the common functionalities on the controllers which
;; display particle
;; which are:
;; -- XController
;; -- YController
;; -- XYController
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide DisplayController%
         DisplayControllerHooks<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a DisplayController% is a (new DisplayController% [model Model<%>]
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

;; DisplayController% class implements methods provided in Controller<%>
;; and ControllerHooks<%> interfaces.
;; This implements all the common functionalities of controllers who display
;; particle, which are:
;; -- XController
;; -- YController
;; -- XYController
(define DisplayController%
  (class* Controller%
    ; Interfaces whose methods are implemented
    (Controller<%> ControllerHooks<%>)   

    ;; model to which this controller belongs
    (inherit-field model)    
    ;; initial values of x, y (center of controller)
    (inherit-field x y)
    ; Width and height of the controller, this will be set by each controller
    ; sub-class
    (inherit-field width height)
    ; Controller's cache for storing particle's (x,y) coordinate
    (inherit-field particle-x particle-y)
    ; Half width and height of controller
    (inherit-field half-width half-height)
    ; Fields to store delta when controller is being dragged
    (inherit-field saved-mx saved-my)
    ; Controller's cache for storing particle's velocity
    (inherit-field particle-vx particle-vy)
    ; whether handler is selected or controller is selected
    (inherit-field controller-selected? handler-selected?)
    ;; Fields to store delta when particle is being dragged
    (init-field [particle-saved-mx 0] [particle-saved-my 0])
    ;; Fields which have radius of inner and outer circle which
    ;; is used to represent particle
    (field [INNER-CIRCLE-RADIUS 2] [OUTER-CIRCLE-RADIUS 10])
    
    (super-new)

    ;; Each subclass will implement it's own implementation for what
    ;; to do when particle is dragged
    (abstract handle-controller-selection)

    ; after-button-down : Integer Integer -> Void
    ; GIVEN: the location of a button-down event
    ; EFFECT: Sets handler selected and stores delta if in draggable
    ; section of controller. Sets particle selected and stores delta
    ; if it is in controller except handler
    ; STRATEGY: Cases on whether the event is in draggable section or
    ; not but inside controller
    (define/override (after-button-down mx my)
      (local
        ((define in-handler? (send this in-draggable-section? mx my)))
        (cond
          [in-handler?
           (begin
             (set! handler-selected? #t)
             (set! saved-mx (- mx x))
             (set! saved-my (- my y))) ]
          [(and (send this in-this? mx my) (not in-handler?))
           (begin
             (set! controller-selected? #t)
             (set! particle-saved-mx (- mx particle-x))
             (set! particle-saved-my (- my particle-y))
             (send model execute-command
                   (make-set-selected controller-selected?)))]
          [else 'trap])))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes controller or handler unselected
    (define/override (after-button-up mx my)
      (begin
        (set! handler-selected? #f)
        (set! controller-selected? #f)
        (send model execute-command
              (make-set-selected
               controller-selected?))))

    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether handler or controller is selected.
    ; EFFECT: Moves controller if handler is selected else if controller
    ; is selected then move particle inside controller
    (define/override (after-drag mx my)
      (cond
        [handler-selected?
         (begin
           (set! x (- mx saved-mx))
           (set! y (- my saved-my)))]
        [controller-selected?
         (handle-controller-selection mx my)]
        [else 'trap]))    

    ;; data-image : -> Image
    ;; RETURNS: Image which represents particle
    (define/override (data-image)
      (overlay
       (circle INNER-CIRCLE-RADIUS SOLID BLACK)
       (circle OUTER-CIRCLE-RADIUS SOLID RED)))
    
    ))


;; DisplayControllerHooks<%> has methods which are abstract in
;; DisplayController%
;; Each class which inherits from DisplayController% will implement this
;; interface and provide it's implementation for given methods
(define DisplayControllerHooks<%>
  (interface ()

    ;; handle-controller-selection : Real Real -> Void
    ;; GIVEN: given coordinate of mouse
    ;; EFFECT: Drags the particle according to which subclass implements it.
    handle-controller-selection

    ))

