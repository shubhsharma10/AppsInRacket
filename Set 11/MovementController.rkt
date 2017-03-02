#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require "Interfaces.rkt")
(require "Utility.rkt")
(require "Controller.rkt")

;; Constants
(define MOVEMENT-CONTROLLER-WIDTH 180)
(define MOVEMENT-CONTROLLER-HEIGHT 70)

;; MovementController.rkt
;;
;; This inherits Controller% class and controllers which changes particle
;; by keyboard inherits this class
;; This implements all the common functionalities on the controllers which
;; allows changing particle by keyboard
;; which are:
;; -- PositionController
;; -- VelocityController
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide MovementController%
         MovementControllerHooks<%>)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; a MovementController% is a (new MovementController% [model Model<%>]
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

;; Only model field is passed while creating object of this class, rest
;; fields are optional

;; MovementController% class implements methods provided in
;; MovementController<%> and ControllerHooks<%> interfaces.
;; This implements all the common functionalities of controllers who display
;; particle, which are:
;; -- PositionController
;; -- VelocityController
(define MovementController%
  (class* Controller%

    ;; Interface whose methods are implemented
    (MovementController<%>  ControllerHooks<%>)    

    ;; model to which this controller belongs
    (inherit-field model)
    ;; initial values of x, y (center of controller)
    (inherit-field x y)
    ;; Width and height of the controller, this will be set by each controller
    ;; sub-class
    (inherit-field width height)
    ;; Controller's cache for storing particle's (x,y) coordinate
    (inherit-field particle-x particle-y)
    ;; Half width and height of controller
    (inherit-field half-width half-height)
    ;; Fields to store delta when controller is being dragged
    (inherit-field saved-mx saved-my)
    ;; Controller's cache for storing particle's velocity
    (inherit-field particle-vx particle-vy)
    ;; factor by which velocity or position is incremented/decremented
    (inherit-field update-factor)
    ;; whether handler is selected or controller is selected
    (inherit-field handler-selected? controller-selected?)
    ;; Font size for text shown in controller
    (field [FONT-SIZE 12])
    
    (super-new [width MOVEMENT-CONTROLLER-WIDTH]
               [height MOVEMENT-CONTROLLER-HEIGHT])

    (abstract handle-arrow-up-key-event
              handle-arrow-down-key-event
              handle-arrow-right-key-event
              handle-arrow-left-key-event)

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
        (begin
          (if in-handler?
              (begin
                (set! handler-selected? true)
                (set! saved-mx (- mx x))
                (set! saved-my (- my y)))
              'trap)
          (if (and (send this in-this? mx my)
                   (not in-handler?))
              (set! controller-selected? true)
              (set! controller-selected? false)))))

    ; after-button-up : Integer Integer -> Void
    ; GIVEN: the (x,y) location of a button-up event
    ; EFFECT: makes controller or handler unselected    
    (define/override (after-button-up mx my)      
      (if (send this in-draggable-section? mx my)
          (set! handler-selected? false)
          'trap))
    
    ; after-drag : Integer Integer -> Void
    ; GIVEN: the location of a drag event
    ; STRATEGY: Cases on whether handler is selected.
    ; EFFECT: Moves controller if handler is selected
    (define/override (after-drag mx my)
      (if handler-selected?
          (begin
            (set! x (- mx saved-mx))
            (set! y (- my saved-my)))
          'trap))

    ;; after-key-event: KeyEvent -> Void
    ;; Given: A keyevent
    ;; EFFECT: Implementation for each key event is left upto subclass
    ;; STRATEGY: Divide into cases on kev
    ;; EXAMPLES: See test cases    
    (define/override (after-key-event kev)
      (if controller-selected?
          (cond
            [(key=? UP kev) (handle-arrow-up-key-event)]
            [(key=? DOWN kev) (handle-arrow-down-key-event)]
            [(key=? RIGHT kev) (handle-arrow-right-key-event)]
            [(key=? LEFT kev) (handle-arrow-left-key-event)])
          'trap))

    ;; get-position-text: -> Text
    ;; RETURNS: Text which has particle's position
    (define/public (get-position-text)
      (text (string-append
             "X = "
             (number->string (get-number-in-decimal particle-x 2))
             " Y = "
             (number->string (get-number-in-decimal particle-y 2)))
            FONT-SIZE
            (send this controller-color)))

    ;; get-velocity-text: -> Text
    ;; RETURNS: Text which has particle's velocity
    (define/public (get-velocity-text)  
      (text (string-append
             "VX = "
             (number->string particle-vx)
             " VY = "
             (number->string particle-vy))
            FONT-SIZE
            (send this controller-color)))

    ;; data-image: -> Image
    ;; RETURNS: Image as per subclass's requirement.
    ;; Implementation is left to sub class
    (define/override (data-image)
      'trap)

    ;; get-number-in-decimal: Real Integer -> Real
    ;; RETUNRS: Given number in decimal upto precision after decimal
    ;; STRATEGY: Combine simpler functions
    (define (get-number-in-decimal number precision)
      (local ((define p (expt 10 precision))
              (define n (* (exact->inexact number) p)))
        (/ (round n) p)))        
    
    ))

;; the abstract methods of MovementController
(define MovementControllerHooks<%>
  (interface ()

    ;; -> Void
    ;; EFFECT: Handles arrow up/down/left/right key event
    ;; Implementation is left to each subclass which inherits
    ;; MovementController% class
    handle-arrow-up-key-event
    handle-arrow-down-key-event
    handle-arrow-right-key-event
    handle-arrow-left-key-event

    ))