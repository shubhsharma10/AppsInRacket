#lang racket

;; Controllertest.rkt
;;
;; Only for testing Model.rkt

(require "Interfaces.rkt")
(require "MovementController.rkt")


(provide Controllertest%)

;; a Controllertest% is a (new Controllertest% [model Model<%>])

(define Controllertest%
  (class* MovementController% (MovementController<%> MovementControllerHooks<%>)

    ;; the model to which this Controllertest belongs to.
    ;; Inherited from MovementController class.
    (inherit-field model) 
    
    (super-new)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; registers this TestController with the model for sending
    
    (send model register this)

    ;; -> Void
    ;; EFFECT: Implementation is left upto each subclass
    (define/override (handle-arrow-up-key-event) 'trap)
    (define/override (handle-arrow-down-key-event) 'trap)
    (define/override (handle-arrow-right-key-event) 'trap)
    (define/override (handle-arrow-left-key-event) 'trap)
    
    ))

