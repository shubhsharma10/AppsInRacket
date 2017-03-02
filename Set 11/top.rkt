#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Program simulates a dimensionless particle bouncing in a
;; 150x100 rectangle.
;; For this system we have 5 viewer-controllers:
;; A position controller that uses the arrow keys to move the particle in the
;; x or y direction.
;; A velocity controller that uses the arrow keys to alter the velocity of the
;; particle in the x or y direction.
;; Both the position and velocity controllers display both the position and
;; velocity of the particle.
;; An XY controller, which shows a representation of the particle bouncing
;; in the rectangle.With this controller, the user can drag the particle
;; using the mouse.
;; Dragging the mouse causes the particle to follow the mouse pointer via a
;; Smooth Drag.
;; An X controller, which is like the XY controller, except that it displays
;; only the x coordinate of the particle's motion.Dragging the mouse in the X
;; controller alters the particle's position in the x direction.
;; A Y controller, which is like the X controller except that it works in the
;; y direction.

;; run with (run 0.5)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "Interfaces.rkt")
(require "model.rkt")
(require "ControllerFactory.rkt")
(require "Controller.rkt")
(require "DisplayController.rkt")
(require "MovementController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "XYController.rkt")
(require "PositionController.rkt")
(require "VelocityController.rkt")

(check-location "11" "WidgetWorks.rkt")
(check-location "11" "Interfaces.rkt")
(check-location "11" "Model.rkt")
(check-location "11" "ControllerFactory.rkt")
(check-location "11" "Controller.rkt")
(check-location "11" "DisplayController.rkt")
(check-location "11" "MovementController.rkt")
(check-location "11" "XController.rkt")
(check-location "11" "YController.rkt")
(check-location "11" "XYController.rkt")
(check-location "11" "PositionController.rkt")
(check-location "11" "VelocityController.rkt")

(provide run)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run : PosReal -> Void
;; GIVEN: a frame rate, in sec/tick
;; EFFECT: Creates and runs the MVC simulation with the given frame rate.

(define (run rate)
  (let (; Creates the container
        (c (container-init CANVAS-WIDTH CANVAS-HEIGHT))
        ; Create the model
        (m (make-model)))
    (begin
      ; adds model to container
      (send c add-stateful-widget m)
      ; create controller factory and adds it to container
      (send c add-stateful-widget (make-controller-factory c m))
      ; runs the simulation at given rate
      (send c run rate))))
