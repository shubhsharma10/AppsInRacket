#lang racket

(require "WidgetWorks.rkt")
(require "model.rkt")
(require "extras.rkt")
(require "Interfaces.rkt")
(require "XYController.rkt")
(require "XController.rkt")
(require "YController.rkt")
(require "PositionController.rkt")
(require "VelocityController.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

(provide make-controller-factory)

;; make-controller-factory : Container Model -> Controller
;; RETRUNS: Controller factory
(define (make-controller-factory c m)
  (new ControllerFactory% [c c][m m]))

;; Controller Factory adds the controllers to the world that send
;; and receive updates from the model
;; Contstructor Template for ControllerFactory%:
;;  (new ControllerFactory% [c c][m m]))
(define ControllerFactory%
  (class* object% (SWidget<%>)

    ; the container in which the controllers will live
    (init-field c)   ; Container

    ; the model to which the controllers will be connected
    (init-field m)   ; Model

    (super-new)

    ;; Keys on which controllers are created
    (field [CREATE-XY-CONTROLLER "z"])
    (field [CREATE-X-CONTROLLER "x"])
    (field [CREATE-Y-CONTROLLER "y"])
    (field [CREATE-POS-CONTROLLER "p"])
    (field [CREATE-VEL-CONTROLLER "v"])

    ;; after-key-event: KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: a new viewer is created based on the given key 
    ;;         event and is added to the list of stateful 
    ;;         widgets in the world.
    ;; STRATEGY: Divide into cases on key event kev
    (define/public (after-key-event kev)
      (cond
        [(key=? kev CREATE-XY-CONTROLLER)
         (add-viewer make-xycontroller)]
        [(key=? kev CREATE-X-CONTROLLER)
         (add-viewer make-xcontroller)]
        [(key=? kev CREATE-Y-CONTROLLER)
         (add-viewer make-ycontroller)]
        [(key=? kev CREATE-POS-CONTROLLER)
         (add-viewer make-position-controller)]
        [(key=? kev CREATE-VEL-CONTROLLER)
         (add-viewer make-velocity-controller)]))


    ;; add-viewer: Controller -> Void
    ;; GIVEN: a class
    ;; EFFECT: a new class is created based on the given class and is
    ;;         added to the list of stateful widgets in the world.
    ;; STRATEGY: Combine simpler functions
    (define/public (add-viewer make-viewer)
      (send c add-stateful-widget (make-viewer m)))

    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one which has all the 
    ;;          controllers painted on it.
    ;; DETAILS: Factory doesn't add anything to scene as it is invisible
    (define/public (add-to-scene s) s)

    ;; the factory doesn't respond to any other events
    ;; after-tick: -> Void
    (define/public (after-tick) 'trap)
    ;; after-button-down: Integer Integer -> Void
    (define/public (after-button-down mx my)'trap)
    ;; after-drag: Integer Integer -> Void
    (define/public (after-drag mx my)'trap)
    ;; after-move: Integer Integer -> Void
    (define/public (after-move mx my)'trap)
    ;; after-button-up: Integer Integer -> Void
    (define/public (after-button-up mx my)'trap)

    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS

(begin-for-test
  (local
    ((define m  (make-model))
     (define c  (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define cf (make-controller-factory c m)))
    (send cf after-button-down 10 10)
    (send cf after-key-event "p")
    (send cf after-key-event "v")
    (send cf after-key-event "x")
    (send cf after-key-event "y")
    (send cf after-key-event "z")
    (check-equal? (length(get-field sobjs c)) 5
                  "The length of the sobjs is 5")
    (check-equal? (length(get-field controllers m)) 5
                  "The length of the controller is 5")
    (check-equal? (send cf add-to-scene (empty-scene 600 500))
                  (empty-scene 600 500)
                  "The scene is returned")
    (send cf after-drag 100 100)
    (send cf after-button-up 100 100)
    (send cf after-move 120 120) 
    (send cf after-tick)
    ))



