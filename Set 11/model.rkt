#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=150 and y=0 to y=100. It accepts commands and reports when its
;; status changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require rackunit)
(require 2htdp/image)
(require "extras.rkt")
(require "Interfaces.rkt")
(require "PerfectBounce.rkt")
(require "Controllertest.rkt")


(provide make-model)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

;; ListOfX
;; Here X can be of any data type

;; ListOfX(LOX) is either
;; -- empty
;; -- (cons X LOX)

;; TEMPLATE:
;; lox-fn : LOX -> ??
;; (define (lox-fn lox)
;;   (cond
;;     [(empty? lox)...]
;;     [ else (...
;;               (x-fn (first lox))
;;               (lox-fn (rest lox)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make-model: -> model
;; GIVEN: No arguments
;; RETURNS: An object representing particle.
;; EXAMPLES: See test cases.

(define (make-model)
  (new Model%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a Model is a (new Model% [x Real] [y Real]
;;                          [vx Real] [vy Real]
;;                          [selected? Boolean] [controllers ListOfController]
;; x,y are coordinates of center of the particle
;; vx,vy are velocity of the particle
;; selected? tells whether model is selected(being dragged) or not
;; controllers is ListOfController who have registered to model

;; all fields are optional

(define Model%
  (class* object% (Model<%>)

    ;; boundaries of the particle:
    (field [lo-x 0] [hi-x XY-CONTROLLER-WIDTH])
    (field [lo-y 0] [hi-y XY-CONTROLLER-HEIGHT])
    
    ;; position and velocity of the object
    ;; the default position value is the center of the rectangle.
    ;; the default velocity is 0.
    (init-field [x (/ (+ lo-x hi-x) 2)])
    (init-field [y (/ (+ lo-y hi-y) 2)])
    (init-field [vx 0])
    (init-field [vy 0])

    ;;represents if the controller is selected or not
    (init-field [selected? #f])
    
    ; ListOfController.  The list of registered controllers
    (init-field [controllers empty])    

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

    (super-new)
    
    ;; after-tick -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: After each tick, this model updates the center
    ;;         of the particle and also the velocities and publishes the
    ;;         updated centers and velocities to each of its subscribed
    ;;         controllers.
    ;; STRATEGY: Cases on selected?
    ;; EXAMPLES: See test-cases
    
    (define/public (after-tick)
      (if selected?
          (begin
            (publish-position)
            (publish-velocity))
          (begin
            (particle-after)
            (publish-position)
            (publish-velocity))))    
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    ;; register: Controller -> Void
    ;; GIVEN: a controller
    ;; EFFECT: register the new controller and sends the particles position,
    ;; velocity and selected data.
    ;; STRATEGY: Combine simpler functions
    ;; EXAMPLES: See test-cases
    
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal (make-report-position x y))
        (send c receive-signal (make-report-velocity vx vy))
        (send c receive-signal (make-report-selected selected?))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; execute-command: Command -> Void
    ;; GIVEN: a command
    ;; EFFECT: decodes the command, executes it, and sends updates to the
    ;; controllers.
    ;; STRATEGY: Divide into cases on cmd
    ;; EXAMPLE: See tests
    
    (define/public (execute-command cmd)
      (cond
        [(set-position? cmd)
         (begin
           (set! x (validate-x x (set-position-x cmd)))
           (set! y (validate-y y (set-position-y cmd)))
           (publish-position))]
        [(set-x-position? cmd)
         (begin
           (set! x (validate-x x (set-x-position-x cmd)))
           (publish-position))]
        [(set-y-position? cmd)
         (begin
           (set! y (validate-y y (set-y-position-y cmd)))
           (publish-position))]
        [(set-velocity? cmd)
         (begin
           (set! vx (+ vx (set-velocity-vx cmd)))
           (set! vy (+ vy (set-velocity-vy cmd)))
           (publish-velocity))]
        [(set-selected? cmd)
         (begin
           (set! selected? (set-selected-selected? cmd)))]))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-down: Integer Integer -> Void
    ;; GIVEN: X and Y mouse-coordinates
    ;; EFFECT: No effect
    ;; EXAMPLE: See tests
    (define/public (after-button-down mx my) 'trap)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-button-up: Integer Integer -> Void
    ;; GIVEN: X and Y mouse-coordinates
    ;; EFFECT: No effect
    ;; EXAMPLE: See tests
    
    (define/public (after-button-up mx my) 'trap)
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-drag: Integer Integer -> Void
    ;; GIVEN: X and Y mouse-coordinates
    ;; EFFECT: No effect
    ;; EXAMPLE: See tests
    
    (define/public (after-drag mx my) 'trap)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-move: Integer Integer -> Void
    ;; GIVEN: X and Y mouse-coordinates
    ;; EFFECT: No effect
    ;; EXAMPLE: See tests

    (define/public (after-move mx my) 'trap)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; after-key-event: Kev -> Void
    ;; GIVEN: KeyEvent
    ;; EFFECT: No effect
    ;; EXAMPLE: See tests
    
    (define/public (after-key-event kev) 'trap)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: A Scene
    ;; RETURNS: A Scene
    ;; STRATEGY: Use for-each on controllers
    ;; EXAMPLE: See tests
    
    (define/public (add-to-scene s) s)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; publish-position: -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Publishes coordinates of particle to subscribed controllers
    ;; STRATEGY: Use for-each on controllers
    ;; EXAMPLE: See tests
    
    (define (publish-position)
      (let ((msg (make-report-position x y)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers)))
    
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; publish-velocity: -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: Publishes current velocity to subscribed controllers
    ;; STRATEGY: Use for-each on controllers
    ;; EXAMPLE: See tests
    
    (define (publish-velocity)
      (let ((msg (make-report-velocity vx vy)))
        (for-each
         (lambda (obs) (send obs receive-signal msg))
         controllers)))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; particle-after -> Void
    ;; GIVEN: No arguments
    ;; EFFECT: After each tick, this model updates the center
    ;;         of the particle and also the velocities.
    ;; EXAMPLES: See test-cases
    
    (define (particle-after)
      (let ([pt (particle-after-tick (make-particle x y vx vy)
                                     (make-rect lo-x hi-x lo-y hi-y))])
        (set! x (particle-x pt))
        (set! y (particle-y pt))
        (set! vx (particle-vx pt))
        (set! vy (particle-vy pt))))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; validate-x: Real Real -> Real
    ;; GIVEN: the x-coordinate of the model and the new x-coordinate from the
    ;; controller
    ;; RETURNS: the x-coordinate
    ;; EFFECT: If the new x-position is equal to the min-boundary of the 
    ;; canvas and the velocity is negative then velocity is reversed. If the 
    ;; new x-position is equal to the max-boundary of the canvas and the 
    ;; velocity is positive then velocity is reversed.
    ;; STRATEGY: Divide on cass on the new x-coordinate fetched from the
    ;; controller
    ;; EXAMPLE: See tests
    
    (define (validate-x oldX newX)
      (cond
        [(or (< newX lo-x) (> newX hi-x)) oldX]
        [(or (and (= newX lo-x) (< vx 0))
             (and (= newX hi-x) (> vx 0)))
         (begin
           (set! vx (- vx))
           (publish-velocity)
           newX)]
        [else newX]))

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; validate-y: Real Real -> Real
    ;; GIVEN: the y-coordinate of the model and the new Y-coordinate from the
    ;; controller
    ;; RETURNS: the y-coordinate
    ;; EFFECT: If the new y-position is equal to the min-boundary of the 
    ;; canvas and the velocity is negative then velocity is reversed. If the 
    ;; new y-position is equal to the max-boundary of the canvas and the 
    ;; velocity is positive then velocity is reversed.
    ;; STRATEGY: Divide on cass on the new y-coordinate fetched from the
    ;; controller
    ;; EXAMPLE: See tests
    
    (define (validate-y oldY newY)
      (cond
        [(or (< newY lo-y) (> newY hi-y)) oldY]
        [(or (and (= newY lo-y) (< vy 0))
             (and (= newY hi-y) (> vy 0)))
         (begin
           (set! vy (- vy))
           (publish-velocity)
            newY)]        
        [else newY]))
  
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests
(begin-for-test
  (local
    ((define m (make-model))
     (define p (new Controllertest% [model m]))
     (define scene (empty-scene 500 600)))
    (check-equal? (length (get-field controllers m)) 1
                  "The length of the controllers is 1")
    (send m after-tick)
    (check-equal? (get-field x m) 75
                  "The x-corodinate is 75")
    (check-equal? (get-field y m) 50
                  "The y-corodinate is 50")
    (check-equal? (get-field selected? m) #f
                  "The selected field is false")
    (send m execute-command (make-set-position 10 20))
    (check-equal? (get-field x m) 10
                  "The x-corodinate is 75")
    (check-equal? (get-field y m) 20
                  "The y-corodinate is 20")
    (send m execute-command (make-set-selected #t))
    (check-equal? (get-field selected? m) #t
                  "The selected field is false")
    (send m after-tick)
    (check-equal? (get-field x m) 10
                  "The x-corodinate is 10")
    (check-equal? (get-field y m) 20
                  "The y-corodinate is 20")
    (send m execute-command (make-set-selected #f))
    (send m execute-command (make-set-velocity -5 -10))
    (check-equal? (get-field vx m) -5
                  "The vx-velocity is -5")
    (send m after-tick)
    (send m after-tick)
    (check-equal? (get-field x m) 0
                  "The x-corodinate is 0")
    (check-equal? (get-field y m) 0
                  "The y-corodinate is 0")
    (check-equal? (get-field vx m) 5
                  "The vx-velocity is 5")
    (check-equal? (get-field vy m) 10
                  "The vy-velocity is 10")
    (send m execute-command (make-set-velocity -10 10))
    (check-equal? (get-field vx m) -5
                  "The vx-velocity is -5")
    (send m execute-command (make-set-x-position 5))
    (send m execute-command (make-set-y-position 20))
    (send m execute-command (make-set-x-position 0))
    (check-equal? (get-field vx m) 5
                  "The vx-velocity is 5")
    (send m execute-command (make-set-x-position 150))
    (check-equal? (get-field vx m) -5
                  "The vx-velocity is -5")
    (check-equal? (get-field vy m) 20
                  "The vy-velocity is -5")
    (send m execute-command (make-set-velocity 0 -25))
    (check-equal? (get-field vy m) -5
                  "The vy-velocity is -5")
    (send m execute-command (make-set-y-position 0))
    (check-equal? (get-field vy m) 5
                  "The vy-velocity is -5")
    (send m execute-command (make-set-y-position 100))
    (check-equal? (get-field vy m) -5
                  "The vy-velocity is -5")
    (send m execute-command (make-set-x-position -1))
    (check-equal? (get-field x m) 150
                  "The x-coordinate is 150")
    (send m execute-command (make-set-y-position -1))
    (check-equal? (get-field y m) 100
                  "The y-coordinate is 100")
    (send m after-button-down 150 100)
    (check-equal? (get-field selected? m) #f
                  "The selected field is false")
    (send m after-drag 170 170)
    (check-equal? (get-field x m) 150
                  "The x-coordinate is 150")
    (send m after-button-up 170 170)
    (send m after-move 190 190)
    (check-equal? (get-field x m) 150
                  "The x-coordinate is 150")
    (send m after-key-event "p")
    (check-equal? (length (get-field controllers m)) 1
                  "The length of the controllers is 1")
    (check-equal? (send m add-to-scene scene) scene
                  "The same scene is represented")
    )) 