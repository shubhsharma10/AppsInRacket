#lang racket

;; interfaces for MVC problem

(require "WidgetWorks.rkt")

(provide Controller<%>
         MovementController<%>
         Model<%>
         CANVAS-WIDTH
         CANVAS-HEIGHT
         XY-CONTROLLER-WIDTH
         XY-CONTROLLER-HEIGHT
         BLACK
         BLUE
         RED
         UP
         DOWN
         LEFT
         RIGHT
         HANDLER-HEIGHT
         HANDLER-WIDTH
         MARGIN
         SOLID
         OUTLINE)

;; structs for model command language
(provide 
 (struct-out set-position)
 (struct-out set-x-position)
 (struct-out set-y-position)
 (struct-out set-selected)
 (struct-out set-velocity)
 (struct-out report-position)
 (struct-out report-velocity)
 (struct-out report-selected))

;; CONSTANTS

;; CANVAS related constants
(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

;; Controller related constants
(define XY-CONTROLLER-WIDTH 150)
(define XY-CONTROLLER-HEIGHT 100)
(define MARGIN 50)

;; Arrow key related constants
(define UP "up")
(define DOWN "down")
(define RIGHT "right")
(define LEFT "left")

;; Color related constants
(define BLACK "black")
(define BLUE "blue")
(define RED "red")

;; Handler related constants
(define HANDLER-HEIGHT 10)
(define HANDLER-WIDTH 10)

;; Circle type related constants
(define SOLID "solid")
(define OUTLINE "outline")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Color is one of
;; -- BLACK
;; -- BLUE
;; -- RED
;; ITERPRETATION: Self-evident

;; TEMPLATE
;; color-fn : Color -> ??
#|
(define (color-fn input)
   (cond
     [(string=? input BLACK) ..]
     [(string=? input RED) ..]
     [(string=? input BLUE) ..]))
|#

;; A Controller is an object of any class that implements
;; Controller<%>

;; There will be several such classes, and there may be several
;; objects of each such class.
(define Controller<%>    
  (interface (SWidget<%>)
    
    ;; Signal -> Void
    ;; receive a signal from the model and adjust controller
    ;; accordingly 
    receive-signal

    ;; Real Real -> Boolean
    in-this?

    ;; -> Color
    controller-color
    handler-color

    ;; -> Image
    viewer-image

    ;; NonNegInt NonNegInt Color -> Image
    rect-image

    ;; Real Real -> Boolean
    in-draggable-section?    
    ))

;; A MovementController is an object of any class that implements
;; MovementController<%> interface which inherits Controller<%> and
;; implemented by all controllers who changes particle's position or velocity
(define MovementController<%>
  (interface (Controller<%>)

    ;; -> Text
    get-position-text
    get-velocity-text
    ))

;; A Model is an object of any class that implements Model<%>.  Models
;; will receive signals from the Container, so they must implement the
;; SWidget<%> interface in order to do so.
(define Model<%>
  (interface (SWidget<%>)
    
    ;; Controller -> Void
    ;; Registers the given controller to receive signal
    register          
    
    ;; Command -> Void
    ;; Executes the given command
    execute-command   
    ))

;; CONTROLLER/MODEL PROTOCOL:

;; As soon as a controller registers with the model, the model sends
;; the controller a pair of Signals so the controller will know the
;; current state of the model.

;; The controller then sends the model commands, which the model is
;; supposed to execute.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS FOR COMMUNICATING WITH MODEL

;; A Command is one of 
;; -- (make-set-position x y)
;; -- (make-set-x-position x)
;; -- (make-set-y-position y)
;; -- (make-set-velocity vx vy)
;; -- (make-set-selected selected?)

;; A Signal is one of
;; -- (make-report-position x y)
;; -- (make-report-velocity vx vy)
;; -- (make-report-selected selected?)

;; x,y,vx,vy are all Reals.

;; provide the structs for Command and Signal
;; the syntax of provide in #lang racket has more options in it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Set-Position is a (make-set-position x y)
;; where x,y  indicates new coordinates
;;
;; TEMPLATE:
;; set-position-fn : Set-Position -> ??
;; (define (set-position-fn ss)
;;   (...
;;     (set-position-x? ss)
;;     (set-position-y? ss)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct set-position (x y) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Set-X-Position is a (make-set-x-position x)
;; where x indicates new x coordinate
;;
;; TEMPLATE:
;; set-x-position-fn : Set-X-Position -> ??
;; (define (set-x-position-fn ss)
;;   (...
;;     (set-x-position-x? ss)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct set-x-position (x) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Set-Y-Position is a (make-set-y-position y)
;; where y indicates new y coordinate
;;
;; TEMPLATE:
;; set-y-position-fn : Set-Y-Position -> ??
;; (define (set-y-position-fn ss)
;;   (...
;;     (set-y-position-x? ss)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct set-y-position (y) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Set-Velocity is a (make-set-velocity vx vy)
;; where vx,vy  indicates delta to which velocity needs to be changed
;;
;; TEMPLATE:
;; set-velocity-fn : Set-Velocity -> ??
;; (define (set-velocity-fn ss)
;;   (...
;;     (set-velocity-vx? ss)
;;     (set-velocity-vy? ss)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct set-velocity (vx vy) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Set-Selected is a (make-set-selected selected?)
;; where selected? indicates whether an object is selected or not
;;
;; TEMPLATE:
;; set-selected-fn : Set-Selected -> ??
;; (define (set-selected-fn ss)
;;   (...
;;     (set-selected-selected? ss)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct set-selected (selected?) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Report-Position is a (make-report-position x y)
;; where x indicates the x co-ordinate and y indicates
;; the y co-ordinate of the center of the particle
;;
;; TEMPLATE:
;; report-position-fn : Report-Position -> ??
;; (define (report-position-fn rp)
;;   (...
;;     (report-position-x rp)
;;     (report-position-y rp)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct report-position (x y) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Report-Velocity is a (make-report-velocity vx vy)
;; where vx indicates the velocity along the x-direction and vy 
;; indicates the velocity along the y-direction of the particle
;;
;; TEMPLATE:
;; report-velocity-fn : Report-Velocity -> ??
;; (define (report-velocity-fn rv)
;;   (...
;;     (report-velocity-vx rv)
;;     (report-velocity-vy rv)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct report-velocity (vx vy) #:transparent)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interpretation
;; A Report-Selected is a (make-report-selected selected?)
;; where selected? indicates whether an object is selected or not
;;
;; TEMPLATE:
;; report-selected-fn : Report-Selected -> ??
;; (define (report-selected-fn rs)
;;   (...
;;     (report-selected-selected? rs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-struct report-selected (selected?) #:transparent)











