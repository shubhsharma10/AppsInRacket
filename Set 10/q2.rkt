#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces2.rkt")
(require "cubefactory.rkt")
(require "sblock.rkt")

(check-location "10" "WidgetWorks.rkt")
(check-location "10" "interfaces2.rkt")
(check-location "10" "sblock.rkt")
(check-location "10" "cubefactory.rkt")
(check-location "10" "q2.rkt")

(provide cubelets-init
         container-init
         SBlock<%>
         make-block
         run)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA-DEFINITION
;; World is an object of any class that implements Container<%>

;; SWidget is an object of any class that implements the SWidget<%>
;; interface.

;; SWidget is stateful.

;; SBlock is an object of any class that implements the SBlock<%>
;; interface.

;; SBlock is stateful.

;; Cf is an object of class Cubefactory that
;; implements SWidget<%>


;; A ListOfSBlock is either :
;; -- empty                
;; -- (cons SBlock ListOfSBlock) 

;; Template for ListOfSBlock
;;(define (sblock-fn los)
;;  (cond
;;    [(empty? los)...]
;;    [else
;;     (...(send (first (los))...)
;;         (sblock-fn (rest (los))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; cubelets-init : -> Container
;; GIVEN: no arguments
;; RETURNS: a Container, initially with no blocks, which when run, will
;; run in a 600x500 canvas and process the events in the description above.

(define (cubelets-init)
    ; create container
    (container-init CANVAS-WIDTH CANVAS-HEIGHT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; run: PosNum -> Container
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: runs the container with cubefactory in it
;; at the given frame rate.
(define (run rate)
  (local
    ((define world (cubelets-init))
     ; create cube factory 
     (define cf (make-cubefactory world)))
    (begin
      ;; put the factory in the container
      (send world add-stateful-widget cf)
      (send world run rate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS:

(begin-for-test
  (local
    ((define W (cubelets-init))) 
  (check-equal? (length(get-field sobjs W)) 0)
    ))
    