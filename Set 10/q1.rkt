#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "interfaces.rkt")
(require "metatoy.rkt")
(require "clock.rkt")
(require "throbber.rkt")
(require "politician.rkt")

(check-location "10" "WidgetWorks.rkt")
(check-location "10" "interfaces.rkt")
(check-location "10" "politician.rkt")
(check-location "10" "clock.rkt")
(check-location "10" "throbber.rkt")
(check-location "10" "metatoy.rkt")
(check-location "10" "q1.rkt")


(provide run
         make-metatoy
         make-clock
         make-throbber
         make-politician
         container-init
         Metatoy<%>
         Toy<%>)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITION

;; A Toy is an object of any class that implements <Toy%> which inherits
;; SWidget<%>, this is also an stateful widget because of SWidget

;; A Metatoy is an object of Metatoy<%> interface which inherits SWidget<%>
;; This is also a stateful object.

;; A ListOfX is either :
;; -- empty                
;; -- (cons X ListOfX)

;; Here X can be of any data type

;; Template for ListOfX
;;(define (lox-fn lox)
;;  (cond
;;    [(empty? lox)...]
;;    [else
;;     (...(send (first (lox))...)
;;         (lox-fn (rest (lox))))]))

;; run : PosNum -> Void
;; GIVEN: a frame rate (in seconds/tick)
;; EFFECT: creates a MetaToy with no toys in it, and runs it using WidgetWorks
;; at the given frame rate.
(define (run rate)
  (local
    (;; creates the container
     (define world (container-init CANVAS-WIDTH CANVAS-HEIGHT))   
     ;; creates the metatoy
     (define mt
       (make-metatoy empty)))
    (begin
      ;; put the metatoy in the container
      (send world add-stateful-widget mt)
      ;; Run the container at given speed
      (send world run rate))))
