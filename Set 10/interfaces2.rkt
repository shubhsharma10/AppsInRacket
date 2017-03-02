#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")

(provide SBlock<%>
         CANVAS-WIDTH
         CANVAS-HEIGHT)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:

(define CANVAS-WIDTH 600)
(define CANVAS-HEIGHT 500)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACES:


;; A SBlock% is an object of any class that implements SBlock<%>
;; It inherits from SWidget<%> provided in WidgetWorks.rkt

(define SBlock<%> 
  (interface
      
      ;; The interface SBlock<%> inherits from the interface SWidget<%>.
      ;; This means that any class that implements SBlock<%> must implement
      ;; all the methods from SWidget<%> plus all the methods defined here.
      (SWidget<%>)
    
    ;; get-team : -> ListOfSBlock<%>
    ;; RETURNS: the teammates of this sblock
    get-team
    
    ;; add-teammate: SBlock -> Void
    ;; EFFECT: adds the given sblock to this block's team
    add-teammate

    ;; add-to-all-cubes SBlock -> Void
    ;; EFFECT: adds the given sblock to all cubes list
    add-to-all-cubes
    
    ;; sblock-x : -> Integer
    ;; sblock-y : -> Integer
    ;; RETURNS: the x or y coordinates of this sblock
    sblock-x
    sblock-y

    ;; Int Int -> Void
    ;; EFFECT: Updates (x,y) coordinate in order to move it along with leader
    drag-teammate

    ;; Int Int -> Void
    ;; EFFECT: Store delta wrt to leader's (x,y) coordinate in saved-mx,saved-my
    update-delta
    ))