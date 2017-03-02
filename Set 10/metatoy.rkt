#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(require "politician.rkt")
(require "throbber.rkt")
(require "clock.rkt")
(require "interfaces.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-metatoy
         Metatoy%)

;;make-metatoy : ListOfToys -> Metatoy
;;RETURNS: a Metatoy with the given list of toys.
;;NOTE: The Metatoy<%> interface extends the World<%> interface, so the
;;result of make-metatoy is something that big-bang can use as a world.
;;EXAMPLES: See test cases

(define (make-metatoy sobjs)
  (new Metatoy% [sobjs sobjs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Metatoy% class

;; Constructor template for Metatoy%:
;; (new Metatoy% [sobjs ListOfToys])
;; Interpretation: An object of class Metatoy% creates SWidget<%> and adds
;; them to list of toys it has

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    ; the list to which the metatoy adds toys
    (init-field sobjs)  
    
    (super-new)
    
    ; Constants for key and mouse events
    (field [NEW-THROBBER-EVENT "t"] [NEW-CLOCK-EVENT "c"]
           [NEW-POLITICIAN-EVENT "p"] [MOUSE-DOWN "button-down"]
           [MOUSE-UP "button-up"] [MOUSE-DRAG "drag"]
           [MOUSE-MOVE "move"])
    
    ;; --------------------------------------------------------------------
    ;; Interface required functions::
    ;; --------------------------------------------------------------------
    
    ;; after-tick: -> Void
    ;; STRATEGY:Use HOF for-each on sobjs
    ;; EFFECT: Passes this event to list of toys it has
    ;; EXAMPLE:See tests below
    (define/public (after-tick)
      (for-each
       (lambda (sobj) (send sobj after-tick))
       sobjs))
    
    
    ;; after-button-down: Int Int -> Void
    ;; EFFECT: Passes this event to list of toys it has
    ;; STRATEGY: Use HOF for-each on sobjs
    ;; EXAMPLE:See tests below
    (define/public (after-button-down mx my)
      (for-each
       (lambda (sobj) (send sobj after-button-down mx my))
       sobjs))
    
    ;; after-button-up: Int Int -> Void
    ;; EFFECT: Passes this event to list of toys it has
    ;; STRATEGY: Use HOF for-each on sobjs
    ;; EXAMPLE:See tests below
    (define/public (after-button-up mx my)
      (for-each
       (lambda (sobj) (send sobj after-button-up mx my))
       sobjs))
    
    
    ;; after-drag: Int Int -> Void
    ;; EFFECT: Passes this event to list of toys it has
    ;; STRATEGY: Use HOF for-each on sobjs
    ;; EXAMPLE:See tests below
    (define/public (after-drag mx my)
      (for-each
       (lambda (sobj) (send sobj after-drag mx my))
       sobjs))
    
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN:A scene
    ;; RETURNS:A world scene after adding all sobjs
    ;; STRATEGY: Use foldr HOF on sobjs
    ;; EXAMPLE:See tests below
    (define/public (add-to-scene s)
      (foldr
       (lambda (sobj scene)
         (send sobj add-to-scene scene))
       s
       sobjs))
    
    
    ;; after-move: Int Int -> Void
    ;; EFFECT: Passes this event to list of toys it has
    ;; EXAMPLE:See tests below
    (define/public (after-move mx my)
      (for-each
       (lambda (sobj) (send sobj after-move mx my))
       sobjs))
    
    
    ;; after-key-event KeyEvent -> Void
    ;; GIVEN: A keyEvent
    ;; EFFECT: Creates a stateful widget and adds it to sobjs list 
    ;; STRATEGY: Uses cases on key event
    ;; EXAMPLE:See tests below
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (set! sobjs (cons
                      (make-throbber (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
                      sobjs))]
        
        [(key=? kev NEW-CLOCK-EVENT)
         (set! sobjs (cons
                      (make-clock (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
                      sobjs))]
        
        [(key=? kev NEW-POLITICIAN-EVENT)
         (set! sobjs (cons
                      (make-politician (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2))
                      sobjs))]
        [else this]))
    
    ;; get-toys : -> ListOfToy
    ;; GIVEN: no argument 
    ;; RETURNS: a list of toys.
    ;; EXAMPLE:See tests below
    (define/public (get-toys)
      sobjs)
    
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tests

(begin-for-test
  (local
    ((define M0 (make-metatoy empty)))
    (send M0 after-key-event "t")
    (check-equal? (send(first(send M0 get-toys)) toy-x) 250
                  "Toy-x is 250")
    (send M0 after-key-event "c")
    (check-equal? (send(first(send M0 get-toys)) toy-x) 250
                  "Toy-x is 250")
    (send M0 after-key-event "p")
    (check-equal? (send(first(send M0 get-toys)) toy-x) 250
                  "Toy-x is 250")
    (send M0 after-key-event "f")
    (check-equal? (send(first(send M0 get-toys)) toy-x) 250
                  "Toy-x is 250")
    (send M0 after-tick)
    (send M0 after-tick)
    (check-equal? (send(first(send M0 get-toys)) toy-y) 229
                  "Toy-y is 229")
    (send M0 after-button-down 250 300)
    (check-equal? (get-field selected? (third(send M0 get-toys))) #t
                  "selected is true")
    (send M0 after-drag 260 310)
    (check-equal? (send (third(send M0 get-toys)) toy-x) 260
                  "Toy-x is 260")
    (send M0 after-button-up 260 310)
    (check-equal? (get-field selected? (third(send M0 get-toys))) #f
                  "Selected is false")
    (send M0 after-move 270 320)
    (check-equal? (send (third(send M0 get-toys)) toy-x) 260
                  "Toy-x is 260")
    ))

(begin-for-test
  (local
    ((define M1 (make-metatoy empty))
     (define TEXT (place-image (text "0" 10 "Red") 250 300 EMPTY-CANVAS))
     (define CLOCK (place-image
                    (rectangle 60 40 OUTLINE-CIRCLE "blue") 250 300 TEXT)))
    (send M1 after-key-event "c")
    (check-equal? (send M1 add-to-scene EMPTY-CANVAS)
                  CLOCK
                  "Image on empty-canvas")))
