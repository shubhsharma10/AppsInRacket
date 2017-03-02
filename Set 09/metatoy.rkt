#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces.rkt")
(require "politician.rkt")
(require "clock.rkt")
(require "throbber.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide make-metatoy
         new-metatoy-state
         Metatoy%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:

(define NEW-THROBBER-EVENT "t")
(define NEW-CLOCK-EVENT "c")
(define NEW-POLITICIAN-EVENT "p")
(define MOUSE-DOWN "button-down")
(define MOUSE-UP "button-up")
(define MOUSE-DRAG "drag")
(define MOUSE-MOVE "move")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;; make-metatoy : ListOfToy -> Metatoy
;; GIVEN: a list of toys
;; RETURNS: an object of class Metatoy% containing the given list of
;; toys.
;; STRATEGY: combine simpler functions to make a new metatoy object.
;; EXAMPLES: See test cases

(define (make-metatoy objs)
  (new-metatoy-state objs 0))

;; new-metatoy-state : ListOfToy PosInt -> Metatoy
;; GIVEN: a list of toys, time
;; RETURNS: an object of class Metatoy% containing list of toys.
;; STRATEGY: combine simpler functions to make a new metatoy object.
;; EXAMPLES: See test cases

(define (new-metatoy-state objs t)
  (new Metatoy% [objs objs][t t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Metatoy% class

;; Constructor template for Metatoy%:
;; (new Metatoy% [objs ListOfToy][t Time])
;; Interpretation: An object of class Metatoy% takes signals from
;; big-bang and distributes them to its objects as appropriate.

(define Metatoy%
  (class* object% (Metatoy<%>)
    
    (init-field objs) ;INTERP: ListOfToy
    (init-field t)    ;INTERP: Time
    
    (super-new)
    
    ;; get-toys : -> ListOfToy
    ;; GIVEN: no argument 
    ;; RETURNS: a list of toys.
    
    (define/public (get-toys)
      objs)
    
    ;; after-tick : -> World
    ;; GIVEN: no argument
    ;; RETURNS: a World which is a metatoy object at the next-tick.
    ;; STRATEGY: Use HOF map on the toys in the Metatoy.
    ;; EXAMPLES: See test cases
    
    (define/public (after-tick)
      (new-metatoy-state
       (map
        (lambda (obj) (send obj after-tick))
        objs)
       (+ 1 t)))
    
    ;; to-scene : -> World
    ;; GIVEN: no argument
    ;; RETURNS: a scene that depicts this World containing metatoy object.
    ;; STRATEGY: Use HOF foldr on the toys in the Metatoy.
    ;; EXAMPLES: See test cases
    
    (define/public (to-scene)
      (foldr
       (lambda (obj scene)
         (send obj add-to-scene scene))
       EMPTY-CANVAS
       objs))
    
    
    ;; after-key-event : KeyEvent -> World
    ;; GIVEN: key
    ;; RETURNS: the state of the world that contains metatoy object
    ;; should follow the given key event
    ;; STRATEGY: Cases on kev
    ;; EXAMPLES: See test cases
    
    (define/public (after-key-event kev)
      (cond
        [(key=? kev NEW-THROBBER-EVENT)
         (new-metatoy-state
          (cons (make-throbber (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)) objs)
          t)]
        [(key=? kev NEW-CLOCK-EVENT)
         (new-metatoy-state
          (cons (make-clock (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2) ) objs)
          t)]
        [(key=? kev NEW-POLITICIAN-EVENT)
         (new-metatoy-state
          (cons (make-politician (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)) objs)
          t)]
        [else
         (new-metatoy-state
          (map
           (lambda (obj) (send obj after-key-event kev))
           objs)
          t)]))
    
    
    ;; after-mouse-event : Int Int MouseEvent -> World
    ;; GIVEN: (x,y) coordinate of mouse and mouse event
    ;; RETURN: World after following mouse event containing the metatoy object
    ;; STRATEGY: Using Template of MouseEvent on mev
    ;; EXAMPLES: See test cases
    
    (define/public (after-mouse-event mx my mev)
      (cond
        [(mouse=? mev MOUSE-DOWN)
         (after-button-down mx my)]
        [(mouse=? mev MOUSE-DRAG)
         (after-drag mx my)]
        [(mouse=? mev MOUSE-UP)
         (after-button-up mx my)]
        [(mouse=? mev MOUSE-MOVE)
         (after-move mx my)]
        [else this]))
    
    
    ;; after-button-down : Int Int -> Metatoy
    ;; GIVEN: (x,y) coordinate of mouse and mouse event
    ;; RETURN: a metatoy after the button-down event 
    ;; STRATEGY: Use HOF map on ListOfToys
    ;; EXAMPLES: See test cases
    
    (define (after-button-down mx my)
      (new-metatoy-state
       (map        
        (lambda (obj) (send obj after-button-down mx my))
        objs)
       t))
    
    
    ;; after-button-up : Int Int -> Metatoy
    ;; GIVEN: (x,y) coordinate of mouse and mouse event
    ;; RETURN: a metatoy after the button-up event 
    ;; STRATEGY: Use HOF map on ListOfToys
    ;; EXAMPLES: See test cases
    
    (define (after-button-up mx my)
      (new-metatoy-state
       (map
        (lambda (obj) (send obj after-button-up mx my))
        objs)
       t))
    
    ;; after-drag : Int Int -> Metatoy
    ;; GIVEN: (x,y) coordinate of mouse and mouse event
    ;; RETURN: a metatoy after the drag event 
    ;; STRATEGY: Use HOF map on ListOfToys
    ;; EXAMPLES: See test cases
    
    (define (after-drag mx my)
      (new-metatoy-state
       (map
        (lambda (obj) (send obj after-drag mx my))
        objs)
       t))
    
    ;; after-move : Int Int -> Metatoy
    ;; GIVEN: (x,y) coordinate of mouse and mouse event
    ;; RETURN: a metatoy after the move event 
    ;; STRATEGY: Use HOF map on ListOfToys
    ;; EXAMPLES: See test cases
    
    (define (after-move mx my)
      (new-metatoy-state
       (map
        (lambda (obj) (send obj after-move mx my))
        objs)
       t))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS For Tests

(define C0 (make-clock (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)))
(define P0 (make-politician (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)))
(define T0 (make-throbber (/ CANVAS-WIDTH 2) (/ CANVAS-HEIGHT 2)))
(define M0 (make-metatoy empty))

(define C1 (make-clock 50 50))
(define C2 (send C1 after-tick))
(define C3 (send C1 after-button-down 45 45))
(define C4 (send C3 after-button-up 45 45))
(define C5 (send C3 after-drag 46 46))
(define C6 (send C3 after-move 40 40))
(define M1 (make-metatoy (list C1)))
(define M2 (make-metatoy (list C2)))
(define M3 (make-metatoy (list C3)))
(define TEXT (place-image (text "0" 10 "Red") 50 50 EMPTY-CANVAS))
(define CLOCK (place-image
               (rectangle 60 40 OUTLINE-CIRCLE "blue") 50 50 TEXT))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:

(begin-for-test
  (check-equal? (send (first (send M2 get-toys)) toy-data)
                (send (first (send (send M1 after-tick) get-toys)) toy-data)
                "Toy-data should be same")
  (check-equal? (send (first (send (send M0 after-key-event NEW-CLOCK-EVENT)
                                   get-toys))
                      toy-data)
                0
                "Toy-data should be zero")
  (check-equal? (send (first (send (send M0 after-key-event
                                         NEW-POLITICIAN-EVENT)get-toys))
                      toy-data)
                300
                "Toy-data should be 300")
  (check-equal? (send (first (send (send M0 after-key-event NEW-THROBBER-EVENT)
                                   get-toys))
                      toy-data)
                5
                "Toy-data should be 5")
  (check-equal? (send (first (send (send M1 after-key-event "z") get-toys))
                      toy-data)
                0
                "Toy-data should be 0")
  (check-equal? (send (first (send (send M1 after-mouse-event 45 45
                                         MOUSE-DOWN)
                                   get-toys)) for-test:selected?)
                #t
                "The value should be true")
  (check-equal? (send (first (send (send M3 after-mouse-event 45 45
                                         MOUSE-UP)
                                   get-toys)) for-test:selected?)
                #f
                "The value should be false")
  (check-equal? (send (first (send (send M3 after-mouse-event 46 46
                                         MOUSE-DRAG)
                                   get-toys)) toy-x)
                51
                "Toy-data should be 51")
  (check-equal? (send (first (send (send M3 after-mouse-event 40 40
                                         MOUSE-MOVE)
                                   get-toys)) toy-x)
                50
                "Toy-data should be 50")
  (check-equal? (send (first (send (send M3 after-mouse-event 40 40
                                         "enter")
                                   get-toys)) toy-x)
                (send (first (send M3 get-toys)) toy-x)
                "Toy-x should be same")
  (check-equal? (send M1 to-scene)
                CLOCK
                "clock should be present in scene"))