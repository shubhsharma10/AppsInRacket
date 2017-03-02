#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "interfaces2.rkt")
(require "WidgetWorks.rkt")

(provide make-block
         SBlock%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SBlock% class

;; It implements SBlock<%> interface.

;; Constructor template for SBlock%
;; (new SBlock [x Int] [y Int] [team ListOfSBlock]
;;             [saved-mx Int] [saved-my Int] [selected? Boolean]
;;             [all-cubes ListOfSBlock])
;; The last five arguments are optional
;; INTERP: An object of class SBlock% represents a stateful block

(define SBlock%
  (class* object% (SBlock<%>)
    
    ;; the init-fields are the values that may vary from one SBlock to
    ;; the next.
    
    ;; (x,y) coordinates of center of sblock
    (init-field x y)
    
    ;; saved coordinates of mouse
    (init-field [saved-mx 0] [saved-my 0])
    
    ;; List of sblock which represents current sblock's team
    (init-field [team empty])
    
    ;; True iff sblock is selected
    (init-field [selected? false])
    
    ;; List of all cubes present in container
    (init-field [all-cubes empty])
    
    ;; Width of the square
    (field [SQUARE-WIDTH 20])

    ;; half of the given square width
    (field [HALF-WIDTH (/ SQUARE-WIDTH 2)])
    
    ;; Color for selected and unselected shape
    (field [SELECTED-COLOR "red"] [UNSELECTED-COLOR "green"])
    
    ;; Type of the shape
    (field [SHAPE-TYPE "outline"])
    
    (super-new)
    
    ;; -------------------------------------------------------------------------
    ;; Interface required methods
    ;; -------------------------------------------------------------------------
    
    ;; after-tick: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: No effect
    ;; DETAILS: SBlock ignores this event
    ;; EXAMPLE: See tests
    (define/public (after-tick)
      this)   
    
    
    ;; after-key-event: -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: No effect
    ;; DETAILS: SBlock ignores this event
    ;; EXAMPLE: See tests
    (define/public (after-key-event kev)
      this)
    
    
    ;; after-button-down: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of mouse
    ;; EFFECT: Stores delta in saved-mx, saved-my if mouse is in
    ;; sblock
    ;; STRATEGY:Combine Simpler Functions
    ;; EXAMPLE: See tests
    (define/public (after-button-down mx my)
      (if (in-block? mx my)
          (begin
            (set! selected? true)
            (set! saved-mx (- mx x))
            (set! saved-my (- my y)))
          this))
    
    
    ;; after-button-up: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of mouse
    ;; EFFECT: Set selected? to false if mouse is in sblock
    ;; STRATEGY:Combine simpler functions
    ;; EXAMPLE: See tests
    (define/public (after-button-up mx my)
      (if (in-block? mx my)
            (set! selected? false)
          this))
    
    
    ;; after-drag: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of mouse
    ;; EFFECT: Update (x,y) coordinates with delta stored in saved-mx,saved-my
    ;; to allow smooth drag and drag team mates also if mouse is in sblock
    ;; STRATEGY:Combine simpler functions
    ;; EXAMPLE: See tests
    (define/public (after-drag mx my)
       (if selected?
          (begin
            (update-teammates-delta)
            (set! x (- mx saved-mx))
            (set! y (- my saved-my))
            (for-each (lambda (mate) (send mate drag-teammate x y)) team)
            (find-team-for-all))           
          this))
    
    
    ;; update-delta: Int Int -> Void
    ;; GIVEN: (x,y) coordinates of leader's center
    ;; EFFECT: Store delta wrt to leader's center coordinates in saved-mx,
    ;; saved-my
    ;; EXAMPLES: See tests
    (define/public (update-delta a b)
      (begin
        (set! saved-mx (- a x))
        (set! saved-my (- b y))))
    
    
    ;; drag-teammate: Int Int -> Void
    ;; GIVEN: (x,y) coordinate of leader's center
    ;; EFFECT: Update (x,y) coordinate with stored delta in saved-mx,saved-my
    ;; EXAMPLES: See tests
    (define/public (drag-teammate a b)
      (begin
        (set! x (- a saved-mx))
        (set! y (- b saved-my))))
    
    
    ;; add-to-scene: Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: Given scene with sblock painted on it
    ;; STRATEGY:Combine Simpler functions
    ;; EXAMPLE: See tests
    
    (define/public (add-to-scene scene)
      (if selected?
          (place-image (square SQUARE-WIDTH SHAPE-TYPE SELECTED-COLOR)
                       x y scene)
          (place-image (square SQUARE-WIDTH SHAPE-TYPE UNSELECTED-COLOR)
                       x y scene)))
    
    
    ;; after-move: Int Int -> Void
    ;; GIVEN: coordinate of the mouse
    ;; EFFECT : No effect
    ;; DETAILS: SBlock ignores mouse-move event
    ;; EXAMPLES: See test cases
    
    (define/public  (after-move mx my)
      this)
    
    ;; get-team: -> ListOfSBlock
    ;; GIVEN: -> No argument
    ;; RETURNS: List of sblock who are team mates of this sblock
    ;; EXAMPLES: See tests
    
    (define/public (get-team)
      team)
    
    ;; add-teammate: SBlock -> Void
    ;; GIVEN: -> A sblock
    ;; EFFECT: adds the given sblock to this sblock's team
    ;; STRATEGY: Combine simpler functions
    ;; EXAMPLES: See tests
    
    (define/public (add-teammate sb)
      (if(or (present? sb) (same-as-this? sb))
         this
         (set! team (cons sb team))))
    
    ;; add-to-all-cubes: SBlock -> Void
    ;; GIVEN: -> A sblock
    ;; EFFECT: adds the given sblock to this sblock's team
    ;; EXAMPLES: See tests
    (define/public (add-to-all-cubes sb)
      (if (or (present-in-all-cubes? sb) (same-as-this? sb))
          this
          (set! all-cubes (cons sb all-cubes))))
    
    
    ;; sblock-x : -> Integer
    ;; RETURNS: the x coordinate of this sblock
    (define/public (sblock-x)
      x)
    
    
    ;; sblock-y : -> Integer
    ;; RETURNS: the y coordinate of this sblock
    (define/public (sblock-y)
      y)
    
    ;; ------------------------------------------------------------------------
    ;; Non-Interface required functions
    ;; ------------------------------------------------------------------------
    
    
    ;; in-block? : Int Int -> Boolean
    ;; GIVEN: location of the mouse coordinates
    ;; RETURNS: true iff the location is inside the clock.
    ;; STRATEGY:Combine Simpler functions
    ;; EXAMPLES: See test cases
    
    (define (in-block? px py)
      (and (<= (- x (/ SQUARE-WIDTH 2)) px  (+ x (/ SQUARE-WIDTH 2)))
           (<= (- y (/ SQUARE-WIDTH 2)) py  (+ y (/ SQUARE-WIDTH 2)))))
    
    ;; present?: SBlock -> Boolean
    ;; GIVEN: A SBlock
    ;; RETURNS: True iff given sblock is present in team
    ;; STRATEGY: Use ormap HOF on team
    
    (define (present? sb)
      (ormap
       (lambda (cube) (equal? sb cube)) team))
    
    
    ;; present-in-all-cubes?: SBlock -> Boolean
    ;; GIVEN: A SBlock
    ;; RETURNS: True iff given sblock is present in team
    ;; STRATEGY: Use ormap HOF on team
    
    (define (present-in-all-cubes? sb)
      (ormap
       (lambda (cube) (equal? sb cube)) all-cubes))
    
    
    ;; same-as-this?: SBlock -> Boolean
    ;; GIVEN: A SBlock
    ;; RETURNS: True iff given sblock is same as this
    ;; STRATEGY: Combine simple function
    
    (define (same-as-this? sb)
      (equal? sb this))
    
    
    ;; find-team-for-all -> Void
    ;; EFFECT: Find team for this sblock and add this with team to
    ;; team of each sblock in team
    ;; STRATEGY: Combine simple functions
    ;; EXAMPLE: See tests
    
    (define (find-team-for-all)
      (begin
        (find-nbrs-and-push-to-team)
        (push-team-to-mates)))
    
    
    ;; find-nbrs-and-push-to-team -> Void
    ;; EFFECT: Find immediate neighbour and add it's neighbour and itself in
    ;; team of this sblock
    ;; EXAMPLE: See tests
    
    (define (find-nbrs-and-push-to-team)
      (local
        ((define nbrs (get-immi-nbrs)))
        (for-each (lambda (nbr) (add-teammate nbr)) nbrs)
        (add-nbrs-from-immi-nbrs nbrs)))
    

    ;; add-nbrs-from-immi-nbrs: ListOfSBlock -> Void
    ;; EFFECT: Find neighbours from immediate neighbour and add it to this team
    ;; EXAMPLE: See tests
    
    (define (add-nbrs-from-immi-nbrs nbrs)
      (for-each (lambda (nbr)
                    (for-each
                     (lambda (mate) (add-teammate mate)) (get-field team nbr)))
                  nbrs))
    
    
    ;; push-team-to-mates : -> Void
    ;; EFFECT: Add team and leader to each teammates's team
    ;; STRATEGY: Use for-each on team
    ;; EXAMPLE: See tests
    (define (push-team-to-mates)
      (for-each (lambda (cb)
                  (for-each (lambda (mt)
                              (send cb add-teammate mt)) (cons this team)))
                team))
    
    
    ;; get-immi-nbrs : -> ListOfSBlock
    ;; RETURNS: List of immediate neighbours of this sblock
    ;; STRATEGY: Use filter HOF on all-cubes
    ;; EXAMPLE: See tests
    
    (define/public (get-immi-nbrs)
      (filter (lambda (cb) (cubes-overlap? cb)) all-cubes))
    
    
    ;; update-teammates-delta: -> Void
    ;; EFFECT: Sends delta for each team mate wrt to leader's center coordinates
    ;; STRATEGY: Use map HOF on team
    ;; EXAMPLES: See tests
    
    (define (update-teammates-delta)
      (for-each
       (lambda (cb) (send cb update-delta x y)) team)
      this)
    
    
    ;; cubes-overlap? : SBlock% -> Boolean
    ;; RETURNS: True iff given sblock is in contact or intersect
    ;; with this sblock
    ;; STRATEGY: Combine simple functions
    ;; EXAMPLES: See tests
    (define/public (cubes-overlap? cube1)
      (local
        ((define X1  (- (get-field x cube1) HALF-WIDTH))
         (define X2  (+ (get-field x cube1) HALF-WIDTH))
         (define Y1  (- (get-field y cube1) HALF-WIDTH))
         (define Y2  (+ (get-field y cube1) HALF-WIDTH))         
         (define X11 (- x HALF-WIDTH))
         (define X21 (+ x HALF-WIDTH))
         (define Y11 (- y HALF-WIDTH))
         (define Y21 (+ y HALF-WIDTH)))
        (or (point-in-square? X1 Y1 X11 Y11 X21 Y21)
            (point-in-square? X2 Y2 X11 Y11 X21 Y21)
            (point-in-square? X1 Y2 X11 Y11 X21 Y21)
            (point-in-square? X2 Y1 X11 Y11 X21 Y21))))   
    
    
    ;; point-in-square? : Int Int Int Int Int Int -> Boolean
    ;; GIVEN: One corner of a sblock and coordinate of top-left and
    ;; bottom-right corners of another sblock
    ;; RETURNS: True iff point is in or touches sblock
    ;; EXAMPLE: See tests
    (define/public (point-in-square? X1 Y1 X11 Y11 X21 Y21)
      (and (<= X11 X1 X21)
           (<= Y11 Y1 Y21)))
    ))

;; make-sblock : Int Int ListOfSBlock -> SBlock
;; GIVEN: Center's coordinates and list
;; RETURNS: A sblock with given coordinates
;; EXAMPLE: See tests
(define (make-block init-x init-y list)
  (new SBlock% [x init-x] [y init-y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;TESTS:

(begin-for-test
  (local
    ((define B1 (make-block 100 100 empty))
     (define B2 (make-block 200 200 empty))
     (define B3 (make-block 250 250 empty))
     (define B4 (make-block 300 300 empty)))
    (send B1 after-tick)
    (send B1 add-to-all-cubes B1)
    (check-equal? (length(get-field all-cubes B1)) 0
                  "The length is zero.")
    (send B1 add-to-all-cubes B2)
    (send B1 add-to-all-cubes B3)
    (send B1 add-to-all-cubes B4)
    (send B2 add-to-all-cubes B1)
    (send B2 add-to-all-cubes B3)
    (send B2 add-to-all-cubes B4)
    (check-equal? (send (third(get-field all-cubes B1)) sblock-x) 200
                  "The x corodinate of the sblock is 200.")
    (check-equal? (length(get-field all-cubes B1)) 3
                  "The length is 3")
    (check-equal? (send B1 sblock-x) 100
                  "The x coordinate of the sblock is 100")
    (send B1 after-key-event "b")
    (check-equal? (send B1 get-team) empty
                  "The team for the given sblock is empty")
    (send B1 after-button-down 100 100)
    (check-equal? (get-field selected? B1) #t
                  "Selected is true")
    (send B1 after-drag 180 180)
    (check-equal? (length(get-field team B1)) 1
                  "Length of the team is 1")
    (check-equal? (send B1 sblock-y) 180
                  "The y coordinate of the sblock is 180")
    (check-equal? (send B1 sblock-x) 180
                  "The x coordinate of the sblock is 180")
    (check-equal? (send B2 sblock-x) 200
                  "The x coordinate of the sblock is 200")
    (check-equal? (send B2 sblock-y) 200
                  "The y coordinate of the sblock is 100")
    (check-equal? (send B1 get-team) (list B2)
                  "The list contains B2")
    (send B1 after-button-up 180 180)
    (send B2 after-button-down 200 200)
    (check-equal? (get-field selected? B2) #t
                  "The selected is true.")
    (send B2 after-drag 230 230)
    (check-equal? (length(send B2 get-team)) 2
                  "The length is 2")
    (send B2 after-drag 280 280)
    (check-equal? (length(send B2 get-team)) 3
                  "The length is 3")
    (check-equal? (send B1 sblock-x) 260
                  "The x coordinate of the sblock is 260")
    (send B1 after-button-down 260 260)
    (check-equal? (get-field selected? B1) #t
                  "Selected is true")
    (send B1 after-drag 350 350)
    (check-equal? (length(send B1 get-team)) 3
                  "The length of the team is 3")
    (send B1 after-button-up 350 350)
    (send B1 after-move 400 400)
    (check-equal? (send B1 sblock-x) 350)
    (send B1 after-button-down 400 400)
    (check-equal? (get-field selected? B1) #f
                  "Selected is false")
    (send B1 after-drag 450 450)
    (check-equal? (send B1 sblock-y) 350)
    (send B1 after-button-up 450 450)
    (check-equal? (get-field selected? B1) #f
                  "Selected is false")
    ))

(begin-for-test
  (local
    ((define B1 (make-block 100 100 empty))
     (define scene (empty-scene 500 600))
     (define SELBLOCK (place-image (square 20 "outline" "red")
                                   100 100 scene))
     (define UNSELBLOCK (place-image (square 20 "outline" "green")
                                     100 100 scene)))
    (check-equal? (send B1 add-to-scene scene) UNSELBLOCK
                  "Unselected block is displayed.")
    (send B1 after-button-down 100 100)
    (check-equal? (send B1 add-to-scene scene) SELBLOCK
                  "Selected block is displayed.")
    ))
