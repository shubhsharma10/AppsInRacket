;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Probe Probelm -2

(require rackunit)
(require "extras.rkt")
(check-location "07" "q2.rkt")

(provide probe-possible-outcome?
         make-turn-right
         make-turn-left
         make-move-forward)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS

(define N "north")
(define S "south")
(define W "west")
(define E "east")

(define LEFT "left")
(define RIGHT "right")

(define ADD-FACTOR 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA-DEFINITION

;; A TurnDirection is one of
;; LEFT
;; RIGHT
;; INTERP : self-evident

;; TEMPLATE:
;; turn-direction-fn : TurnDirection -> ??
;; (define (turn-direction-fn turn-dir)
;;   (cond
;;     [(string=? turn-dir LEFT) ....]
;;     [(string=? turn-dir RIGHT) ....]))

;; A Direction is one of
;; N
;; S
;; E
;; W
;; INTERP: self explanatory

;; TEMPLATE:
;; direction-fn: Direction -> ??
;; (define (direction-fn dir)
;;  (cond
;;    [(string=? dir N)...]
;;    [(string=? dir S)...]
;;    [(string=? dir E)...]
;;    [(string=? dir W)...]))

(define-struct probe (x-max x-min y-max y-min direction))
;; A Probe is a
;; (make-prob Real Real Real Real Direction)
;; INTERP:
;; x-max is the maximum coordinate on x-axis which the center of probe can have
;; x-min is the minimum coordinate on x-axis which the center of probe can have
;; y-max is the maximum coordinate on y-axis which the center of probe can have
;; y-min is the minimum coordinate on y-axis which the center of probe can have
;;
;; TEMPLATE:
;; (define (probe-fn pro)
;; (...
;;   (probe-x-max pro)
;;   (probe-x-min pro)
;;   (probe-y-max pro)
;;   (probe-y-min pro)
;;   (probe-direction pro)))

(define-struct turn-left())
;; A TurnLeft instruction is a (make-turn-left)

(define-struct turn-right())
;; A TurnRight instruction is a (make-turn-right)

(define-struct move-forward (step))
;; A MoveForward instruction is a (make-move-forward PosInt)

;; An Instruction is one of
;; -- (make-turn-left)            Interp: a TurnLeft instruction
;; -- (make-turn-right)           Interp: a TurnRight instruction
;; -- (make-move-forward PosInt)  Interp: a MoveForward instruction to move by
;;                               the given number of steps.

;; A Program is a ListOfInstruction
;; INTERP: A sequence of instructions, to be executed from left to
;;         right.

;; ListofInstruction
;; A ListofInstruction(LOI) is either
;; -- empty                   ;; INTERP: A list of Instruction with no elements
;; -- (cons Instruction LOI)  ;; INTERP: first element is Instruction and whose
;;                                     other elements are represented by LOI

;; TEMPLATE:
;; instruction-fn: Instruction -> ??
;; (define (instruction-fn inst)
;;   (cond
;;     [(turn-left? inst) .....]
;;     [(turn-right? inst).....]
;;     [(move-forward? inst)...]))
;; 
;; loi-fn : LOI -> ??
;; HALTING-MEASURE : (length  loi)
;; (define (loi-fn loi)
;;   (cond
;;     [(empty? loi)...]
;;     [else (...
;;        (instruction-fn (first loi))
;;        (loi-fn (rest loe)))]))

;; END-DATA-DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; probe-possible-outcome? : Int Int Program Int Int -> Boolean
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;;        coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;;          and executing program p according to the tolerances given above,
;;          could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].
;; STRATEGY: Use foldl HOF on prog
(define (probe-possible-outcome? x0 y0 prog x1 y1)
  (check-in-range? x1 y1
                   (foldl
                    ;; Instruction Probe -> Probe
                    ;; Probe after executing instruction
                    probe-execute-instr
                    (make-probe x0 x0 y0 y0 "north") prog)))

;; probe-execute-instr : Instruction Probe -> Probe
;; RETURNS: Probe after executing instruction
;; EXAMPLE: (probe-execute-instr (make-turn-left) (make-probe 50 10 100 20 W))
;;         = (make-probe 50 10 100 20 S)
;; STRATEGY: Use Instruction template on instr
(define (probe-execute-instr instr prob)
  (cond
    [(turn-left? instr)    (probe-turn prob LEFT)]
    [(turn-right? instr)   (probe-turn prob RIGHT)]
    [(move-forward? instr) (probe-move-forward prob
                                               (move-forward-step instr))]))

;; check-in-range? : Int Int Probe -> Boolean
;; GIVEN: (x,y) coordinate and Probe
;; RETURNS: True iff (x,y) is in range of max,min range of Probe
;; STRATEGY: Use Probe template of prob and combine simple functions
(define (check-in-range? x y prob)
  (and (in-range? x (probe-x-max prob) (probe-x-min prob))
       (in-range? y (probe-y-max prob) (probe-y-min prob))))

;; in-range?: Int Int Int -> Boolean
;; GIVEN: number, max and minimum allowed value
;; RETURNS: True iff val is between high and low
;; STRATEGY: Combine simple functions
(define (in-range? val high low)
  (and (>= val low) (<= val high)))

;; probe-turn: Probe TurnDirection -> Probe
;; GIVEN: Probe and direction of turn
;; RETURNS: Probe after turning left or right
;; EXAMPLE: (probe-turn  (make-probe 50 10 100 20 W) LEFT)
;;          = (make-probe 50 10 100 20 S)
;; STRATEGY: Use Probe template on prb
(define (probe-turn prb tunr-dir)
  (make-probe (probe-x-max prb) (probe-x-min prb)
              (probe-y-max prb) (probe-y-min prb)
              (get-direction-after-turn prb tunr-dir)))

;; get-direction-after-turn : Probe TurnDirection -> Direction
;; GIVEN: Probe and direction of turn
;; RETURNS: Probe direction after turn
;; EXMAPLE: (get-direction-after-turn
;;             (make-probe 50 10 100 20 W) LEFT)
;;         = S
;; STRATEGY: Use TurnDirection template on turn-dir
(define (get-direction-after-turn prb turn-dir)
  (cond
    [(string=? turn-dir LEFT) (get-direction-after-left prb)]
    [else (get-opposite-dir (get-direction-after-left prb))]))

;; get-direction-after-left: Probe -> Direction
;; GIVEN: A Probe
;; RETURNS: Direction of probe after taking left direction
;; EXAMPLE:
;; (get-direction-after-left (make-probe 50 10 100 20 W))
;; = S
;; STRATEGY: Use Direction template on probe direction
(define (get-direction-after-left prb)
  (let ([prb-dir (probe-direction prb)])
    (cond
      [(string=? prb-dir N) W]
      [(string=? prb-dir S) E]
      [(string=? prb-dir E) N]
      [(string=? prb-dir W) S])))

;; get-opposite-dir : Direction -> Direction
;; GIVEN: Direction
;; RETURNS: Direction opposite to given direction
;; EXAMPLE: (get-opposite-dir S) = N
;; STRATEGY: Use Direction template on dir
(define (get-opposite-dir dir)
  (cond
    [(string=? dir W) E]
    [(string=? dir E) W]
    [(string=? dir N) S]
    [(string=? dir S) N]))

;; probe-move-forward: Probe Int -> Probe
;; GIVEN: Probe with no of steps for movement
;; RETURNS: Probe after movement
;; EXAMPLE : (probe-move-forward (make-probe 50 10 100 20 W) 3)
;;           = (make-probe 49 5 100 20 W)
;; STRATEGY: Use Probe template on prb
(define (probe-move-forward prb dist)
  (make-probe (change-in-x-max prb (probe-direction prb) dist)
              (change-in-x-min prb (probe-direction prb) dist)
              (change-in-y-max prb (probe-direction prb) dist)
              (change-in-y-min prb (probe-direction prb) dist)
              (probe-direction prb)))

;; change-in-x-max: Probe Direction Int -> Int
;; GIVEN: Probe with direction and steps for movement
;; RETURNS: maximum x coordinate after cosidering direction
;; EXAMPLE: (change-in-x-max (make-probe 50 10 100 20 W) W 3)
;;          = 49
;; STRATEGY: Use Direction template on dir
(define (change-in-x-max prb dir dist)
  (cond
    [(string=? dir W) (sanitize-coord-max (probe-x-max prb)
                                          (- dist ADD-FACTOR))]
    [(string=? dir E) (+ (probe-x-max prb) (+ dist ADD-FACTOR))]
    [ else (probe-x-max prb)]))

;; change-in-x-min: Probe Direction Int -> Int
;; GIVEN: Probe with direction and steps for movement
;; RETURNS: minimum x coordinate after cosidering direction
;; EXAMPLE: (change-in-x-min (make-probe 50 10 100 20 W) W 3)
;;          = 5
;; STRATEGY: Use Direction template on dir
(define (change-in-x-min prb dir dist)
  (cond
    [(string=? dir W) (- (probe-x-min prb) (+ dist ADD-FACTOR))]
    [(string=? dir E) (sanitize-coord-min (probe-x-min prb)
                                          (- dist ADD-FACTOR))]
    [ else (probe-x-min prb)]))

;; change-in-y-max: Probe Direction Int -> Int
;; GIVEN: Probe with direction and steps for movement
;; RETURNS: maximum y coordinate after cosidering direction
;; EXAMPLE: (change-in-y-max (make-probe 50 10 100 20 N) N 3)
;;          = 99
;; STRATEGY: Use Direction template on dir
(define (change-in-y-max prb dir dist)
  (cond
    [(string=? dir N) (sanitize-coord-max (probe-y-max prb)
                                          (- dist ADD-FACTOR))]
    [(string=? dir S) (+ (probe-y-max prb) (+ dist ADD-FACTOR))]
    [ else (probe-y-max prb)]))

;; change-in-y-min: Probe Direction Int -> Int
;; GIVEN: Probe with direction and steps for movement
;; RETURNS: minimum y coordinate after cosidering direction
;; EXAMPLE: (change-in-y-min (make-probe 50 10 100 20 N) N 3)
;;          15
;; STRATEGY: Use Direction template on dir
(define (change-in-y-min prb dir dist)
  (cond
    [(string=? dir N) (- (probe-y-min prb) (+ dist ADD-FACTOR))]
    [(string=? dir S) (sanitize-coord-min (probe-y-min prb)
                                          (- dist ADD-FACTOR))]
    [ else (probe-y-min prb)]))

;; sanitize-coord-max : Int Int -> Int
;; GIVEN: Current coordinate and possible coordinate after move
;; RETURNS: Coordinate after considering maximum possible value
;; EXAMPLE:  (sanitize-coord-max 5 -1)
;;          = 5
;; STRATEGY: Combine simple function
(define (sanitize-coord-max curr fwd)
  (if (> (- curr fwd) curr) curr (- curr fwd)))

;; sanitize-coord-min : Int Int -> Int
;; GIVEN: Current coordinate and possible coordinate after move
;; RETURNS: Coordinate after considering minimum possible value
;; EXAMPLE: (sanitize-coord-min 5 -1)
;;           5
;; STRATEGY: Combine simple function
(define (sanitize-coord-min curr fwd)
  (if(< (+ curr fwd) curr) curr (+ curr fwd)))

;; scalable-stress-program : Int -> Program
;; GIVEN: an integer n
;; RETURNS: a Program of length (* 6 (max 0 n))
;; STRATEGY: Combine simple function
;; HALTING-MEASURE: when n becomes 0
(define (scalable-stress-program n)
  (if (> n 0)
      (append (list (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-right)
                    (make-move-forward 10)
                    (make-turn-left)
                    (make-turn-left))
              (scalable-stress-program (- n 1)))
      empty))

;; stress-benchmark : Int -> Boolean
;; GIVEN: an integer, preferably non-negative, stating the desired
;;     degree of difficulty for the benchmark
;; RETURNS: true
;; EFFECT: reports how much time probe-possible-outcome?
;;          takes to compute the correct answer
;; STRATEGY: Combine simple functions
(define (stress-benchmark n)
  (let ((pgm (scalable-stress-program n)))
    (time (probe-possible-outcome? 0 0 pgm (* 10 n) (* 10 n)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-test
  (check-equal? (stress-benchmark 1000) #t)
  
  (check-equal? (stress-benchmark 10000) #t)
  
  (check-equal? (sanitize-coord-max 5 -1)
                5
                "Result should be 5")
  
  (check-equal? (sanitize-coord-min 5 -1)
                5
                "Result should be 5")
  
  (check-equal?
   (probe-turn (make-probe 50 10 100 20 N) LEFT)
   (make-probe 50 10 100 20 W)
   "Resultant porbe should in W direction")
  
  (check-equal? (get-direction-after-left (make-probe 50 10 100 20 W))
                S
                "Resultant direction should be South")
  
  (check-equal? (get-opposite-dir E)
                W
                "Resultant direction should be West")
  
  (check-equal? (get-opposite-dir S)
                N
                "Resultant direction should be North")
  
  (check-false (probe-possible-outcome? 20 100 (list (make-turn-right)
                                                     (make-move-forward 1)
                                                     (make-turn-right)
                                                     (make-move-forward 1)
                                                     (make-turn-left)) 30 105)
               "The output of this program should be false")
  (check-false (probe-possible-outcome? 20 100 (list (make-turn-right)
                                                     (make-move-forward 10)
                                                     (make-turn-right)
                                                     (make-turn-right)
                                                     (make-move-forward 5))
                                        30 105)
               "the output of the program should be false")
  (check-false (probe-possible-outcome? 20 100 (list (make-turn-right)
                                                     (make-move-forward 10)
                                                     (make-turn-right)
                                                     (make-turn-right)
                                                     (make-move-forward 5)
                                                     (make-turn-right)
                                                     (make-move-forward 2))
                                        32 105)
               "the output of the program should be false"))



