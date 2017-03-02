;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname probe) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Design set of function to facilitate movement and analyze position of probe on Pluto

(require rackunit)
(require "extras.rkt")
(check-location "02" "probe.rkt")
(require 2htdp/universe)
(provide
  probe-at
  probe-turned-left
  probe-turned-right
  probe-direction-equal?
  probe-location-equal?
  probe-forward-possible-outcome?
  )

;; DATA DEFINITIONS:
;; a Direction is one of
;; -- "north"
;; -- "south"
;; -- "east"
;; -- "west"
;; INTERPRETATION : self-evident

;; TEMPLATE:
;; direction-fn : Direction -> ??
; (define (direction-fn dn)
;   (cond
;     [(equal? dn "north")...]
;     [(equal? dn "south")...]
;     [(equal? dn "east")...]
;     [(equal? dn "west")...]))
(define N "North")
(define S "South")
(define E "East")
(define W "West")

;; DATA DEFINITIONS:
(define-struct probe (x y direction))

;; a Probe is a (make-probe int int Direction)
;; x           is x-coordinate of center of Probe
;; y           is y-coordinate of center of Probe
;; direction   is direction in which Probe is facing

;; TEMPLATE:
;; probe-fn : Probe -> ??
(define (probe-fn pb)
  (... (probe-x pb)
       (probe-y pb)
       (probe-direction pb)))


;; probe-at : Integer Integer -> Probe
;; GIVEN:an x-coordinate and a y-coordinate
;; RETURNS: a probe with its center at those coordinates, facing north.
;; EXAMPLE:
;; (probe-at 4 6) = (make-probe 4 6 "north")
;; (probe-at 2 4) = (make-probe 2 4 "north")
;; STRATEGY: Using constructor template of Probe
(define (probe-at x y)
  (make-probe x y "north"))
;; TESTS:
(begin-for-test
  (check-equal? (probe-at 4 6) (make-probe 4 6 "north") "Probe should be initialized with center at (4,6) facing north")
  (check-equal? (probe-at 2 4) (make-probe 2 4 "north") "Probe should be initialized with center at (4,6) facing north"))

;; probe-turned-left: Probe -> Probe
;; GIVEN:   a probe
;; RETURNS: a probe like the original, but turned 90 degrees left.
;; EXAMPLE:
;; (probe-turned-left (make-probe 4 6 "north")) = (make-probe 4 6 "west")
;; STRATEGY: Using Direction template on probe
(define (probe-turned-left pb)
  (cond
    [(equal? (probe-direction pb) N)
            (make-probe (probe-x pb) (probe-y pb) W)]
    [(equal? (probe-direction pb) S)
            (make-probe (probe-x pb) (probe-y pb) E)]
    [(equal? (probe-direction pb) E)
            (make-probe (probe-x pb) (probe-y pb) N)]
    [(equal? (probe-direction pb) W)
            (make-probe (probe-x pb) (probe-y pb) S)]))
;; TESTS:
(begin-for-test
  (check-equal? (probe-turned-left (make-probe 4 6 N)) (make-probe 4 6 W) "Resultant probe should be in West direction")
  (check-equal? (probe-turned-left (make-probe 4 6 S)) (make-probe 4 6 E) "Resultant probe should be in East direction")
  (check-equal? (probe-turned-left (make-probe 4 6 E)) (make-probe 4 6 N) "Resultant probe should be in North direction")
  (check-equal? (probe-turned-left (make-probe 4 6 W)) (make-probe 4 6 S) "Resultant probe should be in South direction"))

;; probe-turned-right: Probe -> Probe
;; GIVEN:   a probe
;; RETURNS: a probe like the original, but turned 90 degrees right.
;; EXAMPLE:
;; (probe-turned-right: (make-probe 4 6 "north")) = (make-probe 4 6 "east")
;; STRATEGY: Apply Direction template on probe
(define (probe-turned-right pb)
  (cond
    [(equal? (probe-direction pb) N)
            (make-probe (probe-x pb) (probe-y pb) E)]
    [(equal? (probe-direction pb) S)
            (make-probe (probe-x pb) (probe-y pb) W)]
    [(equal? (probe-direction pb) E)
            (make-probe (probe-x pb) (probe-y pb) S)]
    [(equal? (probe-direction pb) W)
            (make-probe (probe-x pb) (probe-y pb) N)]))
;; TESTS:
(begin-for-test
  (check-equal? (probe-turned-right (make-probe 4 6 N)) (make-probe 4 6 E) "Resultant probe should be in East direction")
  (check-equal? (probe-turned-right (make-probe 4 6 S)) (make-probe 4 6 W) "Resultant probe should be in West direction")
  (check-equal? (probe-turned-right (make-probe 4 6 E)) (make-probe 4 6 S) "Resultant probe should be in South direction")
  (check-equal? (probe-turned-right (make-probe 4 6 W)) (make-probe 4 6 N) "Resultant probe should be in North direction"))

;; probe-direction-equal? : Probe Probe -> boolean
;; GIVEN:   two probes
;; RETURNS: true iff probes are facing in same direction, else false
;; EXAMPLE:
;; (probe-direction-equal? (make-probe 4 6 "north") (make-probe 4 6 "east")) = #false
;; STRATEGY: Cases
(define (probe-direction-equal? pb1 pb2)
  (cond
    [(equal? (probe-direction pb1) (probe-direction pb2))
            #true]
    [else   #false]))
;; TESTS:
(begin-for-test
  (check-equal? (probe-direction-equal? (make-probe 4 6 "north") (make-probe 4 9 "east")) #false "Both probes are not in same direction")
  (check-equal? (probe-direction-equal? (make-probe 4 6 "north") (make-probe 2 5 "north")) #true "Both probes are in same direction"))


;; probe-location-equal? : Probe Probe -> boolean
;; GIVEN:   two probes
;; RETURNS: true iff probes are at same location, else false
;; EXAMPLE:
;; (probe-location-equal? (make-probe 4 6 "north") (make-probe 4 6 "east"))  = #true
;; (probe-location-equal? (make-probe 4 6 "north") (make-probe 4 4 "north")) = #false
;; STRATEGY: Cases
(define (probe-location-equal? pb1 pb2)
  (cond
    [(and (equal? (probe-x pb1) (probe-x pb2)) (equal? (probe-y pb1) (probe-y pb2)))
            #true]
    [else   #false]))
;; TESTS:
(begin-for-test
  (check-equal? (probe-location-equal? (make-probe 4 6 "north") (make-probe 4 6 "east"))  #true "Both probes are centered at same position")
  (check-equal? (probe-location-equal? (make-probe 4 6 "north") (make-probe 4 4 "north")) #false "Both probes are not centered at same position"))

;; probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
;; GIVEN:   Two probes and a distance
;; RETURNS: true iff the first probe, given a move-forward command with
;; the specified number of steps, could wind up in the state described by
;; the second probe.
;; EXAMPLE:
;; (probe-forward-possible-outcome? (make-probe 3 0 N) 4 (make-probe 3 4 W) = #false)
;; STRATEGY: Combine simple functions
(define (probe-forward-possible-outcome? pb1 distance pb2)
  (cond
    [(probe-direction-equal? pb1 pb2) (can-probes-meet? pb1 distance pb2)]
    [ else #false]))
;;TEST:
(begin-for-test
  (check-equal? (probe-forward-possible-outcome? (make-probe 3 0 N) 4 (make-probe 3 4 W))  #false "Probe1 cannot end up at same position as Probe2")
  (check-equal? (probe-forward-possible-outcome? (make-probe 3 4 N) 4 (make-probe 3 0 N))  #true "Probe1 can end up at same position as Probe2 by moving equal to distance"))

;; can-probes-meet? : Probe PosInt Probe -> Boolean
;; GIVEN: Two probes in same direction and a distance
;; RETURNS: true iff the first probe given a move forware command with number of steps
;; could wind up in the state described by the second probe
;; EXAMPLE:
;; (can-probes-meet? (make-probe 3 -4 N) 2 (make-probe 3 -7 N)) = #true
;; STRATEGY: Cases
(define (can-probes-meet? pb1 distance pb2)
  (cond
    [(equal? (probe-direction pb1) N) (can-probes-meet-in-north? pb1 distance pb2) ]
    [(equal? (probe-direction pb1) S) (can-probes-meet-in-south? pb1 distance pb2) ]
    [(equal? (probe-direction pb1) E) (can-probes-meet-in-east? pb1 distance pb2) ]
    [(equal? (probe-direction pb1) W) (can-probes-meet-in-west? pb1 distance pb2) ]))
;;TEST:
(begin-for-test
  (check-equal? (can-probes-meet? (make-probe 3 -4 N) 2 (make-probe 3 -7 N))  #true  "Probes can meet in North direction")
  (check-equal? (can-probes-meet? (make-probe 3 -4 W) 2 (make-probe 3 -7 E))  #false "Probes can't meet in West direction")
  (check-equal? (can-probes-meet? (make-probe 3 -4 S) 2 (make-probe 3 -7 S))  #false "Probes can't meet in South direction")
  (check-equal? (can-probes-meet? (make-probe 3 -4 E) 2 (make-probe 3 -7 E))  #false "Probes can't meet in East direction"))

;; can-probes-meet-in-north? : Probe PosInt Probe -> Boolean
;; GIVEN:   Two probes in North direction and a distance
;; RETURNS: true iff the first probe given a move forware command with number of
;; steps could wind up in the state descirbed by the second probe
;; EXAMPLE:
;; (can-probes-meet-in-north? (make-probe 3 -4 N) 2 (make-probe 3 -7 N)) = #true
;; STRATEGY: Combine simple functions
(define (can-probes-meet-in-north? pb1 distance pb2)
  (cond
    [(equal? (- (probe-y pb1) distance)       (probe-y pb2)) #true]
    [(equal? (- (probe-y pb1) (- distance 1)) (probe-y pb2)) #true]
    [(equal? (- (probe-y pb1) (+ distance 1)) (probe-y pb2)) #true]
    [ else #false]))
;;TEST:
(begin-for-test
  (check-equal? (can-probes-meet-in-north? (make-probe 3 -4 N) 2 (make-probe 3 -7 N))  #true "Probe1 moved one step more than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-north? (make-probe 3 -4 N) 2 (make-probe 3 -6 N))  #true "Probe1 moved equal to distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-north? (make-probe 3 -4 N) 2 (make-probe 3 -5 N))  #true "Probe1 moved one step less than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-north? (make-probe 3 -4 N) 2 (make-probe 3 -9 N))  #false "Probe1 can't end up at same position as Probe2"))

;; can-probes-meet-in-south? : Probe PosInt Probe -> Boolean
;; GIVEN:   Two probes in South direction and a distance
;; RETURNS: true iff the first probe given a move forware command with number of
;; steps could wind up in the state descirbed by the second probe
;; EXAMPLE:
;; (can-probes-meet-in-south? (make-probe 3 -4 S) 2 (make-probe 3 -1 S)) = #true
;; STRATEGY: Combine simple functions
(define (can-probes-meet-in-south? pb1 distance pb2)
  (cond
    [(equal? (+ (probe-y pb1) distance)       (probe-y pb2)) #true]
    [(equal? (+ (probe-y pb1) (- distance 1)) (probe-y pb2)) #true]
    [(equal? (+ (probe-y pb1) (+ distance 1)) (probe-y pb2)) #true]
    [ else #false]))
;;TEST:
(begin-for-test
  (check-equal? (can-probes-meet-in-south? (make-probe 3 -4 S) 2 (make-probe 3 -1 S))  #true "Probe1 moved one step more than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-south? (make-probe 3 -4 S) 2 (make-probe 3 -2 S))  #true "Probe1 moved equal to distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-south? (make-probe 3 -4 S) 2 (make-probe 3 -3 S))  #true "Probe1 moved one step less than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-south? (make-probe 3 -4 S) 2 (make-probe 3 -6 S))  #false "Probe1 can't end up at same position as Probe2"))

;; can-probes-meet-in-east? : Probe PosInt Probe -> Boolean
;; GIVEN:   Two probes in East direction and a distance
;; RETURNS: true iff the first probe given a move forware command with number of
;; steps could wind up in the state descirbed by the second probe
;; EXAMPLE:
;; (can-probes-meet-in-east? (make-probe 3 -4 E) 2 (make-probe 4 -4 E)) = #true
;; STRATEGY: Combine simple functions
(define (can-probes-meet-in-east? pb1 distance pb2)
  (cond
    [(equal? (+ (probe-x pb1) distance)       (probe-x pb2)) #true]
    [(equal? (+ (probe-x pb1) (- distance 1)) (probe-x pb2)) #true]
    [(equal? (+ (probe-x pb1) (+ distance 1)) (probe-x pb2)) #true]
    [ else #false]))
;;TEST:
(begin-for-test
  (check-equal? (can-probes-meet-in-east? (make-probe 3 -4 E) 2 (make-probe 4 -4 E))  #true "Probe1 moved one step less than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-east? (make-probe 3 -4 E) 2 (make-probe 5 -4 E))  #true "Probe1 moved equal to distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-east? (make-probe 3 -4 E) 2 (make-probe 6 -4 E))  #true "Probe1 moved one step more than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-east? (make-probe 3 -4 E) 2 (make-probe 2 -4 E))  #false "Probe1 can't end up at same position as Probe2"))

;; can-probes-meet-in-west? : Probe PosInt Probe -> Boolean
;; GIVEN:   Two probes in West direction and a distance
;; RETURNS: true iff the first probe given a move forware command with number of
;; steps could wind up in the state descirbed by the second probe
;; EXAMPLE:
;; (can-probes-meet-in-west? (make-probe 3 -4 W) 2 (make-probe 0 -4 W)) = #true
;; STRATEGY: Combine simple functions
(define (can-probes-meet-in-west? pb1 distance pb2)
  (cond
    [(equal? (- (probe-x pb1) distance)       (probe-x pb2)) #true]
    [(equal? (- (probe-x pb1) (- distance 1)) (probe-x pb2)) #true]
    [(equal? (- (probe-x pb1) (+ distance 1)) (probe-x pb2)) #true]
    [ else #false]))
;;TEST:
(begin-for-test
  (check-equal? (can-probes-meet-in-west? (make-probe 3 -4 W) 2 (make-probe 0 -4 W))  #true "Probe1 moved one step more than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-west? (make-probe 3 -4 W) 2 (make-probe 1 -4 W))  #true "Probe1 moved equal to distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-west? (make-probe 3 -4 W) 2 (make-probe 2 -4 W))  #true "Probe1 moved one step less than distance which is same position as Probe2")
  (check-equal? (can-probes-meet-in-west? (make-probe 3 -4 W) 2 (make-probe 5 -4 W))  #false "Probe1 can't wind up at same position as Probe2"))


                                                                      
