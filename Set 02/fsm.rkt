;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname fsm) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Design set of function to illustrate working of Finite State Machine

(require rackunit)
(require "extras.rkt")
(check-location "02" "fsm.rkt")
(require 2htdp/universe)
(provide
  initial-state
  next-state
  accepting-state?
  error-state?
  )

;; DATA DEFINITIONS:
;; a MachineInput is one of
;; -- "q"
;; -- "x"
;; -- "u"
;; -- "a"
;; -- "b"
;; -- "d"
;; -- "e"
;; -- "f"
;; INTERPRETATION : self-evident

;; TEMPLATE:
;; machine-input-fn : MachineInput -> ??
; (define (machine-input-fn input)
;   (cond
;     [(stirng=? input "q")...]
;     [(stirng=? input "x")...]
;     [(stirng=? input "u")...]
;     [(stirng=? input "a")...]
;     [(stirng=? input "b")...]
;     [(stirng=? input "d")...]
;     [(stirng=? input "e")...]
;     [(stirng=? input "f")...]))

;; DATA DEFINITIONS:
;; a State is one of
;; -- "S0"
;; -- "S1"
;; -- "S2"
;; -- "S3"
;; -- "ErrorState"
;; INTERPRETATION :
;; "S0" is the initial state.Any machine input is first run here.There can be infinite number of consecutive q or x
;;     and "S0" will remain current machine state.
;; "S1" is the state we arrive from initial state if input is u.There can be infinite number of "u" machine inputs
;;     and "S1" will remain current mahcine state.
;; "S2" is the state we arrive from "S0","S1" if machine input is "a" or "b". There can be infinte number of consecutive "a" or "b" machine inputs
;;     and "S2" will remain current mahcine state.
;; "S3" is the final accepting state, we arrive from "S0","S1","S2" if machine input is "d". There can be infinte number of "e" or "f" machine inputs
;;     and "S3" will remain current machine state
;; "ErrorState" is the state we arrive when any invalid machine input is entered on an accepting state
;; Examples:
(define state0 "S0")
(define state1 "S1")
(define state2 "S2")
(define state3 "S3")
(define stateE "ErrorState")

;; TEMPLATE:
;; state-fn : State -> ??
;  (define (state-fn state)
;     (cond
;       [(string=? state "S0")...]
;       [(string=? state "S1")...]
;       [(string=? state "S2")...]
;       [(string=? state "S3")...]
;       [(string=? state "ErrorState")...]))

;; initial-state : Number -> State
;; GIVEN:   a number
;; RETURNS: a representation of the initial state
;; of machine. The given number is ignored
;; (initial-state 15) = state0
;; (initial-state 39) = state0
;; STRATEGY: Simple
(define (initial-state num)
  state0)
;; TESTS:
(begin-for-test
  (check-equal? (initial-state 15) state0 "initial state should be state0")
  (check-equal? (initial-state 39) state0 "initial state should be state0"))

;; next-state: State MachineInput -> State
;; GIVEN: a state of the machine and a machine input
;; RETURNS: the state that should the follow the given input
;; (next-state state0 "u") = state1
;; (next-state state1 "a") = state2
;; (next-state state3 "u") = stateE
;; (next-state stateE "u") = stateE
;; STRATEGY: Using State template
(define (next-state st input)
  (cond
    [(string=? st state0) (next-state0-input input)]
    [(string=? st state1) (next-state1-input input)]
    [(string=? st state2) (next-state2-input input)]
    [(string=? st state3) (next-state3-input input)]
    [(string=? st stateE) stateE]))
;; TESTS:
(begin-for-test
  (check-equal? (next-state state0 "u") state1 "next state should be state1")
  (check-equal? (next-state state1 "a") state2 "next state should be state2")
  (check-equal? (next-state state3 "u") stateE "next state should be stateE")
  (check-equal? (next-state state2 "d") state3 "next state should be state3")
  (check-equal? (next-state stateE "u") stateE "next state should be stateE"))

;; next-state0-input : MachineInput -> State
;; GIVEN: a machine input
;; RETURNS: the state that should the follow the given input on state 0
;; (next-state0-input "q") = state0
;; (next-state0-input "e") = stateE
;; STRATEGY:  Using MachineInput template
(define (next-state0-input input)
  (cond
    [(string=? input "q") state0]
    [(string=? input "x") state0]
    [(string=? input "u") state1]
    [(string=? input "a") state2]
    [(string=? input "b") state2]
    [(string=? input "d") state3]
    [else stateE]))
;; TESTS:
(begin-for-test
  (check-equal? (next-state0-input "q") state0 "next state should be state0")
  (check-equal? (next-state0-input "x") state0 "next state should be state0")
  (check-equal? (next-state0-input "u") state1 "next state should be state1")
  (check-equal? (next-state0-input "a") state2 "next state should be state2")
  (check-equal? (next-state0-input "b") state2 "next state should be state2")
  (check-equal? (next-state0-input "d") state3 "next state should be state3")
  (check-equal? (next-state0-input "e") stateE "next state should be stateE"))

;; next-state1-input : MachineInput -> State
;; GIVEN: a machine input
;; RETURNS: the state that should the follow the given input on state 1
;; (next-state1-input "u") = state1
;; (next-state1-input "e") = stateE
;; STRATEGY: Using MachineInput template
(define (next-state1-input input)
  (cond
    [(string=? input "u") state1]
    [(string=? input "a") state2]
    [(string=? input "b") state2]
    [(string=? input "d") state3]
    [else stateE]))
;; TESTS:
(begin-for-test
  (check-equal? (next-state1-input "u") state1 "next state should be state1")
  (check-equal? (next-state1-input "a") state2 "next state should be state2")
  (check-equal? (next-state1-input "b") state2 "next state should be state2")
  (check-equal? (next-state1-input "d") state3 "next state should be state3")
  (check-equal? (next-state1-input "e") stateE "next state should be stateE"))

;; next-state2-input : MachineInput -> State
;; GIVEN: a machine input
;; RETURNS: the state that should the follow the given input on state 2
;; (next-state2-input "a") = state2
;; (next-state2-input "e") = stateE
;; STRATEGY:  Using MachineInput template
(define (next-state2-input input)
  (cond
    [(string=? input "a") state2]
    [(string=? input "b") state2]
    [(string=? input "d") state3]
    [ else stateE]))
;; TESTS:
(begin-for-test
  (check-equal? (next-state2-input "a") state2 "next state should be state2")
  (check-equal? (next-state2-input "b") state2 "next state should be state2")
  (check-equal? (next-state2-input "d") state3 "next state should be state3")
  (check-equal? (next-state2-input "e") stateE "next state should be stateE"))

;; next-state3-input: MachineInput -> State
;; GIVEN: a machine input
;; RETURNS: the state that should the follow the given input on state 3
;; (next-state3-input "d") = stateE
;; (next-state3-input "e") = state3
;; STRATEGY:  Using MachineInput template
(define (next-state3-input input)
  (cond
    [(string=? input "e") state3]
    [(string=? input "f") state3]
    [ else stateE]))
;; TESTS:
(begin-for-test
  (check-equal? (next-state3-input "d") stateE "next state should be stateE")
  (check-equal? (next-state3-input "e") state3 "next state should be state3")
  (check-equal? (next-state3-input "f") state3 "next state should be state3"))

;; accepting-state? : State -> boolean
;; GIVEN: a machine state
;; RETURNS: true iff the given state is a final (accepting) state
;; (accepting-state? state0) = #false
;; (accepting-state? state2) = #false
;; (accepting-state? state3) = #true
;; STRATEGY: if-else conditional statement
(define (accepting-state? state)
  (if (string=? state state3)
      #true
   #false))
;; TESTS:
(begin-for-test
  (check-equal? (accepting-state? state0) #false "state0 is not an final accepting state")
  (check-equal? (accepting-state? state2) #false "state2 is not an final accepting state")
  (check-equal? (accepting-state? state3) #true  "state3 is an final accepting state"))

;; error-state? : State -> boolean
;; GIVEN: a machine state
;; RETURNS: true iff there is no path (empty or non-empty) from the given
;; state to an accepting state
;; (error-state? state0) = #false
;; (error-state? state4) = #false
;; (error-state? stateE) = #true
;; STRATEGY: if-else conditional statement
(define (error-state? state)
  (if (string=? state stateE)
      #true
   #false))
;; TESTS:
(begin-for-test
  (check-equal? (error-state? state0) #false "state0 is not an error state")
  (check-equal? (error-state? state3) #false "state4 is not an error state")
  (check-equal? (error-state? stateE) #true  "stateE is an error state"))

