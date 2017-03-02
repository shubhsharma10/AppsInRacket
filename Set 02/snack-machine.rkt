;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname snack-machine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Design set of function to illustrate working of Vending machine

(require rackunit)
(require "extras.rkt")
(check-location "02" "snack-machine.rkt")
(require 2htdp/universe)

(provide
  initial-machine
  machine-next-state
  machine-output
  machine-remaining-kale
  machine-remaining-carrots
  machine-bank
  )

;; DATA DEFINITION:
;; a CustomerInput is one of
;; -- a PosInt
;; -- "kale"
;; -- "carrots"
;; -- "change"
;; INTERPRETATION :
;; PosInt    implies number of quarters inserted
;; "kale"    implies requesting a bag of kale chips
;; "carrots" implies requesting a bag of carrots sticks
;; "change"  implies request to return all unspent money customer has inserted

;; TEMPLATE
;; customer-input-fn : CustomerInput -> ??
; (define (customer-input-fn input)
;   (cond
;     [(equal? input KALE)...]
;     [(equal? input CARROTS)...]
;     [(equal? input CHANGE)...]
;     [ else ...]))

;; DATA DEFINITION:
;; a MachineOutput is one of
;; -- "kale"
;; -- "carrots"
;; -- "Out of Item"
;; -- a PosInt
;; -- "Nothing"
;; INTERPRETATION :
;; "kale"        implies machine dispenses a bag of kale chips.
;; "carrots"     implies machine dispenses a bag of carrot sticks.
;; "Out of Item" implies machine displays "Out of Item".
;; PosInt        implies machine releases specified number of quarters.
;; "Nothing"     implies machine does nothing.
(define KALE "kale")
(define CARROTS "carrots")
(define OOI "Out of Item")
(define NON "Nothing")
(define CHANGE "Change")
(define KALE-COST 75)
(define CARROT-COST 50)
(define QUARTER-VALUE 25)

;; TEMPLATE:
;; machine-output-fn : MachineOutput -> ??
;  (define (machine-output-fn output)
;     (cond
;       [(equal? output KALE)...]
;       [(equal? output CARROTS)...]
;       [(equal? output OOI)...]
;       [(equal? output NON)...]
;       [ else ...]))

(define-struct machine-state (kale carrot bank-money change-money))

;; a MachineState is a (make-machine-state int int int int)
;; kale    is the number of bags of kale chips
;; carrot  is the number of bags of carrot sticks
;; bank-money   is the number of cents present in bank
;; change-money is the number of cents present in bank while customer is making a purchase


;; machine-state-fn : MachineState -> ??
(define (machine-state-fn state)
  (... (machine-state-kale state) (machine-state-carrot state)
       (machine-state-bank-money state) (machine-state-change-money state)))


;; initial-machine : NonNegInt NonNegInt -> MachineState
;; GIVEN: a number of bags of kale chips and carrot sticks
;; RETURNS: the state of a machine loaded with the given numbers of bags
;; of kale chips and carrot sticks, with an empty bank.
;; (initial-machine 15 20) = (make-machine-state 15 20 0 0)
;; (initial-machine 30 20) = (make-machine-state 30 20 0 0)
;; STRATEGY: Using MachineState constructor template
(define (initial-machine kale carrot)
  (make-machine-state kale carrot 0 0))
;; TESTS:
(begin-for-test
  (check-equal? (initial-machine 15 20) (make-machine-state 15 20 0 0))
  (check-equal? (initial-machine 30 20) (make-machine-state 30 20 0 0)))


;; machine-next-state : MachineState CustomerInput -> MachineState
;; GIVEN: a machine state and a customer input
;; RETURNS: the state of the machine that should follow the customer's
;; input
;; EXAMPLES:
;; (machine-next-state (make-machine-state 15 20 0 0) 2)     = (make-machine-state 15 20 0 50)
;; (machine-next-state (make-machine-state 15 20 0 75) KALE) = (make-machine-state 14 20 75 0)
;; STRATEGY : Using CustomerInput observer template
(define (machine-next-state state input)
  (cond
    [(equal? input KALE)    (handle-kale-input state)]
    [(equal? input CARROTS) (handle-carrots-input state)]
    [(equal? input CHANGE)  (handle-change-input state)]
    [ else (handle-quarters-inserted state input)]))
;; TESTS:
(begin-for-test
  (check-equal? (machine-next-state (make-machine-state 15 20 0 0) 2) (make-machine-state 15 20 0 50) "On entering quarters only change in resultant machine state would
                                                                                                       be change in change money")
  (check-equal? (machine-next-state (make-machine-state 15 20 0 75) KALE) (make-machine-state 14 20 75 0) "On kale input, one bag of kale is dispensed and money has moved
                                                                                                        from change to bank")
  (check-equal? (machine-next-state (make-machine-state 15 20 50 100) CARROTS) (make-machine-state 15 19 100 50) "On carrot input, one bag of carrot sticks is dispensed and money has moved
                                                                                                        from change to bank")
  (check-equal? (machine-next-state (make-machine-state 15 20 100 150) CHANGE) (make-machine-state 15 20 100 0) "On change input, all change moeny is dispensed"))

;; handle-quarters-inserted : MachineState PosInt -> MachineState
;; GIVEN: a machine state and number of quarters inserted
;; RETURNS: the state of the machine that should follow after customer inserts quarters
;; EXAMPLE:
;; (handle-quarters-inserted (make-machine-state 5 0 100 25) 4) = (make-machine-state 5 0 100 125)
;; STRATEGY : Using MachineState observer template
(define (handle-quarters-inserted state input)
  (make-machine-state (machine-state-kale state)
                     (machine-state-carrot state)
                     (machine-state-bank-money state)
                     (+ (machine-state-change-money state) (* QUARTER-VALUE input))))

;; handle-change-input : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: the state of the machine after change is dispensed from machine if available
;; EXAMPLE:
;; (handle-change-input (make-machine-state 5 0 100 25) "change") = (make-machine-state 5 0 100 0)
;; STRATEGY : Using MachineState observer template
(define (handle-change-input state)
  (make-machine-state (machine-state-kale state)
                     (machine-state-carrot state)
                     (machine-state-bank-money state)
                     0))

;; has-enough-kale? : MachineState -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff machine has more or equal than 1 bag of kale chips
;; EXAMPLE:
;; (has-enough-kale? (make-machine-state 5 0 100 25)) = #true
;; STRATEGY: Using MachineState observer template
(define (has-enough-kale? state)
  (>= (machine-state-kale state) 1))

;; has-enough-money-for-kale? : MachineState -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff customer has inserted money to buy more or equal than 1 bag of kale chips
;; EXAMPLE:
;; (has-enough-money-for-kale? (make-machine-state 5 0 100 100)) = #true
;; STRATEGY: Using MachineState observer template
(define (has-enough-money-for-kale? state)
  (>= (machine-state-change-money state) KALE-COST))

;; has-enough-carrot? : MachineState -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff machine has more or equal than 1 bag of carrot sticks
;; EXAMPLE:
;; (has-enough-carrot? (make-machine-state 5 0 100 25)) = #false
;; STRATEGY: Using MachineState observer template
(define (has-enough-carrot? state)
  (>= (machine-state-carrot state) 1))

;; has-enough-money-for-carrot? : MachineState -> Boolean
;; GIVEN: a machine state
;; RETURNS: true iff customer has inserted money to buy more or equal than 1 bag of carrot sticks
;; EXAMPLE:
;; (has-enough-money-for-carrot? (make-machine-state 5 2 100 25)) = #false
;; STRATEGY: Using MachineState observer template
(define (has-enough-money-for-carrot? state)
  (>= (machine-state-change-money state) CARROT-COST))

;; handle-kale-input : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: MachineState for each possible pair of whether customer has put in enough money and machine has enough kale or not
;; EXAMPLES:
;;(handle-kale-input (make-machine-state 10 15 0 50)) = (make-machine-state 10 15 0 50)
;; STRATEGY : Using cases
(define (handle-kale-input state)  
  (cond
    [(and (has-enough-kale? state) (has-enough-money-for-kale? state))
                  (make-machine-state (- (machine-state-kale state) 1)
                                      (machine-state-carrot state)
                                      (+ (machine-state-bank-money state) KALE-COST)
                                      (- (machine-state-change-money state) KALE-COST))]
    [else state]))
;; TESTS:
(begin-for-test
  (check-equal? (handle-kale-input (make-machine-state 10 15 0 50)) (make-machine-state 10 15 0 50) "As money entered by user is less than price
                                                                   of kale, so no change in MachineState occured"))

;; handle-carrots-input : MachineState -> MachineState
;; GIVEN: a machine state
;; RETURNS: MachineState for each possible pair of whether customer has put in enough money and machine has enough carrot or not
;; EXAMPLES:
;;(handle-carrots-input (make-machine-state 10 0 0 50)) = (make-machine-state 10 0 0 50)
;; STRATEGY : Using cases
(define (handle-carrots-input state)  
  (cond
    [(and (has-enough-carrot? state) (has-enough-money-for-carrot? state))
                  (make-machine-state (machine-state-kale state)
                                      (- (machine-state-carrot state) 1)
                                      (+ (machine-state-bank-money state) CARROT-COST)
                                      (- (machine-state-change-money state) CARROT-COST))]
    
    [else state]))
;; TESTS:
(begin-for-test
  (check-equal? (handle-carrots-input (make-machine-state 10 0 0 50)) (make-machine-state 10 0 0 50) "As machine doesn't have enough carrot to dispense
                                                                     , so no change in MachineState occured"))
  
;; machine-output : MachineState CustomerInput -> MachineOutput
;; GIVEN: a machine state and a customer input
;; RETURNS: a MachineOutput that describes the machine's response to the
;; customer input
;; EXAMPLE:
;; (machine-output (make-machine-state 2 3 0 75) KALE) = KALE
;; (machine-output (make-machine-state 2 3 0 75) CARROTS) = CARROTS
;; (machine-output (make-machine-state 2 3 0 75) CHANGE) = 3
;; (machine-output (make-machine-state 2 3 0 50) KALE) = NON
;; (machine-output (make-machine-state 2 3 0 50) 3) = NON
(define (machine-output state input)
  (cond
    [(equal? input KALE)    (output-for-kale state )]
    [(equal? input CARROTS) (output-for-carrot state)]
    [(equal? input CHANGE)  (output-for-change state)]
    [ else NON]))
;; TESTS:
(begin-for-test
 (check-equal? (machine-output (make-machine-state 2 3 0 75) KALE) KALE "Machine can dispense bag of kale chips, So output kale")
 (check-equal? (machine-output (make-machine-state 2 3 0 75) CARROTS) CARROTS "Machine can dispense bag of carrot sticks, so output carrots")
 (check-equal? (machine-output (make-machine-state 2 3 0 75) CHANGE) 3 "Machine can dispense 3 quarters")
 (check-equal? (machine-output (make-machine-state 2 3 0 50) KALE) NON "Machine can't dispense bag of kale chips because of not enough money, so output nothing")
 (check-equal? (machine-output (make-machine-state 2 3 0 50) 3) NON) "On entering quarters, output is nothing")

;; output-for-kale : MachineState -> String
;; GIVEN: MachineState
;; RETURNS: Machine's response if kale was input at MachineState
;; EXAMPLE:
;; (output-for-kale (make-machine-state 0 3 0 75)) = "Out of Item"
;; STRATEGY: Using cases
(define (output-for-kale state)
    (cond
    [(and (has-enough-kale? state) (has-enough-money-for-kale? state))
                  KALE]
    [(and (has-enough-kale? state) (not (has-enough-money-for-kale? state)))
                  NON]
    [ else OOI ]))
;; TESTS:
(begin-for-test
  (check-equal? (output-for-kale (make-machine-state 0 3 0 75)) OOI "Machine doesn't have bag of kale chips, so output Out of Item"))

;; output-for-carrot : MachineState -> String
;; GIVEN: MachineState
;; RETURNS: Machine's response if carrots was input at MachineState
;; EXAMPLE:
;; (output-for-carrot (make-machine-state 5 3 0 25)) = "Nothing"
;; (output-for-carrot (make-machine-state 5 0 0 50)) = "Out of Item"
;; STRATEGY: Using cases
(define (output-for-carrot state)  
  (cond
    [(and (has-enough-carrot? state) (has-enough-money-for-carrot? state))
                 CARROTS]
    
    [(and (has-enough-carrot? state) (not (has-enough-money-for-carrot? state)))
                  NON]
    
    [else OOI]))
;; TESTS:
(begin-for-test
  (check-equal? (output-for-carrot (make-machine-state 5 3 0 25)) "Nothing" "Machine didn't have enough money to dispense bag of carrot sticks
                                                                             , so output message is nothing")
  (check-equal? (output-for-carrot (make-machine-state 5 0 0 50)) "Out of Item" "Machine has zero bag of carrot sticks, so output message Out of Item"))

;; output-for-change : MachineState -> PosInt
;; GIVEN: a machine state
;; RETURNS: number of quarters present in change in machine
;; EXAMPLE:
;; (output-for-change (make-machine-state 10 15 100 75)) = 3
;; STRATEGY: Using observer template of MachineState
(define (output-for-change state)
  (/ (machine-state-change-money state) QUARTER-VALUE))
;; TESTS:
(begin-for-test
  (check-equal? (output-for-change (make-machine-state 10 15 100 75)) 3 "Machine has 3 quarters in change money"))

;; machine-remaining-kale : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of kale chips left in the machine
;; EXAMPLE:
;; (machine-remaining-kale (make-machine-state 10 15 100 25)) = 10
;; STRATEGY: Using observer template of MachineState
(define (machine-remaining-kale state)
  (machine-state-kale state))
;; TESTS:
(begin-for-test
  (check-equal? (machine-remaining-kale (make-machine-state 10 15 100 25)) 10 "Machine has 10 bags of kale chips left"))

;; machine-remaining-carrots : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the number of bags of carrots left in the machine
;; EXAMPLE:
;; (machine-remaining-carrots (make-machine-state 10 15 100 25)) = 15
;; STRATEGY: Using observer template of MachineState
(define (machine-remaining-carrots state)
  (machine-state-carrot state))
;; TESTS:
(begin-for-test
  (check-equal? (machine-remaining-carrots (make-machine-state 10 15 100 25)) 15 "Machine has 15 bags of carrot sticks left"))

;; machine-bank : MachineState -> NonNegInt
;; GIVEN: a machine state
;; RETURNS: the amount of money in the machine's bank, in cents
;; EXAMPLE:
;; (make-machine-state 10 15 100 25) = 100
;; STRATEGY: Using observer template of MachineState
(define (machine-bank state)
  (machine-state-bank-money state) )
;; TESTS:
(begin-for-test
  (check-equal? (machine-bank (make-machine-state 10 15 100 25)) 100 "Bank has 100 cents"))
