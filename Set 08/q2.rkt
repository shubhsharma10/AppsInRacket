;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Resolution Theorem Proving

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(check-location "08" "q2.rkt")

(provide
 make-clause
 make-pos
 make-neg
 is-null-derivable?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DATA DEFINITIONS:

;;A Variable is a Racket Symbol.

(define-struct pos (var))
;; A Pos literal is a (make-pos Symbol)
(define-struct neg (var))
;; A Neg literal is a (make-neg Symbol)

;; TEMPLATE:
;; pos-fn : Pos -> ??
;; (define (pos-fn lit)
;;   (....(pos-var lit))

;; neg-fn : Neg -> ??
;; (define (neg-fn lit)
;;   (....(neg-var lit))

;; A Literal is one of
;; -- (make-pos Symbol)  Interp: a literal containing the variable
;; -- (make-neg Symbol)  Interp: a literal containing the negation of
;;                            the variable


;; A Clause is a set of literals
;; Clause
;; -- SetOfLiterals

;; TEMPLATE:
;; clause-fn : Clause -> ??
;; (define (clause-fn cl)
;; (cond
;;   [(empty? cl) ...]
;;   [else (...(first cl)
;;         ....(rest cl))]))

;; SetOfX is a list of X's without duplication.
;; X can be of any data type here

;; A SetOfX is one of
;; -- empty
;; -- (cons X SetOfX)

;; TEMPLATE:
;; sox-fn : SOX -> ??
;; (define (sox-fn sox)
;;   (cond
;;      [(empty? sox) ...]
;;       [ else (...
;;               (x-fn (first sox))
;;               (sox-fn (rest sox)))]))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-clause : ListOfLiteral -> Clause
;; GIVEN: a list of literals, possibly with duplications
;; RETURNS: a clause containing exactly those literals
;; STRATEGY: Use foldr HOF on lol
(define (make-clause lol)
  (foldr
   ; Literal SetOfLiteral -> SetOfLiteral
   ; Returns SetOfLiteral
   (lambda (x accum) (set-cons x accum)) empty lol))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CONSTANTS:

(define EMPTY-SYMBOL 'empty-symbol)

(define C1
  (make-clause (list (make-pos 'a)
                     (make-neg 'b)
                     (make-pos 'c))))

(define C2
  (make-clause (list (make-pos 'd)
                     (make-pos 'b))))

(define C3
  (make-clause (list (make-neg 'a)
                     (make-pos 'c))))

(define C4 (make-clause (list (make-pos 'b))))

(define C5 (make-clause (list (make-neg 'c))))

(define C6 (make-clause (list (make-pos 'd) (make-neg 'b) (make-pos 'a)
                              (make-neg 'c))))

(define C7 (make-clause (list (make-pos 'a))))

(define C8 (make-clause (list (make-neg 'a))))

(define SAMPLE-INPUT
  (list (list (make-pos 'p1) (make-pos 'p2))
        (list (make-neg 'p1)) (list (make-neg 'p2))))

(define INPUT1
  (list C1 C2 C3 C4 C5))

(define INPUT2
  (list C6 C2 C3 C1 C5 C4))

(define INPUT3
  (list C6 C2 C3 C1 C3 C5 C4 C6))

(define INPUT6
  (list C3 C5 C6 C2 C3 C7 C5 C3 C8))

(define INPUT4
  (list (make-clause (list (make-pos 'a) (make-neg 'b)))
        (make-clause (list (make-neg 'a) (make-pos 'b)))))

(define INPUT5
  (list (make-clause (list (make-neg 'a) (make-neg 'b)))
        (make-clause (list (make-pos 'a) (make-pos 'b)))))

(define INPUT7
  (list (make-clause (list (make-pos 'a) (make-neg 'b)))
        (make-clause (list (make-neg 'a)))
        (make-clause (list (make-pos 'a) (make-pos 'b)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS:

;; is-null-derivable? : ListOfClause -> Boolean
;; GIVEN: a list of clauses
;; RETURNS: true iff the empty clause is derivable from the given
;; clauses using the rule of resolution as given above.
;; EXAMPLE: (is-null-derivable? test)
;;            #t
;; STRATEGY: Combine simple functions
(define (is-null-derivable? loc)
  (is-empty-set? (list-to-set loc) empty ))

;; list-to-set ; ListOfClause -> SetOfClause
;; GIVEN: A list of clause
;; RETURNS: Set of clause i.e. list with not duplication
;; EXAMPLE: (list-to-set SAMPLE-INPUT)= (list (list (make-pos 'p1)
; (make-pos 'p2)) (list (make-neg 'p1)) (list (make-neg 'p2)))
;; STRATEGY: Use foldr HOF on loc
(define (list-to-set loc)
  (foldr
   ; Clause SetOfClause -> SetOfClause
   ; Returns SetOfClause from ListOfClause
   (lambda (x accum) (if (has-clause-in-set? x accum)
                         accum
                         (cons x accum)))
   empty loc))

;; has-clause-in-set? : Clause SetOfClause -> Boolean
;; GIVEN: Clause and set of clause
;; RETURNS: True iff clause is present in set
;; EXAMPLE:(list (make-pos 'p1) (make-pos 'p2))=
; #false
;; STRATEGY: Use ormap HOF on soc
(define (has-clause-in-set? clause soc)
  (ormap
   ; Clause -> Boolean
   ; Returns true iff clause is same as clause passed as parameter
   (lambda (x) (set-equal? clause x)) soc))

;; is-empty-set? : SetOfClause SetOfClause -> Boolean
;; GIVEN: two set of clauses
;; RETURNS: True iff empty clause is derivable from soc
;; WHERE: used-soc is a set of clauses which have been used in resolution rule
;; EXAMPLE:(is-empty-set? (list (list (make-pos 'p1) (make-pos 'p2))
; (list (make-neg 'p1)) (list (make-neg 'p2))) empty)=#true
;; STRATEGY: Use ormap HOF on soc
(define (is-empty-set? soc used-soc)
  (cond
    [(empty? soc) #f]
    [else (ormap
           ; Clause -> Boolean
           ; Returns true iff clause can be derived to empty clause with some
           ; clause in soc
           (lambda (x) (is-rule-pair? x soc used-soc)) soc)]))

;; is-rule-pair? : Clause SetOfClause SetOfClause -> Boolean
;; GIVEN: clause and two list of clauses
;; RETURNS: True iff empty clause is derivable from c1 and difference between
;;         soc and used-soc
;; WHERE: used-soc is list of clauses which have been used in resolution rule
;; EXAMPLE: (is-rule-pair? (list (make-pos 'p1) (make-pos 'p2))
; (list (list (make-pos 'p1) (make-pos 'p2)) (list (make-neg 'p1))
; (list (make-pos 'p2))) empty)=#false
;; HALTING-MEASURE: length (set-diff soc used-soc) or found empty c1
;; STRATEGY: Use ormap HOF on difference between soc and used-soc
(define (is-rule-pair? c1 soc used-soc)
  (let([ soc1 (set-diff soc used-soc)])
    (cond
      [(empty? c1) #t]
      [(empty? soc1) #f]    
      [ else
        (ormap
         ; Clause -> Boolean
         ; Returns true iff x can be derived to empty clause with some set
         ; in soc1
         (lambda (x ) (check-for-rule-pair? c1 x soc used-soc)) soc1)])))

;; check-for-rule-pair? : Clause Clause SetOfClause SetOfClause -> Boolean
;; GIVEN: two clauses and two set of clauses
;; RETURNS: True iff empty clause is derivable from c1 and c2
;; WHERE: used-soc is set of clauses which have been used in reolution rule
;; EXAMPLE:(check-for-rule-pair? (list (make-pos 'p1) (make-pos 'p2))
;  (list (make-pos 'p1) (make-pos 'p2)) (list (list (make-pos 'p1)
; (make-pos 'p2)) (list (make-neg 'p1)) (list (make-pos 'p2))) empty)= #false
;; HALTING-MEASURE: When c1 and c2 can derive empty clause or
;;                 length (set-diff soc used-soc)
;; STRATEGY: Using cases on whether c1 and c2 can derive empty clause
(define (check-for-rule-pair? c1 c2 soc used-soc)
  (cond
    [(equal? (rule-applicable-on-clause c1 c2) EMPTY-SYMBOL) #f]
    [ else (is-rule-pair? (form-new-clause c1
                                           c2
                                           (rule-applicable-on-clause c1 c2))
                          soc
                          (cons c1 (cons c2 used-soc)))]))

;; rule-applicable-on-clause: Clause Clause -> Symbol
;; GIVEN: two clauses
;; RETURNS: Symbol iff clause has only one symbol's complement in other clause
;; STRATEGY: Using cases on length of complement list between clause1
;;           and clause2
;; EXAMPLE:(rule-applicable-on-clause (list (make-pos 'p1) (make-pos 'p2))
; (list (make-neg 'p1) (make-pos 'p2)))= 'p1
(define (rule-applicable-on-clause clause1 clause2)
  (cond
    [(or (empty? clause1)
         (empty? clause2)
         (empty? (complement-list clause1 clause2))
         (> (length (complement-list clause1 clause2)) 1))
     EMPTY-SYMBOL]
    [(= (length (complement-list clause1 clause2)) 1)
     (first (complement-list clause1 clause2))]))

;; form-new-clause: Clause Clause Symbol -> Clause
;; GIVEN: Two clauses and symbol which is present in one clause and other
;;        clause has its complement
;; RETURN: New clause after applying resolution rule
;; STRATEGY: Using cases on whether s1 or neg s1 is in c1
;; EXAMPLE:(form-new-clause (list (make-pos 'p1) (make-pos 'p2))
; (list (make-neg 'p1) (make-pos 'p2)) 'p1)=(list (make-pos 'p2))
(define (form-new-clause c1 c2 s1)
  (cond
    [(my-member? (make-pos s1) c1)
     (make-clause (set-union (set-minus c1 (make-pos s1))
                             (set-minus c2 (make-neg s1))))]
    [(my-member? (make-neg s1) c1)
     (make-clause (set-union (set-minus c1 (make-neg s1))
                             (set-minus c2 (make-pos s1))))]))

;; complement-list: Clause Clause -> SetOfSymbol
;; GIVEN: Two clauses
;; RETURNS: Set of complement symbole present between two clauses
;; STRATEGY: Use foldr HOF on clause1
;; EXAMPLE:(complement-list (list (make-pos 'p1) (make-pos 'p2))
; (list (make-neg 'p1) (make-pos 'p2)))=(list 'p1)
(define (complement-list clause1 clause2)
  (cond
    [(or (empty? clause1) (empty? clause2)) empty]
    [else  (foldr
            ; Literal SetOfSymbol -> SetOfSymbol
            ; Returns SetOfSymbols which are present as complement pair
            ; in clause1 and clause2
            (lambda (x accum) (combine-complements-in-clause x accum clause2))
            empty clause1)]))

;; combine-complements-in-clause : Literal SetOfSymbol Clause -> SetOfSymbol
;; GIVEN: a literal, set of symbol and a clause
;; RETURNS: SetOfSymbol with literal converted to symbol if literal's
;;          complement is present in clause2
;; STRATEGY: Combine simple function
;; EXAMPLE: (combine-complements-in-clause (make-pos 'p1) empty
; (list (make-neg 'p1) (make-pos 'p2)))= (list 'p1)
(define (combine-complements-in-clause lit accum clause2)
  (let([symbol-found (complement-present-for-literal lit clause2 (pos? lit))])
    (if(equal?  symbol-found EMPTY-SYMBOL)
       accum
       (cons symbol-found accum))))

;; complement-present-for-literal: Literal Clause Boolean -> Symbol
;; GIVEN: a literal, a clause and boolean identifying whether literal was pos
;;        or not
;; RETURNS: literal as symbol if its complement is present in clause
;; STRATEGY: Using Template of literal on lit
;; EXAMPLE: (complement-present-for-literal (make-pos 'p1)
; (list (make-neg 'p1) (make-pos 'p2)) #t)='p1
(define (complement-present-for-literal lit clause2 pos?)
  (cond
    [(and pos? (my-member? (make-neg (pos-var lit)) clause2))
     (pos-var lit)]
    [(and (not pos?) (my-member? (make-pos (neg-var lit)) clause2))
     (neg-var lit)]
    [else EMPTY-SYMBOL]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRESS BENCHMARK

;;; Note: The original version of this benchmark defined a function
;;; that didn't satisfy its contract.  That function was not needed
;;; by the benchmark and has been removed.  I have also added a call
;;; to make-clause.

;;; make-stress-input-sat : NonNegInt -> ListOfClause
;;; GIVEN: an integer n
;;; RETURNS: a satisfiable set of clauses of length n
;;; EXAMPLES:
;;;     (make-stress-input-sat 0) => empty
;;;     (make-stress-input-sat 3)
;;;  => (list (make-clause (list (make-pos 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-pos 'p2)
;;;                              (make-neg 'p3)))
;;;           (make-clause (list (make-neg 'p1)
;;;                              (make-neg 'p2)
;;;                              (make-pos 'p3))))

(define (make-stress-input-sat n)
  (local ((define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          (define (iota k)
            (reverse (reverse-iota k))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "p"
                                                       (number->string k))))
                      nums)))
      (map (lambda (k)
             (make-clause   ; see note above
              (map (lambda (i)
                     ((if (= i k) make-pos make-neg)
                      (list-ref syms (- i 1))))
                   nums)))
           nums))))

;;; stress-benchmark2 : NonNegInt -> Boolean
;;; GIVEN: a non-negative integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-sat n) is satisfiable

(define (stress-benchmark2 n)
  (time (is-null-derivable? (make-stress-input-sat n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:

(begin-for-test
  
  (check-equal? (is-null-derivable? empty)
                #f
                "Empty list of clause is satisfiable")
  
  (check-equal? (stress-benchmark2 0)
                #f
                "Empty list of clause is satisfiable")
  
  (check-equal? (complement-list empty empty)
                empty
                "Expected output is empty list as both clauses were empty")
  
  (check-equal? (is-rule-pair? C1 empty empty)
                #f
                "C1 doesn't have any literal's complement as set of
                clause is empty")
  
  (check-equal? (list-to-set (list C1 C1))
                (list C1)
                "C1 should be only once in Set")
  
  (check-equal? (is-null-derivable? INPUT1)
                #t
                "Input1 has empty clause so true should be returned")
  
  (check-equal? (is-null-derivable? INPUT2)
                #t
                "Input2 has empty clause so true should be returned")
  
  (check-equal? (is-null-derivable? INPUT6)
                #t
                "Input1 has empty clause so true should be returned")
  
  (check-equal? (is-null-derivable? INPUT3)
                #t
                "Input3 has empty clause so true should be returned")
  
  (check-equal? (is-null-derivable? INPUT4)
                #f
                "Input4 is satisfiable so false should be returned")
  
  (check-equal? (is-null-derivable? INPUT5) #f
                "Input5 is satisfiable so false should be returned")
  
  (check-equal? (is-null-derivable? INPUT7) #t
                "Input7 has empty clause derivable so true should be returned")
  
  (check-equal? (is-null-derivable? (make-stress-input-sat 3)) #f
                "Satisfiable set is created so false should be returned"))