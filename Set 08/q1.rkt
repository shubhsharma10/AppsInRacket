;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Simplified Garter Snake for detecting loop

(require rackunit)
(require "extras.rkt")
(require "sets.rkt")
(check-location "08" "q1.rkt")

(provide
 any-loops?
 make-def
 make-varexp
 make-appexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
;; A VarExp is (make-varexp Variable)

(define-struct appexp (fn args))
;; An AppExp is (make-appexp Variable ListOfExp)

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.
;; A Node is a Symbol
;; A Graph is a SetOfEdge which is ListOfEdge with no duplicates

(define-struct edge (from to))
;; An Edge is a (make-edge Node Node)

;; ListOfVariable

;; ListOfVariable(LOV) is either
;; -- empty
;; -- (cons Variable LOV)

;; A SetOfVariable is a ListOfVariable without any duplicate

;; ListOfEdge

;; ListOfEdge(LOE) is either
;; -- empty
;; -- (cons Edge LOE)

;; ListOfNode

;; ListOfNode is either
;; -- empty
;; -- (cons Node LON)

;; ListOfEdge(LOE) is either
;; -- empty
;; -- (cons Edge LOE)

;; A SetOfNode is a ListOfNode without any duplicate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; OBSERVER TEMPLATES:

;; pgm-fn : Program -> ??
;(define (pgm-fn p)
;  (lodef-fn p))

;; def-fn : Definition -> ??
;(define (def-fn d)
;  (... (def-name d) (def-args d) (def-body d)))

;; lodef-fn : ListOfDefinition -> ??
; HALTING-MEASURE: length of lod
;(define (lodef-fn lod)
;  (cond
;    [(empty? lod) ..]
;    [else ....(def-fn (first lod))
;              (lodef-fn (rest lod))]))


; loexp-fn -> ListOfExpression -> ??
; HALTING-MEASURE : length of loexp
;(define (loexp-fn loexp)
;  (cond
;    [(empty? loexp) ...]
;    [else (... (exp-fn (first loexp))
;               (loexp-fn (rest loexp)))]))
;
;; exp-fn : Exp -> ??
;(define (exp-fn e)
;  (cond
;    [(varexp? e) (... (varexp-name e))]
;    [(appexp? e) (... (appexp-fn e) (loexp-fn (appexp-args e)))]))

;los-fn : ListOfString -> ??
; HALTING-MEASURE: length of los
;(define (los-fn los)
;  (cond
;    [(empty? los) ..]
;    [else ...(string-fn (first los))
;             (los-fn (rest los))]))

;; edge-fn: Edge->??
; (define (edge-fn edg)
;  (...(edge-from edg)
;      (edge-to edg)))

;; lov-fn : LOV -> ??
;; (define (lov-fn lov)
;;   (cond
;;     [(empty? lov)...]
;;     [ else (...
;;               (variable-fn (first lov))
;;               (lov-fn (rest lov)))]))

;; lon-fn : LON -> ??
;; (define (lon-fn lon)
;;   (cond
;;     [(empty? lon)...]
;;     [ else (...
;;               (node-fn (first lon))
;;               (lon-fn (rest lon)))]))

;; loe-fn : LOE -> ??
;; (define (loe-fn loe)
;;   (cond
;;     [(empty? loe)...]
;;     [ else (...
;;               (edge-fn (first loe))
;;               (loe-fn (rest loe)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define LOOP-PROGRAM
  (list
   (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
   (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'u)
             (make-appexp 'f1 (list (make-appexp 'f4
                                                 (list (make-varexp 'u)
                                                       (make-varexp 'w)))
                                    (make-varexp 'z))))
   (make-def 'f4 (list 'x 'y)
             (make-appexp 'f5
                          (list (make-varexp 'y)
                                (make-varexp 'u))))
   (make-def 'f5 (list 'u)
             (make-appexp 'f2
                          (list (make-appexp 'f3 empty))))
   (make-def 'no-loop (list 'x) (make-varexp 'x))))

(define F1-DEF
  (make-def 'f1 (list 'x 'y) (make-varexp 'x)))

(define F2-DEF
  (make-def 'f2 (list 'x 'y) (make-appexp 'f1 (list (make-varexp 'x)
                                                    (make-varexp 'x)))))

(define NO-LOOP-PROGRAM
  (list F1-DEF F2-DEF))

(define GRAPH-FOR-NO-LOOP
  (list (make-edge 'f2 'f1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; any-loops? : Program -> Boolean
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;; restrictions listed above).
;; RETURNS: true iff there is some function f in p that calls itself
;; either directly or indirectly, as in the example above.
;; EXAMPLE: (any-loops? NO-LOOP-PROGRAM) = #f
;; STRATEGY: Use ormap HOF on pgm
(define (any-loops? pgm)
  (local
    ((define grap (get-graph pgm)))
    (ormap
     ; Definition -> Boolean
     ; Returns true if any loop is present in the graph
     (lambda (x) (member? (def-name x)
                          (reachables1  empty
                                        (successors (def-name x) grap)
                                        grap)))
     pgm)))

;; get-graph : Program -> Graph
;; GIVEN: a valid SGS program p (that is, a GS program that obeys the
;; restrictions listed above).
;; RETURNS: A set of edges which is a graph based on the Program passed
;; EXAMPLE: (get-graph NO-LOOP-PROGRAM) = (list (make-edge 'f2 'f1))
;; STRATEGY: Use foldr HOF on pgm.
(define (get-graph pgm)
  (foldr
   ; Definition SOE-> SOE
   ; Returns a SetOfEdge which is a graph for the given Program
   (lambda(x accum) (append (get-soe x) accum))
   empty
   pgm))

;; get-soe : Definition -> SetOfEdge
;; GIVEN: a Definition
;; RETURNS: A set of edges from the the given Definition which the
; function will be calling in the body
;; EXAMPLE: (get-soe (make-def 'f2 (list 'x 'y) (make-appexp 'f1
;;                  (list (make-varexp 'x) (make-varexp 'x)))))
;;        = (list (make-edge 'f2 'f1))
;; STRATEGY: Use Definition tempalte on defn
(define (get-soe defn)
  (if (appexp? (def-body defn))
      (get-set-of-nodes (get-child (def-body defn)) (def-name defn))
      empty))

;; get-set-of-nodes : SetOfVariable Variable -> SetOfEdge
;; GIVEN: SetOfVariable and Variable
;; RETURNS: A set of edges from the the given Variable
;; EXAMPLE : (get-set-of-nodes (get-child (def-body F1-DEF)) (def-name F1-DEF))
;;         = empty
;; STRATEGY: Use foldr HOF on sov.
(define (get-set-of-nodes sov from)
  (foldr
   ; Variable SOE-> SOE
   ; Returns a SetOfEdge for the given Variable
   (lambda (child soe)(cons (make-edge from child) soe))
   empty
   sov))

;; get-child : Expression -> SetOfVariable 
;; GIVEN:Expression
;; RETURNS: SetOfVariable from given Expression
;; EXAMPLE: (get-child (def-body F2-DEF))
;;          = (list 'f1)
;; HALTING-MEASURE: length(appexp-args expr)
;; STRATEGY: Use foldr HOF on expr arguments.
(define (get-child expr)
  (if (appexp? expr)
      (foldr
       ; Expression SetOfVariable -> SetOfVariable
       ; Returns SetOfVariable which are arguments of expression.
       (lambda (exp sov) (set-union (get-child exp) sov))
       (cons (appexp-fn expr) empty)
       (appexp-args expr))
      empty))

;; all-successors: SetOfNode Graph -> SetOfNode
;; GIVEN: A set of nodes and a Graph
;; RETURNS: the set of all their immediate successors
;; EXAMPLE: (all-successors (list 'f1) GRAPH-FOR-NO-LOOP)
;;          = empty
;; STRATEGY: use HOF foldr on nodes
;; Citation: Using the method from examples given.
; http://www.ccs.neu.edu/course/cs5010f16/Examples/LICENSE
(define (all-successors nodes graph)
  (foldr
   ; Node SetOfNode-> SetOfNode
   ; Returns a SetOfNode which are immediate successor of the given set of node 
   (lambda (node s)
     (set-union
      (successors node graph)
      s))
   empty
   nodes))

;; reachables1: SetOfNode SetOfNode Graph -> SetOfNode
;; GIVEN: two SetOfNode and a finite graph g
;; WHERE:
;;  reached is the set of nodes reachable in graph g in fewer than n steps
;;        from a set of nodes S, for some S and n
;;  recent is the set of nodes reachable from S in n steps but
;;         not in n-1 steps.
;; RETURNS: the set of nodes reachable from S in g.
;; EXAMPLE: (reachables1 empty (list 'f1) GRAPH-FOR-NO-LOOP)
;;         = (list 'f1)
;; HALTING-MEASURE: length (recent)
;; STRATEGY: Use cases on reached is empty or not
;; Citation: Using the method from examples given.
; http://www.ccs.neu.edu/course/cs5010f16/Examples/LICENSE
(define (reachables1 reached recent g)
  (cond
    [(empty? recent) reached]
    [else
     (local
       ((define next-reached (append recent reached))
        (define next-recent 
          (set-diff (all-successors recent g)
                    next-reached)))
       (reachables1 next-reached next-recent g))]))

;; CORRECTNESS REASONING:  If the invariant is true and recent is
;; empty, then there are no more nodes reachable in n steps than in
;; n-1 steps.  So 'reached' contains all the reachable nodes.

;; Otherwise, if the invariant is true, then 'next-reached' is the
;; set of nodes reachable from S in fewer than n+1 steps. 'next-recent' is the
;; set of nodes reachable from S in fewer than n+1 steps but not
;; in fewer than n steps.  

;; Since next and reached are disjoint, then (append next
;; reached) is a set (that is, no duplications), and is the set of nodes
;; reachable from S in fewer than n+1 steps.  So the recursive call to
;; reachables1 satisfies the invariant.

;; TERMINATION REASONING: If the invariant is true, then 'next' is
;; non-empty, so at the recursive call the number of nodes _not_ in
;; 'reached' is smaller.

;; successors : Node Graph -> SetofNode
;; GIVEN: A Node and a Graph
;; RETURNS: the set of all Nodes which comes from the given Node
;; EXAMPLE: (successors (def-name F2-DEF) GRAPH-FOR-NO-LOOP)
;;         = (list 'f1)
;; STRATEGY: use HOF map and filter on soe
;; Citation: Using the method from examples given.
; http://www.ccs.neu.edu/course/cs5010f16/Examples/LICENSE
(define (successors n1 soe)
  (map 
   edge-to  
   (filter
    ; Edge-> SetOfEdge
    ; Returns: A Set of edge where from node is same as n1.
    (lambda (e) (symbol=? (edge-from e) n1))
    soe)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRESS BENCHMARK TESTING

;;; make-stress-input-without-loops : PosInt -> Program
;;; GIVEN: an integer n
;;; RETURNS: an SGS program with no loops that defines n functions
;;; EXAMPLES:
;;;     (make-stress-input-without-loops 1)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x)))
;;;
;;;     (make-stress-input-without-loops 3)
;;;  => (list
;;;      (make-def 'f1 (list 'x 'y) (make-varexp 'x))
;;;      (make-def 'f2 (list 'x 'y)
;;;        (make-appexp 'f1 (list (make-varexp 'x) (make-varexp 'x))))
;;;      (make-def 'f3 (list 'x 'y)
;;;        (make-appexp
;;;         'f2
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x)))
;;;         (make-appexp 'f2 (list (make-varexp 'x) (make-varexp 'x))))))

(define (make-stress-input-without-loops n)
  (local (
          ;;; Returns a list of 1 through k.
          (define (iota k)
            (reverse (reverse-iota k)))
          (define (reverse-iota k)
            (if (= k 0)
                empty
                (cons k (reverse-iota (- k 1)))))
          
          ;;; Given the function names in reverse order,
          ;;; returns their bodies in reverse order.
          (define (make-bodies names)
            (if (empty? (rest names))
                (list (make-varexp 'x))
                (let* ((bodies (make-bodies (rest names)))
                       (body (first bodies))
                       (name (first (rest names))))
                  (cons (make-appexp name (list body body))
                        bodies)))))
    (let* ((nums (iota n))
           (syms (map (lambda (k)
                        (string->symbol (string-append "f"
                                                       (number->string k))))
                      nums))
           (bodies (reverse (make-bodies (reverse syms)))))
      (map (lambda (sym body)
             (make-def sym (list 'x 'y) body))
           syms
           bodies))))

;;; stress-benchmark1 : PosInt -> Boolean
;;; GIVEN: a positive integer n
;;; RETURNS: false
;;; EFFECT: reports how many milliseconds it takes to determine
;;;     (make-stress-input-without-loops n) has no loops

(define (stress-benchmark1 n)
  (time (any-loops? (make-stress-input-without-loops n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS:

(begin-for-test
  (check-equal? (any-loops? LOOP-PROGRAM)
                #t
                "Returns true if there is a loop inside the given Program")
  
  (check-equal? (stress-benchmark1 2)
                #f
                "To check for the given benchmark")
  
  (check-equal? (any-loops? NO-LOOP-PROGRAM) #f
                "Returns false if there is no loop inside the given program"))

