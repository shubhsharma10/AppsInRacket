;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(check-location "06" "q1.rkt")
(require 2htdp/image)
(require 2htdp/universe)

(provide
 initial-world
 run
 world-after-mouse-event
 world-after-key-event
 world-to-trees
 tree-to-root
 tree-to-sons
 node-to-center
 node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; MAIN FUNCTION.

;; run : Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.
;; EXAMPLE:
; (run 1)->(make-world (make-click-location #false 0 0) empty)
(define (run any)
  (big-bang (initial-world any)
            (on-key world-after-key-event)
            (on-draw world-to-scene)
            (on-mouse world-after-mouse-event)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; Dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))

;; Constants related to Node
(define RADIUS 20)
(define MUL-FACTOR 3)

;; Shape constants
(define SQUARE "square")
(define CIRCLE "circle")

;; KeyEvent constants
(define ADD-CIRCLE "c")
(define ADD-SQUARE "s")
(define DELETE "d")

;; Color of the node
(define IMAGE-COLOR "green")

;; Constants for type of the circle
(define SOLID-CIRCLE "solid")
(define OUTLINE-CIRCLE "outline")

;; Color of the line
(define LINE-COLOR "blue")

;; Mouse event constants
(define BUTTON-DOWN "button-down")
(define DRAG "drag")
(define BUTTON-UP "button-up")
(define ENTER "enter")

;; Constant for CREATE-CIRCLE-UNSELECTED
(define CREATE-CIRCLE-UNSELECTED (circle RADIUS
                                         OUTLINE-CIRCLE
                                         IMAGE-COLOR))

;; Constant for CREATE-CIRCLE-SELECTED
(define CREATE-CIRCLE-SELECTED (circle RADIUS SOLID-CIRCLE IMAGE-COLOR))

;; Constant for CREATE-SQUARE-UNSELECTED
(define CREATE-SQUARE-UNSELECTED (square (image-width CREATE-CIRCLE-UNSELECTED)
                                         OUTLINE-CIRCLE IMAGE-COLOR))
;; Constant for CREATE-SQUARE-UNSELECTED
(define CREATE-SQUARE-SELECTED (square (image-width CREATE-CIRCLE-UNSELECTED)
                                       SOLID-CIRCLE IMAGE-COLOR))

;; Canvas Boundries
(define WIDTH-BOUNDARY (- CANVAS-WIDTH RADIUS))
(define HEIGHT-BOUNDARY (- CANVAS-HEIGHT RADIUS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Data Definitions

;; A Shape is one of
;; -- CIRCLE
;; -- SQUARE
;; INTRPRETATION: self-evident

;; TEMPLATE:
;; shape-fn: shape->??
;; (define (shape-fn shap)
;; (cond
;;    [(string=? shap CIRCLE)]
;;    [(string=? shap SQUARE)]))

;; a KeyEvent is one of
;; -- ADD-CIRCLE
;; -- ADD-SQUARE
;; -- DELETE
;; INTERPRETATION:
;; ADD-CIRCLE - Hitting "c" when a node is selected adds a new son to
;;  the selected node. The new node should be a circle whose center has an
;;  x-coordinate that is 3 radii to the left of the center of the currently
;;  leftmost son, and a y-coordinate that is 3 radii down from the center of
;;  the parent. If the node has no sons, then hitting "c" should create
;;  the node's first son, appearing 3 radii down and directly beneath the node.
;;  Hitting "c" when no node is selected creates a new root node
;;  that is a circle. The new node should appear in the center of the top
;;  of the canvas. The root appears tangent to the top of the canvas and
;;  initially has no sons.
;; ADD-SQAURE - Hitting "s" at any time should behave like "c",
;;  except that the new node created is a square instead of a circle.
;; DELETE - Hitting "d" when a node is selected deletes the selected node.
;;  When a node is deleted, its sons become the sons of the parent of the
;;  deleted node. If the deleted node is a root node
;;  (and therefore has no parent), then its sons become root nodes

;; TEMPLATE
;; key-event-fn : KeyEvent -> ??
#|
(define (key-event-fn input)
  (cond
    [(string=? input ADD-CIRCLE) ...]
    [(string=? input ADD-SQAURE) ...]
    [(string=? input DELETE) ...]
|#
;    [(string=? shap CIRCLE)]
;    [(string=? shap SQUARE)]))


(define-struct world(lot))
;; A World is a (make-world ListofTree)

;; INTERPRETATION:
;; lot is the list of tree
;; TEMPLATE:
;; world-fn: World -> ??
;; (define (world-fn w)
;;  (... (world-lot w)...))


(define-struct delta(x y))
;; A Delta is a (make-delta x y)

;; INTERPRETATION:
;; x is the difference of x-coordinate from the 
;; clicked x coordinate by mouse
;; y is the difference of y-coordinate from the 
;; clicked y coordinate by mouse


;; TEMPLATE:
;; delta-fn: Delta -> ??
;; (define (delta-fn d)
;; (... (delta-x d) (delta-y d)))

;; EXAMPLE:
;; (define somediff (make-delta 30 25))

(define-struct node(x y shape selected? dt))
;; A Node is a-
;; (make-node PosInt PosInt Shape Integer Integer Boolean Delta)

;; INTERPRETATION:
;; x is the x position at the center of node 
;; y is the y position at the center of node
;; shape is the Shape of the node.
;; selected? if the node is selected
;; dt contains the structure with the x-difference and y-difference of the
;; cordinates in clicked node.

;; TEMPLATE:
;; node-fn: node -> ??
;;(define (node-fn nod)
;;  (... (node-x nod) (node-y nod)
;;       (node-shape nod) (node-selected? nod)
;;       (node-dt nod)  ... )

(define-struct tree(node children))
;; A Tree is a-
;; (make-tree Node ListOfTree)

;; INTERPRETATION:
;; node is the root of current tree
;; children is the children of the current node

;; A ListOfTree (LOT) is one of
;; -- empty
;; -- (cons Tree ListOfTree)

;; TEMPLATE:
;; tree-fn: Tree -> ??
;; HALTING MEASURE: length of (tree-children tre)
; (define (tree-fn tre)
;  (... (tree-node tre)
;;   (lot-fn (tree-children tre))...))
;;
;; lot-fn : ListOfTree -> ??
;; HALTING MEASURE: lenght of lot
;;(define (lot-fn lot)
;;  (cond
;;    [(empty? lot) ...]
;;    [else (... (tree-fn (first lot))
;;               (lot-fn (rest lot)))]))

;; End-Data-Definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Constant for initial difference between x-cordinate and y-cordinates
(define INITIAL-DELTA (make-delta 0 0))

;; EXAMPLES:

(define EMPTY-NODE
  (make-node 0 0 CIRCLE #f INITIAL-DELTA))

(define ROOT-NODE-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             RADIUS
             CIRCLE #t INITIAL-DELTA))

(define ROOT-NODE-NOT-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             RADIUS
             CIRCLE #f INITIAL-DELTA))

(define SQUARE-CHILD1-NOT-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             (+ (* MUL-FACTOR RADIUS) RADIUS)
             SQUARE #false INITIAL-DELTA))

(define CHILD1-NOT-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             (+ (* MUL-FACTOR RADIUS) RADIUS)
             CIRCLE #false INITIAL-DELTA))

(define CHILD1-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             (+ (* MUL-FACTOR RADIUS) RADIUS)
             CIRCLE #t INITIAL-DELTA))

(define CHILD2-NOT-SELECTED
  (make-node (-(/ CANVAS-WIDTH 2) (* MUL-FACTOR RADIUS))
             (+ (* MUL-FACTOR RADIUS) RADIUS)
             CIRCLE #false INITIAL-DELTA))

(define CHILD2-SELECTED
  (make-node (-(/ CANVAS-WIDTH 2) (* MUL-FACTOR RADIUS))
             (+ (* MUL-FACTOR RADIUS) RADIUS)
             CIRCLE #t INITIAL-DELTA))

(define CHILD3-NOT-SELECTED
  (make-node (-(/ CANVAS-WIDTH 2) (* MUL-FACTOR RADIUS))
             (+ (*(* MUL-FACTOR RADIUS)2) RADIUS)
             CIRCLE #f INITIAL-DELTA))

(define CHILD4-NOT-SELECTED
  (make-node (/ CANVAS-WIDTH 2)
             (+ (*(* MUL-FACTOR RADIUS) 2) RADIUS)
             CIRCLE #false INITIAL-DELTA))

(define TREE1-WITH-ROOT-SELECTED
  (cons
   (make-tree ROOT-NODE-SELECTED empty) empty))

(define TREE1-WITH-NO-SELECTION
  (cons
   (make-tree ROOT-NODE-NOT-SELECTED empty) empty))

(define TREE2-WITH-ROOT-SELECTED
  (list (make-tree ROOT-NODE-SELECTED
                   (list (make-tree CHILD1-NOT-SELECTED empty)))))

(define TREE2-WITH-ROOT-SELECTED-AND-SQUARE-CHILD
  (list (make-tree ROOT-NODE-SELECTED
                   (list (make-tree SQUARE-CHILD1-NOT-SELECTED empty)))))

(define TREE3-WITH-ROOT-SELECTED
  (list
   (make-tree
    ROOT-NODE-SELECTED
    (list (make-tree CHILD2-NOT-SELECTED empty)
          (make-tree CHILD1-NOT-SELECTED empty)))))

(define TREE3-WITH-CHILD2-SELECTED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list (make-tree CHILD2-SELECTED empty)
          (make-tree CHILD1-NOT-SELECTED empty)))))

(define TREE4-WITH-CHILD2-SELECTED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list
     (make-tree CHILD2-SELECTED (list (make-tree CHILD3-NOT-SELECTED empty)))
     (make-tree CHILD1-NOT-SELECTED empty)))))

(define TREE4-WITH-CHILD2-DELETED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list
     (make-tree CHILD3-NOT-SELECTED empty)
     (make-tree CHILD1-NOT-SELECTED empty)))))

(define TREE3-WITH-CHILD2-AND-CHILD1-SELECTED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list (make-tree CHILD2-SELECTED empty)
          (make-tree CHILD1-SELECTED empty)))))

(define TREE5-WITH-CHILD2-AND-CHILD1-SELECTED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list
     (make-tree CHILD2-SELECTED
                (list (make-tree CHILD3-NOT-SELECTED empty)))
     (make-tree CHILD1-SELECTED
                (list (make-tree CHILD4-NOT-SELECTED empty)))))))

(define TREE5-WITH-CHILD2-AND-CHILD1-DELETED
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list
     (make-tree CHILD3-NOT-SELECTED empty)     
     (make-tree CHILD4-NOT-SELECTED empty)))))

(define TREE3-WITH-NO-SELECTION
  (list
   (make-tree
    ROOT-NODE-NOT-SELECTED
    (list (make-tree CHILD2-NOT-SELECTED empty)
          (make-tree CHILD1-NOT-SELECTED empty)))))

(define MIXED-LOT
  (list
   (make-tree (make-node 200 80 CIRCLE #f INITIAL-DELTA) empty)
   (make-tree (make-node 120 200 CIRCLE #f INITIAL-DELTA) empty)
   (make-tree (make-node 250 80 CIRCLE #f INITIAL-DELTA) empty)
   (make-tree (make-node 70 100 CIRCLE #f INITIAL-DELTA) empty)
   (make-tree (make-node 220 80 CIRCLE #f INITIAL-DELTA) empty)))

(define WORLD-SINGLE-TREE
  (make-world (list (make-tree (make-node 168 130 SQUARE #false
                                          (make-delta 0 0)) empty) empty)))

(define WORLD-CIRCLE-SQUARE
  (make-world
   (list
    (make-tree (make-node 250 20 CIRCLE #false (make-delta 0 0)) empty)
    (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty))))

(define SINGLE-TREE
  (list (make-tree (make-node 168 130 SQUARE #false
                              (make-delta 0 0)) empty) empty))

(define WORLD-WITH-THREE-NODES
  (make-world
   (list
    (make-tree (make-node 250 20 CIRCLE #false (make-delta 0 0)) empty)
    (make-tree (make-node 250 20 CIRCLE #false (make-delta 0 0)) empty)
    (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty))))

(define TREE-WITH-TWO-NODES
  (make-tree
   ROOT-NODE-NOT-SELECTED
   (list (make-tree CHILD2-NOT-SELECTED empty)
         (make-tree CHILD1-NOT-SELECTED empty))))

(define WORLD-CIRCLE-SQUARE-SELECT
  (make-world
   (list
    (make-tree (make-node 250 20 CIRCLE #true (make-delta 0 0)) empty)
    (make-tree (make-node 220 147 SQUARE #true (make-delta 0 0)) empty))))

(define LISTS-TREES-SAMPLE
  (list
   (make-tree (make-node 250 20 CIRCLE #false (make-delta 0 0)) empty)
   (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty)))

(define LISTS-TREES-SAMPLE-AFTER
  (list
   (make-tree (make-node 250 20 CIRCLE #true (make-delta 0 -10)) empty)
   (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty)))

(define WORLD-AFTER-BUTTON-DOWN
  (make-world
   (list
    (make-tree (make-node 250 20 CIRCLE #true (make-delta 0 -10)) empty)
    (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty))))

(define WORLD-WITH-LINE
  (make-world
   (list
    (make-tree
     (make-node 250 20 "circle" #false (make-delta 0 0))
     (list
      (make-tree (make-node 190 80 "square" #false (make-delta 0 0)) '())
      (make-tree (make-node 250 80 "circle" #false (make-delta 0 0)) '())))
    (make-tree (make-node 251 21 "square" #true (make-delta -18 -14)) '()))))

(define LISTS-TREES-SAMPLE-DRAG
  (list
   (make-tree (make-node 505 45 CIRCLE #true (make-delta 0 -10)) empty)
   (make-tree (make-node 475 172 SQUARE #false (make-delta 0 0)) empty)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set
;; EXAMPLE:
;; (initial-world 4)=initial world with empty canvas
;; STRATEGY: Use template for World on val
(define (initial-world val)
  (make-world empty))

;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLE: (world-to-trees WORLD-SINGLE-TREE)=SINGLE-TREE
;; STRATEGY: Use template for World on w
(define (world-to-trees w)
  (world-lot w))

;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE: (tree-to-root TREE-WITH-TWO-NODES)=
; (make-node 250 20 CIRCLE #false (make-delta 0 0))
;; STRATEGY:  Use template for TREE on tre
(define (tree-to-root tre)
  (tree-node tre))

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given
;; tree.
;; EXAMPLE: (tree-to-sons TREE-WITH-TWO-NODES)=
;; (list
; (make-tree (make-node 190 80 CIRCLE #false (make-delta 0 0)) empty)
; (make-tree (make-node 250 80 CIRCLE #false (make-delta 0 0)) empty))
;; STRATEGY:  Use template for TREE on tre
(define (tree-to-sons tre)
  (tree-children tre))

;; node-to-center : Node -> Posn
;; RETURNS: the center of the given node as it is to be displayed on the
;; scene.
;; EXAMPLE: (node-to-center (make-node 250 20 CIRCLE
; #false (make-delta 0 0)))=(make-posn 250 20)
;; STRATEGY: Use template for Node on nod
(define (node-to-center nod)
  (make-posn (node-x nod) (node-y nod)))

;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected.
;; EXAMPLE:(node-to-selected? (make-node 250 20 CIRCLE
; #false (make-delta 0 0)))=#false
;; STRATEGY: Use template for Node on nod
(define (node-to-selected? nod)
  (node-selected? nod)) 

;; world-after-key-event : World KeyEvent -> World
;; GIVEN: a world w and a keyevent kev
;; RETURNS: the world that should follow the given world
;; after the given key event.
;; EXAMPLE:
; (world-after-key-event WORLD-CIRCLE-SQUARE ADD-CIRCLE)->
; WORLD-WITH-THREE-NODES
;; STRATEGY: Cases on kev and combine simple functions
(define (world-after-key-event w kev)
  (cond
    [(key=? kev ADD-CIRCLE) (make-world (world-after-add (world-to-trees w)
                                                         CIRCLE))]
    [(key=? kev ADD-SQUARE) (make-world (world-after-add (world-to-trees w)
                                                         SQUARE))]
    [(key=? kev DELETE) (make-world (world-after-delete (world-to-trees w)))]
    [else w]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS SUPPORTING ADD

;; world-after-add : LOT Shape -> LOT
;; RETURNS: LOT after adding tree for shape
;; EXAMPLE: (world-after-add TREE1-WITH-ROOT-SELECTED CIRCLE)
;;          = TREE2-WITH-ROOT-SELECTED
;; STRATEGY: Combine simple functions
(define (world-after-add lot shape)
  (if(is-any-tree-selected? lot)
     (find-and-update-on-lot shape lot)
     (cons (create-root-tree shape) lot)))

;; find-and-update-on-lot : Shape ListOfTree -> ListOfTree
;; RETURNS:  LOT after adding tree for shape
;; EXAMPLE: (find-and-update-on-lot CIRCLE TREE1-WITH-ROOT-SELECTED)
;;          = TREE2-WITH-ROOT-SELECTED
;; STRATEGY:  use map HOF on lot
;; HALTING-MEASURE: length of lot
(define (find-and-update-on-lot shape lot)
  (map
   ; Tree -> Tree
   ; Return tree with sub-tree for shape
   (lambda (x) (find-and-update-selected-tree x shape)) lot))

;; create-root-tree : Shape -> Tree
;; RETURNS: Tree for root node
;; EXAMPLE: (create-root-tree CIRCLE)
;;          = (make-tree ROOT-NODE-NOT-SELECTED empty)
;; STRATEGY: Use Tree template on shape
(define (create-root-tree shape)
  (make-tree (create-node-for-root-tree shape) empty))

;; create-node-for-root-tree : Shape -> Node
;; RETURNS: Node for root tree
;; EXAMPLE: (create-node-for-root-tree CIRCLE)
;;          = ROOT-NODE-NOT-SELECTED
;; STRATEGY: Use Node template on shape
(define (create-node-for-root-tree shape)
  (make-node (/ CANVAS-WIDTH 2) RADIUS shape #f INITIAL-DELTA))   

;; find-and-update-selected-tree : Tree Shape -> Tree
;; RETURNS: Adds child tree for shape in Tree and returns it
;; EXMAPLE: (find-and-update-selected-tree (make-tree ROOT-NODE-SELECTED empty)
;;                                         CIRCLE)
;;          = (make-tree ROOT-NODE-SELECTED
;;                  (list (make-tree CHILD1-NOT-SELECTED empty)))
;; STRATEGY: Use Tempalte of Tree on tree
(define (find-and-update-selected-tree tree shape)
  (if(node-to-selected? (tree-to-root tree))
     (add-new-child-at-root tree shape)
     (apply-find-and-update-on-children tree shape)))

;; add-new-child-at-root : Tree Shape -> Tree
;; RETURNS: Tree with new shape
;; EXAMPLE: (add-new-child-at-root (make-tree ROOT-NODE-SELECTED empty)
;;                                         CIRCLE)
;;          = (make-tree ROOT-NODE-SELECTED
;;                  (list (make-tree CHILD1-NOT-SELECTED empty)))
;; STRATEGY: Combine simple functions
(define (add-new-child-at-root t shape)
  (if (empty? (tree-children t))      
      (get-tree-with-first-child shape t)
      (get-tree-with-extra-child shape
                                 (apply-find-and-update-on-children t shape))))


;; apply-find-and-update-on-children : Tree Shape -> Tree
;; RETURNS: Tree with children which have been searched for
;; seleted node and added
;; EXAMPLE: (apply-find-and-update-on-children
;;            (make-tree ROOT-NODE-NOT-SELECTED
;;                (list (make-tree CHILD2-SELECTED empty)
;;                (make-tree CHILD1-NOT-SELECTED empty)))
;;             CIRCLE)
;; = (make-tree ROOT-NODE-NOT-SELECTED
;;     (list
;;      (make-tree CHILD2-SELECTED (list (make-tree CHILD3-NOT-SELECTED empty)))
;;      (make-tree CHILD1-NOT-SELECTED empty)))
;; STRATEGY: Use Tree template on t and shape
;; HALTING MEASURE: length of (tree-children t)
(define (apply-find-and-update-on-children t shape)
  (make-tree (tree-node t) (find-and-update-on-lot shape (tree-children t))))


;; get-tree-with-first-child : Shape Tree -> Tree
;; RETURNS: Tree with first child sub-tree
;; EXAMPLE:(get-tree-with-first-child CIRCLE( make-tree CHILD2-SELECTED empty))
;;         = (make-tree CHILD2-SELECTED (list
;;                                      (make-tree CHILD3-NOT-SELECTED empty))) 
;; STRATEGY: Call a more general function
(define (get-tree-with-first-child shape t)
  (let ([root (tree-node t)])
    (get-tree-with-new-sub-tree shape (node-x root) t empty)))


;; get-tree-with-extra-child : Shape Tree -> Tree
;; RETURNS: Tree with extra child sub-tree
;; EXAMPLE: (get-tree-with-extra-child CIRCLE
;;                         (make-tree ROOT-NODE-SELECTED
;;                            (list (make-tree CHILD1-NOT-SELECTED empty))))
;;   = (make-tree ROOT-NODE-SELECTED
;;       (list (make-tree CHILD2-NOT-SELECTED empty)
;;             (make-tree CHILD1-NOT-SELECTED empty)))
;; STRATEGY: Call a more general function
(define (get-tree-with-extra-child shape t)
  (let ([child (tree-children t)])
    (get-tree-with-new-sub-tree shape (get-x-coord-for-extra child)
                                t child)))

;; get-tree-with-new-sub-tree: Shape Int Tree ListOfTree -> Tree
;; RETURNS: Tree with sub-tree for newly added child
;; STRATEGY: Use Tree template on shape,x, t and lot
(define (get-tree-with-new-sub-tree shape x t lot)
  (let ([root (tree-node t)])
    (make-tree root
               (cons (create-sub-tree-for-new-child shape x root) lot ))))

;; create-sub-tree-for-new-child : Shape Int Node -> Tree
;; GIVEN: Shape of the node to be created, x-coord of the node
;;        and parent node
;; RETURNS: Sub-tree for the newly added child node
;; STRATEGY: Use Tree template on shape, x-coord and p-node
(define (create-sub-tree-for-new-child shape x-coord p-node)
  (make-tree (make-node x-coord
                        (get-y-coord-for-child p-node)
                        shape #f INITIAL-DELTA) empty))


;; get-x-coord-for-extra : ListOfTree -> Int
;; RETURNS: x-coordinate of new node to be added
;; EXAMPLE: (get-x-coord-for-extra (list (make-tree CHILD2-NOT-SELECTED empty)
;;                                       (make-tree CHILD1-NOT-SELECTED empty)))
;;          = (-  (-(/ CANVAS-WIDTH 2) (* MUL-FACTOR RADIUS))
;;                (* MUL-FACTOR RADIUS))
;; STRATEGY: Combine simpler functions
(define (get-x-coord-for-extra lot)
  (- (node-x (tree-node (get-left-most-tree lot)))
     (* MUL-FACTOR RADIUS)))  

;; get-left-most-tree : ListOfTree -> Tree
;; RETURNS: Left-most tree among lot
;; EXAMPLE: (get-left-most-tree (list (make-tree CHILD2-NOT-SELECTED empty)
;;                                    (make-tree CHILD1-NOT-SELECTED empty)))
;;         = (make-tree CHILD2-NOT-SELECTED empty)
;; STRATEGY: Use foldr HOF on lot
(define (get-left-most-tree lot)
  (foldr
   ; Tree Tree -> Tree
   ; Returns tree with left most root node among list of trees
   (lambda (x left-most)  (get-left-most-node x left-most)) (first lot) lot))

;; get-left-most-node : Tree Tree -> Tree
;; RETURNS: Tree with left-most root node among list of trees
;; STRATEGY: Combine simple functions
(define (get-left-most-node t left-most)
  (if(< (node-x(tree-node t)) (node-x(tree-node left-most)))
     t
     left-most))

;; get-y-coord-for-child : Node -> Int
;; GIVEN: parent-node of the the node, which is to be added.
;; RETURNS: y-coordinate of the node
;; EXAMPLE: (get-y-coord-for-child CHILD2-SELECTED)
;;         =(+ (+ (* MUL-FACTOR RADIUS) RADIUS) (* MUL-FACTOR RADIUS))
;; STRATEGY: Combine simple function
(define (get-y-coord-for-child p-node)
  (+ (node-y p-node) (* MUL-FACTOR RADIUS)))

;; is-any-tree-selected? : ListOfTree -> Boolean
;; RETURNS: True iff any tree is selected
;; EXAMPLE: (is-any-tree-selected? (cons TREE3-WITH-ROOT-SELECTED empty))
;;          = #t
;; STRATEGY: Use ormap HOF on lot
;; HALTING MEASURE: length of lot
(define (is-any-tree-selected? lot)
  (ormap
   ; Tree -> Bool
   ; Returns True iff any node is selected in tree
   (lambda (x) (is-any-node-selected? x)) lot))

;; is-any-node-selected? : Tree -> Boolean
;; RETURNS: true iff any node in tree is selected
;; EXAMPLE: (is-any-node-selected? TREE3-WITH-ROOT-SELECTED) = #t
;; STRATEGY: Use Template of Tree on t
;; HALTING MEASURE: length of (tree-children t)
(define (is-any-node-selected? t)
  (or (node-selected? (tree-node t))
      (is-any-tree-selected? (tree-children t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FUNCTIONS SUPPORTING DELETE

;; world-after-delete ListOfTree -> ListOfTree
;; RETURNS: ListOfTree after deletion of selected node(s)
;; EXAMPLE: (world-after-delete TREE3-WITH-NO-SELECTION)
;;          =  TREE3-WITH-NO-SELECTION
;; STRATEGY: Use foldr HOF on lot
;; HALTING MEASURE: length of lot
(define (world-after-delete lot)
  (foldr
   ; Tree ListOfTree -> ListOfTree
   ; Returns list of tree after deleting selected node
   (lambda (x accum) (find-and-delete-for-tree x accum)) empty lot))

;; find-and-delete-for-tree : Tree ListOfTree -> ListOfTree
;; RETURNS: Finds a selected node and append it's children to accum
;;          and searches for selected node in result
;; STRATEGY: Use Template for Tree on t
;; HALTING MEASURE: length of (tree-children t)
(define (find-and-delete-for-tree t accum)
  (if(node-to-selected? (tree-to-root t))
     (world-after-delete (append (tree-to-sons t) accum))
     (append (cons (make-tree (tree-to-root t)
                              (world-after-delete(tree-to-sons t))) empty)
             accum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-to-scene : World -> Scene
;; GIVEN: A World
;; RETURNS: a Scene that portrays the given world.
;; EXAMPLE: (world-to-scene WORLD-CIRCLE-SQUARE)
;;          = world with a circle and square
;; STRATEGY: Use template for World on w
(define (world-to-scene w)
  (draw-canvas (world-lot w)))                     

;; draw-canvas : LOT -> IMAGE
;; GIVEN: ListOfTree
;; RETURNS: IMAGE of the ListOfTree
;; EXAMPLE: (draw-canvas TREE3-WITH-ROOT-SELECTED)=
;;Image with a selected circle and two of its children
;; STRATEGY: use foldr HOF on lot 
(define (draw-canvas lot)
  (foldr
   ;; Tree Image -> Image
   ;; GIVEN:Tree and the Image from previous recurssion
   ;; RETURNS: Updated image for the Tree provided
   (lambda (tre img)(draw-root tre img))
   EMPTY-CANVAS
   lot))

;; draw-root : TREE IMAGE-> IMAGE
;; GIVEN: A TREE and IMAGE
;; RETURNS: Image of the root and its children for the given tree
;; EXAMPLE: (draw-canvas TREE3-WITH-ROOT-SELECTED)=
;;Inage with a selected circle and two of its children
;; STRATEGY: Combine simple functions and Use template of Tree on tre
;; HALTING MEASURE: length of (tree-children tre)
(define (draw-root tre img)
  (place-image (draw-node (tree-node tre))
               (node-x (tree-node tre))
               (node-y (tree-node tre))
               (draw-descendants (tree-node tre) (tree-children tre) img)))

;; draw-descendants :  Node LOT IMAGE-> IMAGE
;; GIVEN: A Node ListOfTree IMAGE
;; RETURNS: IMAGE of the tree
;; EXAMPLE: (draw-descendants CHILD2-SELECTED TREE3-WITH-ROOT-SELECTED IMG)=
;; Image with a selected circle and two of its children
;; STRATEGY: use foldr HOF on lot and combine simple functions
;; HALTING MEASURE: length of lot
(define (draw-descendants root lot img)
  (foldr
   ;; Tree Image->Image
   ;; Given:Tree and the Image from previous recurssion
   ;; Returns: Updated image for the Tree provided
   (lambda(tre rest-image)(scene+line (draw-root tre rest-image)
                                      (node-x root) (node-y root)
                                      (node-x (tree-node tre))
                                      (node-y (tree-node tre))
                                      LINE-COLOR))
   img
   lot))

;; draw-node : Node-> IMAGE
;; GIVEN: A Node
;; RETURNS: Image of the circle or square depending on the
; shape of the node and its state(selected or unselected
;; EXAMPLE: (draw-node TREE3-WITH-ROOT-SELECTED)=
;;Inage with a selected circle and two of its children
;; STRATEGY: combine simple functions and cases on node shape
(define (draw-node nod)
  (cond
    [(equal? (node-shape nod) CIRCLE) (if (node-selected? nod)
                                          CREATE-CIRCLE-SELECTED
                                          CREATE-CIRCLE-UNSELECTED)]
    [(equal? (node-shape nod) SQUARE) (if (node-selected? nod)
                                          CREATE-SQUARE-SELECTED
                                          CREATE-SQUARE-UNSELECTED)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a world and a description of a mouse event
;; RETURNS: the world that should follow the given mouse event
;; Example:
; (world-after-mouse-event WORLD-CIRCLE-SQUARE 250 30 BUTTON-DOWN)=
; WORLD-AFTER-BUTTON-DOWN
;; STRATEGY: use template for World on w
(define (world-after-mouse-event w mx my mev)
  (make-world (lot-after-mouse-event (world-lot w) mx my mev)))

;; lot-after-mouse-event : ListOfTree Integer Integer MouseEvent -> LOT
;; GIVEN: A ListOfTree and a description of mouse event
;; RETURNS: ListOfTree after mouse event
;; Example:
; (lot-after-mouse-event LISTS-TREES-SAMPLE 250 30 BUTTON-DOWN)=
; LISTS-TREES-SAMPLE-AFTER
;; STRATEGY: using conditions on mouse event
(define (lot-after-mouse-event lot mx my mev)
  (cond
    [(mouse=? mev BUTTON-DOWN) (node-after-button-down lot mx my)]
    [(mouse=? mev DRAG) (lot-after-drag lot mx my #f)]
    [(mouse=? mev BUTTON-UP) (lot-after-button-up lot)]
    [else lot]))

;; node-after-button-down : LOT Integer Integer-> LOT
;; GIVEN: ListOfTree X and Y coordinates where mouse clicked
;; RETURNS: ListOfTree
;; EXAMPLE: (node-after-button-down LISTS-TREES-SAMPLE 250 30)=
;;LISTS-TREES-SAMPLE-AFTER
;; STRATEGY: use map HOF on lot
;; HALTING MEASURE: length of lot
(define (node-after-button-down lot mx my)
  (map
   ;; Tree -> Tree
   ;; Given:Tree
   ;; Returns: Updated Tree after button-down event
   (lambda(tre)(node-is-in tre mx my))
   lot))

;; lot-after-button-up : LOT -> LOT
;; GIVEN: ListOfTree
;; RETURNS: ListOfTree
;; EXAMPLE: (lot-after-button-up LISTS-TREES-SAMPLE)=
;; (list
; (make-tree (make-node 250 20 CIRCLE #false (make-delta 0 0)) empty)
; (make-tree (make-node 220 147 SQUARE #false (make-delta 0 0)) empty))
;; STRATEGY: use map HOF on lot
;; HALTING MEASURE: length of lot
(define (lot-after-button-up lot)
  (map   
   ;; Tree -> Tree
   ;; Given:Tree
   ;; Returns: Updated Tree after button-up event
   (lambda(tre)(node-after-button-up tre))
   lot))

;; node-is-in: Tree Integer Integer -> Tree
;; GIVEN: Tree and X and Y Coordinates
;; RETURNS: Tree
;; EXAMPLE: (node-is-in (make-tree (make-node 168 130 SQUARE #true
;  (make-delta 0 0)) empty) 172 140)=
; (make-tree (make-node 168 130 SQUARE #true (make-delta -4 -10)) empty)
;; STRATEGY: Use Template of Tree on tre
;; HALTING MEASURE: length of (tree-children tre)
(define (node-is-in tre  mx my)
  (let ([nod (tree-node tre)])
    (if (in-node? (tree-node tre) mx my)
        (make-tree (create-in-node nod mx my)
                   (node-after-button-down (tree-children tre) mx my)) 
        (make-tree nod (node-after-button-down (tree-children tre) mx my)))))

;; create-in-node: Node Integer Integer -> Node
;; GIVEN: Node and X and Y Coordinates
;; RETURNS: Node
;; EXAMPLE: (create-in-node (make-node 168 130 SQUARE #true
;  (make-delta 0 0)) 172 140)=
; (make-node 168 130 "square" #true (make-delta -4 -10))
;; STRATEGY: Use Node template on nod, mx and my
(define (create-in-node nod mx my)
  (make-node (node-x nod) (node-y nod) (node-shape nod) #t
             (make-delta (compute-cord-diff (node-x nod) mx)
                         (compute-cord-diff (node-y nod) my))))

;; node-after-button-up: Tree-> Tree
;; GIVEN: Tree
;; RETURNS: Updated Tree after button up
;; EXAMPLE: (node-after-button-up (make-tree (make-node 168 130 SQUARE #true
;  INITIAL-DELTA) empty))=
; (make-tree (make-node 168 130 SQUARE #false INITIAL-DELTA) empty)
;; STRATEGY: Using template of Tree on tre
;; HALTING MEASURE: length of (tree-children tre)
(define (node-after-button-up tre)
  (make-tree (make-node (node-x (tree-node tre)) (node-y (tree-node tre))
                        (node-shape (tree-node tre)) #f INITIAL-DELTA)
             (lot-after-button-up (tree-children tre))))

;; lot-after-drag : LOT Integer Integer Boolean -> LOT
;; GIVEN: LOT X-Coordinates Y-Coordinates Selected?
;; RETURNS: LOT after drag
;; Example:
; (lot-after-drag LISTS-TREES-SAMPLE-AFTER 255 25 #true)->
; LISTS-TREES-SAMPLE-DRAG
;; STRATEGY: use map HOF on lot
;; HALTING MEASURE: length of lot
(define (lot-after-drag lot mx my selec?)
  (map
   ;; Tree -> Tree
   ;; Returns: Updated Tree after drag event
   (lambda(tre)
     (create-node-drag tre mx my selec?)) lot))

;; create-node-drag : Tree Integer Integer Boolean-> Tree
;; GIVEN: Tree X-Coordinates Y-Coordinates Boolean
;; RETURNS: Tree after drag
;; Example:
; (create-node-drag CHILD1-SELECTED 255 25 #false)->
; (list (make-tree (make-node 255 15 CIRCLE #true (make-delta 0 -10)) empty)
; (make-tree (make-node 475 172 SQUARE #false (make-delta 0 0)) empty))
;; STRATEGY: use cases on selec? and template of Tree on tre
;; HALTING MEASURE: length of (tree-children tre)
(define (create-node-drag tre mx my selec?)
  (cond
    [(and (node-selected? (tree-node tre)) (equal? selec? #f))
     (node-after-drag tre mx my)]
    [(equal? selec? #t) (child-node-after-drag tre mx my selec?)]
    [else (make-tree (tree-node tre) (lot-after-drag (tree-children tre)
                                                     mx my selec?))]))

;; node-after-drag : Tree Integer Integer -> Tree
;; GIVEN: Tree X-Coordinates Y-Coordinates
;; RETURNS: Tree after drag
;; Example:
; (node-after-drag CHILD1-SELECTED 255 25)->
; (list (make-tree (make-node 255 15 CIRCLE #true (make-delta 0 -10)) empty)
; (make-tree (make-node 475 172 SQUARE #false (make-delta 0 0)) empty))
;; STRATEGY: use Template of Tree on tre, mx and my
;; HALTING MEASURE: length of (tree-children tre)
(define (node-after-drag tre mx my)
  (let ([nod (tree-node tre)])
    (make-tree (create-node nod mx my)
               (lot-after-drag (tree-children tre)
                               (- (add-diff mx (delta-x (node-dt nod)))
                                  (node-x nod))
                               (- (add-diff my (delta-y(node-dt nod)))
                                  (node-y nod)) #t))))

;; create-node : Node Integer Integer -> Node
;; GIVEN: Node X-Coordinates Y-Coordinates
;; RETURNS: Node after drag
;; Example:
; (create-node CHILD1-SELECTED 255 25)->
; (make-node 255 25 "circle" #true (make-delta 0 0))
;; STRATEGY: use Node template on nod, mx and my
(define (create-node  nod mx my)
  (make-node (add-diff mx (delta-x (node-dt nod)))
             (add-diff my (delta-y (node-dt nod)))
             (node-shape nod) (node-selected? nod) (node-dt nod)))

;; child-node-after-drag : Tree Integer Integer Selected?-> Tree
;; GIVEN: Tree X-Coordinates Y-Coordinates Boolean
;; RETURNS: Child Tree after drag
;; Example:
;; (node-after-drag (make-tree CHILD1-SELECTED LISTS-TREES-SAMPLE-DRAG) 255 25)
;; = (make-tree (make-node 255 25 CIRCLE #true (make-delta 0 0))
;; (list (make-tree (make-node 5 -65 CIRCLE #true (make-delta 0 -10)) empty)
;; (make-tree (make-node 480 117 SQUARE #false (make-delta 0 0)) empty)))
;; STRATEGY: use Tree template on tre, mx, my and selec?
;; HALTING MEASURE: length of (tree-children tre)
(define (child-node-after-drag tre mx my selec?)
  (make-tree (make-node (+ (node-x (tree-node tre)) mx)
                        (+ (node-y (tree-node tre)) my)
                        (node-shape (tree-node tre))
                        (node-selected? (tree-node tre))
                        (node-dt (tree-node tre)))
             (lot-after-drag (tree-children tre) mx my selec?)))

;; compute-cord-diff: Integer Integer-> Integer
;; Given: Node coordinate and the clicked Coordinate
;; Returns: Difference betweenthe two
;; EXAMPLE: (compute-cord-diff 6 5)=1
;; STRATEGY: Combine Simpler Functions
(define (compute-cord-diff node-cord click-cord)
  (- node-cord click-cord))

;; add-diff: Integer Integer-> Integer
;; Given: The coordinate of the clicked event and the stored coordinate
;; Returns: The sum of two
;; EXAMPLE: (add-diff 21 33)=54
;; STRATEGY: Combine Simpler Functions
(define (add-diff click-cord stored-cord)
  (+ click-cord stored-cord))

;; in-node? : Node Integer Integer -> Boolean
;; RETURNS: true if the given coordinate is inside the bounding box of
;; the given circle.
;; EXAMPLE: (in-node? (make-node 250 20 CIRCLE #false
;(make-delta 0 0)) empty) 250 36):#true
;; STRATEGY: condition on the shape of Node
(define (in-node? nod x y)
  (if(equal? (node-shape nod) CIRCLE)
     (in-circle? nod x y)
     (in-square? nod x y)))

;; in-circle? : Node Integer Integer -> Boolean
;; RETURNS: true if the given coordinate is inside the bounding box of
;; the given NODE.
;; EXAMPLE: (in-node? (make-node 250 20 CIRCLE #false (make-delta 0 0))
; empty) 250 36):#true
;; STRATEGY: Use template for Node on nod
(define (in-circle? nod x y)
  (< (compute-distance (node-x nod) (node-y nod) x y)
     RADIUS))

;; in-square? : Node Integer Integer -> Boolean
;; RETURNS: true if the given coordinate is inside the bounding box of
;; the given NODE.
;; EXAMPLE: (in-node? (make-node 250 20 SQUARE #false (make-delta 0 0))
; empty) 250 36):#true
;; STRATEGY: Use template for Node on nod
(define (in-square? nod x y)
  (and
   (<= 
    (abs (- (node-x nod) x))
    RADIUS)
   (<=
    (abs (- (node-y nod) y))
    RADIUS)))

;; compute-distance: Int Int Int Int -> Real
;; GIVEN: (x,y) coordinate of center of the node,(x,y) coordinate of mouse
;; Returns: distance between the two points
;; EXAMPLE: (compute-distance 23 45 67 78)=55
;; STRATEGY: Combine Simpler functions
(define (compute-distance x y mx my)
  (sqrt (+ (expt (- mx x) 2) (expt (- my y) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:

(begin-for-test
  
  (check-equal? (node-to-center CHILD1-NOT-SELECTED)
                (make-posn (node-x CHILD1-NOT-SELECTED)
                           (node-y CHILD1-NOT-SELECTED))
                "POSN should have x and y coordinate of center of node")
  
  (check-equal? (world-after-key-event (make-world TREE1-WITH-ROOT-SELECTED)
                                       ADD-CIRCLE)
                (make-world TREE2-WITH-ROOT-SELECTED)
                "Tree with 2 nodes should be present")
  
  (check-equal? (world-after-key-event (initial-world 2) "m")
                (initial-world 2)
                "No change in world as unhandled key was pressed")
  
  (check-equal? (world-after-key-event (make-world TREE1-WITH-ROOT-SELECTED)
                                       ADD-SQUARE)
                (make-world TREE2-WITH-ROOT-SELECTED-AND-SQUARE-CHILD)
                "Tree with 2 nodes should be present and child is a Square")
  
  (check-equal? (world-after-key-event
                 (make-world TREE5-WITH-CHILD2-AND-CHILD1-SELECTED) DELETE)
                (make-world TREE5-WITH-CHILD2-AND-CHILD1-DELETED)
                "Tree should have only 2 child")
  
  (check-equal? (world-after-add empty CIRCLE)
                TREE1-WITH-NO-SELECTION
                "Tree with only root node should be created")
  
  (check-equal? (world-after-add TREE1-WITH-ROOT-SELECTED CIRCLE)
                TREE2-WITH-ROOT-SELECTED
                "Tree should have only one child")
  
  (check-equal? (world-after-add TREE2-WITH-ROOT-SELECTED CIRCLE)
                TREE3-WITH-ROOT-SELECTED
                "Tree should have only 2 child")
  
  (check-equal? (world-after-add TREE3-WITH-CHILD2-SELECTED CIRCLE)
                TREE4-WITH-CHILD2-SELECTED
                "New node should be added to sub-tree")
  
  (check-equal? (world-after-add TREE3-WITH-CHILD2-AND-CHILD1-SELECTED CIRCLE)
                TREE5-WITH-CHILD2-AND-CHILD1-SELECTED
                "New node should be added to both sub-trees")
  
  (check-equal? (world-after-delete TREE3-WITH-NO-SELECTION)
                TREE3-WITH-NO-SELECTION
                "No change in tree as there was no node selected")
  
  (check-equal? (world-after-delete TREE3-WITH-ROOT-SELECTED)
                (cons (make-tree CHILD2-NOT-SELECTED empty)
                      (cons (make-tree CHILD1-NOT-SELECTED empty) empty))
                "There should be two trees as previous root node is deleted")
  
  (check-equal? (world-after-delete TREE5-WITH-CHILD2-AND-CHILD1-SELECTED)
                TREE5-WITH-CHILD2-AND-CHILD1-DELETED
                "root of both sub-trees should be deleted")
  
  (check-equal? (world-after-delete TREE4-WITH-CHILD2-SELECTED)
                TREE4-WITH-CHILD2-DELETED
                "root of sub-tree should be deleted")
  
  (check-equal? (get-left-most-tree MIXED-LOT)
                (make-tree (make-node 70 100 CIRCLE #false INITIAL-DELTA)
                           empty)
                "Node with minimum x-coord value should be root of sub-tree
                returned")
  
  (check-equal? (is-any-tree-selected? TREE3-WITH-ROOT-SELECTED)
                #t
                "Result should be true")
  
  (check-equal? (is-any-tree-selected? TREE3-WITH-CHILD2-SELECTED)
                #t
                "Result should be true")
  
  (check-equal? (is-any-tree-selected? TREE3-WITH-NO-SELECTION)
                #f
                "Result should be false")
  
  (check-equal? (world-to-scene WORLD-CIRCLE-SQUARE)
                (world-to-scene WORLD-CIRCLE-SQUARE)
                "World Circle Square image")
  
  (check-equal? (world-to-scene WORLD-CIRCLE-SQUARE-SELECT)
                (world-to-scene WORLD-CIRCLE-SQUARE-SELECT)
                "World selected Circle Square image")
  
  (check-equal? (world-to-scene WORLD-WITH-LINE)
                (world-to-scene WORLD-WITH-LINE)
                "World with line")
  
  (check-equal? (world-after-mouse-event WORLD-CIRCLE-SQUARE 250 30
                                         BUTTON-DOWN)
                WORLD-AFTER-BUTTON-DOWN
                "World after button down")
  (check-equal? (node-after-button-up (make-tree (make-node 168 130
                                                            SQUARE #true
                                                            INITIAL-DELTA)
                                                 empty))
                (make-tree (make-node 168 130 SQUARE #false INITIAL-DELTA)
                           empty) 
                "Node state after button is up")
  
  (check-equal? (lot-after-drag LISTS-TREES-SAMPLE-AFTER 255 25 #true)
                LISTS-TREES-SAMPLE-DRAG "ListOfTree after dragging")
  
  (check-equal? (in-square? (make-node 251 21 SQUARE #true
                                       (make-delta -18 -14)) 233 7)
                #true "Checking for the click in square")
  
  (check-equal? (lot-after-mouse-event LISTS-TREES-SAMPLE-DRAG 250 30 DRAG)
                (list (make-tree (make-node 250 20 CIRCLE #true
                                            (make-delta 0 -10)) empty)
                      (make-tree (make-node 475 172 SQUARE
                                            #false INITIAL-DELTA)
                                 empty))
                "ListOfTree after dragging")
  
  (check-equal? (lot-after-mouse-event LISTS-TREES-SAMPLE-DRAG 250 30
                                       BUTTON-UP)
                (list (make-tree (make-node 505 45 CIRCLE #false INITIAL-DELTA)
                                 empty)
                      (make-tree (make-node 475 172 SQUARE
                                            #false INITIAL-DELTA)
                                 empty))
                "ListOfTree after button up")
  
  (check-equal? (lot-after-mouse-event LISTS-TREES-SAMPLE-DRAG 250 30
                                       ENTER)
                (list
                 (make-tree (make-node 505 45 CIRCLE
                                       #true (make-delta 0 -10)) '())
                 (make-tree (make-node 475 172 SQUARE
                                       #false INITIAL-DELTA) '()))
                "ListOfTree after Some other mouse event")
  
  (check-equal? (lot-after-button-up LISTS-TREES-SAMPLE)
                (list
                 (make-tree (make-node 250 20 CIRCLE
                                       #false INITIAL-DELTA)
                            empty)
                 (make-tree (make-node 220 147 SQUARE
                                       #false INITIAL-DELTA)
                            empty))
                "ListOfTree after button up"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;