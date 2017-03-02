;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname class-lists) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; class-lists
;; Professor Felleisen and Professor Shivers each keep their class lists on slips of paper,
;; one student on each slip. Professor Felleisen keeps his list on slips of yellow paper.
;; Professor Shivers keeps his list on slips of blue paper.
;; Sometimes they have more than one slip for the same student.
;; Sometimes they record the student names first-name first,
;; and sometimes they record the names last-name first.
;; One day both lists got mixed up.


(require rackunit)
(require "extras.rkt")
(check-location "04" "class-lists.rkt")

(require 2htdp/universe)

(provide
  felleisen-roster
  shivers-roster
  possible-roster?
  acceptable-felleisen-answer?
  make-slip
  slip-color
  slip-name1
  slip-name2) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; Color related constants
(define YELLOW "yellow")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Color is one of
;; -- "yellow"
;; -- "blue"
;; ITERPRETATION: Self-evident

;; TEMPLATE
;; color-fn : Color -> ??
#|
(define (color-fn input)
   (cond
     [(string=? inpuy YELLOW) ..]
     [(string=? inpuy BLUE) ..]))
|#

(define-struct slip (color name1 name2))
;; A Slip is a (make-slip Color String String)
;; Here color is parameter which identifies color of the slip
;; name1 and name2 are first and last name of the person,they
;; can be present in any order

;; TEMPLATE:
;; slip-fn : Slip -> ??
;; (define (slip sl)
;;   (.. (slip-color sl)
;;    (slip-name1 sl) (slip-name2 sl)))

;; ListOfSlips

;; ListOfSlips(LOS) is either
;; -- empty
;; -- (cons Slip LOS)

;; TEMPLATE:
;; los-fn : LOS -> ??
;; (define (los-fn los)
;;   (cond
;;     [(empty? los)...]
;;     [ else (...
;;               (slip-fn (first los))
;;               (los-fn (rest los)))]))

;;; END DATA DEFINITIONS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Examples
(define flst1 (list
              (make-slip YELLOW "Xi" "Wang")
              (make-slip YELLOW "K." "Shriram")))

(define flst2 (list
              (make-slip YELLOW "K." "Shriram")))

(define lst1 (list
              (make-slip YELLOW "Wang" "Xi")
              (make-slip BLUE "Jones" "Tom")
              (make-slip YELLOW "Xi" "Wang")
              (make-slip YELLOW "Shriram" "K.")
              (make-slip YELLOW "K." "Shriram")))

(define slst1 (list
              (make-slip BLUE "Xi" "Wang")
              (make-slip BLUE "K." "Shriram")))
  
(define lst2 (list
              (make-slip BLUE "Wang" "Xi")
              (make-slip YELLOW "Jones" "Tom")
              (make-slip BLUE "Xi" "Wang")
              (make-slip BLUE "Shriram" "K.")
              (make-slip BLUE "K." "Shriram")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; felleisen-roster : ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Felleisen's class, without duplication.
;; EXAMPLE:
;; ( felleisen-roster lst1) = flst1
;; STRATEGY: Use template for LOS on los
;; HALTING-MEASURE: (length los)
(define (felleisen-roster los)
  (cond
    [(empty? los) empty]
    [else (if (slip-in-felleisen? (first los))
                  (if (not-present-in-list? (first los) (felleisen-roster (rest los)))
                     (cons (first los) (felleisen-roster (rest los)))
                   (felleisen-roster (rest los)))
               (felleisen-roster (rest los)))]))

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' class, without duplication.
;; EXAMPLE:
;; ( shivers-roster lst2) = slst1
;; STRATEGY: Use template for LOS on los
;; HALTING-MEASURE: (length los)
(define (shivers-roster los)
  (cond
    [(empty? los) empty]
    [else (if (slip-in-shivers? (first los))
                  (if (not-present-in-list? (first los) (shivers-roster (rest los)))
                     (cons (first los) (shivers-roster (rest los)))
                   (shivers-roster (rest los)))
               (shivers-roster (rest los)))]))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;                  and no student is represented twice.
;; EXAMPLE:
;; (possible-roster? lst1) = #false
;; (possible-roster? slst1) = #true
;; STRATEGY: Combine simple functions
(define (possible-roster? los)
  (if (or (equal? (length (shivers-roster los)) (length los))
          (equal? (length (felleisen-roster los)) (length los)))
       #true
   #false))

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, lst1 and lst2
;; RETURNS: true iff every student on a yellow slip in lst1 appears once
;; and only once in lst2.
;; EXAMPLE:
;; (acceptable-felleisen-answer? lst1 flst1) = #true
;; (acceptable-felleisen-answer? flst1 lst1) = #false
;; (acceptable-felleisen-answer? lst1 flst2) = #false
;; STRATEGY: Use template for LOS on los1
;; HALTING-MEASURE: (length los1)
(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [(empty? los1) #true]
    [(not (equal? (length(felleisen-roster los2)) (length los2))) #false] 
    [else (if (slip-in-felleisen? (first los1))
              (if (has-only-one-copy? (first los1) los2)
                  (acceptable-felleisen-answer? (rest los1) los2)
                  #false)
            (acceptable-felleisen-answer? (rest los1) los2))]))

;; has-only-one-copy? Slip ListOfSlip -> Boolean
;; GIVEN: slip and a list of slips
;; RETURNS: true iff there is only one copy of slip in list.
;; EXAMPLE:
;; (has-only-one-copy? (make-slip YELLOW "Xi" "Wang") lst1) = #false
;; STRATEGY: Combine simple functions
(define (has-only-one-copy? slip los)
  (equal? (copies-found slip los) 1))

;; copies-found: Slip ListOfSlip -> Int
;; GIVEN: slip and list of slip
;; RETURNS: number of copies of slip found in list of slip
;; EXAMPLE:
;; (copies-found (make-slip YELLOW "Xi" "Wang") lst1) = 2
;; STRATEGY: Use template for ListOfSlip on los
;; HALTING-MEASURE: (length los)
(define (copies-found slip los)
  (cond
     [(empty? los) (+ 0)]
       [else (if (compare-slips? slip (first los))
              (+ 1 (copies-found slip (rest los)) )
              (copies-found slip (rest los)))]))

;; slip-in-felleisen? : Slip -> Boolean
;; GIVEN: a slip
;; RETURNS: true iff slip belongs to felleisen
;; EXAMPLE:
;; (slip-in-felleisen? (make-slip YELLOW "Xi" "Wang")) = #true
;; (slip-in-felleisen? (make-slip BLUE "Xi" "Wang")) = #false
;; STRATEGY: Using template of slip on s1
(define (slip-in-felleisen? sl)
  (string=? (slip-color sl) YELLOW))

;; slip-in-shivers? : Slip -> Boolean
;; GIVEN: a slip
;; RETURNS: true iff slip belongs to shivers
;; EXAMPLE:
;; (slip-in-shivers? (make-slip YELLOW "Xi" "Wang")) = #false
;; (slip-in-shivers? (make-slip BLUE "Xi" "Wang")) = #true
;; STRATEGY: Using template of slip on s1
(define (slip-in-shivers? sl)
  (string=? (slip-color sl) BLUE))

;; not-present-in-list? : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a slip and a list of slips 
;; RETURNS: true iff list of slips doesn't have given slip.
;; EXAMPLE:
;; (not-present-in-list? (make-slip YELLOW "Wang" "Xin") lst1) = #true
;; STRATEGY: Use template for ListOfSlip on los
;; HALTING-MEASURE: (length los)
(define (not-present-in-list? slip los)
  (cond
    [(empty? los) #true]
    [else (if (compare-slips? slip (first los))
              #false
           (not-present-in-list? slip (rest los)))]))

;; compare-slips? : slip slip -> Boolean
;; GIVEN: two slips
;; RETURNS: true iff they have information about same students
;; EXAMPLE:
;; (compare-slips? (make-slip YELLOW "Wang" "Xi") (make-slip YELLOW "Xi" "Wang")) = #true
;; STRATEGY: Combine simple functions
(define (compare-slips? slip1 slip2)
  (if (or (and (string=? (slip-name1 slip1) (slip-name1 slip2))
              (string=? (slip-name2 slip1) (slip-name2 slip2)))
         (and (string=? (slip-name2 slip1) (slip-name1 slip2))
              (string=? (slip-name1 slip1) (slip-name2 slip2))))
              #true
  #false))

;; TEST CASES
(begin-for-test
  (check-equal? (compare-slips? (make-slip YELLOW "Wang" "Xi")
                                (make-slip YELLOW "Xi" "Wang"))
                #true)

  (check-equal? (felleisen-roster lst1) flst1)

  (check-equal? (shivers-roster lst2) slst1)

  (check-equal? (possible-roster? lst1) #false)
  (check-equal? (possible-roster? slst1) #true)
  (check-equal? (acceptable-felleisen-answer? lst1 flst1) #true)
  (check-equal? (acceptable-felleisen-answer? lst1 flst2) #false)
  (check-equal? (acceptable-felleisen-answer? lst1 lst2) #false))

  