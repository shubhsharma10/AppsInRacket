;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Q1 : class-lists
;; Professor Felleisen and Professor Shivers each keep their class lists on
;; slips of paper, one student on each slip.
;; Professor Felleisen keeps his list on slips of yellow paper.
;; Professor Shivers keeps his list on slips of blue paper.
;; Sometimes they have more than one slip for the same student.
;; Sometimes they record the student names first-name first,
;; and sometimes they record the names last-name first.
;; One day both lists got mixed up.


(require rackunit)
(require "extras.rkt")
(check-location "05" "q1.rkt")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

;; Color related constants
(define YELLOW "yellow")
(define BLUE "blue")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Color is one of
;; -- "yellow"
;; -- "blue"
;; ITERPRETATION: Self-evident

;; TEMPLATE
;; color-fn : Color -> ??
#|
(define (color-fn input)
   (cond
     [(string=? input YELLOW) ..]
     [(string=? input BLUE) ..]))
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
(define EMPTY-LIST empty)

(define FLST1 (list
               (make-slip YELLOW "Xi" "Wang")
               (make-slip YELLOW "K." "Shriram")))

(define FLST2 (list
               (make-slip YELLOW "K." "Shriram")))

(define MIXED-LIST1 (list
                     (make-slip YELLOW "Wang" "Xi")
                     (make-slip BLUE "Jones" "Tom")
                     (make-slip YELLOW "Xi" "Wang")
                     (make-slip YELLOW "Shriram" "K.")
                     (make-slip YELLOW "K." "Shriram")))

(define SHIVERS-LIST1 (list
                       (make-slip BLUE "Xi" "Wang")
                       (make-slip BLUE "K." "Shriram")))

(define MIXED-LIST2 (list
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
;; ( felleisen-roster MIXED-LIST1) = FMIXED-LIST1
;; STRATEGY: Call a more general function
(define (felleisen-roster los)
  (general-roster los YELLOW))

;; shivers-roster: ListOfSlip -> ListOfSlip
;; GIVEN: a list of slips
;; RETURNS: a list of slips containing all the students in Professor
;; Shivers' class, without duplication.
;; EXAMPLE:
;; ( shivers-roster MIXED-LIST2) = sMIXED-LIST1
;; STRATEGY: Call a more general function
(define (shivers-roster los)
  (general-roster los BLUE))

;; general-roster : ListOfSlip Color -> ListOfSlip
;; GIVEN: List of slips and color
;; RETURNS: list of slip with duplicated removed and color matching with
;;          given color
;; EXAMPLE:
;; (general-roster MIXED-LIST2 BLUE) = sMIXED-LIST1
;; STRATEGY: Use foldr HOF on los after suing filter HOF on los
(define (general-roster los exp-color)
  (foldr append-if-not-in-list empty
         (filter
          (
           ; Slip -> Boolean
           ; True iff slip has same color as parameter
           ; passed
           lambda (x) (same-color? x exp-color)) los)))

;; same-color? : Slip Color -> Boolean
;; GIVEN: Slip and color(exp-color)
;; RETURNS: True iff slip has same color as expected color(exp-color)
;; EXAMPLE:
;; (same-color? (make-slip YELLOW "Xi" "Wang") BLUE) = #false
;; STRATEGY: Combine simple function
(define (same-color? slip exp-color)
  (string=? (slip-color slip) exp-color))

;; append-if-not-in-list : Slip ListOfSlip -> ListOfSlip
;; GIVEN: a slip and list of slip
;; RETURNS: adds slip to ListOfSlip if slip is not already present in list
;; EXAMPLE:
;; (append-if-not-in-list (make-slip YELLOW "Xi" "Wang") FLST1)
;; = FLST1
;; STRATEGY: Combine simple functions
(define (append-if-not-in-list slip los)
  (if (is-in-list? slip los)
      los
      (cons slip los)))

;; is-in-list? : Slip ListOfSlip -> Boolean
;; GIVEN: a slip and a list of Slips
;; RETURNS : true iff slip with same name combination is present in ListOfSlip
;; EXAMPLE:
;; (is-in-list?? (make-slip YELLOW "Xi" "Wang") FLST1)
;; = #true
;; STRATEGY: Use HOF ormap on los
(define (is-in-list? slip los)
  (ormap
   (
    ; Slip -> Boolean
    ; True iff slip has same name as of parameter passed
    lambda (x) (compare-slip-names? x slip))
   los))

;; possible-roster? : ListOfSlip -> Boolean
;; GIVEN: a list of slips
;; RETURNS: true iff all the slips in the list are the same color,
;;                  and no student is represented twice.
;; EXAMPLE:
;; (possible-roster? MIXED-LIST1) = #false
;; (possible-roster? SHIVERS-LIST1) = #true
;; STRATEGY: Combine simple functions
(define (possible-roster? los)
  (if (or (equal? (shivers-roster los) los)
          (equal? (felleisen-roster los) los))
      #true
      #false))

;; acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
;; GIVEN: two lists of slips, MIXED-LIST1 and MIXED-LIST2
;; RETURNS: true iff every student on a yellow slip in MIXED-LIST1 appears once
;; and only once in MIXED-LIST2.
;; EXAMPLE:
;; (acceptable-felleisen-answer? MIXED-LIST1 FLST1) = #true
;; (acceptable-felleisen-answer? FLST1 MIXED-LIST1) = #false
;; (acceptable-felleisen-answer? MIXED-LIST1 FLST2) = #false
;; STRATEGY: Use cases on los1 and los2
(define (acceptable-felleisen-answer? los1 los2)
  (cond
    [(empty? los1) #true]
    [(not (equal? (length (felleisen-roster los2)) (length los2))) #false] 
    [else (subset-of-los2? (felleisen-roster los1) los2)]))

;; has-only-one-copy? Slip ListOfSlip -> Boolean
;; GIVEN: slip and a list of slips
;; RETURNS: true iff there is only one copy of slip in list.
;; EXAMPLE:
;; (has-only-one-copy? (make-slip YELLOW "Xi" "Wang") MIXED-LIST1) = #false
;; STRATEGY: Combine simple functions
(define (has-only-one-copy? slip los)
  (equal? (copies-found slip los) 1))

;; subset-of-los2? : ListOfSlip ListOfSlip -> ListOfSlip
;; RETURNS: True iff los2 has only one copy of each item of los1
;; EXAMPLE:
;; (subset-of-los2? (make-slip YELLOW "Xi" "Wang") MIXED-LIST1) = #false
;; STRATEGY: Use HOF andmap on los1
(define (subset-of-los2? los1 los2)
  (andmap
   (
    ; Slip -> Boolean
    ; True iff ListOfSlip has only one copy of slip
    ; passed as parameter
    lambda (x) (has-only-one-copy? x los2))
   los1))

;; copies-found: Slip ListOfSlip -> Int
;; GIVEN: slip and list of slip
;; RETURNS: number of copies of slip found in list of slip
;; EXAMPLE:
;; (copies-found (make-slip YELLOW "Xi" "Wang") MIXED-LIST1) = 2
;; STRATEGY: Use filter HOF on los
(define (copies-found slip los)
  (- (length los)
     (length
      (filter
       (; Slip -> Boolean
        ; True iff slip has same name as name of slip parameter passed
        lambda (x) (compare-slip-names? x slip)) los))))

;; compare-slip-names? : Slip Color Slip-> Boolean
;; GIVEN: two slips
;; RETURNS: true iff they have same names in slips
;; EXAMPLE:
;; (compare-slip-names? (make-slip YELLOW "Wang" "Xi")
;;                 (make-slip BLUE "Xi" "Wang")) = #true
;; STRATEGY: Combine simple functions
(define (compare-slip-names? slip1 slip2)
  (if (or (and (string=? (slip-name1 slip1) (slip-name1 slip2))
               (string=? (slip-name2 slip1) (slip-name2 slip2)))
          (and (string=? (slip-name2 slip1) (slip-name1 slip2))
               (string=? (slip-name1 slip1) (slip-name2 slip2))))
      #true
      #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TEST CASES
(begin-for-test
  (check-equal? (compare-slip-names? (make-slip YELLOW "Wang" "Xi")
                                     (make-slip YELLOW "Xi" "Wang"))
                #true)
  
  (check-equal? (felleisen-roster MIXED-LIST1) FLST1)
  
  (check-equal? (shivers-roster MIXED-LIST2) SHIVERS-LIST1)
  
  (check-equal? (possible-roster? MIXED-LIST1) #false)
  (check-equal? (possible-roster? SHIVERS-LIST1) #true)
  (check-equal? (acceptable-felleisen-answer? EMPTY-LIST MIXED-LIST1) #true)
  (check-equal? (acceptable-felleisen-answer? EMPTY-LIST EMPTY-LIST) #true)
  (check-equal? (acceptable-felleisen-answer? MIXED-LIST1 EMPTY-LIST) #false)
  (check-equal? (acceptable-felleisen-answer? MIXED-LIST1 FLST1) #true)
  (check-equal? (acceptable-felleisen-answer? MIXED-LIST1 FLST2) #false)
  (check-equal? (acceptable-felleisen-answer? MIXED-LIST1 MIXED-LIST2) #false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;