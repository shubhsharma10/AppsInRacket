;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; q-2
;; a tester that will accept correct solution to to original
;; registrar's problem and rejects any incorrect solution

(require rackunit)
(require "extras.rkt")
(check-location "05" "q2.rkt")

(require 2htdp/universe)
(require 2htdp/image)

(provide
 make-enrollment
 enrollment-student
 enrollment-class
 make-roster
 roster-classname
 roster-students
 behavior-correct?
 enrollments-to-rosters
 enrollments-to-rosters-bad-1
 enrollments-to-rosters-bad-2
 enrollments-to-rosters-bad-3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;; SetOfX is a list of X's without duplication.  Two SetOfX's are
;; considered equal if they have the same members. X can be of
;; any data type here

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

;; DATA DEFINITIONS END
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-struct enrollment(student class))
;; A EnrollmentAssertion is (make-enrollment Student Class)
;; (make-enrollement s c) which implies student s is enrolled in class c.
;; s is student information which is of data type Student,
;; which can be any data type
;; c is class information which is of data type Class,
;; which can be any data type.

;; TEMPLATE:
;; enrollment-fn : EnollmentAssertion -> ??
;; (define (enrollment input)
;;   (... (enrollment-student input) (enrollment-class input)))

(define-struct roster(classname students))
;; A ClassRosterAssertion is (make-roster Class SetOfStudent)
;; (make-roster c ss) which implies that students in class c are exactly
;; the students in set ss.
;; c is class name which is of data type Class.
;; ss is SetOfStudent which is a list of Student without duplication

;; TEMPLATE:
;; roster-fn : ClassRosterAssertion -> ??
;; (define (roster input)
;;    (... (roster-classname input) (roster-students input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; EXAMPLES:

(define JOHN-PDP-ENROLL
  (make-enrollment "John" "PDP"))

(define KAT-NETWORKS-ENROLL
  (make-enrollment "Kathryn" "Networks"))

(define FENG-PDP-ENROLL
  (make-enrollment "Feng" "PDP"))

(define AMY-PDP-ENROLL
  (make-enrollment "Amy" "PDP"))

(define AMY-NETWORKS-ENROLL
  (make-enrollment "Amy" "Networks"))

(define MIXED-ENROLLMENTS1
  (list
   JOHN-PDP-ENROLL
   KAT-NETWORKS-ENROLL
   FENG-PDP-ENROLL
   AMY-PDP-ENROLL  
   AMY-NETWORKS-ENROLL))

(define PDP-ROSTER-WITH-AMY
  (make-roster "PDP" (list "Amy")))

(define PDP-ROSTER-WITH-JOHN
  (make-roster "PDP" (list "John")))

(define PDP-ROSTER-WITH-ALL
  (make-roster "PDP" (list "Amy" "Feng" "John")))

(define NETWORKS-ROSTER-WITH-AMY
  (make-roster "Networks" (list "Amy")))

(define NETWORKS-ROSTER-WITH-KAT
  (make-roster "Networks" (list "Kathryn")))

(define NETWORKS-ROSTER-WITH-ALL
  (make-roster "Networks" (list "Amy" "Kathryn")))

(define ROSTER-LIST1
  (list
   (make-roster "PDP" (list "John"))
   (make-roster "Networks" (list "Kathryn"))
   (make-roster "PDP" (list "Feng"))
   (make-roster "PDP" (list "Amy"))
   (make-roster "Networks" (list "Amy"))))

(define MIXED-ROSTERS1
  (list
   PDP-ROSTER-WITH-ALL
   NETWORKS-ROSTER-WITH-ALL))

(define MIXED-ENROLLMENTS2
  (list
   (make-enrollment 11 "AB")
   (make-enrollment 13 "CD")
   (make-enrollment 15 "CD")
   (make-enrollment 17 "AB")))

(define MIXED-ROSTERS2
  (list
   (make-roster "AB" (list 17 11))
   (make-roster "CD" (list 15 13))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A ProposedSolution is a function with contract
;; SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; that is, it is a function that takes a SetOfEnrollmentAssertion and
;; produces a SetOfClassRosterAssertion

#|
EXAMPLE:
If soln1 is a ProposedSolution, we might have
(soln1
(list (make-enrollment "John" "PDP")
(make-enrollment "Kathryn" "Networks")
(make-enrollment "Feng" "PDP")
(make-enrollment "Amy" "PDP")
(make-enrollment "Amy" "Networks")))
=>
(list
(make-roster "PDP" (list "John" "Feng" "Amy"))
(make-roster "Networks" (list "Kathryn" "Amy")))|#

;; behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
;; GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
;; RETURNS: true iff the output of soln-fn on se is an example of correct
;; behavior by a ProposedSolution.
;; EXAMPLE: (behavior-correct? enrollments-to-rosters-bad-1 MIXED-ENROLLMENTS1)
;;          = #false
;; STRATEGY: Combine simple functions
(define (behavior-correct? soln-fn se)
  (and (subset-of-second-set? (soln-fn se)
                              (enrollments-to-rosters se)
                              compare-rosters?)
       (subset-of-second-set? (enrollments-to-rosters se)
                              (soln-fn se)
                              compare-rosters?)))

;; enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: a set of enrollments
;; RETURNS: a correct set of class rosters for the given enrollments
;; EXAMPLE: (enrollments-to-rosters MIXED-ENROLLMENTS1)
;;        = MIXED-ROSTERS1
;; STRATEGY: Combine simple functions
(define (enrollments-to-rosters se)
  (combine-duplicates (convert-all-to-roster-list se)))

;; subset-of-second-set? : SetOfX SetOfX (X X -> Boolean)-> Boolean
;; GIVEN: Two set of X, X can be any data type here
;;      and compare function which will be used to compare two X
;; RETURNS: True iff first set is subset of second set
;; EXAMPLE: (subset-of-second-set?
;;            MIXED-ROSTERS1 MIXED-ROSTERS1 compare-rosters?)
;;           = #true
;; STRATEGY: Use HOF andmap on set1
(define (subset-of-second-set? set1 set2 compare-fn)
  (andmap
   ;; X -> Boolean
   ;; true if every item of set1 is present only once in set2
   (lambda (x) (has-only-one-copy? x set2 compare-fn))
   set1))

;; has-only-one-copy? X SetOfX (X X -> Boolean) -> Boolean
;; GIVEN: X, a set of X, X can be of any data type here
;;        and compare function which will be used to compare two X
;; RETURNS: true iff there is only one copy of X in set.
;; EXAMPLE: (has-only-one-copy?
;;                PDP-ROSTER-WITH-AMY
;;                MIXED-ROSTERS1 compare-rosters?)
;;          = #false
;; STRATEGY: Combine simple functions
(define (has-only-one-copy? x sox compare-fn)
  (equal? (copies-found x sox compare-fn) 1))

;; copies-found: X SetOfX (X X -> Boolean)-> Int
;; GIVEN: X, set of X, X can be of any data type here
;;       and compare function which will be used to compare two X
;; RETURNS: number of copies of x found in set of x
;; EXAMPLE: (copies-found
;;           PDP-ROSTER-WITH-AMY MIXED-ROSTERS1 compare-rosters?)
;;          = 0
;; STRATEGY: Use filter HOF on sox
(define (copies-found x sox compare-fn)  
  (length
   (filter
    (; Slip -> Boolean
     ; True iff y is same as of parameter passed
     lambda (y) (compare-fn y x)) sox)))

;; compare-rosters? : Roster Roster Boolean
;; GIVEN: two roster
;; RETURNS: true iff they are both roster have same classname
;;          and same set of students
;; EXAMPLE: (compare-rosters? PDP-ROSTER-WITH-AMY NETWORKS-ROSTER-WITH-AMY)
;;          #false
;; STRATEGY: Combine simple functions
(define (compare-rosters? x1 x2)
  (and (equal? (roster-classname x1) (roster-classname x2))
       (subset-of-second-set? (roster-students x1)
                              (roster-students x2)
                              compare-student?)
       (subset-of-second-set? (roster-students x2)
                              (roster-students x1)
                              compare-student?)))

;; compare-student? : Student Student -> Boolean
;; GIVEN: two students
;; RETURNS: true if both students are same
;; EXAMPLE: (compare-student? "Amy" "Amy") = #true
;; STRATEGY: Combine simple function
(define (compare-student? st1 st2)
  (equal? st1 st2))

;; combine-duplicates:
;; ListOfClassRosterAssertion -> SetOfClassRosterAssertion
;; RETURNS: Combine duplicates and returns SetOfClassRosterAssertion
;; EXAMPLE : (combine-duplicates ROSTER-LIST1) = MIXED-ROSTERS1
;; STRTEGY: apply foldr HOF on lor
(define (combine-duplicates lor)
  (foldr convert-list-to-set empty lor))

;; convert-list-to-set:
;; Roster ListOfClassRosterAssertion -> SetOfClassRosterAssertion
;; GIVEN: roster and ListOfClassRosterAssertion
;; RETURNS: if matching roster is present in list then updates matching roster
;;          or appends to list and returns SetOfClassRosterAssertion
;; EXAMPLE:
;; (convert-list-to-set
;;            NETWORKS-ROSTER-WITH-KAT
;;            (cons NETWORKS-ROSTER-WITH-AMY empty))
;; = (cons NETWORKS-ROSTER-WITH-ALL empty)
;; STRATEGY: Combine simple function
(define (convert-list-to-set rt lst)
  (if (is-matching-roster-present? rt lst)
      (update-matching-roster-in-set rt lst)      
      (cons rt lst)))

;; update-matching-roster-in-set :
;; Roster ListOfClassRosterAssertion -> SetOfClassRosterAssertion
;; GIVEN: roster and ListOfClassRosterAssertion
;; RETURNS: updates matching roster and return it with
;;          rest of list as SetOfClassRosterAssertion
;; EXAMPLE:
;; (update-matching-roster-in-set
;;              NETWORKS-ROSTER-WITH-KAT
;;              (cons NETWORKS-ROSTER-WITH-AMY empty))
;; = (cons NETWORKS-ROSTER-WITH-ALL empty)
;; STRATEGY: Combine simple functions and apply filter HOF on lst
(define (update-matching-roster-in-set rt lst)
  (cons
   (find-and-update-matching-roster rt lst)
   (filter
    ;; Class -> Boolean
    ;; False if class is same as of present in rt
    (lambda (x) (not(same-class? (roster-classname rt) (roster-classname x))))
    lst)))

;; is-matching-roster-present? : Roster ListOfClassRosterAssertion -> Boolean
;; GIVEN : roster and ListOfClassRosterAssertion
;; RETURNS : true iff there exists a roster in list with same class
;;           as of class of rt roster
;; EXAMPLE :
;; (is-matching-roster-present?
;;              NETWORKS-ROSTER-WITH-KAT
;;              (cons NETWORKS-ROSTER-WITH-AMY empty))
;; = #true
;; STRATEGY: Use ormap HOF on lst
(define (is-matching-roster-present? rt lst)
  (ormap
   ;; Roster -> Boolean
   ;; Returns true iff class is same as present in rt
   (lambda (x) (same-class? (roster-classname rt) (roster-classname x)))
   lst))

;; find-and-update-matching-roster : Roster ListOfClassRosterAssertion -> Roster
;; GIVEN: roster and ListOfClassRosterAssertion
;; RETURNS: finds matching roster and appends students to matching roster's
;;          students set and returns matching roster
;; EXAMPLE:
;; (find-and-update-matching-roster
;;              NETWORKS-ROSTER-WITH-KAT
;;              (cons NETWORKS-ROSTER-WITH-AMY empty))
;; = NETWORKS-ROSTER-WITH-ALL
;; STRATEGY: Use ListOfX template on lst
;; HALTING-MEASURE: (length lst) or when roster with same class is found
(define (find-and-update-matching-roster rt lst)
  (cond
    [(empty? lst) rt]
    [(same-class? (roster-classname rt) (roster-classname (first lst)))
     (make-roster
      (roster-classname (first lst))
      (append (roster-students(first lst)) (roster-students rt)))]
    [else (find-and-update-matching-roster rt (rest lst))]))

;; convert-all-to-roster-list :
;; SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN : SetOfEnrollmentAssertion
;; RETURNS: creates roster from each enrollments and returns as a
;;          ListOfClassRosterAssertion
;; EXAMPLE:
;; (convert-all-to-roster-list MIXED-ENROLLMENTS1) = ROSTER-LIST1
;; STRATEGY: Use map HOF on se
(define (convert-all-to-roster-list se)
  (map
   ;; Enrollment -> Roster
   ;; Creates roster from enrollment
   (lambda (x) (make-roster (enrollment-class x)
                            (cons (enrollment-student x) empty)))
   se))

;; same-class? Class Class -> Boolean
;; GIVEN: two class objects
;; RETURNS: True iff both are smae
;; EXAMPLE:
;; (same-class? "PDP" "Networks")
;; = #true
;; STRATEGY: Combine simple function
(define (same-class? class1 class2)
  (equal? class1 class2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bad solution-1

;; enrollments-to-rosters-bad-1:
;;         SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: SetOfEnrollmentAssertion
;; RETURNS: SetOfClassRosterAssertion with only one roster
;;          and one student in student set
;; EXAMPLE: (enrollments-to-rosters-bad-1 MIXED-ENROLLMENTS1)
;;          = (cons PDP-ROSTER-WITH-JOHN empty)
;; STRATEGY: Combine simple function
(define (enrollments-to-rosters-bad-1 se)
  (combine-duplicates (convert-all-to-rosters-bad-1 se)))

;; convert-all-to-rosters-bad-1 :
;; SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN : SetOfEnrollmentAssertion
;; RETURNS: creates roster from only 1 enrollment and returns as a
;;          ListOfClassRosterAssertion
;; EXAMPLE: (convert-all-to-rosters-bad-1 MIXED-ENROLLMENTS1)
;;          = (cons PDP-ROSTER-WITH-JOHN empty)
;; STRATEGY: Roster constructor template on se
(define (convert-all-to-rosters-bad-1 se)
  (list (make-roster (enrollment-class (first se))
                     (cons (enrollment-student (first se)) empty))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bad solution-2

;; enrollments-to-rosters-bad-2:
;;         SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: SetOfEnrollmentAssertion
;; RETURNS : SetOfClassRosterAssertion having only one student per class
;; EXAMPLE:
;; (enrollments-to-rosters-bad-2 MIXED-ENROLLMENTS1)
;; = (cons PDP-ROSTER-WITH-AMY (cons NETWORKS-ROSTER-WITH-AMY empty))
;; STRATEGY: Combine simple functions
(define (enrollments-to-rosters-bad-2 se)
  (combine-duplicates (convert-all-to-rosters-bad-2 se)))

;; convert-all-to-rosters-bad-2 :
;; SetOfEnrollmentAssertion -> ListOfClassRosterAssertion
;; GIVEN : SetOfEnrollmentAssertion
;; RETURNS: creates only one roster per class with only one student
;;          and returns as a  ListOfClassRosterAssertion
;; EXAMPLE:
;; (convert-all-to-rosters-bad-2 MIXED-ENROLLMENTS1)
;; = (cons PDP-ROSTER-WITH-AMY (cons NETWORKS-ROSTER-WITH-AMY empty))
;; STRATEGY: Combine simple functions
(define (convert-all-to-rosters-bad-2 se)
  (convert-all-to-roster-list (remove-duplicate-class se)))

;; remove-duplicate-class:
;;          SetOfEnrollmentAssertion -> SetOfEnrollmentAssertion
;; GIVEN: SetOfEnrollmentAssertion
;; RETURNS: SetOfEnrollmentAssertion with no enrollment having same class
;; STRATEGY: Use foldr HOF on se
(define (remove-duplicate-class se)
  (foldr combine-if-class-not-present empty se))

;; combine-if-class-not-present :
;;          Enrollment SetOfEnrollmentAssertion -> SetOfEnrollmentAssertion
;; GIVEN: Enrollment and SetOfEnrollmentAssertion
;; RETURNS: Append enroll to Set if set doesn't contain any enrollment with
;;          with same class as of enroll
;; EXAMPLE:
;; (combine-if-class-not-present
;;                JOHN-PDP-ENROLL
;;                (cons KAT-NETWORKS-ENROLL empty))
;; = (cons JOHN-PDP-ENROLL (cons KAT-NETWORKS-ENROLL empty))
;; STRATEGY: Use ormap HOF on se 
(define (combine-if-class-not-present enroll se)
  (if (ormap
       ; Class -> Boolean
       ; True iff class is same as present in enroll
       (lambda (x) (same-class? (enrollment-class enroll)
                                (enrollment-class x)))    se)
      se
      (cons enroll se)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bad solution-3

;; enrollments-to-rosters-bad-3:
;;         SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: SetOfEnrollmentAssertion
;; RETURNS: SetOfClassRosterAssertion where only one roster is created
;;          while finding all it's students
;; EXAMPLE: (enrollments-to-rosters-bad-3 MIXED-ENROLLMENTS1)
;;          = (cons PDP-ROSTER-WITH-ALL empty)
;; STRATEGY: Combine simple functions
(define (enrollments-to-rosters-bad-3 se)
  (combine-duplicates (convert-all-to-rosters-bad-3 se)))

;; convert-all-to-rosters-bad-3 : SetOfEnrollmentAssertion -> ListOfRoster
;; GIVEN : SetOfEnrollmentAssertion
;; RETURNS: Creates list of roster for all enrollments who have same classname
;;          as of classname of first item is SetOfEnrollmentAssertion
;; STRATEGY: Combine simple functions
(define (convert-all-to-rosters-bad-3 se)
  (convert-all-to-roster-list
   (cons (first se)
         (get-all-enrollments-of-a-class
          (first se) (rest se)))))

;; get-all-enrollments-of-a-class:
;; Enrollment SetOfEnrollmentAssertion -> SetOfEnrollmentAssertion
;; GIVEN: enrollment and SetOfEnrollmentAssertion
;; RETURNS: SetOfEnrollmentAssertion having same class as present in enroll
;; STRATEGY: Use filter HOF on se
(define (get-all-enrollments-of-a-class enroll se)
  (filter
   ; Class -> Boolean
   ; True iff class is same as present in enroll
   (lambda (x) (same-class? (enrollment-class enroll)
                            (enrollment-class x))) se))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bad solution-4

;; enrollments-to-rosters-bad-4:
;;         SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
;; GIVEN: SetOfEnrollmentAssertion
;; RETURNS: SetOfClassRosterAssertion where only one roster is created
;;          while finding all it's students except one student
;; EXAMPPLE :
;; (enrollments-to-rosters-bad-4 MIXED-ENROLLMENTS1)
;; = (list (make-roster "PDP" (list "Amy" "Feng")))
;; STRATEGY: Combine simple functions
(define (enrollments-to-rosters-bad-4 se)
  (combine-duplicates (convert-all-to-rosters-bad-4 se)))

;; convert-all-to-rosters-bad-4 : SetOfEnrollmentAssertion -> ListOfRoster
;; GIVEN : SetOfEnrollmentAssertion
;; RETURNS: Creates list of a roster for all enrollments who have same classname
;;          as of classname of first item is SetOfEnrollmentAssertion
;;          excpet first enrollment
;; STRATEGY: Combine simple functions
(define (convert-all-to-rosters-bad-4 se)
  (convert-all-to-roster-list
   (get-all-enrollments-of-a-class
    (first se) (rest se))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TESTS:

(begin-for-test
  
  ;; Tests for examples written in definition of methods/functions
  
  (check-equal?
   (subset-of-second-set?
    MIXED-ROSTERS1 MIXED-ROSTERS1 compare-rosters?)
   #true)
  
  (check-equal?
   (has-only-one-copy?
    PDP-ROSTER-WITH-AMY MIXED-ROSTERS1 compare-rosters?)
   #false)
  
  (check-equal?
   (compare-rosters?
    PDP-ROSTER-WITH-AMY NETWORKS-ROSTER-WITH-AMY)
   #false)
  
  (check-equal?
   (copies-found
    PDP-ROSTER-WITH-AMY MIXED-ROSTERS1 compare-rosters?)
   0)
  
  (check-equal?
   (compare-rosters? PDP-ROSTER-WITH-AMY NETWORKS-ROSTER-WITH-AMY)
   #false)
  
  (check-equal?
   (combine-duplicates ROSTER-LIST1) MIXED-ROSTERS1)
  
  (check-equal? (compare-student? "Amy" "Amy") #true)
  
  (check-equal?
   (convert-list-to-set
    NETWORKS-ROSTER-WITH-KAT
    (cons NETWORKS-ROSTER-WITH-AMY empty))
   (cons NETWORKS-ROSTER-WITH-ALL empty))
  
  (check-equal?
   (update-matching-roster-in-set
    NETWORKS-ROSTER-WITH-KAT
    (cons NETWORKS-ROSTER-WITH-AMY empty))
   (cons NETWORKS-ROSTER-WITH-ALL empty))
  
  (check-equal?
   (is-matching-roster-present?
    NETWORKS-ROSTER-WITH-KAT
    (cons NETWORKS-ROSTER-WITH-AMY empty))
   #true)
  
  (check-equal?
   (find-and-update-matching-roster
    NETWORKS-ROSTER-WITH-KAT
    (cons NETWORKS-ROSTER-WITH-AMY empty))
   NETWORKS-ROSTER-WITH-ALL)
  
  (check-equal? (convert-all-to-roster-list MIXED-ENROLLMENTS1) ROSTER-LIST1)
  
  (check-equal?
   (enrollments-to-rosters-bad-1 MIXED-ENROLLMENTS1)
   (cons PDP-ROSTER-WITH-JOHN empty))
  
  (check-equal?
   (convert-all-to-rosters-bad-1 MIXED-ENROLLMENTS1)
   (cons PDP-ROSTER-WITH-JOHN empty))
  
  (check-equal?
   (enrollments-to-rosters-bad-2 MIXED-ENROLLMENTS1)
   (cons PDP-ROSTER-WITH-AMY (cons NETWORKS-ROSTER-WITH-AMY empty)))
  
  (check-equal?
   (convert-all-to-rosters-bad-2 MIXED-ENROLLMENTS1)
   (cons PDP-ROSTER-WITH-AMY (cons NETWORKS-ROSTER-WITH-AMY empty)))
  
  (check-equal?
   (combine-if-class-not-present
    JOHN-PDP-ENROLL
    (cons KAT-NETWORKS-ENROLL empty))
   (cons JOHN-PDP-ENROLL (cons KAT-NETWORKS-ENROLL empty)))
  
  (check-equal?
   (enrollments-to-rosters-bad-3 MIXED-ENROLLMENTS1)
   (cons PDP-ROSTER-WITH-ALL empty))
  
  (check-equal?
   (enrollments-to-rosters-bad-4 MIXED-ENROLLMENTS1)
   (list (make-roster "PDP" (list "Amy" "Feng"))))
  
  ;; Functionality Test
  
  (check-equal? (enrollments-to-rosters MIXED-ENROLLMENTS1) MIXED-ROSTERS1)
  (check-equal? (enrollments-to-rosters MIXED-ENROLLMENTS2) MIXED-ROSTERS2)
  (check-equal? (find-and-update-matching-roster
                 (make-roster "PDP" (list "Mark"))
                 empty)
                (make-roster "PDP" (list "Mark")))
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-1
                 MIXED-ENROLLMENTS1)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-2
                 MIXED-ENROLLMENTS1)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-3
                 MIXED-ENROLLMENTS1)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-4
                 MIXED-ENROLLMENTS1)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-1
                 MIXED-ENROLLMENTS2)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-2
                 MIXED-ENROLLMENTS2)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-3
                 MIXED-ENROLLMENTS2)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters-bad-4
                 MIXED-ENROLLMENTS2)   #false)
  (check-equal? (behavior-correct?
                 enrollments-to-rosters
                 MIXED-ENROLLMENTS1)  #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
