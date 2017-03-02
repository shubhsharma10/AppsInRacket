;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Program for Pretty Printer

(require rackunit)
(require "extras.rkt")
(check-location "07" "q1.rkt")

(provide
 program-to-strings
 make-def
 make-varexp
 make-appexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CONSTANTS

(define INDENTATION 4)

(define EMPTY-STRING "")

(define CLOSING-PARENTHESIS ")")

(define COMMA ",")

(define COMMA-CLOSING "),")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; OBSERVER TEMPLATES:

;; We group these because def-fn and exp-fn are mutually recursive.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define DEF1
  (make-def 'a-very-long-function-name
            (list 'x)
            (make-appexp 'f1 (list (make-varexp 'x)))))

(define DEF2
  (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
            (make-appexp 'f1 (list (make-varexp 'y)))))

(define DEF3
  (make-def 'f3 (list 'x 'z 't 'u)
            (make-appexp
             'f1
             (list (make-appexp
                    'f2 (list (make-varexp 'z)
                              (make-varexp 'y)))
                   (make-varexp 'z)
                   (make-appexp
                    'f1 (list (make-appexp
                               'f2 (list (make-varexp 'z)
                                         (make-varexp 'y)
                                         (make-varexp 'y)
                                         (make-varexp 'y)))
                              (make-varexp 'z)))))))
(define SAMPLE-LOE
  (list (make-appexp
         'f2 (list (make-varexp 'z)
                   (make-varexp 'y)
                   (make-varexp 'y)
                   (make-varexp 'y)))
        (make-varexp 'z)))

(define SAMPLE-PROGRAM
  (list DEF1 DEF2 DEF3))

(define SAMPLE-PROGRAM-1
  (list
   (make-def 'a-very-long-function-name
             empty
             (make-appexp 'a-very-long-function-name-1
                          (list (make-varexp 'x))))))

(define SAMPLE-PROGRAM-2
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1 (list (make-appexp
                         'f2 (list (make-varexp 'z)
                                   (make-varexp 'y)
                                   (make-varexp 'x)))
                        (make-varexp 'z)
                        (make-varexp 'x))))))

(define SAMPLE-PROGRAM-3
  (list
   (make-def 'a-very-long-function-name
             empty
             (make-appexp 'a-very-long-function-name-1
                          empty))))

(define SAMPLE-PROGRAM-4
  (list
   (make-def 'a-very-long-function-name
             empty
             (make-appexp 'a-very-long-function-name-1
                          (list (make-appexp 'f1 empty))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.
;; EXAMPLE:(program-to-strings SAMPLE-PROGRAM-1 20)=
;; (list "def a-very-long-function-name () :" "
;; a-very-long-function-name-1" "     (x)")
;; STRATEGY: Combine simple function
(define (program-to-strings pgm width)
  (lod-to-string pgm width))


;; lod-to-string : ListOfDefinition PosInt -> ListOfString 
;; GIVEN: list of definition and width for each line
;; RETURNS: List of string where each string is in a line as per formatting
;;          rules specified in problem
;; EXAMPLE:(lod-to-string SAMPLE-PROGRAM-1 20)=
;; (list "def a-very-long-function-name () :" "
;; a-very-long-function-name-1" "     (x)")
;; STRATEGY: Use foldr HOF on defs
(define (lod-to-string defs width)
  (foldr
   ;; ListOfDefinition ListOfString-> ListOfString
   ;; Retruns ListofString for the ListOfDefintion
   (lambda(definition accum)
     (append (def-to-string definition width) accum))
   empty
   defs))

;; def-to-string : Definition PosInt -> ListOfString    
;; GIVEN: function definition and width for each line
;; RETURNS: list of string for function definition where each line follows
;;          formatting rules specified in problem
;; EXAMPLE: (def-to-string DEF1 20)=
;; (list "def a-very-long-function-name (x) :" "    f1(x)")
;; STRATEGY: Combine simple functions
(define (def-to-string def width)
  (if (fn-def-exceeds-limit? def width)  
      (def-with-header-rule def width)
      (def-without-header-rule def width)))  

;; fn-def-exceeds-limit? : Definition PosInt -> Boolean    
;; GIVEN : Definition and width for each line
;; RETURNS: True iff header rule needs to be applied
;; STRATEGY: Combine simple function
;; EXAMPLE: (fn-def-exceeds-limit? DEF1 20)= #false
(define (fn-def-exceeds-limit? def width)
  (and (> (string-length (fn-header-to-string def)) width)
       (> (length (def-args def)) 1)))

;; def-with-header-rule : Definition PosInt -> ListOfString  
;; GIVEN: Definition and width for each line
;; RETURNS: Function defintion partitioned into three list
;;          for function name, function arguments and body.
;; STRATEGY: Combine simple functions and Use Definition template on def
;; EXAMPLE: (def-with-header-rule DEF1 20)=
;; (list "def a-very-long-function-name (x," "    f1(x)")
(define (def-with-header-rule def width)
  (append (list (string-append (convert-def-name-to-string def)
                               (symbol->string (first (def-args def))) COMMA))
          (create-arg-list (rest (def-args def))
                           (string-length (convert-def-name-to-string def)))
          (rem-coma-last-sol
           (get-exp-string (def-body def) width INDENTATION))))

;; def-without-header-rule : Definition PosInt -> ListOfString  
;; GIVEN: Definition and width for each line
;; RETURNS: Function defintion partitioned into two list
;;          for function name with arguments and body.
;; STRATEGY: Combine simple functions and Use Definition template on def
;; EXAMPLE: (def-without-header-rule DEF1 20)=
;; (list "def a-very-long-function-name (x) :" "    f1(x)")
(define (def-without-header-rule def width)
  (cons (fn-header-to-string def)
        (rem-coma-last-sol
         (get-exp-string (def-body def) width INDENTATION))))

;; create-arg-list : ListOfVariable PosInt -> ListOfString      
;; GIVEN: function arguments and indentation required for each argument
;; RETURNS: Function argument in different string with formatting
;;         as specified in problem set
;; STRATEGY: Use foldr HOF on args
;; EXAMPLE: (create-arg-list (list 'x) 10)=(list "          x) :")
(define (create-arg-list args indent)
  (foldr
   ;; ListOfArguments ListOfString-> ListOfString
   ;; Retruns ListofString for the ListOfArguments
   (lambda (arg accum) (args-combiner arg accum indent)) empty args))

;; args-combiner : Variable ListOfString PosInt -> ListOfString   
;; GIVEN: argument, list of string and indentation required for each argument
;; RETURNS: List of string for each argument with indentation, "," and ") :"
;;         as per formatting rules
;; WHERE: indent identifies number of whitespaces needed before string
;; STRATEGY: Combine simple functions
;; EXAMPLE: (args-combiner 'x empty 10)=(list "          x) :")
(define (args-combiner arg accum indent)
  (if(empty? accum)
     (list (append-whitespace (append-fn-end (symbol->string arg)) indent))
     (append (list (append-whitespace
                    (append-comma (symbol->string arg)) indent))
             accum)))

;; append-whitespace : String PosInt -> String               
;; GIVEN: string and number
;; RETURNS: appends whitespace indent times as prefix to string
;; EXAMPLE: (append-whitespace "ram" 4)
;;          = "    ram"
;; STRATEGY: combine simple functions
(define (append-whitespace string indent)
  (string-append (replicate indent " ") string))

;; append-comma : String -> String                    
;; GIVEN: a string
;; RETURNS: appends "," to string
;; STRATEGY: Combine simple function
;; EXAMPLE: (append-fn-end "f1(y")="f1(y) :"
(define (append-comma str)
  (string-append str COMMA))

;; append-fn-end : String -> String                
;; GIVEN: a string
;; RETURNS: appends ") :" to string
;; STRATEGY: Combine simple functions
;; EXAMPLE: (append-fn-end "f1(y)")="f1(y)) :"
(define (append-fn-end str)
  (string-append str ") :"))

;; fn-header-to-string : Definition -> String                   
;; GIVEN: function definition
;; RETURNS: function definition and arguments as a string
;; STRATEGY: Combine simple functions
;; EXAMPLE: (fn-header-to-string DEF1)= "def a-very-long-function-name (x) :"
(define (fn-header-to-string def)
  (string-append (convert-def-name-to-string def)
                 (convert-def-args-to-string (def-args def))
                 " :"))

;; loexp-fn : ListOfExpression PosInt PosInt -> String               
;; GIVEN: List of expression, allowed line width and indentation
;; RETURNS: all expression converted in a string with formatting rules
;;          specified in problem set
;; STRATEGY: Use foldr HOF on loexp
;; EXAMPLE: (loexp-fn SAMPLE-LOE 20 10)=
;; "f2(z, y, y, y), z"
(define (loexp-fn loexp width indent)
  (foldr
   ;; ListOfExpression ListOfString-> ListOfString
   ;; Returns ListofString for the ListOfExpression
   (lambda (exp accum) (exp-combiner exp accum width indent))
   EMPTY-STRING loexp))

;; exp-combiner : Expression String PosInt PosInt -> String            
;; GIVEN: expression, string, width and indentation for expression
;; RETURNS: String with all expressions
;; STRATEGY: Combine simple functions
;; WHERE: accum is sublist of a larger list list0
;; HALTING-MEASURE: length of accum
;; EXAMPLE: (exp-combiner (make-appexp 'f1 (list (make-varexp 'y))) "" 20 10)=
;; "f1(y)"
(define (exp-combiner exp accum width intend)
  (if(string=? accum EMPTY-STRING)
     (string-append (exp-to-single-string-fn exp width intend) accum)
     (string-append (exp-to-single-string-fn exp width intend) ", " accum)))

;; exp-fn : Expression PosInt PosInt -> String                            
;; GIVEN: an expression, allowed length in a line and indentation for
;;        expression
;; RETURNS: expression with nested expression converted in
;;          a single string as per formatting rules
;; STRATEGY: Use Expression template on exp
;; HALTING-MEASURE: length of appexp arguments
;; EXAMPLE: (exp-to-single-string-fn (make-appexp 'f1 (list (make-varexp 'y)))
;; 20 10)="f1(y)"
(define (exp-to-single-string-fn exp width indent)
  (cond
    [(varexp? exp) (symbol->string (varexp-name exp))]
    [(appexp? exp) (string-append
                    (get-exp-arg-name (appexp-fn exp))
                    (loexp-fn (appexp-args exp) width indent)
                    CLOSING-PARENTHESIS)]))

;; loexp-body-fn: ListOfExpression PosInt PosInt Boolean -> ListOfString    
;; GIVEN: ListOfExpression,width, indentation count and is third case
;; RETURNS: ListOfString after converting each expression as per formatting
;;          rules
;; STRATEGY: Call a more general function
;; EXAMPLE:(loexp-body-fn SAMPLE-LOE 20 10 #f)=
;; (list "          f2(z," "             y," "
;; y," "             y)," "          z),")
(define (loexp-body-fn loe width indent is-case3?)
  (loexp-exp-body-fn loe width indent is-case3? (length loe)))

;; loexp-exp-body-fn: ListOfExpression PosInt PosInt Boolean PosInt         
;;                    -> ListOfString
;; GIVEN: ListOfExpression,width, indentation count and is third case and n
;; WHERE: n is length of sublist of a larger list
;; RETURNS: ListOfString after converting each expression as per formatting
;;          rules
;; STRATEGY: Using cases on n and whether loe is empty
;; HALTING-MEASURE: n or length of loe
;; EXAMPLE:(loexp-exp-body-fn: SAMPLE-LOE 20 10 #f 2)=
;; (list "          f2(z," "             y," "
;;  y," "             y)," "          z),")
(define (loexp-exp-body-fn loe width indent is-case3? n)
  (cond
    [(empty? loe) empty]
    [(= n 1)
     (append
      (add-bracket-last-string (exp-body-fn (first loe) width indent is-case3?))
      (loexp-exp-body-fn (rest loe) width indent #f (- n 1)))]
    [else
     (append (exp-body-fn (first loe) width indent is-case3?)
             (loexp-exp-body-fn (rest loe) width indent #f (- n 1)))]))

;; add-bracket-last-string ListOfString -> ListOfString                    
;; GIVEN: List of string generated from expression
;; RETURNS: List of string with last string's last char if "," replaced by "),"
;; EXAMPLE: (add-bracket-last-string
;;                (list "         (f1(y),"))
;;         =  (list "         (f1(y)),")
;; STRATEGY: Use foldr HOD on los
(define (add-bracket-last-string los)
  (foldr
   ;; String ListOfString -> ListOfString
   ;; Returns a list of string with last string with the "),"
   (lambda (str lst) (char-add-combiner str lst)) empty los))

;; char-add-combiner : String ListOfString -> ListOfString               
;; GIVEN: String and a list of string
;; RETURNS: List of string with last string's last char if "," replaced by "),"
;; STATEGY: Use cases and combine simple functions
;; WHERE: lst is sublist of a larger list list0
;; EXAMPLE: (char-add-combiner "         (f1(y)," empty)=
;; (list "         (f1(y)),")
(define (char-add-combiner str lst)
  (if (empty? lst)
      (cons (ad-close-brack str) lst)
      (append (list str) lst)))

;; ad-close-brack : String -> String                                    
;; GIVEN : String
;; RETURNS: String's last char if "," replaced by "),"
;; STRATEGY: Use cases and combine simple functions
;; EXAMPLE: (ad-close-brack "         (f1(y),")=
;; "         (f1(y)),"
(define (ad-close-brack str)
  (let ([str-len (string-length str)])
    (if (equal? (substring str (- str-len 1) str-len)
                COMMA)
        (string-append
         (substring str 0 (- str-len 1)) COMMA-CLOSING)
        str)))

;; rem-coma-last-sol ListOfString -> ListOfString                    
;; GIVEN: List of string generated from expression
;; RETURNS: List of string with last string's last char if ")," replaced by ")"
;; STRATEGY: Use foldr HOF on los
;; EXAMPLE(rem-coma-last-sol (list "         (f1(y),"))
;; (list "         (f1(y)")
(define (rem-coma-last-sol los)
  (foldr
   ;; String ListOfString -> ListOfString
   ;; Returns a list of string with last string with the ")"
   (lambda (str lst) (char-removal-combiner str lst)) empty los))

;; char-removal-combiner : String ListOfString -> ListOfString            
;; GIVEN: String and a list of string
;; RETURNS: List of string with last string's last char if ")," replaced by ")"
;; WHERE: lst is sublist of a larger list list0
;; STATEGY: Use cases and combine simple functions
;; (char-removal-combiner str lst)
;; EXAMPLE: (char-removal-combiner "         (f1(y)," empty)
;;          = (list "         (f1(y)")
(define (char-removal-combiner str lst)
  (if(empty? lst)
     (cons (remove-comma-from-end str) lst)
     (append (list str) lst)))

;; remove-comma-from-end : String -> String                           
;; GIVEN : a string
;; RETURNS: string's last char if ")," replaced by ")"
;; STRATEGY: Use cases and combine simple functions
;; EXAMPLE:(remove-comma-from-end "         (f1(y),")="         (f1(y)"
(define (remove-comma-from-end str)
  (let([str-len (string-length str)])
    (if (equal? (substring str (- str-len 2) str-len)
                COMMA-CLOSING)
        (string-append (substring str 0 (- str-len 2)) CLOSING-PARENTHESIS)
        str)))

;; exp-body-fn : Expression PosInt PosInt Boolean -> ListOfString            
;; GIVEN: Expression,allowed line width, indentation and is case 3
;; RETURNS: Expression returned as list of string with formatting rule applied
;;          as per problem
;; WHERE: is-case3? tell whether current expression falls under case Rule 3.3
;;       of problem set or not
;; STRATEGY: Using cases and Template of Expression on exp and is case3?
;; HALTING-MEASURE: length of exp arguments if exp is appexp
;; EXAMPLE:(exp-body-fn (make-appexp 'f1 (list (make-varexp 'y))) 20 10 #t)=
;; (list "         (f1(y),")
(define (exp-body-fn exp width indent is-case3?)
  (cond
    [(and (varexp? exp) (not is-case3?))
     (append-var-body exp width indent "" 0)]
    [(and (appexp? exp) (not is-case3?))
     (append-app-body exp width indent "" 0)]
    [(and (varexp? exp) is-case3?)
     (append-var-body exp width indent "(" 1)]
    [(and (appexp? exp) is-case3?)
     (append-app-body exp width indent "(" 1)]))

;; exp-in-a-string? : Expression PosInt PosInt -> Boolean                 
;; GIVEN: Expression, allowed width and indentation
;; RETURNS: True iff expression as a string can fit in a single line
;; STRATEGY: Combine simple functions
;; EXAMPLE: (exp-in-a-string? (make-appexp 'f1 (list (make-varexp 'y))) 20 10)=
;; #true
(define (exp-in-a-string? exp width indent)
  (<= (+ indent (string-length (exp-to-single-string-fn exp width indent)))
      width))

;; get-exp-string : Expression PosInt PosInt -> ListOfString               
;; GIVEN: Expression, allowed line width and Indentation
;; RETURNS: List of string for expression which may have nested expression
;; STRATEGY: Combine simple functions
;; WHERE: indent identifies number of whitespaces needed before string
;; EXAMPLE: (get-exp-string (make-appexp 'f1 (list (make-varexp 'y))) 20 10)=
;; (list "          f1(y)")
(define (get-exp-string exp width indent)
  (if(exp-in-a-string? exp width indent)
     (list (append-whitespace (exp-to-single-string-fn exp width indent)
                              indent))
     (exp-body-fn exp width indent #f)))

;; convert-def-name-to-string: Definition -> String                     
;; GIVEN: Definition
;; RETURNS: function name as string with "def " prefix
;; STRATEGY: Using template of Definition on def
;; EXAMPLE: (convert-def-name-to-string DEF1)=
;; "def a-very-long-function-name ("
(define (convert-def-name-to-string def)
  (string-append "def " (get-fn-arg-name (def-name def))))

;; get-exp-arg-name : Variable -> String                       
;; GIVEN : variable in appexp function argument
;; RETURNS: argument converted to string as per formatting rules
;; STRATEGY: Combine simple functions
;; EXAMPLE: (get-exp-arg-name 'y)="y("
(define (get-exp-arg-name name)
  (string-append (symbol->string name) "("))

;; get-fn-arg-name : Variable -> String                     
;; GIVEN : variable in function definition argument
;; RETURNS: argument converted in string as per formatting rules
;;          specified in problem set
;; STRATEGY: Combine simple function
;; EXAMPLE:(get-fn-arg-name'y)="y ("
(define (get-fn-arg-name name)
  (string-append (symbol->string name) " ("))

;; convert-def-args-to-string : ListOfVariable -> String               
;; GIVEN: List of variable
;; RETURNS: String consisting of each variable converted in string
;; STRATEGY: Use foldr HOF on args
;; EXAMPLE:(convert-def-args-to-string (list 'y))= "y)"
(define (convert-def-args-to-string args)
  (foldr
   ;; Variable String -> String
   ;; RETURNS: variabled converted in string and appended to accum
   convert-def-arg-to-string CLOSING-PARENTHESIS args))

;; convert-def-arg-to-string : Variable String -> String             
;; GIVEN: variable and string
;; RETURNS: variabled converted in string and appended to accum
;; WHERE: accum is the sublist of larger list list1
;; STRATEGY: Combine simple functions
;; EXAMPLE: (convert-def-arg-to-string 'y ")")= "y)"
(define (convert-def-arg-to-string arg accum)
  (if (string=? accum CLOSING-PARENTHESIS)
      (string-append (symbol->string arg) accum)
      (string-append (symbol->string arg) COMMA accum)))


;; append-app-body : Expression PosInt PosInt String PosInt -> ListOfString  
;; GIVEN: Expression, allowed width in a line, indentation for space, prefix
;;        for expression function name and offset by which whitespaces has to
;;        be reduced in indentation
;; RETURNS: List of string for expression as per formatting rules
;; STRATEGY: Combine simple expression
;; EXAMPLE: (append-app-body (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)=(list "          (f1(y),")
(define (append-app-body exp width indent prefix offset)
  (if (empty? (appexp-args exp))
      (appexp-with-empty-args exp width indent prefix offset)
      (appexp-nonempty-args exp width indent prefix offset)))

;; append-var-body: Expression PosInt PosInt String PosInt -> ListOfString    
;; GIVEN: Expression, allowed line width, indentaiton for whitespace, prefix
;;        for variable name, offset to reduce number of whitespaces as
;;        part of indentation
;; RETURNS: List of one string from variable expression as per rules
;;         stated above
;; WHERE: subtraction of indent and offset is number of whitespaces needed
;;        string
;; STRATEGY: Using Template of variable on exp and combining simple function
;; EXAMPLE: (append-var-body (make-varexp 'y) 20 10 "(" 0)=
;; (list "          (y,")
(define (append-var-body exp width indent prefix offset)
  (list (append-whitespace
         (string-append prefix (symbol->string (varexp-name exp)) COMMA)
         (- indent offset))))

;; appexp-with-empty-args: Expression PosInt PosInt String PosInt            
;;                       -> ListOfString
;; GIVEN: Expression with empty args,allowed width in a line, Indentation
;;        for whitespace, string as a prefix to appexp-fn name and offset by
;;        which to reduce indentation
;; RETURNS: List of strings for expression as per formatting rule
;; EXAMPLE: (appexp-with-empty-args (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)=(list "          (f1()")
(define (appexp-with-empty-args exp width indent prefix offset)
  (if(<= (string-length (appexp-in-a-string exp width indent prefix offset))
         width)
     (cons (appexp-in-a-string exp width indent prefix offset) empty)
     (appexp-in-two-lines exp width indent prefix offset)))

;; appexp-nonempty-args Expression PosInt PosInt String PosInt                
;;                       -> ListOfString
;; GIVEN: Expression with non empty args,allowed width in a line, Indentation
;;        for whitespace, string as a prefix to appexp-fn name and offset by
;;        which to reduce indentation
;; RETURNS: List of strings for expression as per formatting rule
;; EXAMPLE: (appexp-nonempty-args (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)=(list "          (f1(y),")
(define (appexp-nonempty-args exp width indent prefix offset)
  (if (<= (string-length
           (appexp-one-arg-string exp width indent prefix offset))
          width)
      (appexp-same-line exp width indent prefix offset)
      (appexp-args-next-line exp width indent prefix offset)))

;; appexp-in-a-two-lines : Expression PosInt PosInt String PosInt            
;;                       -> ListOfString
;; GIVEN: Expression which has appexp and has no arguments, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: Expression which is a appexp with no arguments in a list of two
;;         string as per formatting rules
;; WHERE: subtraction of indent+1 and offset identifies number of whitespaces
;;        needed before string
;; STRATEGY: Using Template of expression on exp
;; EXAMPLE: (appexp-in-two-lines (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)=(list "          (f1" "           ()")
(define (appexp-in-two-lines exp width indent prefix offset)
  (list (appfn-name-in-a-string exp width indent prefix offset)
        (append-whitespace "()" (- (+ indent 1) offset))))

;; appexp-in-a-string : Expression PosInt PosInt String PosInt -> String     
;; GIVEN: Expression which has appexp and has no arguments, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: Expression which is a appexp with no arguments in a string as per
;;          formatting rules
;; WHERE: subtraction of indent and offset identifies number of whitespaces
;;        needed before string
;; STRATEGY: Using Template of expression on exp
;; EXAMPLE: (appexp-in-a-string (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)= "          (f1()"
(define (appexp-in-a-string exp width indent prefix offset)
  (append-whitespace (string-append
                      prefix
                      (symbol->string (appexp-fn exp))
                      "()")
                     (- indent offset)))

;; appexp-one-arg-string : Expression PosInt PosInt String PosInt             
;;                                -> String
;; GIVEN: Expression, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: Expression which is a appexp with first argument in a string as
;;         per formatting rules
;; WHERE: subtraction of indent and offset identifies number of whitespaces
;;        needed before string
;; STRATEGY: Using Template of expression on exp
;; EXAMPLE: (appexp-one-arg-string (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)= "          (f1(y),"
(define (appexp-one-arg-string exp width indent prefix offset)
  (append-whitespace (string-append
                      prefix
                      (get-exp-arg-name (appexp-fn exp))
                      (exp-to-single-string-fn
                       (first (appexp-args exp)) width indent)
                      (if (empty? (rest (appexp-args exp)))
                          COMMA-CLOSING
                          COMMA))
                     (- indent offset)))

;; appexp-same-line : Expression PosInt PosInt String PosInt                     
;;                                -> ListOfString
;; GIVEN: Expression, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: List of string in which first string is appexp-fn name with first
;;        argument as per formatting rules along with rest of arguments in
;;        next single/multiple line(s)
;; STRATEGY: Using Template of Expression on exp
;; EXAMPLE: (appexp-same-line (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)= (list "          (f1(y),")
(define (appexp-same-line exp width indent prefix offset)
  (cons (appexp-one-arg-string exp width indent prefix offset) 
        (loexp-body-fn (rest (appexp-args exp)) width
                       (+ indent (string-length
                                  (get-exp-arg-name (appexp-fn exp)))) #f)))

;; appfn-name-in-a-string : Expression PosInt PosInt String PosInt           
;;                                -> String
;; GIVEN: Expression, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: Expression which is a appexp in a string as
;;         per formatting rules, arguments will be handled seperatly 
;; STRATEGY: Using Template of Expression on exp
;; WHERE: subtraction of indent and offset identifies number of whitespaces
;;        needed before string
;; EXAMPLE: (appfn-name-in-a-string (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)= "          (f1"
(define (appfn-name-in-a-string exp width indent prefix offset)
  (append-whitespace
   (string-append prefix (symbol->string (appexp-fn exp)))
   (- indent offset)))

;; appexp-args-next-line : Expression PosInt PosInt String PosInt
;;                                -> ListOfString                           
;; GIVEN: Expression, allowed line width
;;        Indentation, prefix for appexp-fn name, offset by which indentation
;;        has to be reduced
;; RETURNS: List of string in which first string is appexp-name per formatting
;;         rules along with arguments in next single/multiple line(s)
;; STRATEGY: Using Template of Expression on exp
;; EXAMPLE:(appexp-args-next-line (make-appexp 'f1
;; (list (make-varexp 'y))) 20 10 "(" 0)=
;;(list "          (f1" "           (y),") 
(define (appexp-args-next-line exp width indent prefix offset)
  (cons   (appfn-name-in-a-string exp width indent prefix offset)
          (loexp-body-fn (appexp-args exp) width (+ indent 2) #t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS

(begin-for-test
  (check-equal? (program-to-strings SAMPLE-PROGRAM 20)
                (list "def a-very-long-function-name (x) :"
                      "    f1(x)"
                      "def f2 (x,"
                      "        a-very-long-variable-name,"
                      "        y) :"
                      "    f1(y)"
                      "def f3 (x,z,t,u) :"
                      "    f1(f2(z, y),"
                      "       z,"
                      "       f1"
                      "        (f2(z,"
                      "            y,"
                      "            y,"
                      "            y),"
                      "         z))")
                "Result should be as expected")
  
  (check-equal? (program-to-strings SAMPLE-PROGRAM-1 20)
                (list "def a-very-long-function-name () :"
                      "    a-very-long-function-name-1"
                      "     (x)")
                "Result should be as expected")
  
  (check-equal? (program-to-strings SAMPLE-PROGRAM-2 20)
                (list  "def a-very-long-function-name (x) :"
                       "    f1(x)"
                       "def f2 (x,"
                       "        a-very-long-variable-name,"
                       "        y) :"
                       "    f1(y)"
                       "def f3 (x,z,t,u) :"
                       "    f1(f2(z, y, x),"
                       "       z,"
                       "       x)")
                "Result should be as expected")
  
  (check-equal? (program-to-strings SAMPLE-PROGRAM-4 20)
                (list "def a-very-long-function-name () :"
                      "    a-very-long-function-name-1"
                      "     (f1()")
                "Result should be as expected")
  
  (check-equal? (program-to-strings SAMPLE-PROGRAM-3 20)
                (list "def a-very-long-function-name () :"
                      "    a-very-long-function-name-1"
                      "     ()")
                "Result should be as expected"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

