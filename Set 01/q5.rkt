;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Delete character from a string at ith position 

(require rackunit)
(require "extras.rkt")
(check-location "01" "q5.rkt")

;; DATA DEFINITIONS: none

;; string-insert : string PosInt -> string
;; GIVEN : a string
;; RETURNS: a string with character removed at ith position from original string
;; Example:
;; (string-delete helloworld 5) => helloorld

;; STRATEGY : combine simpler function
(define (string-delete string i)
  (string-append (substring string 0 5) (substring string 6)))

;; TESTS
(begin-for-test
  (check-equal?
         (string-delete "helloworld" 5)
         "helloorld"
         "(string-delete helloworld 5) should be helloorld"))
