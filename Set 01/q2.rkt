;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Extract last char from a non-empty string

(require rackunit)
(require "extras.rkt")
(check-location "01" "q2.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; DATA DEFINITIONS: none

;; string-last : string -> 1String
;; GIVEN : a non empty string
;; RETURNS: last character from the string
;; Example:
;; (string-last "shubham")) = "m"

;; STRATEGY : combine simpler function
(define (string-last s)
  (string-ith s (- (string-length s) 1)))

;; TESTS
(begin-for-test
  (check-equal?
         (string-last "shubham")
         "m"
         "(string-last shubham) should be m"))
