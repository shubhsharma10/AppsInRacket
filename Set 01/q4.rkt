;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q4) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Insert "_" in a string at ith position 

(require rackunit)
(require "extras.rkt")
(check-location "01" "q4.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; DATA DEFINITIONS: none

;; string-insert : string PosInt -> string
;; GIVEN : a string
;; RETURNS: a string with "_" inserted at ith position
;; Example:
;; (string-insert helloworld 5) => hello_world

;; STRATEGY : combine simpler function
(define (string-insert string i)
  (string-append (substring string 0 5) "_" (substring string 5)))

;; TESTS
(begin-for-test
  (check-equal?
         (string-insert "helloworld" 5)
         "hello_world"
         "(string-insert helloworld 5) should be hello_world"))
