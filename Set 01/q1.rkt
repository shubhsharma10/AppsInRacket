;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Calculate distance of a point from origin

(require rackunit)
(require "extras.rkt")
(check-location "01" "q1.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; DATA DEFINITIONS: none

;; distance-to-origin : Real, Real -> Real
;; GIVEN : x coordinate and y coordinate of a point in 2D plane
;; RETURNS: Distance of said point from origin i.e. (0,0)
;; Example:
;; (distance-to-origin 3 4) = sqrt((3*3) + (4*4)) = 5

;; STRATEGY : combine simpler function
(define (distance-to-origin x y)
  (sqrt (+ (* x x) (* y y))))

;; TESTS
(begin-for-test
  (check-equal?
         (distance-to-origin 3 4)
         5
         "(distance-to origin 3 4) should be 5"))
