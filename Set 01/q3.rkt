;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname q3) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Goal: Calculate image area for a given image

(require rackunit)
(require "extras.rkt")
(check-location "01" "q3.rkt")
(require 2htdp/universe)
(require 2htdp/image)

;; DATA DEFINITIONS: none

;; image-area : image -> Real
;; GIVEN : an image
;; RETURNS: area of the image which is height * width
;; Example:
;; (image-area (empty-scene 100 20)) = 2000

;; STRATEGY : combine simpler function
(define (image-area image)
  (* (image-height image) (image-width image)))

;; TESTS
(begin-for-test
  (check-equal?
         (image-area (empty-scene 100 20))
         2000
         "(image-area (empty-scene 100 20)) should be 2000"))
