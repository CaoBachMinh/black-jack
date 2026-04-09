#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(define-struct card (r s))
(define CARD-WIDTH 30)
(define CARD-HEIGHT 50)

(define spades2-big (bitmap "assets/spades_2.png"))
(define spades2 (scale 0.5 spades2-big))

(define WIDTH 500)
(define HEIGHT 300)
(define CANVAS (empty-scene WIDTH HEIGHT))

(define (draw-card c)
  (place-image spades2
               100 200
               CANVAS))

(define (draw-table x)
  (place-image (rectangle 40 20 "outline" "blue")
               150 250
               CANVAS))

(big-bang 0
  (to-draw draw-card))