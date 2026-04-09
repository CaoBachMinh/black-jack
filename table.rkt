#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

(define card-images
  (hash'cA (bitmap "assets/clubs_A.png")
       'c2 (bitmap "assets/clubs_2.png")
       'c3 (bitmap "assets/clubs_3.png")
       'c4 (bitmap "assets/clubs_4.png")
       'c5 (bitmap "assets/clubs_5.png")
       'c6 (bitmap "assets/clubs_6.png")
       'c7 (bitmap "assets/clubs_7.png")
       'c8 (bitmap "assets/clubs_8.png")
       'c9 (bitmap "assets/clubs_9.png")
       'c10 (bitmap "assets/clubs_10.png")
       'cJ (bitmap "assets/clubs_J.png")
       'cQ (bitmap "assets/clubs_Q.png")
       'cK (bitmap "assets/clubs_K.png")

       'dA (bitmap "assets/diamonds_A.png")
       'd2 (bitmap "assets/diamonds_2.png")
       'd3 (bitmap "assets/diamonds_3.png")
       'd4 (bitmap "assets/diamonds_4.png")
       'd5 (bitmap "assets/diamonds_5.png")
       'd6 (bitmap "assets/diamonds_6.png")
       'd7 (bitmap "assets/diamonds_7.png")
       'd8 (bitmap "assets/diamonds_8.png")
       'd9 (bitmap "assets/diamonds_9.png")
       'd10 (bitmap "assets/diamonds_10.png")
       'dJ (bitmap "assets/diamonds_J.png")
       'dQ (bitmap "assets/diamonds_Q.png")
       'dK (bitmap "assets/diamonds_K.png")

       'hA (bitmap "assets/hearts_A.png")
       'h2 (bitmap "assets/hearts_2.png")
       'h3 (bitmap "assets/hearts_3.png")
       'h4 (bitmap "assets/hearts_4.png")
       'h5 (bitmap "assets/hearts_5.png")
       'h6 (bitmap "assets/hearts_6.png")
       'h7 (bitmap "assets/hearts_7.png")
       'h8 (bitmap "assets/hearts_8.png")
       'h9 (bitmap "assets/hearts_9.png")
       'h10 (bitmap "assets/hearts_10.png")
       'hJ (bitmap "assets/hearts_J.png")
       'hQ (bitmap "assets/hearts_Q.png")
       'hK (bitmap "assets/hearts_K.png")

       'sA (bitmap "assets/spades_A.png")
       's2 (bitmap "assets/spades_2.png")
       's3 (bitmap "assets/spades_3.png")
       's4 (bitmap "assets/spades_4.png")
       's5 (bitmap "assets/spades_5.png")
       's6 (bitmap "assets/spades_6.png")
       's7 (bitmap "assets/spades_7.png")
       's8 (bitmap "assets/spades_8.png")
       's9 (bitmap "assets/spades_9.png")
       's10 (bitmap "assets/spades_10.png")
       'sJ (bitmap "assets/spades_J.png")
       'sQ (bitmap "assets/spades_Q.png")
       'sK (bitmap "assets/spades_K.png")))

(define card-values
  (hash 'cA 1  'c2 2  'c3 3  'c4 4  'c5 5  'c6 6  'c7 7  'c8 8  'c9 9  'c10 10 'cJ 10 'cQ 10 'cK 10
        'dA 1  'd2 2  'd3 3  'd4 4  'd5 5  'd6 6  'd7 7  'd8 8  'd9 9  'd10 10 'dJ 10 'dQ 10 'dK 10
        'hA 1  'h2 2  'h3 3  'h4 4  'h5 5  'h6 6  'h7 7  'h8 8  'h9 9  'h10 10 'hJ 10 'hQ 10 'hK 10
        'sA 1  's2 2  's3 3  's4 4  's5 5  's6 6  's7 7  's8 8  's9 9  's10 10 'sJ 10 'sQ 10 'sK 10))

(define back-card (bitmap "assets/back.png"))

(define WIDTH 1200)
(define HEIGHT 800)
(define CANVAS (empty-scene WIDTH HEIGHT))

(define MASTER-DECK (hash-keys card-images))

(define (get-random-card deck-list)
  (if (empty? MASTER-DECK)  
      (error "The Deck is empty")
  (let ([random-card (list-ref MASTER-DECK (random (length MASTER-DECK)))])
          (set! MASTER-DECK (remove random-card MASTER-DECK))
           random-card)))

(define (draw-back-card n scene)
  (place-image back-card 500 200 scene))

(define (draw-player-card n scene)
  (define current-card (hash-ref card-images (get-random-card card-images)))
  (place-image (frame current-card) 500 600 scene))

(define (draw-dealer-card n scene)
  (define current-card (hash-ref card-images (get-random-card card-images)))
  (place-image (frame current-card) 500 200 scene))

(define (draw-game n)
  (draw-dealer-card n (draw-player-card n CANVAS)))

(define player-hand empty)
(define dealer-hand empty)

(define (add-card card deck)
  (cons card deck))
(check-equal? (add-card 'cA player-hand) '(cA))

(big-bang 0
  (to-draw draw-game))