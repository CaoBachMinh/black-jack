#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

;; --- Canvas Constants ---
(define WIDTH 1300)
(define HEIGHT 800)
(define CANVAS (empty-scene WIDTH HEIGHT))

;; --- Button Constants ---
(define BUTTON-X 1100)
(define BUTTON-Y 600)
(define BUTTON-WIDTH 120)
(define BUTTON-HEIGHT 50)
(define HIT-BUTTON-IMG 
  (overlay (text "DRAW" 24 "white")
           (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "blue")))
(define turn-ended? #f)

;; --- End Button Constants ---
(define END-BUTTON-X 1100)
(define END-BUTTON-Y 650)
(define END-BUTTON-IMG 
  (overlay (text "END TURN" 20 "white")
           (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "red")))

;; A CardSymbol is a Symbol representing a playing card.
;; First letter is suit (c=clubs, d=diamonds, h=hearts, s=spades).
;; Remaining characters are the rank (A, 2-10, J, Q, K).
;; Examples: 'cA, 'h10, 'sQ

;; card-images : Hash
;; Purpose: An immutable hash table mapping CardSymbols to their loaded bitmap images.
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

;; card-values : Hash
;; Purpose: An immutable hash table mapping CardSymbols to their integer point values.
;; Note: Face cards (J, Q, K) are valued at 10. Aces are valued at 1.
(define card-values
  (hash 'cA 1  'c2 2  'c3 3  'c4 4  'c5 5  'c6 6  'c7 7  'c8 8  'c9 9  'c10 10 'cJ 10 'cQ 10 'cK 10
        'dA 1  'd2 2  'd3 3  'd4 4  'd5 5  'd6 6  'd7 7  'd8 8  'd9 9  'd10 10 'dJ 10 'dQ 10 'dK 10
        'hA 1  'h2 2  'h3 3  'h4 4  'h5 5  'h6 6  'h7 7  'h8 8  'h9 9  'h10 10 'hJ 10 'hQ 10 'hK 10
        'sA 1  's2 2  's3 3  's4 4  's5 5  's6 6  's7 7  's8 8  's9 9  's10 10 'sJ 10 'sQ 10 'sK 10))

;; get-card-value : CardSymbol -> Card's Score
;; Retrieves the point value for a given card symbol.
(define (get-card-value card-sym)
  (hash-ref card-values card-sym))

;; calculate-score : (listof CardSymbol) -> Hand's Score
;; Takes a list of cards and returns their total point value.
(define (calculate-score hand)
  (apply + (map get-card-value hand)))

(define back-card (bitmap "assets/back.png"))
(define (draw-back-card n scene)
  (place-image back-card 500 200 scene))

;; MASTER-DECK : (listof CardSymbol)
(define MASTER-DECK (hash-keys card-images))

;; get-random-card : (Master-Deck) -> Card
;; Selects a random element from MASTER-DECK, removes it, and returns it.
(define (get-random-card deck-list)
  (if (empty? MASTER-DECK)  
      (error "The Deck is empty")
  (let ([random-card (list-ref MASTER-DECK (random (length MASTER-DECK)))])
          (set! MASTER-DECK (remove random-card MASTER-DECK))
           random-card)))

(define game-message "")

(define player-hand empty)
(define dealer-hand empty)
(define (add-card card hand)
  (cons card hand))

;; draw-hand : (listof CardSymbol) x-position y-position canvas -> Card's Image
;; Draws a hand of cards, shifting each subsequent card to the right.
(define (draw-hand hand x y hide? scene)
  (cond
    [(empty? hand) scene]
    [else
     (let* ([first-card-symbol (first hand)]
            [card-img (if hide? back-card (hash-ref card-images first-card-symbol))])
       ;; Draw the first card, then recursively draw the rest shifted by 40 pixels
       (place-image (frame card-img) x y 
                    (draw-hand (rest hand) (- x 100) y hide? scene)))]))

;; handle-mouse : Any mouse-x-position mouse-y-position MouseEvent -> Any
(define (handle-mouse state mouse-x mouse-y event)
  (cond
    [(string=? event "button-down")
     
     ;; Calculate boundaries for BOTH buttons
     (let ([hit-L (- BUTTON-X (/ BUTTON-WIDTH 2))]
           [hit-R (+ BUTTON-X (/ BUTTON-WIDTH 2))]
           [hit-T (- BUTTON-Y (/ BUTTON-HEIGHT 2))]
           [hit-B (+ BUTTON-Y (/ BUTTON-HEIGHT 2))]
           
           [end-L (- END-BUTTON-X (/ BUTTON-WIDTH 2))]
           [end-R (+ END-BUTTON-X (/ BUTTON-WIDTH 2))]
           [end-T (- END-BUTTON-Y (/ BUTTON-HEIGHT 2))]
           [end-B (+ END-BUTTON-Y (/ BUTTON-HEIGHT 2))])
       
       (cond
         ;; --- CLICKED HIT BUTTON ---
         [(and (>= mouse-x hit-L) (<= mouse-x hit-R)
               (>= mouse-y hit-T) (<= mouse-y hit-B))

          ;; Only allow draw card if in turn
          (if (not turn-ended?)
              (if (< (calculate-score player-hand) 21)
                  (begin
                    (set! player-hand (add-card (get-random-card MASTER-DECK) player-hand))
                    (set! game-message "") 
                    state)
                  (begin
                    (if (>= (calculate-score player-hand) 21)
                        (set! game-message "Bust! You cannot draw over 21.")
                    state)))
              
              ;; If turn-ended? is true, do nothing when hit is clicked
              state)]
         
         ;; --- CLICKED END TURN BUTTON ---
         [(and (>= mouse-x end-L) (<= mouse-x end-R)
               (>= mouse-y end-T) (<= mouse-y end-B))
          (begin
            (set! turn-ended? #t)
            ;; Run the dealer loop
            (dealer-play!)
            
            ;; Figure out who won to display a message!
            (define p-final (calculate-score player-hand))
            (define d-final (calculate-score dealer-hand))
            
            (cond
              ;; RULE 1: Both player and dealer are over 21
              [(and (> p-final 21) (> d-final 21))
               (set! game-message "Turn Ended! DRAW! 😑")]
  
              ;; RULE 2: Player didn't bust, and they either beat the dealer OR the dealer busted
              [(and (<= p-final 21) (or (> d-final 21) (> p-final d-final)))
               (set! game-message "Turn Ended! YOU WIN! 🎉")]
  
              ;; RULE 3: Exact same score (A standard card game tie/push)
              [(= p-final d-final)
               (set! game-message "Turn Ended! DRAW! (Equal Scores) 😑")]
  
              ;; RULE 4: If none of the above are true, the dealer wins
              [else
               (set! game-message "Turn Ended! DEALER WINS! 💀")])
            state)]
         
         ;; --- CLICKED ANYWHERE ELSE ---
         [else state]))]
    
    [else state]))

;; dealer-play! : -> Void
;; Automates the dealer's turn using a recursive loop.
(define (dealer-play!)
  (define target-score (+ 16 (random 6)))
  
  ;; Define a local recursive function that acts as our "while" loop
  (define (dealer-loop)
    (define d-score (calculate-score dealer-hand))
    
    ;; RULE: If dealer score is less than target score
    (if (< d-score target-score)
        
        ;; TRUE: Draw a card, then run this loop again!
        (begin
          (set! dealer-hand (add-card (get-random-card MASTER-DECK) dealer-hand))
          (dealer-loop)) 
        
        ;; FALSE: Condition met (or busted). Stop looping.
        (void))) 
  
  ;; Start the loop
  (dealer-loop))

;; setup-game : -> GameState
;; Deals the initial cards to start the game
(define (setup-game!)
  (set! player-hand (list (get-random-card MASTER-DECK) 
                          (get-random-card MASTER-DECK)))
  (set! dealer-hand (list (get-random-card MASTER-DECK) 
                          (get-random-card MASTER-DECK)))
  (set! turn-ended? #f))

;; draw-game : GameState -> Image
;; Renders the board and the UI buttons
(define (draw-game state)
  (place-image (text game-message 20 "red") 1100 550 ; Draw error message at top
               ;; Draw the button, then the dealer's hand, then the player's hand
               (place-image HIT-BUTTON-IMG BUTTON-X BUTTON-Y
                            (place-image END-BUTTON-IMG END-BUTTON-X END-BUTTON-Y
                                         (draw-hand dealer-hand 800 200 (not turn-ended?) 
                                                    (draw-hand player-hand 800 600 #f CANVAS))))))

(setup-game!)
(big-bang 0
  (to-draw draw-game)
  (on-mouse handle-mouse))