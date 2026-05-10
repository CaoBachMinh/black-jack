#lang racket

(require 2htdp/universe)
(require 2htdp/image)
(require rackunit)

;; ==========================================
;; --- Constants ---
;; ==========================================
(define WIDTH 1300)
(define HEIGHT 800)
(define CANVAS (empty-scene WIDTH HEIGHT))

(define BUTTON-X 1100)
(define BUTTON-Y 600)
(define BUTTON-WIDTH 120)
(define BUTTON-HEIGHT 50)
(define HIT-BUTTON-IMG 
  (overlay (text "DRAW" 24 "white")
           (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "blue")))

(define END-BUTTON-X 1100)
(define END-BUTTON-Y 650)
(define END-BUTTON-IMG 
  (overlay (text "END TURN" 20 "white")
           (rectangle BUTTON-WIDTH BUTTON-HEIGHT "solid" "red")))

;; ==========================================
;; --- Data Definitions ---
;; ==========================================

;; A CardSymbol is a Symbol.
;; Interpretation: Represents a playing card. 
;; The first letter is the suit (c=clubs, d=diamonds, h=hearts, s=spades).
;; Remaining characters are the rank (A, 2-10, J, Q, K).
;; Examples: 'cA, 'h10, 'sQ

;; A Hand is a ListOf<CardSymbol>.
;; Interpretation: A list of cards currently held by either the player or the dealer.

;; A Deck is a ListOf<CardSymbol>.
;; Interpretation: A list of cards that are available to be drawn.

;; A Score is one of:
;; - Integer
;; - (list Integer Integer)
;; Interpretation: A player's score. It is a list of two integers if they have a "soft" Ace 
;; (e.g., an Ace that can be counted as 1 or 11 without busting).

;; A PlayerType (or DealerType) is one of:
;; - "Ace-Ace hand"
;; - "BlackJack"
;; - "5-Card Charlie"
;; - "None"
; Interpretation: Represents the initial special win-condition state of a hand.

;; A PlayerScore (or DealerScore) is an Integer.
;; Interpretation: The calculated best valid score for a hand.

;; A Message is a String.
;; Interpretation: Represents the final game outcome (Win, Lose, or Draw).

(define card-images
  (hash 'cA (bitmap "assets/clubs_A.png")   'c2 (bitmap "assets/clubs_2.png")
        'c3 (bitmap "assets/clubs_3.png")   'c4 (bitmap "assets/clubs_4.png")
        'c5 (bitmap "assets/clubs_5.png")   'c6 (bitmap "assets/clubs_6.png")
        'c7 (bitmap "assets/clubs_7.png")   'c8 (bitmap "assets/clubs_8.png")
        'c9 (bitmap "assets/clubs_9.png")   'c10 (bitmap "assets/clubs_10.png")
        'cJ (bitmap "assets/clubs_J.png")   'cQ (bitmap "assets/clubs_Q.png")
        'cK (bitmap "assets/clubs_K.png")

        'dA (bitmap "assets/diamonds_A.png") 'd2 (bitmap "assets/diamonds_2.png")
        'd3 (bitmap "assets/diamonds_3.png") 'd4 (bitmap "assets/diamonds_4.png")
        'd5 (bitmap "assets/diamonds_5.png") 'd6 (bitmap "assets/diamonds_6.png")
        'd7 (bitmap "assets/diamonds_7.png") 'd8 (bitmap "assets/diamonds_8.png")
        'd9 (bitmap "assets/diamonds_9.png") 'd10 (bitmap "assets/diamonds_10.png")
        'dJ (bitmap "assets/diamonds_J.png") 'dQ (bitmap "assets/diamonds_Q.png")
        'dK (bitmap "assets/diamonds_K.png")

        'hA (bitmap "assets/hearts_A.png")   'h2 (bitmap "assets/hearts_2.png")
        'h3 (bitmap "assets/hearts_3.png")   'h4 (bitmap "assets/hearts_4.png")
        'h5 (bitmap "assets/hearts_5.png")   'h6 (bitmap "assets/hearts_6.png")
        'h7 (bitmap "assets/hearts_7.png")   'h8 (bitmap "assets/hearts_8.png")
        'h9 (bitmap "assets/hearts_9.png")   'h10 (bitmap "assets/hearts_10.png")
        'hJ (bitmap "assets/hearts_J.png")   'hQ (bitmap "assets/hearts_Q.png")
        'hK (bitmap "assets/hearts_K.png")

        'sA (bitmap "assets/spades_A.png")   's2 (bitmap "assets/spades_2.png")
        's3 (bitmap "assets/spades_3.png")   's4 (bitmap "assets/spades_4.png")
        's5 (bitmap "assets/spades_5.png")   's6 (bitmap "assets/spades_6.png")
        's7 (bitmap "assets/spades_7.png")   's8 (bitmap "assets/spades_8.png")
        's9 (bitmap "assets/spades_9.png")   's10 (bitmap "assets/spades_10.png")
        'sJ (bitmap "assets/spades_J.png")   'sQ (bitmap "assets/spades_Q.png")
        'sK (bitmap "assets/spades_K.png")))

(define card-values
  (hash 'cA 1  'c2 2  'c3 3  'c4 4  'c5 5  'c6 6  'c7 7  'c8 8  'c9 9  'c10 10 'cJ 10 'cQ 10 'cK 10
        'dA 1  'd2 2  'd3 3  'd4 4  'd5 5  'd6 6  'd7 7  'd8 8  'd9 9  'd10 10 'dJ 10 'dQ 10 'dK 10
        'hA 1  'h2 2  'h3 3  'h4 4  'h5 5  'h6 6  'h7 7  'h8 8  'h9 9  'h10 10 'hJ 10 'hQ 10 'hK 10
        'sA 1  's2 2  's3 3  's4 4  's5 5  's6 6  's7 7  's8 8  's9 9  's10 10 'sJ 10 'sQ 10 'sK 10))

(define back-card (bitmap "assets/back.png"))
(define INITIAL-DECK (hash-keys card-images))

;; A GameState is a (game-state Deck Hand Hand String Boolean)
;; Interpretation: The complete immutable state of the game at any given tick.
(struct game-state (deck player-hand dealer-hand message turn-ended?) #:transparent)

;; --- Pure Game Logic & Helpers ---

;; get-card-value : CardSymbol -> Score
;; Purpose: Returns the numeric blackjack value for a given card symbol.
(define (get-card-value card-sym)
  (hash-ref card-values card-sym))

;; has-2-aces? : Hand -> Boolean
;; Purpose: Determines if a given hand contains exactly two Aces.
(define (has-2-aces? hand)
  (= (length (filter (lambda (card) (member card '(cA dA hA sA))) hand)) 2))

;; has-ace? : Hand -> Boolean
;; Purpose: Determines if a given hand contains at least one Ace.
(define (has-ace? hand)
  (ormap (lambda (card) (member card '(cA dA hA sA))) hand))

;; has-10? : Hand -> Boolean
;; Purpose: Determines if a given hand contains a 10-value card (10, J, Q, K).
(define (has-10? hand)
  (ormap (lambda (card) (member card '(c10 d10 h10 s10 cJ dJ hJ sJ cQ dQ hQ sQ cK dK hK sK))) hand))

;; get-best-valid-score : Score -> Score
;; Purpose: Given a Score (which might be a list of two options due to a soft Ace), 
;; returns the highest valid integer score.
(define (get-best-valid-score score)
  (if (list? score) (second score) score))

;; calculate-score : Hand -> Score
;; Purpose: Computes the optimal Blackjack score for a given hand, returning a list of two 
;; possibilities if a soft Ace is present.
(define (calculate-score hand)
  (let* ([hand-size (length hand)]
         [raw-score (apply + (map get-card-value hand))]
         [has-ace (has-ace? hand)]
         [non-ace-score (if has-ace (- raw-score 1) raw-score)])
    (cond
      [(not has-ace) raw-score]
      [(>= hand-size 4) raw-score]
      [(= hand-size 2) (+ non-ace-score 11)]
      [(= hand-size 3)
       (cond
         [(> non-ace-score 11) raw-score]
         [(= non-ace-score 11) (list (+ non-ace-score 1) (+ non-ace-score 10))]
         [(< non-ace-score 11) (list (+ non-ace-score 1) (+ non-ace-score 11))])])))

;; check-initial-special-cases : Hand -> PlayerType
;; Purpose: Checks if a hand meets the criteria for instant-win rules (Ace-Ace, Blackjack, 5-Card Charlie).
(define (check-initial-special-cases hand)
  (cond
    [(not (= (length hand) 2)) "None"]
    [(has-2-aces? hand) "Ace-Ace hand"]
    [(and (has-ace? hand) (has-10? hand)) "BlackJack"]
    [(and (= (length hand) 5) (<= (get-best-valid-score (calculate-score hand)) 21)) "5-Card Charlie"]
    [else "None"]))

;; draw-card : Deck -> (list CardSymbol Deck)
;; Purpose: Draws a random card from the provided deck, returning the drawn card and the remaining deck.
(define (draw-card deck)
  (if (empty? deck)  
      (error "The Deck is empty")
      (let* ([random-card (list-ref deck (random (length deck)))]
             [remaining-deck (remove random-card deck)])
        (list random-card remaining-deck))))

;; --- Game Actions (State Transformers) ---

;; dealer-play : Hand Deck -> (list Hand Deck)
;; Purpose: Automates the dealer's drawing phase until they reach their target score.
;; Returns the dealer's final hand and the resulting remaining deck.
(define (dealer-play current-hand current-deck)
  (define target-score (+ 16 (random 6)))
  
  (define (loop hnd dck)
    (define d-score (get-best-valid-score (calculate-score hnd)))
    (if (< d-score target-score)
        (let* ([drawn (draw-card dck)]
               [new-card (first drawn)]
               [new-deck (second drawn)])
          (loop (cons new-card hnd) new-deck))
        (list hnd dck))) ; Return final hand and deck
  
  (loop current-hand current-deck))

;; determine-winner : PlayerType DealerType PlayerScore DealerScore -> Message (Win | Lose | Draw)
;; Purpose: Compares player and dealer hands, scores, and special cases to determine the final win message.
(define (determine-winner p-spec d-spec p-final d-final)
  (cond
    [(and (string=? d-spec p-spec) (not (string=? p-spec "None"))) "Turn Ended! DRAW! 😑"]
    [(and (string=? d-spec "Ace-Ace hand") (string=? p-spec "BlackJack")) "Turn Ended! DEALER WINS! 💀"]
    [(and (string=? d-spec "BlackJack") (string=? p-spec "Ace-Ace hand")) "Turn Ended! YOU WIN! 🎉"]
    [(and (string=? d-spec "None") (not (string=? p-spec "None"))) "Turn Ended! YOU WIN! 🎉"]
    [(and (string=? p-spec "None") (not (string=? d-spec "None"))) "Turn Ended! DEALER WINS! 💀"]
    [(and (string=? p-spec "5-Card Charlie") (not (string=? d-spec "5-Card Charlie"))) "Turn Ended! YOU WIN! 🎉"]
    [(and (string=? d-spec "5-Card Charlie") (not (string=? p-spec "5-Card Charlie"))) "Turn Ended! DEALER WINS! 💀"]
    [(and (string=? d-spec "5-Card Charlie") (string=? p-spec "5-Card Charlie"))
     (cond [(< p-final d-final) "Turn Ended! YOU WIN! 🎉"]
           [(> p-final d-final) "Turn Ended! DEALER WINS! 💀"]
           [else "Turn Ended! DRAW! 😑"])]
    [(and (> p-final 21) (> d-final 21)) "Turn Ended! DRAW! 😑"]
    [(and (<= p-final 21) (or (> d-final 21) (> p-final d-final))) "Turn Ended! YOU WIN! 🎉"]
    [(= p-final d-final) "Turn Ended! DRAW! (Equal Scores) 😑"]
    [else "Turn Ended! DEALER WINS! 💀"]))

;; handle-hit : GameState -> GameState
;; Purpose: Processes the "DRAW" button click. Updates the GameState by moving one card 
;; from the deck to the player's hand, or sets an error message if invalid.
(define (handle-hit state)
  (define p-hand (game-state-player-hand state))
  (define deck (game-state-deck state))
  (define d-hand (game-state-dealer-hand state))
  (define curr-score (calculate-score p-hand))
  
  (cond 
    [(game-state-turn-ended? state) (struct-copy game-state state [message ""])]
    [(>= (length p-hand) 5)
     (struct-copy game-state state 
                  [message (string-append "Maximum 5 cards reached!, Your score: " 
                                          (number->string (get-best-valid-score curr-score)))])]
    [(and (not (list? curr-score)) (>= curr-score 21))
     (struct-copy game-state state [message "Bust! You cannot draw over 21."])]
    [else
     (let* ([drawn (draw-card deck)]
            [new-card (first drawn)]
            [new-deck (second drawn)]
            [new-hand (cons new-card p-hand)]
            [new-score (calculate-score new-hand)]
            [new-msg (if (list? new-score)
                         (string-append "Your current score is either " 
                                        (number->string (first new-score)) " OR " 
                                        (number->string (second new-score)))
                         (string-append "Your current score: " (number->string new-score)))])
       (game-state new-deck new-hand d-hand new-msg #f))]))

;; handle-end : GameState -> GameState
;; Purpose: Processes the "END TURN" button click. Triggers the dealer's play sequence 
;; and computes the final winner, locking the game.
(define (handle-end state)
  (if (game-state-turn-ended? state)
      state ; Do nothing if already ended
      (let* ([dealer-special (check-initial-special-cases (game-state-dealer-hand state))]
             [player-special (check-initial-special-cases (game-state-player-hand state))]
             
             ;; Dealer plays functionally only if they don't have a winning special initial hand
             [dealer-needs-play (and (not (string=? dealer-special "Ace-Ace hand"))
                                     (not (string=? dealer-special "BlackJack")))]
             [dealer-result (if dealer-needs-play
                                (dealer-play (game-state-dealer-hand state) (game-state-deck state))
                                (list (game-state-dealer-hand state) (game-state-deck state)))]
             
             [final-d-hand (first dealer-result)]
             [final-deck (second dealer-result)]
             [p-final (get-best-valid-score (calculate-score (game-state-player-hand state)))]
             [d-final (get-best-valid-score (calculate-score final-d-hand))]
             [msg (determine-winner player-special dealer-special p-final d-final)])
        
        (game-state final-deck (game-state-player-hand state) final-d-hand msg #t))))

;; handle-mouse : GameState Integer Integer String -> GameState
;; Purpose: Universe event handler. Maps mouse clicks to their corresponding state-transformer functions.
(define (handle-mouse state mouse-x mouse-y event)
  (cond
    [(string=? event "button-down")
     (let ([hit-L (- BUTTON-X (/ BUTTON-WIDTH 2))] [hit-R (+ BUTTON-X (/ BUTTON-WIDTH 2))]
           [hit-T (- BUTTON-Y (/ BUTTON-HEIGHT 2))] [hit-B (+ BUTTON-Y (/ BUTTON-HEIGHT 2))]
           [end-L (- END-BUTTON-X (/ BUTTON-WIDTH 2))] [end-R (+ END-BUTTON-X (/ BUTTON-WIDTH 2))]
           [end-T (- END-BUTTON-Y (/ BUTTON-HEIGHT 2))] [end-B (+ END-BUTTON-Y (/ BUTTON-HEIGHT 2))])
       (cond
         [(and (>= mouse-x hit-L) (<= mouse-x hit-R) (>= mouse-y hit-T) (<= mouse-y hit-B))
          (handle-hit state)]
         [(and (>= mouse-x end-L) (<= mouse-x end-R) (>= mouse-y end-T) (<= mouse-y end-B))
          (handle-end state)]
         [else state]))]
    [else state]))

;; --- Drawing & Initialization ---

;; draw-hand : Hand Integer Integer Boolean Image -> Image
;; Purpose: Recursively places card images onto the given scene. 
;; If hide? is true, the first card in the sequence is drawn face-down.
(define (draw-hand hand x y hide? scene)
  (cond
    [(empty? hand) scene]
    [else
     (let* ([first-card-symbol (first hand)]
            [card-img (if hide? back-card (hash-ref card-images first-card-symbol))])
       (place-image (frame card-img) x y 
                    (draw-hand (rest hand) (- x 150) y hide? scene)))]))

;; draw-game : GameState -> Image
;; Purpose: Universe render handler. Translates the current GameState into the visual Canvas representation.
(define (draw-game state)
  (place-image (text (game-state-message state) 20 "red") 1100 550
               (place-image HIT-BUTTON-IMG BUTTON-X BUTTON-Y
                            (place-image END-BUTTON-IMG END-BUTTON-X END-BUTTON-Y
                                         (draw-hand (game-state-dealer-hand state) 800 200 
                                                    (not (game-state-turn-ended? state)) 
                                                    (draw-hand (game-state-player-hand state) 800 600 #f CANVAS))))))

;; create-initial-state : -> GameState
;; Purpose: Sets up the beginning of a game by drawing 4 initial cards and assembling the first GameState struct.
(define (create-initial-state)
  (let* ([d1 (draw-card INITIAL-DECK)] [p-card1 (first d1)] [deck1 (second d1)]
         [d2 (draw-card deck1)]        [p-card2 (first d2)] [deck2 (second d2)]
         [d3 (draw-card deck2)]        [d-card1 (first d3)] [deck3 (second d3)]
         [d4 (draw-card deck3)]        [d-card2 (first d4)] [deck4 (second d4)]
         
         [p-hand (list p-card1 p-card2)]
         [d-hand (list d-card1 d-card2)]
         [p-special (check-initial-special-cases p-hand)])
    
    (cond
      [(string=? p-special "Ace-Ace hand")
       (game-state deck4 p-hand d-hand "You have an Ace-Ace hand!" #t)]
      [(string=? p-special "BlackJack")
       (game-state deck4 p-hand d-hand "You have a BlackJack!" #t)]
      [else
       (game-state deck4 p-hand d-hand "" #f)])))

;; --- Execution ---
 (big-bang (create-initial-state)
   (to-draw draw-game)
   (on-mouse handle-mouse))

;; --- Unit Tests (Functional Testing) ---
(module+ test
  (require rackunit)

  (displayln "Running tests...")

  ;; --- Tests for get-card-value ---
  (check-equal? (get-card-value 'cA) 1 "Ace should equal 1")
  (check-equal? (get-card-value 'd7) 7 "7 should equal 7")
  (check-equal? (get-card-value 'sK) 10 "King should equal 10")
  (check-equal? (get-card-value 'cJ) 10 "Jack should equal 10")

  ;; --- Tests for has-ace? ---
  (check-not-false (has-ace? '(cA h5)) "Hand has an Ace")
  (check-false (has-ace? '(c10 h5)) "Hand does not have an Ace")

  ;; --- Tests for has-2-aces? ---
  (check-true (has-2-aces? '(cA dA)) "Hand has exactly 2 Aces")
  (check-false (has-2-aces? '(cA h5)) "Hand has 1 Ace")
  (check-false (has-2-aces? '(c10 h5)) "Hand has 0 Aces")

  ;; --- Tests for has-10? (Face cards and 10s) ---
  (check-not-false (has-10? '(c10 h5)) "Hand has a 10")
  (check-not-false (has-10? '(hJ h5)) "Hand has a Jack")
  (check-not-false (has-10? '(dQ h5)) "Hand has a Queen")
  (check-false (has-10? '(h9 h5)) "Hand has no 10 or face card")

  ;; --- Tests for calculate-score ---
  (check-equal? (calculate-score '(c5 h5)) 10 "Standard calculation (no aces)")
  (check-equal? (calculate-score '(c10 d10)) 20 "Two tens = 20")
  (check-equal? (calculate-score '(cA c8)) (list 9 19) "Soft Ace: Ace + 8 can be 9 or 19")
  (check-equal? (calculate-score '(cA cK c9)) 20 "Hard Ace: Ace is forced to be 1 to avoid busting")
  
  ;; --- Tests for get-best-valid-score ---
  (check-equal? (get-best-valid-score (list 9 19)) 19 "Should extract the higher valid score")
  (check-equal? (get-best-valid-score 20) 20 "Should just return the number if not a list")

  ;; --- Tests for check-initial-special-cases ---
  (check-equal? (check-initial-special-cases '(cA dA)) "Ace-Ace hand" "Should detect double Aces")
  (check-equal? (check-initial-special-cases '(cA cK)) "BlackJack" "Should detect BlackJack")
  (check-equal? (check-initial-special-cases '(c10 h5)) "None" "Normal hand is None")
  (check-equal? (check-initial-special-cases '(c2 d2 h2 s2 c3)) "5-Card Charlie" "5 cards under 21 is a Charlie")
  (check-equal? (check-initial-special-cases '(c10 d10 h10 c5 d5)) "None" "5 cards OVER 21 is NOT a Charlie")

  ;; --- Pure Function Test: draw-card ---
  ;; We verify that the deck passed in is NEVER mutated, and instead 
  ;; a new smaller deck is returned.
  (let* ([fake-deck '(cA c2 c3 c4)]
         [result (draw-card fake-deck)]
         [drawn-card (first result)]
         [remaining-deck (second result)])
    
    (check-true (not (false? (member drawn-card fake-deck))) "The drawn card must come from the deck")
    (check-equal? (length remaining-deck) 3 "The new deck should be exactly 1 card smaller")
    (check-false (member drawn-card remaining-deck) "The drawn card should no longer be in the new deck")
    (check-equal? fake-deck '(cA c2 c3 c4) "CRITICAL: The original deck list must remain unchanged (Pure Function)"))

  (displayln "All tests passed successfully! ✅")
)