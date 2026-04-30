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
;; This function will calculate score without including special cases
(define (calculate-score hand)
  (let* ([hand-size (length hand)]
         [raw-score (apply + (map get-card-value hand))]
         [has-ace (has-ace? hand)]
         [non-ace-score (if has-ace (- raw-score 1) raw-score)] ; if has ace in hand, then simply - 1 from raw point
         )
    (cond
      [(not has-ace) raw-score]
      [(>= hand-size 4) raw-score]
      [(= hand-size 2) (+ non-ace-score 11)]
      [(= hand-size 3)
       (cond
         [(> non-ace-score 11) raw-score]
         [(= non-ace-score 11)
          ;Return list to let player decide whether hit or stand
          (list (+ non-ace-score 1)  ;12 
                (+ non-ace-score 10))] ;21
         [(< non-ace-score 11)
          ;Return list to let player decide whether hit or stand
          (list (+ non-ace-score 1)
                (+ non-ace-score 11))])])))
     

;; Check if intial hand (with 2 intital cards) has specicial cases: AA hand or BlackJack
;;check-initial-special-cases : hand -> Boolean
(define (check-initial-special-cases hand)
  (cond
    [(not (= (length hand) 2)) "None"]
    [(has-2-aces? hand) "Ace-Ace hand"]
    [(and (has-ace? hand) (has-10? hand)) "BlackJack"]
    [(and (= (length hand) 5) (<= (calculate-score hand) 21)) "5-Card Charlie"]
    [else "None"]))
      

;;Check if the hand has exactly 2 Aces
;;has-2-aces? : hand -> Boolean
(define (has-2-aces? hand)
  (=(length (filter (lambda(card)
            (member card '(cA dA hA sA)))hand))
    2)
  )

;; Check if hand has at leats 1 Ace
;; has-ace?: hand -> #f or ListOf<card>
(define (has-ace? hand)
  (ormap (lambda(card)
           (member card '(cA dA hA sA)))
         hand))

;;check if hand has at least 1 card in (10,J,Q,K)
;; has-10?: hand -> #f or ListOf<card>
(define (has-10? hand)
  (ormap (lambda(card)
           (member card '(c10 d10 h10 s10 cJ dJ hJ sJ cQ dQ hQ sQ cK dK hK sK)))
         hand))

;;Use when having Ace but decide to stand early
;; get-best-valid-score: listOf<score> -> score

(define (get-best-valid-score score)
  (if(list? score)
     (second score)
     score))
  

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
                    (draw-hand (rest hand) (- x 150) y hide? scene)))]))

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
          (handle-hit-click state)
          ]
         
         ;; --- CLICKED END TURN BUTTON ---
         [(and (>= mouse-x end-L) (<= mouse-x end-R)
               (>= mouse-y end-T) (<= mouse-y end-B))
          (handle-end-click state)
          ]
         
         ;; --- CLICKED ANYWHERE ELSE ---
         [else state]))]
    
    [else state]))



(define (handle-hit-click state)
  (let ([player-current-score (calculate-score player-hand)])
    (cond 
      [turn-ended?
       (begin (set! game-message "") state)]
      [(>= (length player-hand) 5)
       (begin (set! game-message (string-append "Maximum 5 cards reached!, Your score: " (number->string player-current-score))) state)]
      
     [(and (not (list? player-current-score))  (< player-current-score 21))
          (begin
            (set! player-hand (add-card (get-random-card MASTER-DECK) player-hand))
            ;;score after hitting
            (let ([player-after-hit-score (calculate-score player-hand)])
              (if (list? player-after-hit-score)
                  (set! game-message 
                    (string-append "Your current score is either "
                                   (number->string (first player-after-hit-score))  ; closed here
                                    " OR "
                                    (number->string (second player-after-hit-score)))) ; closed here
                  ;;if not a list
                  (set! game-message (string-append "Your current score: " (number->string player-after-hit-score))))) 
             state)]
     [(and (not(list? player-current-score)) (>= player-current-score 21))
           (begin
               (set! game-message "Bust! You cannot draw over 21.")
              state)]
       ;;If it is a list
     [(list? player-current-score)
         (begin
           (set! player-hand (add-card (get-random-card MASTER-DECK) player-hand))
           (let ([player-after-hit-score (calculate-score player-hand)])
             (set! game-message (string-append "Your current score: " (number->string player-after-hit-score))))
           state)]
     [else state])))
         



(define (handle-end-click state)
          (begin
            (set! turn-ended? #t)
           (let* ([dealer-special-case-check (check-initial-special-cases dealer-hand)]
                  [player-special-case-check (check-initial-special-cases player-hand)]

                  ;;Like a trick to run a code inside let
                  [dummy (cond
                           [(string=? dealer-special-case-check "Ace-Ace hand") ""]
               
                           [(string=? dealer-special-case-check "BlackJack") ""]
                            ;; Run the dealer loop
                           [else (dealer-play!)])]
                  [p-final (get-best-valid-score(calculate-score player-hand))]
                  [d-final (get-best-valid-score(calculate-score dealer-hand))])
                  
           
            ;; Figure out who won to display a message!
            
            (cond

             ;;If 2 have Double-Ace hand  or BlackJack
              [(and (string=? dealer-special-case-check  player-special-case-check)
                    (not (string=? player-special-case-check "None")))
               (set! game-message "Turn Ended! DRAW! 😑")]

              ;; If Dealer AA and Player BlackJack, Dealer win
              [(and (string=? dealer-special-case-check "Ace-Ace hand") (string=? player-special-case-check "BlackJack" ))
               (set! game-message "Turn Ended! DEALER WINS! 💀")]

              ;;If Player AA and Dealer BlackJack, Player win
              [(and (string=? dealer-special-case-check "BlackJack") (string=? player-special-case-check "Ace-Ace hand"))
               (set! game-message "Turn Ended! YOU WIN! 🎉" )]


              ;;If Player have Special cases and Dealer dont
               [(and (string=? dealer-special-case-check "None") (not (string=? player-special-case-check "None")))
               (set! game-message "Turn Ended! YOU WIN! 🎉" )]

               ;;If Dealer has special cases and player doesnt
               [(and (string=? player-special-case-check "None") (not (string=? dealer-special-case-check "None")))
               (set! game-message "Turn Ended! DEALER WINS! 💀" )]


              ;; Speical rule: If one have 5 cards with points <= 21, then win
               ;; If player has 5-card Charlie but dealer doesn't
               [(and (string=? player-special-case-check "5-Card Charlie") (not (string=? dealer-special-case-check "5-Card Charlie")))
                (set! game-message "Turn Ended! YOU WIN! 🎉")]
               ;; If player doesn't have 5-card Charlie but dealer does
               [(and (string=? dealer-special-case-check "5-Card Charlie") (not (string=? player-special-case-check "5-Card Charlie")))
                (set! game-message "Turn Ended! DEALER WINS! 💀")]

              ;; Speical rule: Charlie  if both have 5 cards with points <=21, then compare who has less point will win
                [(and (string=? dealer-special-case-check "5-Card Charlie") (string=? player-special-case-check "5-Card Charlie"))
                 (cond
                   [(< p-final d-final) (set! game-message "Turn Ended! YOU WIN! 🎉")]
                   [(> p-final d-final) (set! game-message "Turn Ended! DEALER WINS! 💀")]
                   [(= p-final d-final) (set! game-message "Turn Ended! DRAW! 😑")]
                  )]                
              
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
            state)))

;; dealer-play! : -> Void
;; Automates the dealer's turn using a recursive loop.
(define (dealer-play!)  
  (define target-score (+ 16 (random 6)))
  
  ;; Define a local recursive function that acts as our "while" loop
  (define (dealer-loop)
    (define d-score (get-best-valid-score(calculate-score dealer-hand)))
    
    
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

  ;; if having special cases here
  (define player-special-case-check (check-initial-special-cases player-hand))

   (cond
     [(string=? player-special-case-check "Ace-Ace hand")
      (begin 
      (set! game-message "You have an Ace-Ace hand!")
      (set! turn-ended? #t))
      ]
     [(string=? player-special-case-check "BlackJack")
      (begin
      (set! game-message "You have a BlackJack!")
      (set! turn-ended? #t))
      ]
     [else (set! turn-ended? #f)]
     ))
                                                       
                                                

;; draw-game : GameState -> Image
;; Renders the board and the UI buttons
(define (draw-game state)
  (place-image (text game-message 20 "red") 1100 550 ; Draw error message at top
               ;; Draw the button, then the dealer's hand, then the player's hand
               (place-image HIT-BUTTON-IMG BUTTON-X BUTTON-Y
                            (place-image END-BUTTON-IMG END-BUTTON-X END-BUTTON-Y
                                         (draw-hand dealer-hand 800 200 (not turn-ended?) 
                                                    (draw-hand player-hand 800 600 #f CANVAS))))))





;;Test cases

(check-equal? (calculate-score '(h2 h3)) 5)
(check-equal? (calculate-score '(hK hQ)) 20)
(check-equal? (calculate-score '(hA h9)) 20)
(check-equal? (calculate-score '(hA h5)) 16)
(check-equal? (calculate-score '(hA h2 h3 h4)) 10)
(check-equal? (calculate-score '(dA d2 d3 d4 d5)) 15)
(check-equal? (calculate-score '(dA d7 d5)) 13)
(check-equal? (calculate-score '(dA d10 s10)) 21)
(check-equal? (calculate-score '(dA d5 s6)) '(12 21))
(check-equal? (calculate-score '(dA s7 d4)) '(12 21))
(check-equal? (calculate-score '(dA s6 d4)) '(11 21))

;; --- NOT 2 CARDS: always "None" ---
(check-equal? (check-initial-special-cases '()) "None" "empty hand - not special")
(check-equal? (check-initial-special-cases '(hA)) "None" "1 card - not special")
(check-equal? (check-initial-special-cases '(hA cA h5)) "None" "3 cards with 2 aces - not special")
(check-equal? (check-initial-special-cases '(hA hK h5)) "None" "3 cards with ace+king - not special")
(check-equal? (check-initial-special-cases '(hA cA h5 h6)) "None" "4 cards - not special")

;; --- ACE-ACE HAND ---
(check-equal? (check-initial-special-cases '(hA cA)) "Ace-Ace hand" "hearts ace + clubs ace")
(check-equal? (check-initial-special-cases '(hA dA)) "Ace-Ace hand" "hearts ace + diamonds ace")
(check-equal? (check-initial-special-cases '(sA dA)) "Ace-Ace hand" "spades ace + diamonds ace")
(check-equal? (check-initial-special-cases '(cA sA)) "Ace-Ace hand" "clubs ace + spades ace")

;; --- BLACKJACK: ace + 10/J/Q/K ---
(check-equal? (check-initial-special-cases '(hA h10)) "BlackJack" "ace + 10")
(check-equal? (check-initial-special-cases '(hA hJ))  "BlackJack" "ace + jack")
(check-equal? (check-initial-special-cases '(hA hQ))  "BlackJack" "ace + queen")
(check-equal? (check-initial-special-cases '(hA hK))  "BlackJack" "ace + king")
(check-equal? (check-initial-special-cases '(hK hA))  "BlackJack" "king + ace (reversed order)")

;; --- NONE: 2 cards, no special case ---
(check-equal? (check-initial-special-cases '(h5 h6))  "None" "5+6 no special")
(check-equal? (check-initial-special-cases '(h2 hK))  "None" "2+king no special")
(check-equal? (check-initial-special-cases '(h9 h8))  "None" "9+8 no special")


#|

;; =====================
;; has-2-aces? tests
;; =====================
;; TRUE cases
(check-equal? (has-2-aces? '(hA cA))       #t "2 aces: hearts + clubs")
(check-equal? (has-2-aces? '(hA dA))       #t "2 aces: hearts + diamonds")
(check-equal? (has-2-aces? '(sA dA))       #t "2 aces: spades + diamonds")
(check-equal? (has-2-aces? '(hA cA h5))    #t "2 aces in 3 card hand")

;; FALSE cases
(check-equal? (has-2-aces? '(hA h5 h6))   #f "only 1 ace")
(check-equal? (has-2-aces? '(h5 h6 h7))   #f "no aces")
(check-equal? (has-2-aces? '())            #f "empty hand")
(check-equal? (has-2-aces? '(hA))          #f "only 1 card, 1 ace")

;; =====================
;; has-ace? tests
;; =====================
;; TRUE cases
(check-equal? (has-ace? '(hA h5))          '(hA) "hearts ace")
(check-equal? (has-ace? '(cA h5))          '(cA) "clubs ace")
(check-equal? (has-ace? '(dA h5))          '(dA) "diamonds ace")
(check-equal? (has-ace? '(sA h5))          '(sA) "spades ace")
(check-equal? (has-ace? '(hA cA h5))       '(hA) "2 aces still returns true")

;; FALSE cases
(check-equal? (has-ace? '(h5 h6))          #f "no ace")
(check-equal? (has-ace? '(hK hQ))          #f "face cards no ace")
(check-equal? (has-ace? '())               #f "empty hand")

;; =====================
;; has-10? tests
;; =====================
;; TRUE cases - each suit of 10
(check-equal? (has-10? '(h10 h5))          '(h10) "hearts 10")
(check-equal? (has-10? '(c10 h5))          '(c10) "clubs 10")
(check-equal? (has-10? '(d10 h5))          '(d10) "diamonds 10")
(check-equal? (has-10? '(s10 h5))          '(h10) "spades 10")

;; TRUE cases - face cards
(check-equal? (has-10? '(hJ h5))           '(hJ) "hearts jack")
(check-equal? (has-10? '(hQ h5))           '(hQ) "hearts queen")
(check-equal? (has-10? '(hK h5))           '(hK) "hearts king")
(check-equal? (has-10? '(cJ dQ sK h5))     '(cJ dQ sK) "multiple face cards")

;; FALSE cases
(check-equal? (has-10? '(h5 h6))           #f "no 10 or face card")
(check-equal? (has-10? '(hA h9))           #f "ace and 9, no 10")
(check-equal? (has-10? '())                #f "empty hand")

;; =====================
;; get-best-valid-score tests
;; =====================
;; List input - always returns second (higher) value
(check-equal? (get-best-valid-score '(8 18))   18  "returns higher: 8 or 18")
(check-equal? (get-best-valid-score '(12 21))  21  "returns higher: 12 or 21")
(check-equal? (get-best-valid-score '(2 12))   12  "returns higher: 2 or 12")

;; Single number input - returns as is
(check-equal? (get-best-valid-score 20)        20  "single score 20")
(check-equal? (get-best-valid-score 14)        14  "single score 14")
(check-equal? (get-best-valid-score 21)        21  "single score 21")
(check-equal? (get-best-valid-score 5)         5   "single score 5")

|#


(setup-game!)
(big-bang 0
  (to-draw draw-game)
  (on-mouse handle-mouse))




