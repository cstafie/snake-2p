#lang racket
(require 2htdp/image 2htdp/universe lens "./fps.rkt")

;; STRUCTS
(struct/lens posn [x y] #:transparent)
(struct/lens img-posn [img posn] #:transparent)
(struct/lens game [snakes scene] #:transparent)
(struct/lens snake [direction body color length keys inputs] #:transparent)

;; CONSTANTS
(define TICK-RATE 1/40)
(define SNAKE-SIZE 5)
(define BOARD-WIDTH 160)
(define BOARD-HEIGHT 120)
(define EMPTY-SCENE (empty-scene (* SNAKE-SIZE BOARD-WIDTH) (* SNAKE-SIZE BOARD-HEIGHT)))
(define MAX-KEY-QUE-LENGTH 5)
(define SNAKE-STARTING-LENGTH 10)
(define BG-COLOR "white")

;; KEY CONSTANTS
(define PLAYER1-KEYS (list "w" "a" "s" "d"))
(define PLAYER1-START (posn (/ BOARD-WIDTH  10) (/ BOARD-HEIGHT 2)))
(define PLAYER2-KEYS (list "up" "right" "down" "left"))
(define PLAYER2-START (posn  (- BOARD-WIDTH  (/ BOARD-WIDTH  10)) (/ BOARD-HEIGHT 2)))
(define PLAYER3-KEYS (list "i" "j" "k" "l"))
(define PLAYER3-START (posn (/ BOARD-WIDTH 2) 1))

;; DIRECTIONS AND KEYS
(define DIRECTION-TO-VALUE
  (hash
   'up    (posn 0 -1)
   'right (posn 1 0)
   'down  (posn 0 1)
   'left  (posn -1 0)))
(define KEY-TO-DIRECTION
  (hash "up" 'up "right" 'right "down" 'down "left" 'left
        "w" 'up "d" 'right "s" 'down "a" 'left
        "i" 'up "l" 'right "k" 'down "j" 'left))

;; MAIN
(define (start players)
  (define snakes
    (list
     (snake 'right (list PLAYER1-START) "blue" SNAKE-STARTING-LENGTH PLAYER1-KEYS empty)
     (snake 'left  (list PLAYER2-START) "red" SNAKE-STARTING-LENGTH PLAYER2-KEYS empty)
     (snake 'down  (list PLAYER3-START) "green" SNAKE-STARTING-LENGTH PLAYER3-KEYS empty)
     ;(snake 'up    (list (posn 40 50)) "yellow" SNAKE-STARTING-LENGTH PLAYER2-KEYS empty)
     ))
  (big-bang (game (take snakes (min players (length snakes))) EMPTY-SCENE)
    (on-tick update-game TICK-RATE)
    (on-key manage-inputs)
    (to-draw render-game)
    (stop-when game-over? manage-end)
    (name "Sssnek")
    (close-on-stop 1)))

;; POSN HELPERS
(define (img-posn-x ip) (lens-view (lens-compose posn-x-lens img-posn-posn-lens) ip))
(define (img-posn-y ip) (lens-view (lens-compose posn-y-lens img-posn-posn-lens) ip))

(define (posn+ a b)
  (posn (+ (posn-x a) (posn-x b)) (+ (posn-y a) (posn-y b))))

;; SNAKE HELPERS
(define (snake-head s)
  (first (snake-body s)))
(define (snake-tail s)
  (rest (snake-body s)))

(define (snake-next-direction s)
  (define current-direction (snake-direction s))
  (define next-direction
    (cond [(empty? (snake-inputs s)) current-direction]
          [else (first (snake-inputs s))]))
  (cond [(opposite-directions? current-direction next-direction) current-direction]
        [else next-direction]))

(define (snake-add-input s k)
  (let ([inputs (snake-inputs s)])
    (cond [(and (< (length inputs) MAX-KEY-QUE-LENGTH) (member k (snake-keys s)))
           (lens-set snake-inputs-lens s (append (snake-inputs s) (list (hash-ref KEY-TO-DIRECTION k))))]
          [else s])))

(define (snake-next-head s)
  (location+direction (first (snake-body s)) (snake-direction s)))

;; DIRECTION HELPERS
(define (opposite-directions? d1 d2)
  (equal? (posn+
           (hash-ref DIRECTION-TO-VALUE d1)
           (hash-ref DIRECTION-TO-VALUE d2))
          (posn 0 0)))

(define (location+direction loc dir)
  (define val (hash-ref DIRECTION-TO-VALUE dir))
  (posn+ loc val))

;; RENDER HELPERS
(define (render-square color x y scene)
  (place-image
   (square SNAKE-SIZE "solid" color)
   (* x SNAKE-SIZE)
   (* y SNAKE-SIZE)
   scene))

;; RENDER
(define (render-game g)
  (add-fps (game-scene g)))

;(define (render-snakes snakes)
;  (define (render-snake s scene)
;    (cond [(empty? (snake-body s)) scene]
;          [else
;           (define piece (first (snake-body s)))
;           (render-snake
;            (lens-set snake-body-lens s (rest (snake-body s)))
;            (render-square (snake-color s) (posn-x piece) (posn-y piece) scene))]))
;  (foldr render-snake EMPTY-SCENE snakes))

;; UPDATE

(define (update-game g)
  (let* ([directed-snakes (direct-snakes (game-snakes g))]
         [tail-img-posns (snake-tails directed-snakes)]
         [moved-snakes (move-snakes directed-snakes)]
         [img-posns (append (snake-heads moved-snakes) tail-img-posns)]
         [new-scene (render-img-posns img-posns (game-scene g))])
    (game moved-snakes new-scene)))

(define (snake-tails directed-snakes)
  (define (snake-tail-img-posn s)
    (cond [(equal? (length (snake-body s)) (snake-length s))
           (list (img-posn (square SNAKE-SIZE "solid" BG-COLOR) (last (snake-body s))))]
          [else empty]))
  (foldr (λ (v l) (append (snake-tail-img-posn v) l)) empty directed-snakes))

(define (snake-heads moved-snakes)
  (define (snake-head-img-posn s)
    (img-posn (square SNAKE-SIZE "solid" (snake-color s)) (first (snake-body s))))
  (map snake-head-img-posn moved-snakes))

(define (snake-img-posns snakes)
  (append (snake-heads snakes) (snake-tails snakes)))

(define (render-img-posns img-posns scene)
  (define (render-img-posn ip s)
    (place-image/align
     (img-posn-img ip)
     (* (img-posn-x ip) SNAKE-SIZE) (* (img-posn-y ip) SNAKE-SIZE)
     "left" "top" s))
  (foldr render-img-posn scene img-posns))

(define (direct-snakes snakes)
  (define (direct-snake s)
    (lens-transform/list
     s
     snake-direction-lens (λ (d) (snake-next-direction s))
     snake-inputs-lens (λ (i) (cond [(empty? (snake-inputs s)) empty]
                                    [else (rest (snake-inputs s))]))))
  (map direct-snake snakes))

(define (move-snakes snakes)
  (define (move-snake s)
    (let ([body (snake-body s)])
      (lens-set snake-body-lens s
                (take (cons (snake-next-head s) body)
                      (min (snake-length s) (add1 (length body)))))))
  (map move-snake snakes))

;; INPUTS
(define (manage-inputs g key)
  (lens-set game-snakes-lens g
            (foldr (λ (s l) (cons (snake-add-input s key) l)) empty (game-snakes g))))

;; GAME END
(define (game-over? g)
  (cond [(collision? g) #t]
        [else #f]))

(define foldor (λ (v l) (or v l)))

(define (is-outside? x a b)
  (or (< x a) (> x b)))

(define (snakes-outside-board? snakes)
  (define (snake-outside-board? s)
    (define head (first (snake-body s)))
    (or (is-outside? (posn-x head) 0 BOARD-WIDTH)
        (is-outside? (posn-y head) 0 BOARD-HEIGHT)))
  (foldr foldor false (map snake-outside-board? snakes)))

(define (snake-collided-self? s)
  (member (snake-head s) (snake-tail s))) 

(define (snakes-collided-each-other? snakes)
  (define (snake-collided-with-other? s)
    (member (snake-head s) (flatten (map snake-body (remove s snakes)))))
  (foldr foldor false (map snake-collided-with-other? snakes)))

(define (collision? g)
  (define snakes (game-snakes g))
  (or (foldr foldor false (map snake-collided-self? snakes))
      (snakes-collided-each-other? snakes)
      (snakes-outside-board? snakes)))
  
(define (manage-end g) (game-scene g))
