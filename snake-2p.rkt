#lang racket
(require 2htdp/image 2htdp/universe lens)

;; STRUCTS
(struct/lens pair [x y] #:transparent)
(struct/lens game [snakes] #:transparent)
(struct/lens snake [direction body color length keys inputs] #:transparent)

;; CONSTANTS
(define TICK-RATE 1/40)
(define SNAKE-SIZE 5)
(define BOARD-WIDTH 160)
(define BOARD-HEIGHT 120)
(define EMPTY-SCENE (empty-scene (* SNAKE-SIZE BOARD-WIDTH) (* SNAKE-SIZE BOARD-HEIGHT)))
(define MAX-KEY-QUE-LENGTH 5)
(define SNAKE-STARTING-LENGTH 3000)

;; KEY CONSTANTS
(define PLAYER1-KEYS (list "w" "a" "s" "d"))
(define PLAYER1-START (pair (/ BOARD-WIDTH  10) (/ BOARD-HEIGHT 2)))
(define PLAYER2-KEYS (list "up" "right" "down" "left"))
(define PLAYER2-START (pair  (- BOARD-WIDTH  (/ BOARD-WIDTH  10)) (/ BOARD-HEIGHT 2)))
(define PLAYER3-KEYS (list "i" "j" "k" "l"))
(define PLAYER3-START (pair (/ BOARD-WIDTH 2) 1))

;; DIRECTIONS AND KEYS
(define DIRECTION-TO-VALUE
  (hash
   'up    (pair 0 -1)
   'right (pair 1 0)
   'down  (pair 0 1)
   'left  (pair -1 0)))
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
     ;(snake 'up    (list (pair 40 50)) "yellow" SNAKE-STARTING-LENGTH PLAYER2-KEYS empty)
     ))
  (big-bang
      (game (take snakes (min players (length snakes))))
    (on-tick update-game TICK-RATE)
    (on-key manage-inputs)
    (to-draw render-game)
    (stop-when game-over? manage-end)
    (close-on-stop 1)
  ))

;; PAIR HELPERS
(define (pair+ a b)
  (pair (+ (pair-x a) (pair-x b)) (+ (pair-y a) (pair-y b))))

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

;; DIRECTION HELPERS
(define (opposite-directions? d1 d2)
  (equal? (pair+
           (hash-ref DIRECTION-TO-VALUE d1)
           (hash-ref DIRECTION-TO-VALUE d2))
          (pair 0 0)))

(define (location+direction loc dir)
  (define val (hash-ref DIRECTION-TO-VALUE dir))
  (pair+ loc val))

;; RENDER HELPERS
(define (render-square color x y scene)
  (place-image
   (square SNAKE-SIZE "solid" color)
   (* x SNAKE-SIZE)
   (* y SNAKE-SIZE)
   scene))

;; RENDER
(define (render-game w)
  (render-snakes (game-snakes w)))

(define (render-snakes snakes)
  ;(println (snake-inputs (first snakes)))
  (define (render-snake s scene)
    (cond [(empty? (snake-body s)) scene]
          [else
           (define piece (first (snake-body s)))
           (render-snake
            (lens-set snake-body-lens s (rest (snake-body s)))
            (render-square (snake-color s) (pair-x piece) (pair-y piece) scene))]))
  (foldr render-snake EMPTY-SCENE snakes))

;; UPDATE

(define (update-game w)
  (lens-set game-snakes-lens w (update-snakes (game-snakes w))))

(define (update-snakes s)
  ((compose move-snakes direct-snakes) s))

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
                (take (cons (location+direction (first body) (snake-direction s)) body)
                      (min (snake-length s) (add1 (length body)))))))
  (map move-snake snakes))

;; INPUTS
(define (manage-inputs w key)
  (lens-set game-snakes-lens w
            (foldr (λ (s l) (cons (snake-add-input s key) l)) empty (game-snakes w))))

;; GAME END
(define (game-over? w)
  (cond [(collision? w) #t]
        [else #f]))

(define foldor (λ (v l) (or v l)))

(define (is-outside? x a b)
  (or (< x a) (> x b)))

(define (snakes-outside-board? snakes)
  (define (snake-outside-board? s)
    (define head (first (snake-body s)))
    (or (is-outside? (pair-x head) 0 BOARD-WIDTH)
        (is-outside? (pair-y head) 0 BOARD-HEIGHT)))
  (foldr foldor false (map snake-outside-board? snakes)))

(define (snake-collided-self? s)
  (member (snake-head s) (snake-tail s))) 

(define (snakes-collided-each-other? snakes)
  (define (snake-collided-with-other? s)
    (member (snake-head s) (flatten (map snake-body (remove s snakes)))))
  (foldr foldor false (map snake-collided-with-other? snakes)))

(define (collision? w)
  (define snakes (game-snakes w))
  (or (foldr foldor false (map snake-collided-self? snakes))
      (snakes-collided-each-other? snakes)
      (snakes-outside-board? snakes)))
  
(define (manage-end w)
  (render-snakes (game-snakes w)))
