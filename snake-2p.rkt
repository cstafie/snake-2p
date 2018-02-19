#lang racket
(require 2htdp/image 2htdp/universe lens)

;; STRUCTS
(struct/lens pair (x y) #:transparent)
(struct/lens game (snakes scene) #:transparent)
(struct/lens snake (direction body color length keys inputs) #:transparent)

;; CONSTANTS
(define TICK-RATE 1/5)
(define SNAKE-SIZE 10)
(define BOARD-WIDTH (* SNAKE-SIZE 80))
(define BOARD-HEIGHT (* SNAKE-SIZE 60))
(define EMPTY-SCENE (empty-scene BOARD-WIDTH BOARD-HEIGHT))
(define EMPTY-KEYS empty)
(define EMPTY-SNAKE (snake empty empty empty empty empty empty))
(define SNAKE-STARTING-LENGTH 10)

;; KEY CONSTANTS
(define PLAYER1-KEYS (list "w" "a" "s" "d"))
(define PLAYER2-KEYS (list "up" "right" "down" "left"))
(define KEYS (append PLAYER1-KEYS PLAYER2-KEYS))

;(define KEY-SETS
;  (hash
;   'wasd (list "w" "a" "s" "d")
;   'arrow-keys (list "up" "right" "down" "left")))

;;DIRECTIONS AND KEYS
(define DIRECTION-TO-VALUE
  (hash
   'up    (pair 0 -1)
   'right (pair 1 0)
   'down  (pair 0 1)
   'left  (pair -1 0)))
(define KEY-TO-DIRECTION
  (hash "up"    'up
        "right" 'right
        "down"  'down
        "left"  'left
        "w"     'up
        "d"     'right
        "s"     'down
        "a"     'left))

;; PAIR HELPERS
(define (add-pair a b)
  (pair (+ (pair-x a) (pair-x b)) (+ (pair-y a) (pair-y b))))

;; DIRECTION HELPERS
(define (opposite-directions? d1 d2)
  (equal? (add-pair
           (hash-ref DIRECTION-TO-VALUE d1)
           (hash-ref DIRECTION-TO-VALUE d2))
          (pair 0 0)))

(define (key? d) (member d KEYS))

(define (location+direction loc dir)
  (define val (hash-ref DIRECTION-TO-VALUE dir))
  (add-pair loc val))

;; SNAKE HELPERS
(define (snake-empty? s) (empty? (snake-body s)))

;; RENDER HELPERS
(define (render-square color x y scene)
  (place-image
   (square SNAKE-SIZE "solid" color)
   (* x SNAKE-SIZE)
   (* y SNAKE-SIZE)
   scene))

;; MAIN
(define (start)
  (define snakes
    (list
     (snake 'right (list (pair 10 30)) "blue" SNAKE-STARTING-LENGTH PLAYER1-KEYS)
     (snake 'left  (list (pair 70 30)) "red" SNAKE-STARTING-LENGTH PLAYER2-KEYS)
     ;;(snake 'down  (list (pair 40 10)) "yellow" SNAKE-STARTING-LENGTH PLAYER1-KEYS)
     ;;(snake 'up    (list (pair 40 50)) "green" SNAKE-STARTING-LENGTH PLAYER2-KEYS)
     ))
  (big-bang
      (game snakes EMPTY-SCENE EMPTY-KEYS)
    (on-tick update-game TICK-RATE)
    (on-key manage-inputs)
    (to-draw render-game)
    (stop-when game-over? manage-end)))

;; RENDER
(define (render-game w)
  (render-snakes (game-snakes w)))

(define (render-snakes snakes)
  (define (render-snake s scene)
    (cond [(snake-empty? s) scene]
          [else
           (define piece (first (snake-body s)))
           (render-snake
            (lens-set snake-body-lens s (rest (snake-body s)))
            (render-square (snake-color s) (pair-x piece) (pair-y piece) scene))]))
  (foldr (λ (v scene) (render-snake v scene)) EMPTY-SCENE snakes))

;; UPDATE

(define (update-game w)
  (lens-set game-snakes-lens w ((compose move-snakes direct-snakes) (game-snakes w))))

(define (direct-snakes snakes keys)
  (define (direct-snake s)
    (define current-direction (snake-direction s))
    (define relevant-keys (filter (λ (k) (member k (snake-keys s))) keys))
    (define next-direction
      (cond [(empty? relevant-keys) current-direction]
            [else (hash-ref KEY-TO-DIRECTION (first relevant-keys))]))
    (cond [(opposite-directions? current-direction next-direction) s]
          [else (lens-set snake-direction-lens s next-direction)]))

  
  (map direct-snake snakes))

(define (move-snakes snakes)
  (define (move-snake s)
    (define body (snake-body s))
    (define direction (snake-direction s))
    (define snake-len (snake-length s))
    (define new-body (cons (location+direction (first body) direction) body))
    (snake direction
           (cond [(> (length new-body) snake-len)
                  (take new-body snake-len)]
                 [else new-body])
           (snake-color s)
           snake-len
           (snake-keys s)))
  (map move-snake snakes))

;; INPUTS
  
(define (manage-inputs w key)
  (lens-set game-snakes-lens w
    (foldr
     (λ (s l) (cons (lens-set snake-inputs-lens s (append (snake-inputs s) key)) l))
     empty
     (game-snakes w))))
     
;  (cond [(key? key) (game (game-snakes w) (game-scene w) (cons key (game-keys w)))]
;        [else w]))
;;(game (direct-snakes (game-snakes w) key) empty (game-scene w)))

;; END

(define (game-over? w)
  (cond [(collision? w) #t]
        [else #f]))

(define (snake-collided-with-body? head body)
  (member head body))

(define foldor (λ (v l) (or v l)))

(define (is-outside? x a b)
  (or (< x a) (> x b)))

(define (snakes-outside-board? snakes)
  (define (snake-outside-board? s)
    (define head (first (snake-body s)))
    (or (is-outside? (* (pair-x head) SNAKE-SIZE) 0 BOARD-WIDTH)
        (is-outside? (* (pair-y head) SNAKE-SIZE) 0 BOARD-HEIGHT)))
  (foldr foldor false (map snake-outside-board? snakes)))

(define (snake-collided-self? s)
  (define body (snake-body s))
  (define head (first body))
  (define tail (rest body))
  (snake-collided-with-body? head tail))

(define (snakes-collided-each-other? snakes)
  (define (snake-collided-with-other? s)
    (define combine-snakes (λ (s1 s2) (append (snake-body s1) (snake-body s2))))
    (define other-snakes (foldr combine-snakes EMPTY-SNAKE (filter (λ (x) (not (eq? s x))) snakes)))
    (snake-collided-with-body? (first (snake-body s)) other-snakes))
  (foldr foldor false (map snake-collided-with-other? snakes)))

(define (collision? w)
  (define snakes (game-snakes w))
  (or (foldr foldor false (map snake-collided-self? snakes))
      (snakes-collided-each-other? snakes)
      (snakes-outside-board? snakes)))
  
(define (manage-end w)
  (game-scene w))
