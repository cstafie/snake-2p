#lang racket

(require 2htdp/image 2htdp/universe)

;; STRUCTS

(struct pair (x y) #:transparent)
(struct game (snakes power-up-manger scene keys) #:transparent)
;;(struct player (snake keys color))
(struct snake (direction body color length keys) #:transparent)
;;(struct power-up-manager (timer-range max-power-ups power-ups) #:transparent)
;;(struct poker-up (name location) #:transparent)
;;(struct fps-counter (frames last-time) #:transparent) ;curious about building an fps counter

;; CONSTANTS

(define TICK-RATE 1/20)
(define SNAKE-SIZE 10)
;;(define POWER-UP-TIMER-RANGE (pair 5 15))
;;(define MAX-POWER-UPS 3)
(define BOARD-WIDTH (* SNAKE-SIZE 80))
(define BOARD-HEIGHT (* SNAKE-SIZE 60))
(define EMPTY-SCENE (empty-scene BOARD-WIDTH BOARD-HEIGHT))
(define SNAKE-STARTING-LENGTH 100)

(define PLAYER1-KEYS (list "w" "a" "s" "d"))
(define PLAYER2-KEYS (list "up" "right" "down" "left"))
(define KEYS (append PLAYER1-KEYS PLAYER2-KEYS))
(define DIRECTION-TO-VALUE (list (pair 'up (pair 0 -1))
                                 (pair 'right (pair 1 0))
                                 (pair 'down (pair 0 1))
                                 (pair 'left (pair -1 0))))
(define KEY-TO-DIRECTION (list (pair "up" 'up)
                               (pair "right" 'right)
                               (pair "down" 'down)
                               (pair "left" 'left)
                               (pair "w" 'up)
                               (pair "d" 'right)
                               (pair "s" 'down)
                               (pair "a" 'left)))
(define OPPOSITE-DIRECTION (list (pair 'up 'down)
                                 (pair 'down 'up)
                                 (pair 'left 'right)
                                 (pair 'right 'left)))

(define EMPTY-SNAKE (snake empty empty empty empty empty))

;; HELPERS

(define (get-val key dict)
  (pair-y (first (filter (λ (x) (equal? key (pair-x x))) dict))))

;; DIRECTION HELPERS

(define (opposite-direction? d1 d2)
  (equal? (get-val d1 OPPOSITE-DIRECTION) d2))

(define (direction? d)
  (member d KEYS))

(define (location+direction loc dir)
  (define val (get-val dir DIRECTION-TO-VALUE))
  (pair (+ (pair-x loc) (pair-x val))
        (+ (pair-y loc) (pair-y val))))

;; MAIN

(define (start)
  (define p1-snake (snake 'right (list (pair 10 30)) "blue" SNAKE-STARTING-LENGTH PLAYER1-KEYS))
  (define p2-snake (snake 'left  (list (pair 70 30)) "red" SNAKE-STARTING-LENGTH PLAYER2-KEYS))
  ;; (define p1
  ;;(define p3-snake (snake 'down  (list (pair 40 10)) 'none "yellow" SNAKE-STARTING-LENGTH))
  ;;(define p4-snake (snake 'up  (list (pair 40 50)) 'none "green" SNAKE-STARTING-LENGTH))
  ;;(define pum (power-up-manager POWER-UP-TIMER-RANGE MAX-POWER-UPS '()))
  
  (big-bang
    (game (list p1-snake p2-snake) empty EMPTY-SCENE empty)
    (on-tick update-game TICK-RATE)
    (on-key manage-inputs)
    (to-draw render-game)
    (stop-when game-over? manage-end)))

;; RENDER

(define (render-game w)
  (render-snakes (game-snakes w) (game-scene w)))

(define (render-snakes snakes scene)
  (define (render-snake snake)
    (define (render-snake-piece piece)
      (set! scene (place-image
                   (square SNAKE-SIZE "solid" (snake-color snake))
                   (* (pair-x piece) SNAKE-SIZE)
                   (* (pair-y piece) SNAKE-SIZE)
                   scene)))
    ;(println (snake-body snake))
    (map render-snake-piece (snake-body snake)))
  (map render-snake snakes)
  scene)

;; UPDATE

(define (update-game w)
  (game
   (move-snakes (direct-snakes (game-snakes w) (game-keys w)))
   empty
   (game-scene w)
   empty))

(define (direct-snakes snakes keys)
  (define (direct-snake s)
    (define current-direction (snake-direction s))
    (define relevant-keys (filter (λ (k) (member k (snake-keys s))) keys))
    (define next-direction
      (cond [(empty? relevant-keys) current-direction]
            [else (get-val (last relevant-keys) KEY-TO-DIRECTION)]))
    (cond [(opposite-direction? current-direction next-direction) s]
          [else (snake next-direction (snake-body s) (snake-color s) (snake-length s) (snake-keys s))]))
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
  (cond [(direction? key) (game (game-snakes w) empty (game-scene w) (cons key (game-keys w)))]
        [else w]))
  ;;(game (direct-snakes (game-snakes w) key) empty (game-scene w)))

;; END

(define (game-over? w)
  (cond [(collision? w) #t]
        [else #f]))

(define (snake-collided-with-body? head body)
  (member head body))

(define foldor (λ (v l) (or v l)))

(define (snake-collided-self? s)
  (define body (snake-body s))
  (define head (first body))
  (define tail (rest body))
  (cond [(empty? tail) #f]
        [else (snake-collided-with-body? head tail)]))

(define (snakes-collided-each-other? snakes)
  (define (snake-collided-with-other? s)
    (define combine-snakes (λ (s1 s2) (append (snake-body s1) (snake-body s2))))
    (define other-snakes (foldr combine-snakes EMPTY-SNAKE (filter (λ (x) (not (eq? s x))) snakes)))
    (snake-collided-with-body? (first (snake-body s)) other-snakes))
  (foldr foldor false (map snake-collided-with-other? snakes)))

(define (collision? w)
  (define snakes (game-snakes w))
  (or (foldr foldor false (map snake-collided-self? snakes)))
      (snakes-collided-each-other? snakes))
  
(define (manage-end w)
  (game-scene w))
