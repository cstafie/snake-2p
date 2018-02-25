#lang racket
(provide add-fps get-fps)

(require 2htdp/image)

(define last-update 0) 

(define (get-fps)
  (let* ([current (current-inexact-milliseconds)]
         [fps (/ 1000 (- current last-update))])
    (set! last-update current)
    fps))

(define (add-fps s)
  (cond [(zero? last-update)
          (get-fps)
          s]
         [else (place-image/align
                (text (real->decimal-string (get-fps) 0) 10 "black")
                1 1 "left" "top" s)]))