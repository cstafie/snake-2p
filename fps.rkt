
(module fps racket (provide add-fps get-fps)
  (require 2htdp/image)

  (define last-update 0)
  (define update-rate 10) ; if set to 1 it will update every frame, 2 every 2 frames
  (define update-count 0)
  (define old-fps 0)

  (define (get-fps)
    (set! update-count (add1 update-count))
    (cond [(not (equal? update-count update-rate)) old-fps]
          [else
           (let* ([current (current-inexact-milliseconds)]
                  [fps (* (/ 1000 (- current last-update)) update-rate)])
             (set! last-update current)
             (set! update-count 0)
             (set! old-fps fps)
             fps)]))

  (define (add-fps s)
    (cond [(zero? last-update)
           (get-fps)
           s]
          [else (place-image/align
                 (text (real->decimal-string (get-fps) 0) 10 "black")
                 1 1 "left" "top" s)]))
)