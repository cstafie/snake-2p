#lang racket

(struct pair [x y] #:transparent)

(define (build-table size)
  (define (build-pairs x)
    (build-list size (λ (y) (pair x y))))
  (build-list size build-pairs))
                   