#lang racket
(#%require "draw-adt.rkt")
(provide make-position)

(define (make-position x y)

  (let ((xpos x)
        (ypos  y))
    (define (set-x! x)
      (set! xpos x))
    (define (set-y! y)
      (set! ypos y))
    (define (dispatch-pos m)
      (cond ((eq? m 'get-x) xpos)
            ((eq? m 'get-y) ypos)
            ((eq? m 'set-x!) set-x!)
            ((eq? m 'set-y!) set-y!)))
    dispatch-pos))
    