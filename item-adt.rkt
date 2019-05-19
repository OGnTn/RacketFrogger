#lang racket
(require "position-adt.rkt")
(provide make-item)

(define (make-item x y type score)
  (let ((position (make-position x y))
        (picked-up? #f))

    (define (pick-up!) ;;Het item als picked-up flaggen.
      (set! picked-up? #t))
    
    (define (item-dispatch m)
      (cond ((eq? m 'position) position)
            ((eq? m 'type) type)
            ((eq? m 'picked-up?) picked-up?)
            ((eq? m 'pick-up!) pick-up!)
            ((eq? m 'score) score)))
    item-dispatch))