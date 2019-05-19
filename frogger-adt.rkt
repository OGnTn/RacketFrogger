#lang racket
(require "position-adt.rkt")

(provide make-frogger)

(define (make-frogger obj-list score-adt river-pos)
  
  (let ((position (make-position 10 8))
        (original-position (make-position 10 8))
        (original-pill-timer 5000)
        (vert-unit 1)
        (horz-unit 1))

 (define pill-timer 0)


    (define (subtract-time delta-time) ;;de deltatijd van de timer aftrekken.
      (set! pill-timer (- pill-timer delta-time)))

    (define (immunity) ;immuniteit door de timer op 5 sec te zetten.
      (set! pill-timer original-pill-timer))

    (define (dispatch-frogger m)
      (cond 
            ((eq? m 'type) 'frogger)
            ((eq? m 'original-position) original-position)
            ((eq? m 'position) position)
            ((eq? m 'pill-timer) pill-timer)
            ((eq? m 'time) subtract-time)
            ((eq? m 'immunity) immunity)))
    
    dispatch-frogger))

 