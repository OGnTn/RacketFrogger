#lang racket
(require "car-adt.rkt")
(provide make-naive-car)

(define (make-naive-car x y direction speed starting-lane speed-multiplier)
  (let ((generic-car (make-car x y direction speed starting-lane 'naive))) ;;generieke auto aanmaken.
    (define change-timer 2000)
    (define original-change-timer change-timer)
    
    (define (subtract-time delta-time) ;tijd van de timer aftrekken.
      (set! change-timer (- change-timer delta-time)))
    
    (define (random-speed-change) ;willekeurig van snelheid veranderen als de timer onder 0 is.
      (when (and (< change-timer 0)
                 (eq? (generic-car 'current-state) 'normal))
        (begin (if (> (random 2) 0)
                   ((generic-car 'set-speed!) (* (generic-car 'original-speed) speed-multiplier))
                   ((generic-car 'set-speed!) (- (generic-car 'original-speed) (- (* (generic-car 'original-speed)
                                                                                     speed-multiplier)
                                                                                  (generic-car 'original-speed)))))
               (set! change-timer original-change-timer))))
     (define (naive-dispatch m)
       (cond ((eq? m 'random-change) random-speed-change)
             ((eq? m 'subtract-time) subtract-time)
             (else (generic-car m))))
    naive-dispatch))
      