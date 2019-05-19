#lang racket
(require "car-adt.rkt")
(provide make-adapting-car)

(define (make-adapting-car x y direction speed starting-lane speed-multiplier)
  (let ((generic-car (make-car x y direction speed starting-lane 'adapting))) ;;generieke auto aanmaken.
    
    (define (speed-up frogger) ;;de auto versnellen als de y-waarden van frogger en de auto dezelfde zijn. De versnelling is met waarde speed-multiplier.
      (if (and (= ((frogger 'position) 'get-y) ((generic-car 'position) 'get-y))
               (eq? (generic-car 'current-state) 'normal))
          (begin ((generic-car 'set-speed!) (* (generic-car 'current-speed) speed-multiplier)) ((generic-car 'change-state!) 'sped-up))
          (when (eq? (generic-car 'current-state) 'sped-up)
            (begin ((generic-car 'set-speed!) (generic-car 'original-speed)) ((generic-car 'change-state!) 'normal)))))

    (define (adapting-car-dispatch m)
      (cond ((eq? m 'speed-up) speed-up)
            (else (generic-car m))))
    adapting-car-dispatch))
          
