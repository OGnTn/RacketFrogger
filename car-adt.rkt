#lang racket
(require "position-adt.rkt")
(provide make-car)

(define (make-car x y direction speed starting-lane type)
  (let ((position (make-position x y))
        (original-position (make-position x y))
        (original-speed speed)
        (current-speed speed)
        (current-lane starting-lane)
        (current-state 'normal))
    (define right-car-border 22)
    (define left-car-border -2)

    (define (move-in-direction!) ;;de auto in zijn richting bewegen.
      (cond ((eq? direction 'right) ((position 'set-x!) (+ (position 'get-x) current-speed)))
            ((eq? direction 'left) ((position 'set-x!) (- (position 'get-x) current-speed)))))
    
    (define (out-of-border-check) ;kijken of de auto uit de rand is, en terugteleporteren.
      (when (> (position 'get-x) right-car-border)
        (begin ((position 'set-x!) left-car-border)))
      (when (< (position 'get-x) left-car-border)
        (begin ((position 'set-x!) right-car-border))))
    
    (define (set-speed! new-speed) ;snelheid aanpassen.
      (set! current-speed new-speed))
    
    (define (change-lane! change) ;de lane aanpassen.
      (set! current-lane (+ change current-lane)))

    (define (change-state! state) ;de staat aanpassen.
      (set! current-state state))

      (define (car-dispatch m)
        (cond ((eq? m 'move!) move-in-direction!)
              ((eq? m 'check) out-of-border-check)
              ((eq? m 'change-lane!) change-lane!)
              ((eq? m 'current-lane) current-lane)
              ((eq? m 'direction) direction)
              ((eq? m 'current-speed) current-speed)
              ((eq? m 'original-speed) original-speed)
              ((eq? m 'set-speed!) set-speed!)
              ((eq? m 'current-state) current-state)
              ((eq? m 'change-state!) change-state!)
              ((eq? m 'type) 'car)
              ((eq? m 'car-type) type)
              ((eq? m 'position) position)))
      
      car-dispatch))

      
        
  