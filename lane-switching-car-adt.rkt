#lang racket
(require "car-adt.rkt")
(provide make-lane-switching-car)

(define (make-lane-switching-car x y direction speed starting-lane)
  (let ((generic-car (make-car x y direction speed starting-lane 'lane-switching))) ;generieke auto aanmaken.
    (define lower-lane 3) ;;grenzen definieren van de lokale groep van lanes.
    (define upper-lane 1)
    
    (define (lane-switch up-down? car-list) ;de auto van lane verandern als er geen collision zal optreden.
      (if (eq? up-down? 'up)
          (when (< (generic-car 'current-lane) lower-lane)
            (when (eq? (check-for-possible-collision ((generic-car 'position) 'get-x) (- ((generic-car 'position) 'get-y) 1) car-list) #f)
              (begin (((generic-car 'position) 'set-y!) (- ((generic-car 'position) 'get-y) 1)) ((generic-car 'change-lane!) 1))))
          (when (< upper-lane (generic-car 'current-lane))
            (when (eq? (check-for-possible-collision ((generic-car 'position) 'get-x) (+ ((generic-car 'position) 'get-y) 1) car-list) #f)
              (begin (((generic-car 'position) 'set-y!) (+ ((generic-car 'position) 'get-y) 1)) ((generic-car 'change-lane!) -1))))))

    (define (check-to-switch-lanes frogger car-list) ;kijken of frogger zich op een andere lane bevindt, als dat zo is, er dichter naartoe switchen.
      (when (not (= ((frogger 'position) 'get-y) ((generic-car 'position) 'get-y)))
        (if (< ((frogger 'position) 'get-y) ((generic-car 'position) 'get-y))
            (lane-switch 'up car-list)
            (lane-switch 'down car-list))))

    (define (check-for-possible-collision x y car-list) ;kijken of de auto niet met een andere gaat botsen.
      (if (null? car-list)
          #f
          (if (and (> (round (((car car-list) 'position) 'get-x)) (- x 3)) (< (round (((car car-list) 'position) 'get-x)) (+ x 3)) (= (((car car-list) 'position) 'get-y) y))
              (car car-list)
              (check-for-possible-collision x y (cdr car-list)))))
    
    (define (lane-switching-dispatch m)
      (cond ((eq? m 'lane-switch-check) check-to-switch-lanes)
            (else (generic-car m))))

    
    lane-switching-dispatch))
