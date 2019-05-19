#lang racket

(provide make-score)

(define (make-score)
  (let ((score 0)
        (health 5)
        (original-health 5)
        (high-score 0))
    (define (add-score amount)
      (set! score (+ score amount)))
    (define (reset-score)
      (set! score 0))

    (define (subtract-health)
      (set! health (- health 1)))

    (define (reset-health)
      (set! health original-health))

    (define (update-highscore!)
      (when (< high-score score)
        (set! high-score score)))

    (define (score-dispatch m)
      (cond ((eq? m 'add-score) add-score)
            ((eq? m 'reset-score) reset-score)
            ((eq? m 'subtract-health) subtract-health)
            ((eq? m 'current-health) health)
            ((eq? m 'update-highscore!) update-highscore!)
            ((eq? m 'high-score) high-score)
            ((eq? m 'reset-health) reset-health)
            ((eq? m 'current-score) score)))
    score-dispatch))