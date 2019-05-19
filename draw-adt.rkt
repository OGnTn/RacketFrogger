#lang Racket
(require "Graphics.rkt")

(provide make-draw)



;; Teken-ADT
(define (make-draw title pwidth pheight horizontal-tiles vertical-tiles)

  (define window (make-window pwidth pheight title))

  ((window 'set-background!) "black")

  (define (close)
    ((window 'close)))

  (define horizontal-unit (/ pwidth horizontal-tiles))
  (define vertical-unit (/ pheight vertical-tiles))

  ;lagen aanmaken.
  
  (define highway-layer (window 'make-layer))
  (define berm-layer (window 'make-layer))
  (define object-layer (window 'make-layer))
  (define score-layer (window 'make-layer))
  (define frogger-layer (window 'make-layer))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;USER INTERFACE.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define play-button 0)
  (define quit-button 0)
  
  (define (set-button! index)
    ((play-button 'set-current!) index))

  (define (set-quit-button! index)
    ((quit-button 'set-current!) index))
  ;;de tiles voor de verschillende knoppen aan maken.

  (define (make-quit-button) 
    (define quit-button-no-highlight (make-bitmap-tile "./Sprites/quit_button_highlight.png" "./Sprites/play_button_mask.png"))
    ((quit-button-no-highlight 'set-x!) 512)
    ((quit-button-no-highlight 'set-y!) 350)
    (define play-button-highlight (make-bitmap-tile "./Sprites/quit_button_unhighlight.png" "./Sprites/play_button_mask.png"))
    ((quit-button-no-highlight 'set-x!) 512)
    ((quit-button-no-highlight 'set-y!) 350)
    (define quit-button-seq (make-tile-sequence (list quit-button-no-highlight play-button-highlight)))
    ((score-layer 'add-drawable) quit-button-seq)
    ((quit-button-seq 'set-x!) 512)
    ((quit-button-seq 'set-y!) 350)
    ((quit-button-seq 'set-current!) 1)
    (set! quit-button quit-button-seq))

  (define (remove-quit-button)
    ((score-layer 'remove-drawable) quit-button))

    
  (define (make-play-button)
    (define play-button-no-highlight (make-bitmap-tile "./Sprites/play_button_nohighlight.png" "./Sprites/play_button_mask.png"))
    ((play-button-no-highlight 'set-x!) 512)
    ((play-button-no-highlight 'set-y!) 200)
    (define play-button-highlight (make-bitmap-tile "./Sprites/play_button_highlight.png" "./Sprites/play_button_mask.png"))
    ((play-button-highlight 'set-x!) 512)
    ((play-button-highlight 'set-y!) 200)
    (define play-button-tile-seq (make-tile-sequence (list play-button-no-highlight play-button-highlight)))
    ((score-layer 'add-drawable) play-button-tile-seq)
    ((play-button-tile-seq 'set-x!) 512)
    ((play-button-tile-seq 'set-y!) 200)
    (set! play-button play-button-tile-seq))
  
  (define (remove-play-button)
    ((score-layer 'remove-drawable) play-button))
  (define score-tile 0)
  (define health-tile 0)
  (define highscore-tile 0)
  (define (make-score-tile)
    (set! score-tile (make-tile 640 360))
    ((score-layer 'add-drawable) score-tile))
  
  (define (make-health-tile) ;tile van de levens aanmaken
    (set! health-tile (make-tile 640 60))
    ((score-layer 'add-drawable) health-tile))
  (define (make-highscore-tile)
    (set! highscore-tile (make-tile 640 60))
    ((score-layer 'add-drawable) highscore-tile))

  (define (draw-highscore highscore) ;;highscore op het scherm tekenen.
    (when (not (eq? 0 highscore-tile))
      (highscore-tile 'clear)
      ((highscore-tile 'set-x!) 10)
      ((highscore-tile 'set-y!) 10)
      ((highscore-tile 'draw-text) (string-append "high-score: " highscore) 20 0 0 "red")))
  
  (define (draw-health health) ;levens op het scherm tekenen.
    (health-tile 'clear)
    ((health-tile 'set-x!) 1125)
    ((health-tile 'set-y!) 1)
    ((health-tile 'draw-text) (string-append "lives left: " health) 20 0 0 "red"))

  (define (unshow-highscore) ;highscore verstoppen.
    (when (not (eq? 0 highscore-tile))
      ((score-layer 'remove-drawable) highscore-tile)))

  (define (unshow-score) ;;score verstoppen.
    
    (when (not (eq? 0 score-tile))
      ((score-tile 'set-x!) 1000)
      ((score-tile 'set-y!) 1000)
      ((score-tile 'draw-text) (number->string 0) 20 0 0 "red")
      ((score-layer 'remove-drawable) score-tile))
    (when (not (eq? 0 health-tile))
      ((score-layer 'remove-drawable) health-tile)))
  
  
  (define (draw-score score) ;;de score tekenen.

    (score-tile 'clear)
    ((score-tile 'set-x!) 10)
    ((score-tile 'set-y!) 1)
    ((score-tile 'draw-text) (string-append "score: " score) 20 0 0 "red"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;object-tiles aanmaken.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-pill-tile x y)
    (define pill-tile (make-bitmap-tile "./Sprites/pill.png" "./Sprites/pill_mask.png"))
    ((object-layer 'add-drawable) pill-tile)
    ((pill-tile 'set-x!) (* x horizontal-unit))
    ((pill-tile 'set-y!) (* y vertical-unit))
    pill-tile)
  
  (define (make-coin-tile x y)
    (define coin-tile (make-bitmap-tile "./Sprites/coin.png" "./Sprites/coin_mask.png"))
    ((object-layer 'add-drawable) coin-tile)
    ((coin-tile 'set-x!) (* x horizontal-unit))
    ((coin-tile 'set-y!) (* y vertical-unit))
    coin-tile)
  
  (define (make-insect-tile adt)
    (define insect-tile (cond ((eq? (adt 'score) 30) (make-bitmap-tile "./Sprites/insect_30.png" "./Sprites/insect1_mask.png"))
                              ((eq? (adt 'score) 40) (make-bitmap-tile "./Sprites/insect_40.png" "./Sprites/insect1_mask.png"))
                              ((eq? (adt 'score) 50) (make-bitmap-tile "./Sprites/insect_50.png" "./Sprites/insect1_mask.png"))
                              ((eq? (adt 'score) 60) (make-bitmap-tile "./Sprites/insect_60.png" "./Sprites/insect1_mask.png"))))
    ((object-layer 'add-drawable) insect-tile)
    ((insect-tile 'set-x!) (* ((adt 'position) 'get-x) horizontal-unit))
    ((insect-tile 'set-y!) (* ((adt 'position) 'get-y) vertical-unit))
    insect-tile)

  (define (make-bush-tile x y)
    (define bush-tile (make-bitmap-tile "./Sprites/bush_new.png" "./Sprites/bush_new_mask.png"))
    ((object-layer 'add-drawable) bush-tile)
    ((bush-tile 'set-x!) (* x horizontal-unit))
    ((bush-tile 'set-y!) (* y vertical-unit))
    bush-tile)

  (define (make-log-tile x y)
    (define log-tile (make-bitmap-tile "./Sprites/log.png" "./Sprites/log_mask.png"))
    ((object-layer 'add-drawable) log-tile)
    ((log-tile 'set-x!) (* x horizontal-unit))
    ((log-tile 'set-y!) (* y vertical-unit))
    log-tile)

  (define (make-car-tile x y car-adt)
    ;    (cond ((eq? (car-adt 'car-type) 'regular) (define car-tile (make-bitmap-tile "./Sprites/car_new.png" "./Sprites/car_new_mask.png")))
    ;          (else (define car-tile (make-bitmap-tile "./Sprites/lane-switching-car_new.png" "./Sprites/car_new_mask.png"))))
    (let ((car-tile (cond ((eq? (car-adt 'car-type) 'regular) (make-bitmap-tile "./Sprites/car_new.png" "./Sprites/car_new_mask.png"))
                          ((eq? (car-adt 'car-type) 'adapting) (make-bitmap-tile "./Sprites/adapting-car_new.png" "./Sprites/car_new_mask.png"))
                          ((eq? (car-adt 'car-type) 'naive) (make-bitmap-tile "./Sprites/naive-car_new.png" "./Sprites/car_new_mask.png"))
                          (else (make-bitmap-tile "./Sprites/lane-switching-car_new.png" "./Sprites/car_new_mask.png")))))
      
      ((object-layer 'add-drawable) car-tile)
      ((car-tile 'set-x!) (* x horizontal-unit))
      ((car-tile 'set-y!) (* y vertical-unit))
      car-tile))
  

  

  (define frogger-seq 0)
  (define (make-frogger-tile x y)
    (define frogger-tile  (make-bitmap-tile "./Sprites/Frogger.png" "./Sprites/Frogger_Mask.png"))
    (define frogger-high-tile (make-bitmap-tile "./Sprites/Frogger_high.png" "./Sprites/Frogger_Mask.png"))
    (define frogger-tile-seq (make-tile-sequence (list frogger-tile frogger-high-tile)))
    ((frogger-layer 'add-drawable) frogger-tile-seq)
    ((frogger-tile-seq 'set-x!) (* x horizontal-unit))
    ((frogger-tile-seq 'set-y!) (* y vertical-unit))
    (set! frogger-seq frogger-tile-seq)
    frogger-tile-seq)

  (define (set-frogger-high!) ;;procedures om frogger van kleur te doen veranderen.
    ((frogger-seq 'set-current!) 1))
  (define (set-frogger-normal!)
    ((frogger-seq 'set-current!) 0))


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;Procedures om de layout van de levels te maken.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define bermen '())
 
  (define (make-bermen)
    (define first-berm (make-bitmap-tile "./Sprites/middenberm_new.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) first-berm)
    ((first-berm 'set-x!) (* 0 horizontal-unit))
    ((first-berm 'set-y!) (* 8 vertical-unit))

    (define last-berm (make-bitmap-tile "./Sprites/middenberm_new.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) last-berm)
    ((last-berm 'set-x!) (* 0 horizontal-unit))
    ((last-berm 'set-y!) (* 0 vertical-unit))
    (define midden-berm (make-bitmap-tile "./Sprites/middenberm_new.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) midden-berm)
    ((midden-berm 'set-x!) (* 0 horizontal-unit))
    ((midden-berm 'set-y!) (* 4 vertical-unit))
    
    (set! bermen (list first-berm midden-berm last-berm))
    )

  (define (remove-bermen)
    (let remove ((berm-list bermen))
      (if (null? berm-list)
          'done
          (let ((current-berm (car berm-list)))
            ((berm-layer 'remove-drawable) current-berm)
            (remove (cdr berm-list))))))

  
  
  
  (define (make-bermen-river)
    (define first-berm (make-bitmap-tile "./Sprites/middenberm_new.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) first-berm)
    ((first-berm 'set-x!) (* 0 horizontal-unit))
    ((first-berm 'set-y!) (* 8 vertical-unit))

    (define last-berm (make-bitmap-tile "./Sprites/middenberm_new.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) last-berm)
    ((last-berm 'set-x!) (* 0 horizontal-unit))
    ((last-berm 'set-y!) (* 0 vertical-unit))
    (define midden-berm (make-bitmap-tile "./Sprites/river.png" "./Sprites/middenberm_mask.png"))
    ((berm-layer 'add-drawable) midden-berm)
    ((midden-berm 'set-x!) (* 0 horizontal-unit))
    ((midden-berm 'set-y!) (* 4 vertical-unit))
    
    (set! bermen (list first-berm midden-berm last-berm)))

  (define first-highway 0)
  (define last-highway 0)
  
  (define (make-lanes)
    (set! first-highway (make-bitmap-tile "./Sprites/weg.png" "./Sprites/weg_mask.png"))
    ((highway-layer 'add-drawable) first-highway)
    ((first-highway 'set-y!) (* 5 vertical-unit))
  
    (set! last-highway (make-bitmap-tile "./Sprites/weg.png" "./Sprites/weg_mask.png"))
    ((highway-layer 'add-drawable) last-highway)
    ((last-highway 'set-y!) (* 1 vertical-unit)))
  
  (define (remove-lanes)
    
    (when (and (not (eq? first-highway 0)) (not (eq? last-highway 0)))
      ((highway-layer 'remove-drawable) first-highway)
      ((highway-layer 'remove-drawable) last-highway)))
  

  (define (set-game-loop-function! fun)
    ((window 'set-update-callback!) fun))
  (define (set-key-function! fun)
    ((window 'set-key-callback!) fun))

  
  (define (draw! adt) ;;algemene tekenfunctie
    (let ((new-x (* horizontal-unit ((adt 'position) 'get-x)))
          (new-y (* vertical-unit ((adt 'position) 'get-y))))
      (((cdr (assoc adt item-assoc)) 'set-x!) new-x)
      (((cdr (assoc adt item-assoc)) 'set-y!) new-y)))

  (define (make-adt-tile adt) ;procedure om de juiste tile voor het object aan te maken.
    (cond ((eq? (adt 'type) 'coin) (make-coin-tile ((adt 'position) 'get-x)
                                                   ((adt 'position) 'get-y)))
          ((eq? (adt 'type) 'insect) (make-insect-tile adt))
          ((eq? (adt 'type) 'bush) (make-bush-tile ((adt 'position) 'get-x)
                                                   ((adt 'position) 'get-y)))
          ((eq? (adt 'type) 'pill) (make-pill-tile ((adt 'position) 'get-x)
                                                   ((adt 'position) 'get-y)))
          ((eq? (adt 'type) 'car) (make-car-tile ((adt 'position) 'get-x)
                                                 ((adt 'position) 'get-y)
                                                 adt))
          ((eq? (adt 'type) 'log) (make-log-tile ((adt 'position) 'get-x)
                                                 ((adt 'position) 'get-y)))
          ((eq? (adt 'type) 'frogger) (make-frogger-tile ((adt 'position) 'get-x)
                                                         ((adt 'position) 'get-y)))))
  
  (define item-assoc '())
  (define (make-item-assoc itemlist) ;;de associatielijst opbouwen van de juiste tiles met de juist objecten.
    
    (if (null? itemlist)
        'done
        (begin (set! item-assoc (cons
                                 (cons (car itemlist)
                                       (make-adt-tile (car itemlist)))
                                 item-assoc))
                 
               (make-item-assoc (cdr itemlist)))))

  (define (remove-all-drawables!) ;;alle objecten verwijderen.
    (let iter ((assoc item-assoc))
      
      (if (null? assoc)
          (set! item-assoc '())
          (let ((current-drawable (car assoc)))
            (if (eq? ((car current-drawable) 'type) 'frogger)
                ((frogger-layer 'remove-drawable) (cdr current-drawable))
                ((object-layer 'remove-drawable) (cdr current-drawable)))
            (iter (cdr assoc))))))
    
  ;;;;;;;;;;;;;;
  ;; Dispatch ;;
  ;;;;;;;;;;;;;;
  
  (define (dispatch-draw-adt msg)
    (cond ((eq? msg 'set-game-loop-function!) set-game-loop-function!)
          ((eq? msg 'set-key-function!) set-key-function!)
          ((eq? msg 'close) close)
          ;; Teken functies.
          ((eq? msg 'draw!) draw!)
          ((eq? msg 'set-high!) set-frogger-high!)
          ((eq? msg 'set-normal!) set-frogger-normal!)
          ((eq? msg 'make-bermen) make-bermen)
          ((eq? msg 'make-bermen-river) make-bermen-river)
          ((eq? msg 'remove-bermen) remove-bermen)
          ((eq? msg 'remove-lanes) remove-lanes)
          ((eq? msg 'make-lanes) make-lanes)
          ((eq? msg 'draw-score) draw-score)
          ((eq? msg 'draw-health) draw-health)
          ((eq? msg 'make-highscore-tile) make-highscore-tile)
          ((eq? msg 'draw-highscore) draw-highscore)
          ((eq? msg 'unshow-highscore) unshow-highscore)
          ((eq? msg 'unshow-score) unshow-score)
          ((eq? msg 'make-score-tile) make-score-tile)
          ((eq? msg 'make-health-tile) make-health-tile)
          ((eq? msg 'make-play-button) make-play-button)
          ((eq? msg 'make-quit-button) make-quit-button)
          ((eq? msg 'remove-play-button) remove-play-button)
          ((eq? msg 'remove-quit-button) remove-quit-button)
          ((eq? msg 'set-button!) set-button!)
          ((eq? msg 'set-quit-button!) set-quit-button!)
          ((eq? msg 'add-to-assoc) make-item-assoc)
          ((eq? msg 'remove-drawables!) remove-all-drawables!)
          ((eq? msg 'assoc-list) item-assoc)
          ;; Functies die interne waarden blootgeven.
          ((eq? msg 'canvas-h) pheight)
          ((eq? msg 'canvas-w) pwidth)
          ((eq? msg 'horz-unit) horizontal-unit)
          ((eq? msg 'vert-unit) vertical-unit)))
  dispatch-draw-adt)