#lang Racket
(require "Graphics.rkt")
(require "draw-adt.rkt")
(require "frogger-adt.rkt")
(require "car-adt.rkt")
(require "score.rkt")
(require "item-adt.rkt")
(require "lane-switching-car-adt.rkt")
(require "adapting-car-adt.rkt")
(require "naive-car-adt.rkt")

(define window-px-height 720)
(define window-px-width 1280)
(define left-border 0)
(define right-border 19)
(define upper-border 0)
(define lower-border 8)



(define (make-game-adt)
  (define draw-adt (make-draw "Frogger" window-px-width window-px-height 20 9)) ;Draw adt aanmaken
  (define vert-unit 1)
  (define horz-unit 1)
  (define current-level 0)
  (define level 1)
  (define item-list '())
  (define car-list '())
  (define current-coin-list '())
  (define frogger-adt 0)
  (define river-pos -1)
  (define score-adt 0)
  
  (define (start)   ;het spel starten
    (set! score-adt (make-score))
    (set-next-level!))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;Logica van Frogger.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (check-coin-list) ;kijken of alle coins opgeraapt zijn.
    (if (not (null? current-coin-list))
        (let check-coin-list-let ((coin-list current-coin-list))
          (if (null? coin-list)
              #t
              (and ((car coin-list) 'picked-up?) (check-coin-list-let (cdr coin-list)))))
        #f))

  (define (check-for-level-complete) ;een volgend level starten als alle coins opgeraapt zijn
    (when (check-coin-list)
      
      (set! level (+ level 1))
      ((draw-adt 'remove-lanes))
      (set-next-level!)))

  (define (check-for-game-over) ;kijken of de levens van frogger op zijn, als dit zo is terug naar het main menu.
    (when (= (score-adt 'current-health) 0)
      (begin ((draw-adt 'remove-drawables!))(draw-adt 'remove-bermen) ((draw-adt 'remove-lanes)) ((draw-adt 'remove-bermen)) ((score-adt 'reset-health))(set! level 1) (set-next-level!))))

  (define has-changed-colour #t)
  
  (define (check-if-high) ;kijken of de kleur van frogger terug op normaal gezet moet worden.
    (when (and (< (frogger-adt 'pill-timer) 0) (not has-changed-colour))
      (begin ((draw-adt 'set-normal!)) ;er wordt gebruikt gemaakt van de bool has-changed-colour om zeker te zijn dat dit maar 1 maal wordt opgeroepen.
             (set! has-changed-colour #t))))

  (define (move! direction) ;move-functie met collision van frogger.
    (let ((original-x ((frogger-adt 'position) 'get-x)) ; bijhouden vanwaar frogger kwam
          (original-y ((frogger-adt 'position) 'get-y)))
      (actual-move direction)
      (let ((out-of-border (or (< ((frogger-adt 'position) 'get-x) left-border) ;kijken of frogger uit de rand is
                               (> ((frogger-adt 'position) 'get-x) right-border)
                               (< ((frogger-adt 'position) 'get-y) upper-border)
                               (> ((frogger-adt 'position) 'get-y) lower-border))))
        (if out-of-border
            (begin (((frogger-adt 'position) 'set-x!) original-x)
                   (((frogger-adt 'position) 'set-y!) original-y))
            (begin
              
              (let ((collision (check-list item-list ((frogger-adt 'position) 'get-x) ((frogger-adt 'position) 'get-y)))) ;het adt van de collision wordt in de let bijgehouden
                (if (eq? ((frogger-adt 'position) 'get-y) river-pos) ;kijken of frogger zich op de rivier bevindt, en of hij op een log staat.
                    (if (eq? #f collision)
                        (fell-in-river)
                        (when (not (eq? (collision 'type) 'log))
                          (fell-in-river)))
                    (when (not (eq? #f collision))
                      (if (eq? 'bush (collision 'type)) ;als hij collide met een bush moet hij terugteleporteren.
                          (begin (((frogger-adt 'position) 'set-x!) original-x)
                                 (((frogger-adt 'position) 'set-y!) original-y))
                          (cond ((eq? (collision 'type) 'pill) ;als hij een pil opneemt moet immunity geactiveerd worden.
                                 (begin ((frogger-adt 'immunity))
                                        ((draw-adt 'set-high!))
                                        (set! has-changed-colour #f)
                                        ((score-adt 'add-score) (collision 'score))
                                        (((collision 'position) 'set-x!) -1000)
                                        (((collision 'position) 'set-y!) -1000)
                                        ((collision 'pick-up!))
                                        ((draw-adt 'draw!) collision)))
                                ((eq? (collision 'type) 'log) 'ok)
                                (else 
                                 (begin ((score-adt 'add-score) (collision 'score)) ; bij elke andere collision wordt het object opgenomen en de score bijgeteld.
                                        (((collision 'position) 'set-x!) -1000)
                                        (((collision 'position) 'set-y!) -1000)
                                        ((collision 'pick-up!))
                                        ((draw-adt 'draw!) collision)))))))))))))

  (define (check-list obj-list x-pos y-pos) ; recursieve procedure om collision te checken.
  
    (if (null? obj-list)
        #f
        (if (collision? (car obj-list) x-pos y-pos)
            (collision? (car obj-list) x-pos y-pos)
            (check-list (cdr obj-list) x-pos y-pos))))
  
  (define (collision? object x-pos y-pos)
    (if (and
         (= (round ((object 'position) 'get-x)) x-pos)
         (= (round ((object 'position) 'get-y)) y-pos))
        object
        #f))

  (define (actual-move direction) ; functie die frogger werkelijk gaat verplaatsen.
    (cond ((eq? direction 'up) (((frogger-adt 'position) 'set-y!) (- ((frogger-adt 'position) 'get-y) vert-unit))) 
          ((eq? direction 'down) (((frogger-adt 'position) 'set-y!) (+ ((frogger-adt 'position) 'get-y) vert-unit)))
          ((eq? direction 'left) (((frogger-adt 'position) 'set-x!) (- ((frogger-adt 'position) 'get-x) horz-unit)))
          ((eq? direction 'right) (((frogger-adt 'position) 'set-x!) (+ ((frogger-adt 'position) 'get-x) horz-unit)))))
    
  (define (reset) ;frogger resetten als hij doodgaat.
    (when (< (frogger-adt 'pill-timer) 0)
      (fell-in-river)))
  
  (define (fell-in-river)
    (((frogger-adt 'position) 'set-x!) ((frogger-adt 'original-position) 'get-x))
    (((frogger-adt 'position) 'set-y!) ((frogger-adt 'original-position) 'get-y))
    ((score-adt 'subtract-health))
    ((score-adt 'update-highscore!))
    ((score-adt 'reset-score)))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;Logica van de auto's.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      

  (define (check-car-list list delta-time) ;de speciefieke functies die elke frame op auto's worden uitgevoerd. 
    (when (not (null? list))
      (begin (car-collision? (car list)) ;kijken of frogger een auto aanraakt
             ((draw-adt 'draw!) (car list))
             (((car list) 'move!))
             (((car list) 'check)) ; de out-of-border-check van auto's
             (cond ((eq? ((car list) 'car-type) 'lane-switching) (((car list) 'lane-switch-check) frogger-adt car-list))
                   ((eq? ((car list) 'car-type) 'naive) (begin (((car list) 'random-change)) (((car list) 'subtract-time) delta-time)))
                   ((eq? ((car list) 'car-type) 'adapting) (((car list) 'speed-up) frogger-adt)))
             (check-car-list (cdr list) delta-time))))
  
  
  (define (car-collision? car-adt)
    (when (and
           (= (round ((car-adt 'position) 'get-x)) (round ((frogger-adt 'position) 'get-x)))
           (= (round ((car-adt 'position) 'get-y)) (round ((frogger-adt 'position) 'get-y))))
      (begin (reset))))
  
  (define (car-slow car-list1 car-list2) ;procedure die kijkt of een auto moet vertragen voor een andere auto.
    (define (check-individual car-list1 car-list2)
      (when (not (null? car-list1))
        (begin (recursive-check (car car-list1) car-list2) (check-individual (cdr car-list1) car-list2))))
    (define (recursive-check current-car car-list2)
      (when (not (null? car-list2))
        (begin (when (not (eq? current-car (car car-list2)))
                 (when (= ((current-car 'position) 'get-y) (((car car-list2) 'position) 'get-y))
                   (if (eq? (current-car 'direction) 'left)
                       (if (and (< (abs (- (abs (round ((current-car 'position) 'get-x))) (abs (round (((car car-list2) 'position) 'get-x))))) 5)
                                (>= (current-car 'current-speed) ((car car-list2) 'current-speed))
                                (> ((current-car 'position) 'get-x) (((car car-list2) 'position) 'get-x)))
                           (begin ((current-car 'set-speed!) ((car car-list2) 'current-speed)) ((current-car 'change-state!) 'slowed))
                           (when (< 5 ((current-car 'position) 'get-x)) (begin ((current-car 'change-state!) 'normal) ((current-car 'set-speed!) (current-car 'original-speed))))) 
                       (if (and (< (- (abs(round ((current-car 'position) 'get-x))) (abs (round (((car car-list2) 'position) 'get-x)))) 5)
                                (>= (current-car 'current-speed) ((car car-list2) 'current-speed))
                                (< ((current-car 'position) 'get-x) (((car car-list2) 'position) 'get-x)))
                           (begin ((current-car 'set-speed!) ((car car-list2) 'current-speed)) ((current-car 'change-state!) 'slowed)) 
                           (when (< 15 ((current-car 'position) 'get-x)) (begin ((current-car 'set-speed!) (current-car 'original-speed)) ((current-car 'change-state!) 'slowed)))))))
               (recursive-check current-car (cdr car-list2)))))
    (check-individual car-list1 car-list2))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;Input en game-loop procedures.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
       
  (define (my-game-loop-function delta-time) ;de gameloop die tijdens de levels uitgevoerd wordt.
    (check-if-high)
    (check-for-level-complete)
    (check-for-game-over)
    ((draw-adt 'draw!) frogger-adt)
    (when (not (eq? 0 score-adt))
      (begin ((draw-adt 'draw-score) (number->string (score-adt 'current-score))) ((draw-adt 'draw-health) (number->string(score-adt 'current-health)))))
    (check-car-list car-list delta-time)
    (car-slow car-list car-list)
    ((frogger-adt 'time) delta-time))

  (define (return delta-time) ;deze gameloop doet niets en wordt tijdens het main-menu uitgevoerd.
    'ok)
  


  (define (input pressed? key) ;functie om de input te krijgen tijdens de levels.
    
    (when (eq? pressed? 'down)
      (cond ((eq? key 'up) (move! 'up))
            ((eq? key 'down) (move! 'down))
            ((eq? key 'left) (move! 'left))
            ((eq? key 'right) (move! 'right))
            ((eq? key 'shift) ((draw-adt 'remove-drawables!)))
            (else (error "Not a valid input")))))
  
  (define index 0)
  (define (input-main-menu pressed? key) ;functie om de input te krijgen tijdens het main menu. De play en quit buttons worden afhankelijk van de input gehighlight.
    (when (eq? pressed? 'down)
      (cond ((eq? key 'up) (begin (set! index 0) ((draw-adt 'set-button!) index) ((draw-adt 'set-quit-button!) 1)))
            ((eq? key 'down) (begin (set! index 1)((draw-adt 'set-button!) index)((draw-adt 'set-quit-button!) 0)))
            ((eq? key #\return) (if (= index 0)
                                  (begin ((draw-adt 'remove-play-button))
                                         ((draw-adt 'remove-quit-button))
                                         (set! level (+ level 1))
                                         (set-next-level!)
                                         ((draw-adt 'set-key-function!) input)
                                         ((draw-adt 'set-game-loop-function!) my-game-loop-function))
                                  ((draw-adt 'close)))))))
  
  
  
  

  

  (define (set-next-level!) ;het volgende level starten, afhankelijk van op welk getal level staat.
    (let level-rec ((levels level-list)
                    (level-n level))
      (if (null? levels)
          (begin ((draw-adt 'remove-drawables!))(draw-adt 'remove-bermen) ((draw-adt 'remove-lanes)) ((draw-adt 'remove-bermen)) ((score-adt 'reset-health))(set! level 1) (set-next-level!))
          (if (= 1 level-n)
              (begin ((draw-adt 'remove-drawables!)) (draw-adt 'remove-lanes) ((draw-adt 'remove-bermen))(set! current-level ((car levels))))
              (level-rec (cdr levels) (- level-n 1))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;Procedures om de verschillende levels aan te maken.;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (define (make-menu) ; proc om het mainmenu aan te maken
    
    (display "starting menu")(newline)
    ((draw-adt 'unshow-score))
    ((draw-adt 'make-highscore-tile))
    ((draw-adt 'draw-highscore) (number->string (score-adt 'high-score)))
    ((draw-adt 'make-play-button))
    ((draw-adt 'make-quit-button))
    ((draw-adt 'set-key-function!) input-main-menu)
    ((draw-adt 'set-game-loop-function!) return))

  

  (define (make-level1)
    (display "loading level 1") (newline)
    ((draw-adt 'make-lanes))
    ((draw-adt 'unshow-highscore))
    ((draw-adt 'make-score-tile))
    ((draw-adt 'make-health-tile))
    
    (define river-loc -1)
    
    ((draw-adt 'make-bermen))
    (set! current-coin-list (list (make-item 1 4 'coin 10)
                                  (make-item 18 1 'coin 10)))
    ((draw-adt 'add-to-assoc) current-coin-list) ;afzonderlijke lijst om te kunnen kijken of alle munten picked-up zijn.
    (set! item-list (list 
                     (make-item 10 4 'pill 50)
                     (make-item 12 7 'insect 30)
                     (make-item 18 2 'insect 40)
                     (make-item 5 2 'insect 50)
                     (make-item 15 6 'insect 60)
                     (make-item 4 4 'bush 0)
                     (make-item 14 4 'bush 0)))
    ((draw-adt 'add-to-assoc) item-list)
    (set! item-list (append item-list current-coin-list))
    (set! car-list (list (make-car 0 5 'right 1/4 3 'regular) ;;lijst met auto's aanmaken
                         (make-car 12 3 'left 1/2 1 'regular)
                         (make-car 5 3 'left 1/4 1 'regular)
                         (make-lane-switching-car 5 6 'right 1/4 2)
                         (make-lane-switching-car 5 7 'right 1/4 1)
                         (make-adapting-car 5 2 'right 1/4 2 3)
                         (make-naive-car 3 1 'left 1/4 1 1.8)
                         ))
    ((draw-adt 'add-to-assoc) car-list)
    (set! frogger-adt (make-frogger item-list score-adt river-loc))
    
    ((draw-adt 'add-to-assoc) (list frogger-adt))
    (when (> (frogger-adt 'pill-timer) 0)
      ((draw-adt 'set-normal!))))

  (define (make-level2)
    (display "loading level 2")(newline)
    ((draw-adt 'make-lanes))
    
    (define river-loc 4)
    (set! river-pos river-loc)
    ((draw-adt 'make-bermen-river))
    (set! current-coin-list (list (make-item 1 5 'coin 10)
                                  (make-item 18 1 'coin 10)))
    ((draw-adt 'add-to-assoc) current-coin-list)
    (set! item-list (list (make-item 10 3 'pill 50)
                          (make-item 12 7 'insect 30)
                          (make-item 18 2 'insect 40)
                          (make-item 5 2 'insect 50)
                          (make-item 15 6 'insect 60)
                          (make-item 4 4 'log 0)
                          (make-item 14 4 'log 0)))
    ((draw-adt 'add-to-assoc) item-list)
    (set! item-list (append item-list current-coin-list))
    (set! car-list (list (make-car 0 5 'right 1/4 3 'regular) ;;lijst met auto's aanmaken
                         (make-car 12 3 'left 1/2 1 'regular)
                         (make-car 5 3 'left 1/4 1 'regular)
                         (make-lane-switching-car 5 6 'right 1/4 2)
                         (make-lane-switching-car 5 7 'right 1/4 1)
                         (make-adapting-car 5 2 'right 1/4 2 3)
                         (make-naive-car 3 1 'left 1/4 1 1.8)
                         ))
    ((draw-adt 'add-to-assoc) car-list)
    (set! frogger-adt (make-frogger item-list score-adt river-loc))
    
    ((draw-adt 'add-to-assoc) (list frogger-adt))
    (when (> (frogger-adt 'pill-timer) 0)
      ((draw-adt 'set-normal!))))

  (define (make-level3)
    (display "loading level 3")(newline)
    ((draw-adt 'make-lanes))
    
    (define river-loc 4)
    (set! river-pos river-loc)
    ((draw-adt 'make-bermen-river))
    (set! current-coin-list (list (make-item 1 6 'coin 10)
                                  (make-item 16 2 'coin 10)))
    ((draw-adt 'add-to-assoc) current-coin-list)
    (set! item-list (list (make-item 5 3 'pill 50)
                          (make-item 3 7 'insect 30)
                          (make-item 18 2 'insect 40)
                          (make-item 5 2 'insect 50)
                          (make-item 15 6 'insect 60)
                          (make-item 4 4 'log 0)
                          (make-item 14 4 'log 0)))
    ((draw-adt 'add-to-assoc) item-list)
    (set! item-list (append item-list current-coin-list))
    (set! car-list (list (make-car 0 5 'right 1/4 3 'regular) ;;lijst met auto's aanmaken
                         (make-car 12 3 'left 1/2 1 'regular)
                         (make-car 5 3 'left 1/4 1 'regular)
                         (make-lane-switching-car 5 5 'right (/ 1 3.5) 3)
                         (make-lane-switching-car 5 7 'right 1/4 1)
                         (make-adapting-car 5 2 'right 1/4 2 3)
                         (make-naive-car 3 1 'left 1/4 1 1.8)
                         (make-car 10 1 'left 1/2 1 'regular)
                         ))
    ((draw-adt 'add-to-assoc) car-list)
    (set! frogger-adt (make-frogger item-list score-adt river-loc))
    
    ((draw-adt 'add-to-assoc) (list frogger-adt))
    (when (> (frogger-adt 'pill-timer) 0)
      ((draw-adt 'set-normal!))))
  
  (define level-list (list make-menu make-level1 make-level2 make-level3)) ;;de lijst met levels.
    
  (define (game-dispatch m)
    (cond ((eq? m 'start) start)))
  game-dispatch)

(define game (make-game-adt))
((game 'start))




