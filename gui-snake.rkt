;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "DMdA-vanilla-reader.ss" "deinprogramm")((modname gui-snake) (read-case-sensitive #f) (teachpacks ((lib "image2.ss" "teachpack" "deinprogramm") (lib "sound.ss" "teachpack" "deinprogramm") (lib "gui.ss" "teachpack" "deinprogramm"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image2.ss" "teachpack" "deinprogramm") (lib "sound.ss" "teachpack" "deinprogramm") (lib "gui.ss" "teachpack" "deinprogramm")))))
;Ein Segment besteht aus
; X-Koordinate (number)
; Y-Koordinate (number)
(: make-segment (number number -> segment))
(: segment? (any -> boolean))
(: segment-x (segment -> number))
(: segment-y (segment -> number))
(define-record-procedures segment
  make-segment segment?
  (segment-x segment-y))

;segment=?
;konsumiert zwei segments und testet sie auf Gleichheit
(: segment=? (segment segment -> boolean))
(check-expect (segment=? (make-segment 199 98) (make-segment 200 98)) #f)
(check-expect (segment=? (make-segment 666 98) (make-segment 666 98)) #t)
(define segment=?
  (lambda (s1 s2)
    (and (= (segment-x s1) (segment-x s2))
         (=  (segment-y s1) (segment-y s2)))))



;ein spielfeld besteht aus 
; -Breite (natural)
; -Höhe (natural)
(: make-spielfeld (natural natural -> spielfeld))
(: spielfeld? (any -> boolean))
(: spielfeld-breite (spielfeld -> natural))
(: spielfeld-höhe (spielfeld -> natural))
(define-record-procedures spielfeld
  make-spielfeld spielfeld?
  (spielfeld-breite spielfeld-höhe))

;inside?
;testet, ob ein segment in einem spielfeld liegt
(: inside? (segment spielfeld -> boolean))
(check-expect (inside? (make-segment 99 100) (make-spielfeld 99 99)) #f)
(check-expect (inside? (make-segment 99 100) (make-spielfeld 99 100)) #t)
(define inside?
  (lambda (seg sp)
    (and (<= (segment-x seg) (spielfeld-breite sp))
         (>= (segment-x seg) 0)
         (>= (segment-y seg) 0)
         (<= (segment-y seg) (spielfeld-höhe sp)))))

;Eine Schlange ist charakterisiert durch eine Liste aus Segmenten
(define schlange
  (signature
   (list-of segment)))

;head
;gibt das Kopfsegment der Schlange zurück
(: head (schlange -> segment))
(check-expect (head (list (make-segment 10 20) (make-segment 20 10))) (make-segment 20 10))
(define head
  (lambda (lis)
    (cond ((empty? (rest lis)) (first lis))
           ((pair? lis)
            (head (rest lis))))))

;tail
;gibt den Schwanz der Schlange zurück
(: tail (schlange -> (list-of segment)))
(check-expect (tail (list (make-segment 10 20) (make-segment 20 10))) (list (make-segment 10 20)))
(define tail
  (lambda (lis)
    (cond ((empty? (rest lis)) empty)
          ((pair? lis)
           (make-pair (first lis) (tail (rest lis)))))))

;Testschlange
(define test-schlange (list (make-segment 10 30) (make-segment 30 30) (make-segment 50 30)))

;bite?
;konsumiert eine schlange und testet ob sie sich selber beisst
(: bite? (schlange -> boolean))
(check-expect (bite? test-schlange) #f)
(check-expect (bite? (make-pair (make-segment 50 30) test-schlange)) #t)
(define bite? 
  (lambda (snk)
    (let ((kopf (head snk))
          (schwanz (tail snk)))
      (fold #f
            (lambda (a b)
              (or (segment=? kopf a)
                  b))
            schwanz))))

;Konstante für die zurückzulegende Strecke pro tick
(define speed 20)

;eine der vier bewegungsrichtungen
(define richtung (signature (one-of "up" "down" "right" "left")))

;move-segment
;bewegt ein segment in eine richtung
(: move-segment (segment richtung -> segment))
(check-expect (move-segment (make-segment 100 100) "up") (make-segment 100 80))
(check-expect (move-segment (make-segment 100 100) "down") (make-segment 100 120))
(check-expect (move-segment (make-segment 100 100) "left") (make-segment 80 100))
(check-expect (move-segment (make-segment 100 100) "right") (make-segment 120 100))
(define move-segment
  (lambda (seg str)
    (cond ((string=? str "up") (make-segment (segment-x seg) (- (segment-y seg) speed)))
          ((string=? str "down") (make-segment (segment-x seg) (+ (segment-y seg) speed)))
          ((string=? str "left") (make-segment (- (segment-x seg) speed) (segment-y seg)))
          ((string=? str "right") (make-segment (+ (segment-x seg) speed) (segment-y seg))))))

;move-snake
;konsumiert eine Schlange und "bewegt" sie
(: move-snake (schlange richtung -> schlange))
(check-expect (move-snake (list (make-segment 20 20) (make-segment 40 20)) "right") (list (make-segment 40 20) (make-segment 60 20)))
(define move-snake
  (lambda (snk k)
    (append
     (rest snk)
     (list (move-segment (head snk) k)))))
;eine 2e8 ist eine Signatur für Zahlen zwischen 0 und 255
(define zwei8
  (signature
   (predicate (lambda (x)
                (<= 0 x 255)))))
;Eine Farbe besteht aus
; Rot Grün Blau (natural >=0 <=255)
(: make-farbe (zwei8 zwei8 zwei8 -> farbe))
(: farbe? (any -> boolean))
(: farbe-r (farbe -> zwei8))
(: farbe-g (farbe -> zwei8))
(: farbe-b (farbe -> zwei8))
(define-record-procedures farbe
  make-farbe farbe?
  (farbe-r farbe-g farbe-b))
;Eine game-interaction besteht aus
; -der Hintergrundfarbe
; -Spiel an/aus
(: make-interaction ((one-of "green" "blue" "yellow") boolean farbe -> interaction))
(: interaction? (any -> boolean))
(: interaction-bgcol (interaction -> (one-of "green" "blue" "yellow")))
(: interaction-state (interaction -> boolean))
(: interaction-sncol (interaction -> farbe))
(define-record-procedures interaction
  make-interaction interaction?
  (interaction-bgcol interaction-state interaction-sncol))
;Ein game besteht aus 
; -schlange (schlange) 
; -richtung (richtung)
; -futter (segment)
; -spielfeld (spielfeld)
; -eine Benutzerinteraktion 
(: make-game (schlange richtung segment spielfeld interaction -> game))
(: game? (any -> boolean))
(: game-schlange (game -> schlange))
(: game-richtung (game -> richtung))
(: game-futter (game -> segment))
(: game-spielfeld (game -> spielfeld))
(: game-interaction (game -> interaction))
(define-record-procedures game
  make-game game?
  (game-schlange game-richtung game-futter game-spielfeld game-interaction))

;change-direction
;konsumiert ein game und eine richtung und gibt ein game mit aktualisierter richtung zurück
(: change-direction (game richtung -> game))
(define change-direction
  (lambda (g r)
    (make-game (game-schlange g) r (game-futter g) (game-spielfeld g) (game-interaction g))))

;Graphiken
(define futter-image (rectangle 20 20 "solid" "red"))
(define schlange-image (rectangle 20 20 "solid" "black"))

;Testwerte
(define test-game (make-game test-schlange "right" (make-segment 110 30) (make-spielfeld 30 30) (make-interaction "green" #t (make-farbe 0 0 0))))

;draw-snake
;malt die Schlange auf die szene
(: draw-snake (schlange image farbe -> image))
(define draw-snake
  (lambda (snk sc c)
    (cond ((empty? snk) sc)
          ((pair? snk)
           (place-image (rectangle 20 20 "solid" (make-color (farbe-r c) (farbe-g c) (farbe-b c)))
                        (segment-x (first snk))
                        (segment-y (first snk))
                        (draw-snake (rest snk) sc c))))))
     
;redraw-game
;konsumiert ein game und zeichnet Schlange und Futter
(: redraw-game (game -> image))
(define redraw-game
  (lambda (g)
    (let ((höhe (spielfeld-höhe (game-spielfeld g)))
          (breite (spielfeld-breite (game-spielfeld g))))
    (draw-snake (game-schlange g) 
                (place-image futter-image
                             (segment-x (game-futter g))
                             (segment-y (game-futter g))
                             (overlay (rectangle breite
                                                 höhe
                                                 "solid" (interaction-bgcol (game-interaction g)))
                                      (empty-scene breite
                                                   höhe)))
                (interaction-sncol (game-interaction g))))))

;start-game
(define start-schlange (list (make-segment 10 30) (make-segment 30 30) (make-segment 50 30)))
(define start-game (make-game test-schlange "right" (make-segment 110 30) (make-spielfeld (* 30 speed) (* 30 speed)) (make-interaction "green" #f (make-farbe 0 0 0))))
;
;tick-event
;steuert das Spiel
(: tick-event (game -> game))
(define tick-event
  (lambda (g)
    (if (interaction-state (game-interaction g))
        (cond 
          ((or (bite? (game-schlange g))
               (not (inside? (head (game-schlange g)) (game-spielfeld g))))
           (make-game (game-schlange start-game)
                      (game-richtung start-game)
                      (game-futter start-game)
                      (game-spielfeld start-game)
                      (game-interaction g)))
          ((segment=? (head (game-schlange g)) (game-futter g))
           (make-game
            (move-snake (make-pair (make-segment (segment-x (first (game-schlange g)))
                                                 (segment-y (first (game-schlange g)))) 
                                   (game-schlange g)) (game-richtung g))
            (game-richtung g)
            (make-segment (+ 10 (* speed (random 30)))
                          (+ 10 (* speed (random 30))))
            (game-spielfeld g)
            (game-interaction g)))
          (else
           (make-game
            (move-snake (game-schlange g) (game-richtung g))
            (game-richtung g)
            (game-futter g)
            (game-spielfeld g)
            (game-interaction g))))
        g)))

;change-interaction
;konsumiert ein game und eine interaction und gibt das geänderte Game zurück
(: change-interaction (game interaction -> game))
(define change-interaction
  (lambda (g i)
    (make-game (game-schlange g)
               (game-richtung g)
               (game-futter g)
               (game-spielfeld g)
               i)))
  (define universe-gui
    (letrec
        ((the-image (make-image-container (rectangle 100 120 "solid" (make-color 0 0 0))))
         (the-sliderR 
          (make-vertical-slider
           "R"
           0
           255
           0
           (lambda (gui world)
             (make-gui-world (image-container-image-change (rectangle 100 120 "solid" (make-color (slider-value the-sliderR gui)
                                                                                                         (slider-value the-sliderG gui)
                                                                                                         (slider-value the-sliderB gui)))
                                                           the-image gui)
                             (change-interaction world 
                                                 (make-interaction (interaction-bgcol (game-interaction world))
                                                                   (interaction-state (game-interaction world))
                                                                   (make-farbe (slider-value the-sliderR gui)
                                                                               (farbe-g (interaction-sncol (game-interaction world)))
                                                                               (farbe-b (interaction-sncol (game-interaction world)))
                                                                               )))))))
         (the-sliderG
          (make-vertical-slider
           "G"
           0
           255
           0
           (lambda (gui world)
             (make-gui-world (image-container-image-change (rectangle 100 120 "solid" (make-color (slider-value the-sliderR gui)
                                                                                                  (slider-value the-sliderG gui)
                                                                                                  (slider-value the-sliderB gui)))
                                                           the-image gui) 
                                                          (change-interaction world 
                                                 (make-interaction (interaction-bgcol (game-interaction world))
                                                                   (interaction-state (game-interaction world))
                                                                   (make-farbe (farbe-r (interaction-sncol (game-interaction world)))
                                                                               (slider-value the-sliderG gui)
                                                                               (farbe-b (interaction-sncol (game-interaction world))))))))))
         (the-sliderB
          (make-vertical-slider
           "B"
           0
           255
           0
           (lambda (gui world)
             (make-gui-world (image-container-image-change (rectangle 100 120 "solid" (make-color (slider-value the-sliderR gui)
                                                                                                  (slider-value the-sliderG gui)
                                                                                                  (slider-value the-sliderB gui)))
                                                           the-image gui)
                             (change-interaction world 
                                                 (make-interaction (interaction-bgcol (game-interaction world))
                                                                   (interaction-state (game-interaction world))
                                                                   (make-farbe (farbe-r (interaction-sncol (game-interaction world)))
                                                                               (farbe-g (interaction-sncol (game-interaction world)))
                                                                               (slider-value the-sliderB gui))))))))
         (the-sliders (make-vertical-container 
                       (make-vertical-container the-sliderR
                                                  the-sliderG)
                                                  the-sliderB))
         (change-snake-color (make-vertical-container (make-message "Farbe der Schlange")
                                                      (make-vertical-container the-image
                                                                               the-sliders)))
         (change-background (make-vertical-radiobox "Hintergrund"
                                                    (list "Hintergrund 1" "Hintergrund 2" "Hintergrund 3")
                                                    (lambda (gui world)
                                                      (make-gui-world 
                                                       gui 
                                                       (let ((selected (single-selection-value (radiobox-selected change-background gui)))
                                                             (state (interaction-state (game-interaction world)))
                                                             (sncol (interaction-sncol (game-interaction world))))
                                                         (cond 
                                                           ((string=? selected "Hintergrund 1") (change-interaction world (make-interaction "green" state sncol)))
                                                           ((string=? selected "Hintergrund 2") (change-interaction world (make-interaction "yellow" state sncol)))
                                                           ((string=? selected "Hintergrund 3") (change-interaction world (make-interaction "blue" state sncol)))))))
                                                    
                                                    (make-single-selection "Hintergrund 1")))
      
                           
                            
         (start-button (make-button "Spiel starten" (lambda (gui world)
                                                                                      (make-gui-world (button-label-change 
                                                                                                       (cond ((string=? "Spiel starten" (button-label start-button gui)) "Spiel stoppen")
                                                                                                             ((string=? "Spiel stoppen" (button-label start-button gui)) "Spiel starten"))
                                                                                                       start-button
                                                                                                       gui)
                                                                                                       (change-interaction world (make-interaction (interaction-bgcol (game-interaction world))
                                                                                                                                                   (not (interaction-state (game-interaction world)))
                                                                                                                                                   (interaction-sncol (game-interaction world))))))))                
         (my-gui (make-tab-container (make-tab "Spiel" start-button)
                                     (make-tab "Darstellung" (make-vertical-container change-snake-color
                                                                                      (make-frame-container change-background)))
                                     (make-single-selection "Spiel")))
         (my-canvas (make-universe-canvas
                     (on-tick (lambda (gui world)
                                (make-gui-world gui 
                                                (tick-event world))) 0.1)
                     (on-key (lambda (gui world k)
                               (make-gui-world gui
                                               (change-direction world k))))
                     (to-draw (lambda (gui world)
                                (redraw-game world)))))
         (my-window (make-window-with-menu "World"
                                           200
                                           200
                                           (make-horizontal-container my-gui my-canvas)
                                           (make-menu (make-menu-entry "Datei" (make-menu-entry-item "start/stop"
                                                                                                     (lambda (gui world)
                                                                                                       (make-gui-world gui
                                                                                                                       (change-interaction world (make-interaction (interaction-bgcol (game-interaction world))
                                                                                                                                                                   (not (interaction-state (game-interaction world)))
                                                                                                                                                                   (interaction-sncol (game-interaction world))))))))
                                                      (make-menu-entry "Hilfe" (make-menu-entry-item "test1" (lambda (gui world) (make-gui-world gui world))))))))
      my-window))
  
  (gui-world-bang (make-gui-world (make-gui universe-gui)
                                  start-game))
  

     

