#lang racket
(require sgl/gl
         rsound
         racket/runtime-path)

; Constants
(define F_WIDTH 600)
(define F_HEIGHT 600)
(define GRID_SIZE 15)
(define NEIGHBORS '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1)))
(define nil '())

; Globals
(define TILE_SIZE (/ F_HEIGHT GRID_SIZE))
(define GRID_OFF (/ (abs (- F_WIDTH F_HEIGHT)) 2))
(define A_TIMER 0)
(define PATH '())

; Setup the sound settings and load the guitar sounds.
(define-runtime-path demos "demos")
(define song (rs-read (build-path demos "Guitar.C4E4.wav")))
(define Sample-rate 44100.0)
(define (s sec) (* sec Sample-rate))

; The following are some pre-defined guitar sounds.
(define n1 (clip song (s 0) (s 1)))
(define n2 (clip song (s 9) (s 10)))
(define n3 (clip song (s 19) (s 20)))
(define n4 (clip song (s 25) (s 26)))
(define n5 (clip song (s 31) (s 32)))
(define clips (list n1 n2 n3 n4 n5))

; Define the start and end position.
(define start (cons 3 4))
(define goal (cons 10 9))

; Define the player's current position. Subject to change throughout execution.
(define player (cons 3 4))

; Resize the display window.
(define (resize w h)
  (glViewport 0 0 w h)
  (set! F_WIDTH w)
  (set! F_HEIGHT h)
  (set! TILE_SIZE (/ F_HEIGHT GRID_SIZE))
  (set! GRID_OFF (/ (abs (- F_WIDTH F_HEIGHT)) 2)))

; Compute G score. (14 if moving from the current tile to this one requires a diagonal movement, 10 otherwise)
(define (compG p t)
  (let ([xOff (- (send t getCol) (send p getCol))]   ; The column offset between the two tiles.
        [yOff (- (send t getRow) (send p getRow))])  ; The row offset between the two tiles.
    (define (cg)
      (if (and (not (= xOff 0)) (not (= yOff 0)))
          14     ; If the movement from the current tile to this neighbor is a diagonal one, the cost is 14...
          10))   ; ...otherwise, its cost is 10.
    (cg)))

; Compute H score. ((vertical offset + horizontal offset) * 10)
(define (compH t e)
  (let ([xOff (abs (- (send t getCol) (send e getCol)))]   ; Horizontal offset from goal tile.
        [yOff (abs (- (send t getRow) (send e getRow)))])  ; Vertical offset from goal tile.
    (define (ch)
      (* (+ xOff yOff) 10))   ; Multiply the sum of the two offsets by 10 to get the H score.
    (ch)))

; Compute F score. (G score + H score)
(define (compF g h)
  (+ g h))

; Used to access elements of a one-dimensional array representing a two-dimensional array.
(define (get row col)
  (cond ((or (> row (- GRID_SIZE 1)) (< row 0)) (error "Row out of bounds!"))
        ((or (> col (- GRID_SIZE 1)) (< col 0)) (error "Column out of bounds!")))
  (define (h li)
    (list-ref li (+ (* row GRID_SIZE) col)))
  h)

; Get the neighbors of the given tile.
(define (getNeighbors GRID t)
  (let ([ne '()])
    ; Use the offsets defined in NEIGHBORS to get the tiles around the current tile.
    (map (lambda (p)
           (let ([a ((get (+ (send t getRow) (car p)) (+ (send t getCol) (cadr p))) GRID)])
             ; Only get the walkable tiles.
             (when (send a isWalkable)
               (set! ne (append ne (list a))))))
         NEIGHBORS)
    ne))

; Find the tile in the open list with the lowest F score.
(define (lowestF open)
  (define (lowestF-help open t)
    (cond ([empty? open] t)
          ([< (send (car open) getF) (send t getF)] (lowestF-help (cdr open) (car open)))
          (else (lowestF-help (cdr open) t))))
  (lowestF-help open (car open)))

; Checks if the given tile is the same position as the destination tile.
(define (sameTile? A B)
  (if (and (= (send A getRow) (send B getRow)) 
           (= (send A getCol) (send B getCol)))
      #t
      #f))

; Retrace the path from goal to start.
(define (retrace GRID A B)
  (let ([current B])
    (set! PATH (append PATH (list current)))
    (define (retrace-help)
      (unless (sameTile? current A)
        (begin (set! current (send current getParent))
               (set! PATH (append PATH (list current)))
               (retrace-help))))
    (retrace-help)))

; Look through parents to find start.
;(define (parent-ception C n)
;  (if (= n 0)
;      (begin (display "Row of Current Parent: ")
;             (pretty-print (send C getRow))
;             (display "Col of Current Parent: ")
;             (pretty-print (send C getCol)))
;      (parent-ception (send C getParent) (- n 1))))

; Define the A* search function.
(define (search GRID A B)
  (let ([open nil]
        [closed nil]
        [current nil]
        [neighbors nil])
    (set! open (append open (list A)))                             ; Add the start tile to the open list.
    (define (searchLoop)
      (begin (set! current (lowestF open))                         ; Find the tile in the open list with the lowest F score.
             (set! closed (append closed (list current)))          ; Add the current tile to the closed list since we're done "exploring" it.
             (set! open (remove current open))                     ; Remove the current tile from the open list so we don't accidentally "explore" it again.
             (unless (sameTile? current B)                         ; Unless this is the goal tile, keep searching. Otherwise, we're done here.
              (begin (set! neighbors (getNeighbors GRID current))  ; Retrieve the 8 neighbor tiles surrounding the current tile. Un-walkable tiles excluded.
                     (map (lambda (t)                              ; Map over each neighbor tile...
                            (unless (member t closed)              ; Ignore tiles that are on the closed list... we've already "explored" them.
;                              (if (not (member t open))            ; If the tile is not yet on the open list, add it to the open list and compute its F score.
;                                  (begin (send t setG (compG current t))                   ; Compute the G score of the neighbor.
;                                         (send t setH (compH t B))                         ; Compute the H score of the neighbor.
;                                         (send t setF (compF (send t getG) (send t getH))) ; Compute the F score of the neighbor.
;                                         (send t setParent current)                        ; Set the parent of the neighbor to the current tile.
;                                         (set! open (append open (list t))))               ; Add the neighbor to the open list.
;                                  ; If it's on the open list, but not the closed list, see if it's a better path than the current tile.
;                                  ())
                              (when (not (member t open))
                                (begin (send t setG (compG current t))                    ; Compute the G score of the neighbor.
                                         (send t setH (compH t B))                         ; Compute the H score of the neighbor.
                                         (send t setF (compF (send t getG) (send t getH))) ; Compute the F score of the neighbor.
                                         (send t setParent current)                        ; Set the parent of the neighbor to the current tile.
                                         (set! open (append open (list t)))))))
                          neighbors)
                     (unless (empty? open) (searchLoop))))))       ; If there are no more tiles in the open list, we're done searching.
    (searchLoop)
    (display (sameTile? ((get (cdr start) (car start)) GRID) ((get (cdr start) (car start)) GRID)))
    (send B setParent current)
    (retrace GRID A B)))

; Used to move the player around the grid.
(define (move rOff cOff)
  (set! player (cons (+ (car player) rOff) (+ (cdr player) cOff)))   ; Modifies the player's position to reflect the specified movement.
  (play (list-ref clips (random 4))))                                ; Plays one of the guitar sound clips.

; Called after the window renders. Used to update objects.
(define (update)
  (if (>= A_TIMER 1/3)                          ; We want the player to move every 0.5 seconds, so the timer stops at 1/2 second and then acts.
      (begin (set! A_TIMER 0)                   ; Reset the timer, then check to see where the player is and move accordingly.
             (cond ((< (car player) (car goal)) ; If the player is above the goal, move them one down.
                    (move 1 0))  ; Move down.
                   ((> (car player) (car goal)) ; If the player is below the goal, move them one up.
                    (move -1 0)) ; Move up.
                   ((< (cdr player) (cdr goal)) ; If the player is to the left of the goal, move them one to the right.
                    (move 0 1))  ; Move right.
                   ((> (cdr player) (cdr goal)) ; If the player is to the right of the goal, move them one to the left.
                    (move 0 -1)) ; Move left.
                   (else #f)))
      (set! A_TIMER (+ A_TIMER 1/60))))         ; If a half a second hasn't gone by yet, add the time delay to the timer.

(provide (all-defined-out))