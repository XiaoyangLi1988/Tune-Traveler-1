#lang racket
(require sgl/gl
         rsound
         racket/runtime-path)

; Constants
(define F_WIDTH 600)
(define F_HEIGHT 600)
(define GRID_SIZE 15)
(define NEIGHBORS '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1)))  ; Allow for horizontal, vertical and diagonal movements.
;(define NEIGHBORS '((0 -1) (1 0) (0 1) (-1 0)))                               ; Allow for only vertical and horizontal movement.
(define nil '())

; Globals
(define TILE_SIZE (/ F_HEIGHT GRID_SIZE))
(define GRID_OFF (/ (abs (- F_WIDTH F_HEIGHT)) 2))
(define A_TIMER 0)
(define PATH '())
(define PATH-POS 0)


; Setup the sound settings and load the guitar sounds.
(define-runtime-path demos "demos")
(define song1 (rs-read (build-path demos "1st_String_E_64kb.wav")))
(define song2 (rs-read (build-path demos "2nd_String_B__64kb.wav")))
(define song3 (rs-read (build-path demos "3rd_String_G_64kb.wav")))
(define song4 (rs-read (build-path demos "4th_String_D_64kb.wav")))
(define song5 (rs-read (build-path demos "5th_String_A_64kb.wav")))
(define song6 (rs-read (build-path demos "6th_String_E_64kb.wav")))
(define song7 (rs-read (build-path demos "C_64kb.wav")))
(define song8 (rs-read (build-path demos "D_64kb.wav")))
(define song9 (rs-read (build-path demos "Dm_64kb.wav")))
(define song10 (rs-read (build-path demos "E_64kb.wav")))
(define Sample-rate 44100.0)
(define (s sec) (* sec Sample-rate))

; The following are some pre-defined guitar sounds.
(define s1 (clip song1 (s 2) (s 4)))
(define s2 (clip song2 (s 2) (s 4)))
(define s3 (clip song3 (s 2) (s 4)))
(define s4 (clip song4 (s 2) (s 4)))
(define s5 (clip song5 (s 2) (s 4)))
(define s6 (clip song6 (s 2) (s 4)))
(define s7 (clip song7 (s 1.5) (s 3.5)))
(define s8 (clip song8 (s 1) (s 3)))
(define s9 (clip song9 (s 1) (s 3)))
(define s10 (clip song10 (s 1) (s 3)))

(define clips (list s1 s2 s3 s4 s5 s6 s7 s8 s9 s10))

; Define the start and end position.
(define start (cons 0 0))
(define (moveStart row col)
  (set! start (cons col row)))

(define goal (cons 0 0))
(define (moveGoal row col)
  (set! goal (cons col row)))

; Define the player's current position. Subject to change throughout execution.
(define player (cons 0 0))
(define (movePlayer row col)
  (set! player (cons col row)))

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

; Checks if the given row and column are within range of the grid.
(define (validRowCol? row col)
  (if (or (< row 0)
          (> row (- GRID_SIZE 1))
          (< col 0)
          (> col (- GRID_SIZE 1)))
      #f
      #t))

; Used to access elements of a one-dimensional array representing a two-dimensional array.
(define (get row col)
  (cond ((or (> row (- GRID_SIZE 1)) (< row 0)) (error "Row out of bounds!"))
        ((or (> col (- GRID_SIZE 1)) (< col 0)) (error "Column out of bounds!")))
  (define (h li)
    (list-ref li (+ (* row GRID_SIZE) col)))
  h)

; Check if the neighbor being examed is next to a neighbor that is unwalkable.
(define (noNeighborWalls GRID t p)
  (cond ([and (= (car p) -1) (= (cadr p) -1)]  ; Upper-left neighbor. Check tiles above and to the left of t.
         (if (or (not (send ((get (+ (cadr p) (send t getRow)) (+ (car p) 1 (send t getCol))) GRID) isWalkable))
                 (not (send ((get (+ (cadr p) 1 (send t getRow)) (+ (car p) (send t getCol))) GRID) isWalkable)))
             #f
             #t))
        ([and (= (car p) 1) (= (cadr p) -1)]  ; Upper-right neighbor. Check tiles above and to the right of t.
         (if (or (not (send ((get (+ (cadr p) (send t getRow)) (+ (- (car p) 1) (send t getCol))) GRID) isWalkable))
                 (not (send ((get (+ (cadr p) 1 (send t getRow)) (+ (car p) (send t getCol))) GRID) isWalkable)))
             #f
             #t))
        ([and (= (car p) -1) (= (cadr p) 1)]  ; Bottom-left neighbor. Check tiles below and to the left of t.
         (if (or (not (send ((get (+ (- (cadr p) 1) (send t getRow)) (+ (car p) (send t getCol))) GRID) isWalkable))
                 (not (send ((get (+ (cadr p) (send t getRow)) (+ (car p) 1 (send t getCol))) GRID) isWalkable)))
             #f
             #t))
        ([and (= (car p) 1) (= (cadr p) 1)]  ; Bottom-right neighbor. Check tiles below and to the right of t.
         (if (or (not (send ((get (+ (cadr p) (send t getRow)) (+ (- (car p) 1) (send t getCol))) GRID) isWalkable))
                 (not (send ((get (+ (- (cadr p) 1) (send t getRow)) (+ (car p) (send t getCol))) GRID) isWalkable)))
             #f
             #t))
        (else #t)))

; Get the neighbors of the given tile.
(define (getNeighbors GRID t)
  (let ([ne '()])
    ; Use the offsets defined in NEIGHBORS to get the tiles around the current tile.
    (map (lambda (p)
           (let ([a (cons (+ (send t getRow) (car p)) (+ (send t getCol) (cadr p)))])
             ; Only get the walkable tiles.
             (when (and (validRowCol? (car a) (cdr a))
                        (send ((get (car a) (cdr a)) GRID) isWalkable)
                        (noNeighborWalls GRID t p)  ; Comment out to allow walking through corners.
                        )
               (set! ne (append ne (list ((get (car a) (cdr a)) GRID)))))))
         NEIGHBORS)
    ne))

; Find the tile in the open list with the lowest F score.
(define (lowestF open)
  ; Recurse through the entire open list. Each time it encounters a 
  ; tile with a lower F score, it overrides the current "lowest F tile".
  (define (lowestF-help open t)
    (cond ([empty? open] t)
          ([< (send (car open) getF) (send t getF)] 
           (lowestF-help (cdr open) (car open)))
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
    (retrace-help)
    (if (not (sameTile? (list-ref PATH 0) ((get (cdr goal) (car goal)) GRID)))
        (begin (set! PATH '())
               (display "No path could be found."))
        (set! PATH (reverse PATH)))))

; Define the A* search function.
(define (search GRID A B)
  (let ([open nil]        ; The open list, which contains tiles for the algorithm to consider as it walks through the "maze".
        [closed nil]      ; The closed list, which contains tiles that have already been considered (traversed) and can be ignored.
        [current nil]     ; The current tile.
        [neighbors nil])  ; A list containing the neighbors of the current tile.
    (set! open (append open (list A)))                             ; Add the start tile to the open list.
    (define (searchLoop)
      (begin (set! current (lowestF open))                         ; Find the tile in the open list with the lowest F score.
             (set! closed (append closed (list current)))          ; Add the current tile to the closed list since we're done "exploring" it.
             (set! open (remove current open))                     ; Remove the current tile from the open list so we don't accidentally "explore" it again.
             (unless (sameTile? current B)                         ; Unless this is the goal tile, keep searching. Otherwise, we're done here.
               (begin (set! neighbors (getNeighbors GRID current))  ; Retrieve the 8 neighbor tiles surrounding the current tile. Un-walkable tiles excluded.
                      (map (lambda (t)                              ; Map over each neighbor tile...
                             (unless (member t closed)              ; Ignore tiles that are on the closed list... we've already "explored" them.
                               (when (not (member t open))          ; If the neighbor is not in the open list...
                                 (begin (send t setG (compG current t))                   ; Compute the G score of the neighbor.
                                        (send t setH (compH t B))                         ; Compute the H score of the neighbor.
                                        (send t setF (compF (send t getG) (send t getH))) ; Compute the F score of the neighbor.
                                        (send t setParent current)                        ; Set the parent of the neighbor to the current tile.
                                        (set! open (append open (list t)))))))            ; Add the neighbor to the open list.
                           neighbors)
                      (unless (empty? open) (searchLoop))))))       ; If there are no more tiles in the open list, we're done searching.
    (searchLoop)  ; Call the search procedure's main loop.
    (retrace GRID A current)))  ; Retrace the steps from goal to start to find the path that the "player" takes.

; Used to move the player around the grid.
(define (move rOff cOff)
  (set! player (cons (+ (car player) cOff) (+ (cdr player) rOff)))   ; Modifies the player's position to reflect the specified movement.
  (play (list-ref clips (random 4))))                                ; Plays one of the guitar sound clips.

; Called after the window renders. Used to update objects.
(define (update)
  (if (>= A_TIMER 1/3)                          ; We want the player to move every 0.5 seconds, so the timer stops at 1/2 second and then acts.
      (begin (set! A_TIMER 0)                   ; Reset the timer, then check to see where the player is and move accordingly.
             (when (< PATH-POS (- (length PATH) 0))
               (move (- (send (list-ref PATH PATH-POS) getRow) (cdr player))
                     (- (send (list-ref PATH PATH-POS) getCol) (car player)))
               (set! PATH-POS (+ PATH-POS 1))))
      (set! A_TIMER (+ A_TIMER 1/60))))         ; If a half a second hasn't gone by yet, add the time delay to the timer.

(provide (all-defined-out))