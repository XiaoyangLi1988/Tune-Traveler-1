#lang racket/gui
(require sgl
         sgl/gl
         sgl/gl-vectors
         rsound 
         racket/runtime-path)

; Constants
(define F_WIDTH 600)
(define F_HEIGHT 600)
(define GRID_SIZE 15)
(define NEIGHBORS '((0 -1) (1 -1) (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1)))
(define nil '())

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

; Globals
(define TILE_SIZE (/ F_HEIGHT GRID_SIZE))
(define GRID_OFF (/ (abs (- F_WIDTH F_HEIGHT)) 2))
(define A_TIMER 0)

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

; Class to represent grid tiles.
(define tile%
  (class object%
    (init row col walkable)
    (super-new)
    
    (define myRow row)        ; The row that this tile is in.
    (define myCol col)        ; The column that this tile is in.
    (define canWalk walkable) ; Can the "player" traverse this tile?
    (define parent nil)       ; Used to determine which tile this tile was a neighbor of.
    
    (define F 0)              ; This particular tiles F score. (G+H)
    (define G 0)              ; This tile's G score. (10 or 14, depending on movement to get to this tile)
    (define H 0)              ; This tile's H score. ((vertical offset + horizontal offset) * 10)
    
    ; Field getters/setters.
    (define/public (getRow)
      myRow)
    (define/public (getCol)
      myCol)
    
    (define/public (isWalkable)
      canWalk)
    (define/public (setWalk v)
      (set! canWalk v))
    
    (define/public (hasParent)
      (not (equal? parent nil)))
    (define/public (getParent)
      parent)
    (define/public (setParent p)
      (set! parent p))
    
    (define/public (getF)
      F)
    (define/public (setF f)
      (set! F f))
    (define/public (getG)
      G)
    (define/public (setG g)
      (set! G g))
    (define/public (getH)
      H)
    (define/public (setH h)
      (set! H h))
    
    ; Draws the appropriate color for the tile.
    (define/public (draw)
      (cond ((not canWalk)
             (begin (glColor3f 0.4 0.4 0.4)   ; If this is a wall, make the color of the tile gray.
                    (drawTile myRow myCol)))
            ((and (= myRow (car start)) (= myCol (cdr start)))
             (begin (glColor3f 0.0 1.0 0.0)   ; If this is the start tile, make the color green.
                    (drawTile myRow myCol)))
            ((and (= myRow (car goal)) (= myCol (cdr goal)))
             (begin (glColor3f 1.0 0.0 0.0)   ; If this is the goal tile, make the color red.
                    (drawTile myRow myCol)))
            (else #f)))))

; Used to access elements of a one-dimensional array representing a two-dimensional array.
(define (get row col)
  (cond ((or (> row (- GRID_SIZE 1)) (< row 0)) (error "Row out of bounds!"))
        ((or (> col (- GRID_SIZE 1)) (< col 0)) (error "Column out of bounds!")))
  (define (h li)
    (list-ref li (+ (* row GRID_SIZE) col)))
  h)

; Used to create a one-dimensional array that represents a two-dimensional array.
(define (createGrid rows cols)
  (let ([row 0]
        [col 0])
    (define (cg li)
      (cond ((<= col (- cols 1))
             (begin (set! col (+ col 1))
                    (cg (append li (list (new tile% [row row] [col (- col 1)] [walkable #t]))))))
            ((<= row (- rows 1))
             (begin (set! row (+ row 1))
                    (set! col 0)
                    (cg li)))
            (else li)))
    (cg '())))

; The grid that will hold the game objects.
(define GRID (createGrid GRID_SIZE GRID_SIZE))

; Create walls around the grid.
(for ([i GRID_SIZE])
  (send ((get 0 i) GRID) setWalk #f)
  (send ((get (- GRID_SIZE 1) i) GRID) setWalk #f)
  (send ((get i 0) GRID) setWalk #f)
  (send ((get i (- GRID_SIZE 1)) GRID) setWalk #f))

; Get the neighbors of the given tile.
(define (getNeighbors t)
  (let ([ne '()])
    ; Use the offsets defined in NEIGHBORS to get the tiles around the current tile.
    (map (lambda (p)
           (let ([a (get (+ (send t getRow) (car p)) (+ (send t getCol) (cdr p)))])
             ; Only get the walkable tiles.
             (if (send a isWalkable)
                 (set! ne (append ne a))
                 #f)))
         NEIGHBORS)
    ne))

; Define the A* search function.
(define search
  (let ([open '()]
        [closed '()])
    (set! open (append open (list (get (car start) (cdr start))))) ; Add the start position to the open list.
    (let searchLoop ()
      (let ([current nil])
        (set! current (car open))                  ; Get the current tile.
        (append closed (list current))             ; Add it to the closed list, since it has now been traversed.
        (set! open (remove current open))          ; Remove it from the open list.
        (if (and (= (send current getRow) (car goal))               ; If the current row is the goal row...
                 (= (send current getCol) (cdr goal)))              ; ...and the current column is the goal column...
            #f                                                      ; ...break out of the search.
            (let ([neighbors (getNeighbors current)])        ; Get the current tile's neighbors.
              (map (lambda (t) 
                     (unless (member t closed)               ; As long as this tile isn't a member of the closed list...
                       (if (not (member t open))
                           (begin (send t setG (compG current t)) ; compute F, G and H, and set parent
                                  (send t setH (compH t (get (car goal) (cdr goal))))
                                  (send t setF (compF (send t getG) (send t getH)))
                                  (send t setParent current)
                                  (set! open (append open (list t)))) ; then add it to the open list
                           (begin (display "Do this later.")) ; test if using the current G score make the aSquare F score lower, if yes update the parent because it means its a better path
                           ))) 
                   neighbors)))
        (when (= (length open) 0) (searchLoop))))))

; Helper function for drawing tiles at a given row and column.
(define (drawTile r c)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF) (* r TILE_SIZE) 0.0)                           ; Draw the vertex at the top-left of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE) (* r TILE_SIZE) 0.0)                 ; Draw the vertex at the top-right of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE) (+ (* r TILE_SIZE) TILE_SIZE) 0.0)   ; Draw the vertex at the bottom-right of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF) (+ (* r TILE_SIZE) TILE_SIZE) 0.0))            ; Draw the vertex at the bottom-left of the box.

; Used to move the player around the grid.
(define (move rOff cOff)
  (set! player (cons (+ (car player) rOff) (+ (cdr player) cOff)))   ; Modifies the player's position to reflect the specified movement.
  (play (list-ref clips (random 4))))                                ; Plays one of the guitar sound clips.

; Called after the window renders. Used to update objects.
(define (update)
  (if (>= A_TIMER 1/2)                          ; We want the player to move every 0.5 seconds, so the timer stops at 1/2 second and then acts.
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

; Render everything to the frame.
(define (draw-gl)
  ; Clear the screen and draw a blank black background.
  (glClearColor 0.0 0.0 0.0 0.0)
  (glClear GL_COLOR_BUFFER_BIT)
  
  (glShadeModel GL_SMOOTH)
  
  ; Create an orthogonal projection that draws from the top-left to the bottom-right.
  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (glOrtho 0.0 F_WIDTH F_HEIGHT 0.0 -1.0 1.0)
  (glMatrixMode GL_MODELVIEW)
  (glLoadIdentity)
  
  (define center (list (/ F_WIDTH 2) (/ F_HEIGHT 2)))
  (define off 80)
  
  ; Go through the actual grid and find relevant objects to draw.
  (glBegin GL_QUADS)
  (map (lambda (t) (send t draw)) GRID)
  (glEnd)
  
  ; Draw the start and end position squares, as well as the player.
  (glBegin GL_QUADS)
  
  ; Draw the "player". Can't be reduced with call to drawTile because of varying offsets to draw position.
  (define r (cdr player))
  (define c (car player))
  (define s (/ TILE_SIZE 4))
  (glColor3f 0.0 0.0 1.0)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF s) (+ (* r TILE_SIZE) s) 0.0)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE (- s)) (+ (* r TILE_SIZE) s) 0.0)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE (- s)) (+ (* r TILE_SIZE) TILE_SIZE (- s)) 0.0)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF s) (+ (* r TILE_SIZE) TILE_SIZE (- s)) 0.0)
  
  (glEnd) ; Stop drawing quads.
  
  ; Draw the grid.
  (glColor3f 1.0 1.0 1.0)
  (glBegin GL_LINES)
  (for ([row (- GRID_SIZE 1)])
    (begin (glVertex3f GRID_OFF (* (+ row 1) TILE_SIZE) 0.0)
           (glVertex3f (- F_WIDTH GRID_OFF) (* (+ row 1) TILE_SIZE) 0.0)))
  (for ([col (- GRID_SIZE 1)])
    (begin (glVertex3f (+ (* (+ col 1) TILE_SIZE) GRID_OFF) 0.0 0.0)
           (glVertex3f (+ (* (+ col 1) TILE_SIZE) GRID_OFF) F_HEIGHT 0.0)))
  (glVertex3f GRID_OFF 0.0 0.0)
  (glVertex3f (- F_WIDTH GRID_OFF) 0.0 0.0)
  (glVertex3f GRID_OFF F_HEIGHT 0.0)
  (glVertex3f (- F_WIDTH GRID_OFF) F_HEIGHT 0.0)
  (glVertex3f GRID_OFF 0.0 0.0)
  (glVertex3f GRID_OFF F_HEIGHT 0.0)
  (glVertex3f (- F_WIDTH GRID_OFF) 0.0 0.0)
  (glVertex3f (- F_WIDTH GRID_OFF) F_HEIGHT 0.0)
  (glEnd))

; Contains methods for drawing to and updating the canvas on the screen.
(define my-canvas%
  (class* canvas% ()
    (inherit refresh with-gl-context swap-gl-buffers)
    
    ; Used to update objects on the screen.
    (define/public (STEP)
      (update)
      (refresh)
      (sleep/yield 1/60)   ; Sleeps the program for 1/60 of a second. This is the "delay" between updates.
      (queue-callback (lambda _ (send this STEP)) #f))
    
    ; Used to call the draw function.
    (define/override (on-paint)
      (with-gl-context (λ() (draw-gl) (swap-gl-buffers))))
    
    ; Called when the user resizes the screen.
    (define/override (on-size width height)
      (with-gl-context (λ() (resize width height))))
    
    (super-instantiate () (style '(gl)))))

(define win (new frame% [label "Tune Traveler"]
                 [min-width F_WIDTH] 
                 [min-height F_HEIGHT]))
(define gl  (new my-canvas% [parent win]))

(send win show #t)
(send gl STEP)