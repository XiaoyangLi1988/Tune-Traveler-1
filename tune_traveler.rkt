#lang racket/gui
(require sgl
         sgl/gl
         sgl/gl-vectors
         rsound 
         racket/runtime-path)

(require "constants.rkt")
(require "tile.rkt")
(require "levels.rkt")

(define LEVEL 1)

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

; Define the level to use for the algorithm.
(buildMap GRID (list-ref maps LEVEL))

; Reset the player position. (weird bug, player will appear at (0,0) elsewise)
(movePlayer (cdr start) (car start))

; Call the search algorithm to generate the path.
(search GRID ((get (cdr start) (car start)) GRID) ((get (cdr goal) (car goal)) GRID))

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
  (glColor3f 0.0 1.0 1.0)
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
  (glEnd)
  
  ; Draw line from start to end.
  (glColor3f 0.0 1.0 1.0)
  (glBegin GL_LINE_STRIP)
  (map (lambda (node)
         (glVertex3f (+ (* (send node getCol) TILE_SIZE) GRID_OFF (/ TILE_SIZE 2)) 
                     (+ (* (send node getRow) TILE_SIZE) (/ TILE_SIZE 2)) 
                     0.0)) 
       PATH)
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