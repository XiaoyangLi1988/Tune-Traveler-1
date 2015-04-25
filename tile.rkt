#lang racket
(require sgl/gl)
(require "constants.rkt")

; Helper function for drawing tiles at a given row and column.
(define (drawTile r c)
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF) (* r TILE_SIZE) 0.0)                           ; Draw the vertex at the top-left of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE) (* r TILE_SIZE) 0.0)                 ; Draw the vertex at the top-right of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF TILE_SIZE) (+ (* r TILE_SIZE) TILE_SIZE) 0.0)   ; Draw the vertex at the bottom-right of the box.
  (glVertex3f (+ (* c TILE_SIZE) GRID_OFF) (+ (* r TILE_SIZE) TILE_SIZE) 0.0))            ; Draw the vertex at the bottom-left of the box.

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
            ((and (= myRow (cdr start)) (= myCol (car start)))
             (begin (glColor3f 0.0 1.0 0.0)   ; If this is the start tile, make the color green.
                    (drawTile myRow myCol)))
            ((and (= myRow (cdr goal)) (= myCol (car goal)))
             (begin (glColor3f 1.0 0.0 0.0)   ; If this is the goal tile, make the color red.
                    (drawTile myRow myCol)))
            (else #f)))))

(provide (all-defined-out))