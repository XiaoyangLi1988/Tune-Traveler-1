#lang racket
(require racket/file)

(require "constants.rkt")

(define maps (list (file->lines "levels/level1.txt")
                   (file->lines "levels/level2.txt")))

; Checks to make sure the map in the file is a 15x15 grid.
(define (validMap? m)
  (if (and (= (length m) GRID_SIZE) 
           (= (length (string->list (car m))) GRID_SIZE))
      #t
      #f))

; Constructs the map by making wall tiles in the grid unwalkable and setting the start, goal and player positions.
(define (buildMap GRID lvl)
  (if (validMap? lvl)
      (for ([row (in-range 0 GRID_SIZE)])
        (let ([r (string->list (list-ref lvl row))])
          (for ([col (in-range 0 GRID_SIZE)])
            (let ([c (list-ref r col)])
              (when (eq? c #\1) (send ((get row col) GRID) setWalk #f))
              (when (eq? c #\S) (moveStart row col))
              (when (eq? c #\E) (moveGoal row col))))))
      (error "Invalid map format. Please check the input file...")))

(provide (all-defined-out))