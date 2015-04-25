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