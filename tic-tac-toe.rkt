#lang racket
(require 2htdp/universe 2htdp/image rackunit)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;               ;;;;;
;;;;;   RENDERING   ;;;;;
;;;;;               ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; CONSTANTS
;;;

;; placements
(define BOARD-SIZE 300)
(define b-1/3 (/ BOARD-SIZE 3)) ; value of a third of the board size
(define b-2/3 (* 2 (/ BOARD-SIZE 3)))
(define ORIGIN 0)
;; (define BOARD-SIZE (+ BOARD-SIZE b-1/3))
(define BOARD-CENTER (/ BOARD-SIZE 2))
(define OFFSET 15)

(struct posn (x y))
(struct region (min-pos max-pos box))

;grid boxes
(define 1-1box (region (posn ORIGIN ORIGIN) (posn b-1/3 b-1/3) '(1.1)))        ;box 1,1
(define 1-2box (region (posn ORIGIN b-1/3) (posn b-1/3 b-2/3) '(1.2)))         ;box 1,2
(define 1-3box (region (posn ORIGIN b-2/3) (posn b-1/3 BOARD-SIZE) '(1.3)))    ;box 1,3
(define 2-1box (region (posn b-1/3 ORIGIN) (posn b-2/3 b-1/3) '(2.1)))         ;box 2,1
(define 2-2box (region (posn b-1/3 b-1/3) (posn b-2/3 b-2/3) '(2.2)))          ;box 2,2
(define 2-3box (region (posn b-1/3 b-2/3) (posn b-2/3 BOARD-SIZE) '(2.3)))     ;box 2,3
(define 3-1box (region (posn b-2/3 ORIGIN) (posn BOARD-SIZE b-1/3) '(3.1)))    ;box 3,1
(define 3-2box (region (posn b-2/3 b-1/3) (posn BOARD-SIZE b-2/3) '(3.2)))     ;box 3,2
(define 3-3box (region (posn b-2/3 b-2/3) (posn BOARD-SIZE BOARD-SIZE) '(3.3)));box 3,3
(define positions (list 1-1box 1-2box 1-3box 2-1box 2-2box 2-3box 3-1box 3-2box 3-3box))


;; pens
(define ttt-lines-pen (pen "black" 10 "solid" "butt" "bevel"))
(define x-pen (pen "blue" 5 "solid" "round" "round"))
(define o-pen (pen "red" 5 "solid" "round" "round"))

;; texture
#(struct textures (board p1 p2))

;;;
;;; BOARD
;;;

;; ttt-board
; creates a standard 3X3 tic-tac-toe board
(define ttt-board
  (place-image
   (add-line
    (add-line
     (add-line
      (add-line
       (square BOARD-SIZE "solid" "white")
       b-1/3 ORIGIN b-1/3 BOARD-SIZE ttt-lines-pen) ;left vertical line
      b-2/3 ORIGIN b-2/3 BOARD-SIZE ttt-lines-pen) ;right vertical line
     ORIGIN b-1/3 BOARD-SIZE b-1/3 ttt-lines-pen) ;top horizontal line
    ORIGIN b-2/3 BOARD-SIZE b-2/3 ttt-lines-pen) ;bottom horizontal line
   BOARD-CENTER BOARD-CENTER
   (rectangle BOARD-SIZE (+ BOARD-SIZE 150) "solid" "white")))

;; X piece
(define x-piece
  (add-line
   (line -60 -60 x-pen)
   0 60 60 0 x-pen))

;; O piece
(define o-piece
  (circle 30 "outline" o-pen))

;;; v-board
; the board that is shown to the user
; in the beginning, the board will be empty (just a ttt-board)
(define v-board ttt-board)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;               ;;;;;
;;;;;   FUNCTIONS   ;;;;;
;;;;;               ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; GAME MECHANICS
;;;

;;;struct board
(struct board (p1 p2 state) #:mutable #:transparent)

;; shortened functions to access components from the board struct
;;Gets
; Explanation: b is a board; (board-p1) and (board-p2) are the two functions
; that are defined when the structure board is defined above.
(define p1-fill (λ (b) (board-p1 b)))
(define p2-fill (λ (b) (board-p2 b)))

;; Set containing all the cells on the board
(define all-cells
  (set '(1.1) '(2.1) '(3.1)
       '(1.2) '(2.2) '(3.2)
       '(1.3) '(2.3) '(3.3)))

;; Function that checks the unfilled cells (b = board)
(define (free-cells b)
  (set-subtract all-cells (p1-fill b) (p2-fill b)))

;; Checks whether a particular cell is empty (#t if empty)
(define (empty-cell? cell b)
  (set-member? (free-cells b) cell))

;; List of winning positions
(define winning-positions
  (list (set '(1.1) '(2.2) '(3.3))   ;; one diagonal
        (set '(1.3) '(2.2) '(3.1))   ;; the other diagonal
        (set '(1.1) '(1.2) '(1.3))   ;; first column
        (set '(2.1) '(2.2) '(2.3))   ;; second column
        (set '(3.1) '(3.2) '(3.3))   ;; third column
        (set '(1.1) '(2.1) '(3.1))   ;; top row
        (set '(1.2) '(2.2) '(3.2))   ;; middle row
        (set '(1.3) '(2.3) '(3.3)))) ;; bottom row

;; winfs? function returns true if set s contains a winning position
(define wins? (lambda (s)
                (ormap (lambda (x) (subset? x s)) winning-positions)))
;; (if it's not clear, just rewrite as a for loop)

;; player moves: just add a list m to a board b (remember xs and os defined above)
;; FIXME You are lazy, you should really check that m is not occupied already...
(define p1-move (lambda (b m)  (set-board-p1! b (set-add (p1-fill b) m) )))
(define p2-move (lambda (b m)  (set-board-p2! b (set-add (p2-fill b) m) )))
 
(define playing-board (board (set) (set) 'playing))
(define current-player 1)
  
;;;
;;; ??
;;;

#(define (do-move cell board)
  (cond
    ((equal? current-player 1) (p1-move board cell)
                               (set! current-player (add1 current-player))
                               (set! v-board (underlay/xy
                                              v-board
                                              45
                                              45
                                              x-piece)))
    ((equal? current-player 2) (p2-move board cell)
                               (set! current-player (sub1 current-player))
                               (set! v-board (underlay/xy
                                              v-board
                                              45
                                              45
                                              o-piece)))))
#(define mouse-clicked (λ (arg1 x y event)
                        (when (string=? event "button-down")
                          (let ([find-region (λ (x-min x-max y-min y-max)
                                             (and (and (< x-min x) (> x-max x))
                                                  (and (< y-min y) (> y-max y))))])
                            (cond
                              [(and (find-region ORIGIN b-1/3 ORIGIN b-1/3)
                                    (empty-cell? '(1.1) playing-board))
                               (do-move '(1.1))]
                              [(and (find-region b-1/3 b-2/3 ORIGIN b-1/3)
                                    (empty-cell? '(2.1) playing-board))
                               (do-move '(2.1))])))))

(define (do-move cell)
  (cond
    ((equal? current-player 1)
     (p1-move playing-board (region-box cell))
     (set! v-board
           (underlay/xy
            v-board
            (+ OFFSET (posn-x (region-min-pos cell)))
            (+ OFFSET (posn-y (region-min-pos cell)))
            x-piece))
     #(if (wins? p1-fill) p1-win)
     (set! current-player (add1 current-player)))
    ((equal? current-player 2)
     (p2-move playing-board (region-box cell))
     (set! v-board
           (underlay/xy
            v-board
            (+ OFFSET (posn-x (region-min-pos cell)))
            (+ OFFSET (posn-y (region-min-pos cell)))
            o-piece))
     #(if (wins? p2-fill p2-win))
     (set! current-player (sub1 current-player)))))


;; find-box checks for the region the player clicks on
(define (find-box pos-lst x y)
  (cond
    [(empty? pos-lst) #f]
    [(and (and (< (posn-x (region-min-pos (first pos-lst))) x)
               (> (posn-x (region-max-pos (first pos-lst))) x))
          (and (< (posn-y (region-min-pos (first pos-lst))) y)
               (> (posn-y (region-max-pos (first pos-lst))) y))
          (empty-cell? (region-box (first pos-lst)) playing-board))
     (do-move (first pos-lst))]
    [else (find-box (rest pos-lst) x y)]))


(define mouse-clicked (λ (arg1 x y event)
                        (when (mouse=? event "button-down")
                          (when (equal? (board-state playing-board) 'playing)
                            (find-box positions x y))
                          ;check whether the game ended
                          (cond
                            [(wins? (p1-fill playing-board))
                             (set-board-state! playing-board 'p1-win)]
                            [(wins? (p2-fill playing-board))
                             (set-board-state! playing-board 'p2-win)]
                            [(equal? (free-cells playing-board) (set))
                             (set-board-state! playing-board 'tie)]))))
                          

(define buttons (λ (arg1 key-pressed)
                  (cond
                    [(key=? key-pressed "r") (set-board-p1! playing-board (set))
                                             (set-board-p2! playing-board (set))
                                             (set-board-state! playing-board 'playing)
                                             (set! v-board ttt-board)
                                             (set! arg1 0)
                                             (set! current-player 1)])))

(define canvas (λ (arg)
                 (let ([game-text (cond
                                    [(equal? (board-state playing-board) 'p1-win) (text "Player 1 Won!\nPress \"r\" to restart" 20 "blue")]
                                    [(equal? (board-state playing-board) 'p2-win) (text "Player 2 Won!\nPress \"r\" to restart" 20 "red")]
                                    [(equal? (board-state playing-board) 'tie) (text "Looks like a tie\nPress \"r\" to restart" 20 "black")]
                                    [else (text (string-append "Player " (number->string current-player) "'s turn") 20 "black")])])
                 (underlay/xy v-board 50 320 game-text))))

;;;
;;; BIG-BANG
;;;
(big-bang 0
  [to-draw canvas]
  [on-mouse mouse-clicked]
  [on-key buttons])


;;;;;;;;;;;;;;;;;;;;;;;
;;;;;             ;;;;;
;;;;;   TESTING   ;;;;;
;;;;;             ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

