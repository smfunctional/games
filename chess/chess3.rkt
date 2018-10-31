#lang racket/gui
(require "chess-piece.rkt")
(require "chess-board.rkt")


;; The chess game uses a “rank” and “file” coordinate system, where the rank represents the row
;; on the board and it is numbered from 1 to 8, 1 being at the bottom, while the file are the columns
;; of the board, labeled using letters from “a” to “h”.

(define chess-piece-data
  (hash
   "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
   "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

(define (make-chess-piece id [location #f])
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 30 'default 'normal 'normal))
  (new chess-piece% [glyph (string glyph)] [font font] [size 45] [location location]))

;; A test program for our chess-piece% objects:
;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (* 50 8)] [height (* 50 8)]))
;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
(send toplevel show #t)

;; a function which loads a chess game onto the board. The game is encoded as a string
;; with the piece mnemonic followed by its position. For example, “Ra1” means that the white
;; rook is at square “a1”. The function just parses the string, creates the chess pieces and
;; inserts them onto the board. Also, the first thing this function does is to clear the board
;; of previous pieces, by calling the pasteboard%’s clear method

(define initial
  (string-append
   "Ra1Nb1Bc1Qd1Ke1Bf1Ng1Rh1"
   "Pa2Pb2Pc2Pd2Pe2Pf2Pg2Ph2"
   "pa7pb7pc7pd7pe7pf7pg7ph7"
   "ra8nb8bc8qd8ke8bf8ng8rh8"))

(define (setup-board board position)
  (send board clear)
  (define piece-count (/ (string-length position) 3))
  (for ([index (in-range piece-count)])
    (define pos (* index 3))
    (define name (substring position pos (add1 pos)))
    (define location (substring position (add1 pos) (+ (add1 pos) 2)))
    (send board insert (make-chess-piece name location))))

(setup-board board initial)