#lang racket/gui
(require "chess-piece.rkt")
(require "chess-board.rkt")


;; A test program for our chess-piece% objects:
;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (* 75 8)] [height (* 75 8)]))
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
    (send board insert (send (new chess-piece%) make-chess-piece name location))))

(setup-board board initial)