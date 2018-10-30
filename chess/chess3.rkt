#lang racket/gui
(require embedded-gui)
(require "chess-piece.rkt")


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

(define chess-board%
  (class pasteboard%
    (super-new)

    (define highlight-location #f)
    ;; capture drag coordinates
    (define drag-dx 0)
    (define drag-dy 0)
    
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)
        (when highlight-location
          (highlight-square dc highlight-location #f "indianred"))))
    
    ; chess pieces are added to the chess board, using the insert method, which allows placing a snip at any coordinate,
    ; or at (0, 0) if no coordinates are specified.
    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))

    ;; The on-display-size method is q called when the canvas changes size, and inside it we can
    ;; iterate over all the snips in the pasteboard and simply call position-piece for each one.
    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          ;; reposition the piece since the location is stored as text
          ;; such as (d3) its new coordinates will be calculated to the correct location.
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))

    ;; override the on-interactive-move method, which is called once only, when the mouse drag operation
    ;; starts, and receives the mouse event that started the drag. This method, look for the selected snip
    ;; and calculate the difference between the mouse event coordinates and the snip position — this
    ;; represents the place on the snip where the mouse picked it up. This information is stored in the
    ;; pasteboard object as drag-dx, drag-dy variables and it is needed because after-interactive-move
    ;; will position the snip in the square where the mouse pointer is, not where the top-left corner of
    ;; the dragged snip 
    (define/augment (on-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define-values (x y)(values (box 0) (box 0)))
      (send this get-snip-location piece x y #f)
      (set! drag-dx (- (send event get-x) (unbox x)))
      (set! drag-dy (- (send event get-y) (unbox y))))

     (define/augment (after-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (let ((target-piece (piece-at-location this location)))
        (when (and target-piece (not (eq? piece target-piece)))
          (send target-piece set-location #f)
           (send this remove target-piece)))
      (send piece set-location location)
      (position-piece this piece)
      (set! highlight-location #f)
      (send (send this get-canvas) refresh))

    (define/augment (on-move-to snip x y dragging?)
      (when dragging?
        (let ((location (xy->location this (+ x drag-dx) (+ y drag-dy))))
          (unless (equal? highlight-location location)
            (set! highlight-location location)
            (send (send this get-canvas) refresh)))))
  
    ))        
        
    ;; position-piece takes a chess piece, finds the x, y coordinate for it on the chess board based on the location
    ;; stored inside the snip and moves the piece to that location using the pasteboard%s move-to method.
    ;; This function calculates the position based on the current canvas width and height, rather than pre-calculating
    ;;the square positions, so it will work correctly regardless of the size of the board, or even if it is invoked when
    ;; the size of the board changes
    
   (define (position-piece board piece)
     (define-values (canvas-width canvas-height) ; get the size of the canvas
       (let ((c (send board get-canvas)))
          (send c get-size)))
     (define-values (square-width square-height)
        (values (/ canvas-width 8) (/ canvas-height 8)))
     (define-values (rank file)
        (location->rank-file (send piece get-location)))
     (define-values (square-x square-y)
        (values (* file square-width) (* rank square-height)))
     (define piece-width (snip-width piece))
     (define piece-height (snip-height piece))
      
     (send board move-to piece
            (+ square-x  (/ (- square-width piece-width) 2))
            (+ square-y  (/ (- square-height piece-height) 2))))

    ;; The rank-file function used by position-piece, converts a chess board location
    ;; into the row and column of the corresponding square on the board.
   (define (location->rank-file location)
     (unless (and (string? location) (= (string-length location) 2))
        (raise-argument-error 'location "valid chess position a1..h8" location))
     (define file
        (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h) (string-ref location 0)))
     (define rank
        (index-of '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) (string-ref location 1)))
     (unless (and file rank)
        (raise-argument-error 'location "valid chess position at a1 .. h8" location))
     (values rank file))

   (define (rank-file->location rank file)
     (unless (<= 0 rank 8)
       (raise-argument-error 'rank "integer between 0 and 7" rank))
     (unless (<= 0 file 8)
       (raise-argument-error 'rank "integer between 0 and 7" file))
     (string
      (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h) file)
      (list-ref '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) rank)))

    ;; xy->location function, as with other functions, this function queries the size
    ;; of the board, so that the location is always determined correctly
    (define (xy->location board x y)
      (define-values (canvas-width canvas-height)
        (let ((c (send board get-canvas)))
          (send c get-size)))
      (define-values (square-width square-height)
        (values (/ canvas-width 8) (/ canvas-height 8)))
      (define-values (rank file)
        (values (exact-truncate (/ y square-height))
                (exact-truncate (/ x square-width))))
      (rank-file->location rank file))

    ;; This function to find a chess piece at a specified location on the chess board, this
    ;; function simply iterates over all the pieces on the board, using get-location to
    ;; determine if the piece is the correct one.

    (define (piece-at-location board location)
      (let loop ((snip (send board find-first-snip)))
        (if snip
            (if (equal? location (send snip get-location))
                snip
                (loop (send snip next)))
            #f)))

   (define (draw-chess-board dc)
     (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
     (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
     (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
     (define-values (dc-width dc-height) (send dc get-size))
     (define cell-width (/ dc-width 8))
     (define cell-height (/ dc-height 8))
     (define margin 3)

     (send dc clear)
     (send dc set-brush brush)
     (send dc set-pen pen)
     (send dc set-font font)

      (for* ([row (in-range 8)] [col (in-range 8)]
              #:when (or (and (odd? row) (even? col))
                         (and (even? row) (odd? col))))
        (define-values [x y] (values (* col cell-width) (* row cell-height)))
        (send dc draw-rectangle x y cell-width cell-height))

      (for ([(rank index) (in-indexed '("8" "7" "6" "5" "4" "3" "2" "1"))])
        (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
        (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
        (send dc draw-text rank margin y))
    

      (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
        (define-values [w h _1 _2] (send dc get-text-extent file font #t))
        (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
        (send dc draw-text file x (- dc-height h margin))))
                         
;; A function that can highlight a square, the function will simply draw a square
;; at the specified location on the board with a brush% which is used to draw the background
;; and a pen to draw the outline

 (define (highlight-square dc location color-name border-color-name)
   (define-values (rank file) (location->rank-file location))
   (define brush
     (if color-name
         (let* ((base (send the-color-database find-color color-name))
                (color  (make-object color% (send base red) (send base green) (send base blue) 0.3)))
            (send the-brush-list find-or-create-brush color 'solid))
         (send the-brush-list find-or-create-brush "black" 'transparent)))
   (define pen
     (if border-color-name
         (send the-pen-list find-or-create-pen border-color-name 2 'solid)
         (send the-pen-list find-or-create-pen "black" 1 'transparent)))
   (send dc set-pen pen)
   (send dc set-brush brush)
   (define-values (dc-width dc-height) (send dc get-size))
   (define-values (cell-width cell-height) (values (/ dc-width 8)(/ dc-height 8)))
   (send dc draw-rectangle (* file cell-width) (* rank cell-height) cell-width cell-height))

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