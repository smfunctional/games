#lang racket/gui
(require embedded-gui)

(define chess-piece-snip-class
  (make-object
      (class snip-class%
        (super-new)
        (send this set-classname "chess-piece-snip"))))

(send (get-the-snip-class-list) add chess-piece-snip-class)

(define chess-piece%
  (class snip%
    (init-field glyph font size [location #f])
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    ; chess piece needs to know its location, we can store it directly into the chess-piece% object,
    ; as the location field with as setter and getter method.
    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)
    
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! width size))
      (when descent (set-box! space 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
       (send dc draw-text glyph (+ x ox) (+ y oy))))
 ))

; The chess game uses a “rank” and “file” coordinate system, where the rank represents the row
; on the board and it is numbered from 1 to 8, 1 being at the bottom, while the file are the columns
; of the board, labeled using letters from “a” to “h”.

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

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))
    
    ; chess pieces are added to the chess board, using the insert method, which allows placing a snip at any coordinate,
    ; or at (0, 0) if no coordinates are specified.
    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))

    ; position-piece takes a chess piece, finds the x, y coordinate for it on the chess board based on the location
    ; stored inside the snip and moves the piece to that location using the pasteboard%s move-to method.
    ; This function calculates the position based on the current canvas width and height, rather than pre-calculating
    ; the square positions, so it will work correctly regardless of the size of the board, or even if it is invoked when
    ; the size of the board changes
    
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

    ; The rank-file function used by position-piece, converts a chess board location
    ; into the row and column of the corresponding square on the board.
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
        (send dc draw-text file x (- dc-height h margin))))))
                         
  

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

    ;; Insert one of each of the chess pieces onto the board, so we can see them
    ;; and drag them around.
    (for ([id (in-hash-keys chess-piece-data)])
      (define piece (make-chess-piece id))
      (send board insert piece (random (* 50 6)) (random (* 50 6))))