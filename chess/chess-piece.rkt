#lang racket/gui

(provide (all-defined-out))

(define chess-piece-snip-class
  (make-object
      (class snip-class%
        (super-new)
        (send this set-classname "chess-piece-snip"))))

(send (get-the-snip-class-list) add chess-piece-snip-class)

;; Extend chess piece to include valid moves
(define chess-piece%
  (class snip%
    (init-field name glyph font size moves [location #f])
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    ;; To determine the color we simply look at the piece name: if it is upper case,
    ;; it is a white piece, otherwise it is black.
    (define/public (color)
      (if (equal? (string-upcase name) name) 'white 'black))

    ; chess piece needs to know its location, we can store it directly into the chess-piece% object,
    ; as the location field with as setter and getter method.
    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)

    ;; returns a list of valid moves for this piece, considering the type of the piece,
    ;; its current location and the location of other pieces on the board
    (define/public (valid-moves)
      (let ((admin (send this get-admin)))
        (if (and admin location)             ; can be #f is this piece is not in play
            (let ((board (send admin get-editor)))
              (moves board location)) '()))) ;returns an empty list if piece is not on a board
    
    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
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

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Six move functions that encapsulates the movement rules for each piece
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define (valid-rank? rank) (and(>= rank 0) (< rank 8)))
    (define (valid-file? file) (and(>= file 0) (< file 8)))

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
    
    ;; A pawn can only move forward by 1 or two if starting from initial position
    ;; and cross-wise if is can/takes opposite piece.
    (define ((pawn-moves color) board location)
      (define direction (if (eq? color 'white) -1 1))
      (define-values (rank file) (location->rank-file location))
      (define moves '())
      (when (valid-rank? (+ rank direction))
        ;; can move if the destination square is not occupied
        (let ((candidate (rank-file->location (+ rank direction) file)))
          (unless (piece-at-location board location)
            (set! moves (cons candidate moves))
            (when (valid-rank? (+ rank direction))
              (when (or (and (eq? color 'white) (equal? rank 6))
                      (and (eq? color 'black) (equal? rank 1)))
              (let ((candidate (rank-file->location (+ rank direction direction) file)))
                (unless (piece-at-location board candidate)
                  (set! moves (cons candidate moves))))))))
        ;;can move forward left is that square is occupied
        (when (valid-file? (sub1 file))
          (let ((candidate (rank-file->location (+ rank direction) (sub1 file))))
            (let ((piece (piece-at-location board candidate)))
              (when (and piece (not (eq? color (send piece color))))
                (set! moves (cons candidate moves))))))
        ;;can move forward right is that square is occupied
        (when (valid-file? (add1 file))
          (let ((candidate (rank-file->location (+ rank direction) (add1 file))))
            (let ((piece (piece-at-location board candidate)))
              (when (and piece (not (eq? color (send piece color))))
                (set! moves (cons candidate moves)))))))
      moves)

    ;; Two generic method to move pieces by common rules and from offsets
    (define (valid-moves-by-offset color board location offsets)
      (define-values (rank file) (location->rank-file location))
      (for/fold ([moves '()])
                ([offset (in-list offsets)])
        (match-define (list roffset foffset) offset)
        (define-values (nrank nfile) (values (+ rank roffset) (+ file foffset)))
        (if (and (valid-rank? nrank) (valid-file? nfile))
            (let ((candidate (rank-file->location nrank nfile)))
              (let ((piece (piece-at-location board candidate)))
                (if (or (not piece) (not (eq? (send piece color) color)))
                    (cons candidate moves)
                    moves)))
            moves)))

    (define (valid-moves-by-direction color board location rank-direction file-direction)
      (define-values (rank file) (rank-file->location rank file))
      (define moves '())
      (define (check rank file)
        (let ((candidate (rank-file->location rank file)))
          (let ((target-piece (piece-at-location board candidate)))
            (when (or (not target-piece) (not (eq? (send target-piece color) color)))
              (set! moves (cons candidate moves)))
            (if target-piece #f #t))))
      (let loop ((nrank (+ rank rank-direction))
                  (nfile (+ file file-direction)))
        (when (and (valid-rank? nrank) (valid-file? nfile) (check nrank nfile))
          (loop (+ nrank rank-direction) (+ nfile file-direction))))
      moves)

    (define ((king-moves color) board location)
      (valid-moves-by-offset
       color board location
       '((-1 -2) (-1 2) (1 -2) (1 2) (-2 -1) (-2 1) (2 -1) (2 1))))
                 
    (define ((queen-moves color) board location)
      (append
       (valid-moves-by-direction color board location  1 0)
       (valid-moves-by-direction color board location -1 0)
       (valid-moves-by-direction color board location  0 1)
       (valid-moves-by-direction color board location  0 -1)
       (valid-moves-by-direction color board location  1 1)
       (valid-moves-by-direction color board location -1 1)
       (valid-moves-by-direction color board location  1 -1)
       (valid-moves-by-direction color board location -1 -1)))

    (define ((rook-moves color) board location)
      (append
       (valid-moves-by-direction color board location 1 0)
       (valid-moves-by-direction color board location -1 0)
       (valid-moves-by-direction color board location 0 1)
       (valid-moves-by-direction color board location 0 -1)))

    (define ((bishop-moves color) board location)
      (append
       (valid-moves-by-direction color board location 1 1)
       (valid-moves-by-direction color board location -1 1)
       (valid-moves-by-direction color board location 1 -1)
       (valid-moves-by-direction color board location -1 -1)))

    (define ((knight-moves color) board location)
      (valid-moves-by-offset
       color board location
       '((-1 -2) (-1 2) (1 -2) (1 2) (-2 -1) (-2 1) (2 -1) (2 1))))

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

    ;; Chess pieces desciption and glyph stored in a hashtable. Upper case
    ;; denotes a white piece.
    (define chess-piece-data
      (hash
       "K" (cons #\u2654 (king-moves 'white))
       "Q" (cons #\u2655 (queen-moves 'white))
       "R" (cons #\u2656 (rook-moves 'white))
       "B" (cons #\u2657 (bishop-moves 'white))
       "N" (cons #\u2658 (knight-moves 'white))
       "P" (cons #\u2659 (pawn-moves 'white))
       "k" (cons #\u265A (king-moves 'black))
       "q" (cons #\u265B (queen-moves 'black))
       "r" (cons #\u265C (rook-moves 'black))
       "b" (cons #\u265D (bishop-moves 'black))
       "n" (cons #\u265E (knight-moves 'black))
       "p" (cons #\u265F)(pawn-moves 'black)))

    ;; The chess game uses a “rank” and “file” coordinate system, where the rank represents the row
    ;; on the board and it is numbered from 1 to 8, 1 being at the bottom, while the file are the columns
    ;; of the board, labeled using letters from “a” to “h”.
    (define (make-chess-piece id [location #f])
      (match-define (cons glyph moves)(hash-ref chess-piece-data id))
      (define font (send the-font-list find-or-create-font 30 'default 'normal 'normal))
      (new chess-piece% [name id][glyph (string glyph)] [font font]
                        [size 45] [location location] [moves moves]))

    
 ))
