#lang racket/gui

(provide (all-defined-out))

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

 ))
