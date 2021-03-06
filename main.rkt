#lang racket
(require 2htdp/image)
(require lang/posn)
(require 2htdp/universe)

(define-struct hyp-circle (center r))
; (make-hyp-circle (make-posn x 0) y)
(define points empty)
(define circles empty)

(define colors '("red" "yellow" "green" "blue" "pink" "purple"))

; creates the upper half-plane
(define hp-graph
  (scene+line
   (empty-scene 500 500)
   0
   450
   500
   450
   (pen "black" 5 "solid" "round" "bevel")))

; converts posns (x, y) where x, y ∈ [-1, 1] to fit in the plane
(define (normalize p)
  (make-posn
   ; we want x = 0 -> 250
   (+ 250 (posn-x p))
   
   ; we want y = 0 -> 450
   (- 450 (posn-y p))
   ))

; converts all posns
(define (normalize-all lop)
  (map normalize lop))

; draws all posns on the given w, here the upper-half plane
(define (draw-points w)
  (foldl
    (lambda (p img)
      (place-image (circle 2 "solid" "red")
                   (posn-x p) (posn-y p)
                   img))
    w
    (normalize-all points)))

; displays the current w, in this case, the upper-half plane
(define (draw w)
  (place-image
   (rectangle 500 50 "solid" "white")
   250
   475
   (draw-points
    (draw-circles w))))

; adds all defined circle arcs to the given w, in this case the upper-half plane
(define (draw-circles w [color "black"]) 
  (foldl
   (lambda (c img)
     (define normed (normalize (hyp-circle-center c)))
     (cond
       [(eq? -1 (hyp-circle-r c))
        (scene+line
         img
         (posn-x normed)
         (posn-y normed)
         (posn-x normed)
         0 "black")]
       [else (place-image (circle (hyp-circle-r c)
                                  "outline" color)
                          (posn-x normed) (posn-y normed)
                          img)]))
   w
   circles))


; generate 2 circles which have a boundary at the same line l
(define (generate-circles r l)
  (set! circles (cons (make-hyp-circle (make-posn (+ l r) 0) r)
         (cons (make-hyp-circle (make-posn (- l r) 0) r)
                (cons (make-hyp-circle (make-posn l 0) -1)
                       circles)))))

;(generate-circles 200 0)
;(generate-circles 100 0)
;(generate-circles 50 0)
;(generate-circles 25 0)
;(generate-circles 15.5 0)

; generates multiple circles centered at x, with radius <= r
(define (do-circle-generation r x)
  (cond
    [(< r 15) (generate-circles r x)]
    [else (begin
            (generate-circles r x)
            (do-circle-generation (* r .75) x))
          ]))

;where n must be a factor of the total width
; so if width = 500, n= {1, 2, 4, 5, ...}
(define (tile n r)
  (map (curry do-circle-generation r)
   (list (floor (/ 500 n)))))

; total is acting as the +- bound
; so for width 500, total = 250
#;
(define (partition curr bound part)
  (cond
    [(>= curr bound) empty]
    [else (cons 2 3)]))




#;
(map (curry do-circle-generation 200)
       (list -166 0 166))





