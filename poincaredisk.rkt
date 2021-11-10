#lang racket
(require 2htdp/image)
(require lang/posn)




(define points (list (make-posn 1 0)
                     (make-posn (cos (/ pi 4))
                                (sin (/ pi 4)))))

(define circles empty)

(define (normalize-pd p)
     (make-posn
      ; we want x = 0 -> 250
      (+ 250 (* (posn-x p) 200))
   
      ; we want y = 0 -> 250
      (- 250 (* (posn-y p) 200))))


(define (center-transform p q)
  (make-posn
   (/ (+ (/ (posn-x p)
            (+ (sqr (posn-x p))
               (sqr (posn-y p))))
         (/ (posn-x q)
            (+ (sqr (posn-x q))
               (sqr (posn-y q)))))
      2)
   (/ (+ (/ (posn-y p)
            (+ (sqr (posn-x p))
               (sqr (posn-y p))))
         (/ (posn-y q)
            (+ (sqr (posn-x q))
               (sqr (posn-y q)))))
      -2)))


(define (create-geodesic p1 p2)
  (set! circles (cons (center-transform p1 p2) circles)))



(define (normalize-all-pd lop)
  (map normalize-pd lop))

(define (draw-points w)
  (foldl
    (lambda (p img)
      (place-image (circle 2 "solid" "red")
                   (posn-x p) (posn-y p)
                   img))
    w
    (normalize-all-pd points)))

(define (draw-circles w)
  (foldl
   (lambda (p img)
     (place-image (circle 200 "outline" "red")
                  (posn-x p) (posn-y p)
                  img))
   w
  (normalize-all-pd circles)))


(define pd-graph
  (overlay
   (circle 200 "outline" "black")
   (empty-scene 500 500)))

(define (draw w)
  (draw-points w))

(define (draw-ray p img)
  (define trans-p (make-posn (* 3.5 (posn-x p))
                             (* 3.5 (posn-y p))))
  (scene+line img
   (posn-x (normalize-pd (make-posn 0 0)))
   (posn-y (normalize-pd (make-posn 0 0)))
   (posn-x (normalize-pd trans-p))
   (posn-y (normalize-pd trans-p))
   "red"))


(define p1 (make-posn 1 0))
(define p2 (make-posn (cos (/ pi 4))
           (sin (/ pi 4))))


(define (angle p)
  (cond
    [(eq? 0 (posn-x p)) (/ pi 2)]
    [else (atan (/ (posn-y p)
           (posn-x p)))]))

(define (r->d n)
  (* 180 (/ n pi)))


(define (norm-angle-in p a)
  (cond
    [(and (< 0 (posn-x p))
          (< 0 (posn-y p)))
     (- a 180)]
    [(and (> 0 (posn-x p))
          (< 0 (posn-y p)))
     (- a 90)]
    [(and (>= 0 (posn-x p))
          (>= 0 (posn-y p)))
     a]
    [(and (<= 0 (posn-x p))
          (>= 0 (posn-y p)))
     (+ a 90)]))


; theta of p1 MUST BE LESS THAN theta of p2
(define (draw-geodesic p1 p2 img)
  (scene+curve img
               (posn-x (normalize-pd p1))
               (posn-y (normalize-pd p1))
               (- (r->d (angle p1)) 180)
               1/2
               (posn-x (normalize-pd p2))
               (posn-y (normalize-pd p2))
               (if (< 0 (posn-x p2))
                   (r->d (angle p2))
                   (- (r->d (angle p2)) 180))
               1/2
               "black"))


; p is starting point
; theta gets incremented by inc each iteration
; img is what gets drawn on, in this case the poincare disk
(define (generate-geos p theta inc img)
  (cond
    [(> theta (* 2 pi)) img]
    [else
     (draw-geodesic p
                    (make-posn (cos theta)
                               (sin theta))
                    (generate-geos p (+ inc theta) inc img))]))

(define (generate-multiple-geos p inc1 inc2 adj img)
  (cond
    [(> inc2 (* 2 pi)) img]
    [else
     (generate-geos p 0 inc1
                    (generate-multiple-geos (make-posn
                                             (cos inc2)
                                             (sin inc2))
                                            inc1
                                            (+ inc2 adj) adj img))
     ]))




