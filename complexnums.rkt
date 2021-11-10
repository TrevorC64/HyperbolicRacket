#lang racket
(provide complex-num)
(provide show-i)
(provide conj)
(provide i+)
(provide i-)
(provide i*)


(define-struct complex-num (real im))

(define (show-i z)
  (cond
    [(< 0 (complex-num-im z))
     (format "~a + ~si" (complex-num-real z) (complex-num-im z))]
    [(> 0 (complex-num-im z))
     (format "~a - ~si" (complex-num-real z) (abs (complex-num-im z)))]
    [else
     (format "~a" (complex-num-real z))]))



(define (i+ z1 z2)
  (make-complex-num
   (+ (complex-num-real z1) (complex-num-real z2))
   (+ (complex-num-im z1) (complex-num-im z2))))

(define (i- z1 z2)
  (make-complex-num
   (- (complex-num-real z1) (complex-num-real z2))
   (- (complex-num-im z1) (complex-num-im z2))))

(define (i* z1 z2)
  (cond
    [(and (complex-num? z1) (number? z2))
     (make-complex-num
      (* z2 (complex-num-real z1))
      (* z2 (complex-num-im z1)))
     ]
    [(and (number? z1) (complex-num? z2))
     (make-complex-num
      (* z1 (complex-num-real z2))
      (* z1 (complex-num-im z2)))]
    [(and (complex-num? z1) (complex-num? z2))
     (make-complex-num
      (+ (* (complex-num-real z1) (complex-num-real z2))
         (* -1 (complex-num-im z1) (complex-num-im z2)))
      (+ (* (complex-num-real z1) (complex-num-im z2))
         (* (complex-num-im z1) (complex-num-real z2))))
     ]
    [(and (number? z1) (number? z2))
     (make-complex-num
      (* z1 z2)
      0)]
    [else (error "incorrect imputs in i*, given:" z1 z2)]))



(define (conj z)
  (make-complex-num
   (complex-num-real z)
   (* -1 (complex-num-im z))))
   