;2.2
; abstraction of line segments
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment p1 p2)
  (cons p1 p2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (print-point p)
  (newline)
  (display "(") 
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s) 
  (let ((x1 (x-point (start-segment s)))
       (x2 (x-point (end-segment s)))
       (y1 (y-point (start-segment s)))
       (y2 (y-point (end-segment s))))
    (make-point (/ (+ x1 x2) 2) (/ (+ y1 y2) 2))))
  
(print-point (midpoint-segment
    (make-segment (make-point 3 2) (make-point 4 -1))))

;2.3
;abstraction of rectangles
;method 1
(define (make-rec base height)
  (cons base height))

(define (rec-length rec)
    (abs
     (-
      (x-point (end-segment (car rec)))
      (x-point (start-segment (car rec))))))

(define (rec-height rec)
  (abs
   (-
    (y-point (end-segment (cdr rec)))
    (y-point (start-segment (cdr rec))))))

(define (perimeter rec)
  (+ (* 2 (rec-length rec)) (* 2 (rec-height rec))))

(define (area rec)
  (* (rec-length rec) (rec-height rec)))

;should be 6
(assert
 (=
  (area (make-rec
   (make-segment (make-point 2 3) (make-point 5 5))
   (make-segment (make-point 2 3) (make-point 2 5))))
  6))

;should be 10
(assert
 (=
  (perimeter (make-rec
    (make-segment (make-point 2 3) (make-point 5 5))
    (make-segment (make-point 2 3) (make-point 2 5))))
  10))

;alternate representation of rectangle
;because of the abstraction barrier between area / perimeter
;and the rectangle constructor and selectors,
;the original abstraction of area and perimeter works unchanged
(define (make-rec2 p1 p2)
  (cons p1 p2))

(define (rec-height rec)
  (abs
   (- (y-point (cdr rec))
      (y-point (car rec)))))

(define (rec-length rec)
  (abs
   (- (x-point (cdr rec))
      (x-point (car rec)))))

(assert
 (=
  (area (make-rec2
   (make-point 2 3) (make-point 5 5)))
  6))

;should be 10
(assert
 (=
  (perimeter (make-rec2
   (make-point 2 3) (make-point 5 5)))
  10))
