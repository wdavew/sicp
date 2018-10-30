(define (add-interval x y)
  (make-interval (+ (lower-bound x)
                    (lower-bound y))
                 (+ (upper-bound x)
                    (upper-bound y))))

(define (mult-interval x y)
  (let ((p1 (* (lower-bound x)
               (lower-bound y)))
        (p2 (* (lower-bound x)
               (upper-bound y)))
        (p3 (* (upper-bound x)
               (lower-bound y)))
        (p4 (* (upper-bound x)
               (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mult-interval x
                (make-interval
                 (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y)))))

;2.7
(define (make-interval a b) (cons a b))
(define (lower-bound x) (car x))
(define (upper-bound x) (cdr x))

;2.8
(define (sub-interval x y)
  (make-interval (- (lower-bound x)
                    (upper-bound y))
                 (- (upper-bound x)
                    (lower-bound y))))
;2.9
(define (width-interval x)
  (/ (abs (- (upper-bound x)
             (lower-bound x)))
     2.0))

(assert (= (width-interval
            (add-interval (make-interval -4.0 10)
                          (make-interval 8 20.0)))
           (+ (width-interval (make-interval -4.0 10))
              (width-interval (make-interval 8 20.0)))))

(assert (= (width-interval
            (sub-interval (make-interval -4.0 10)
                          (make-interval 8 20.0)))
           (+ (width-interval (make-interval -4.0 10))
              (width-interval (make-interval 8 20.0)))))

;so width(a + b) = width(a) + width(b)

;2.10
(define (safe-div-interval x y)
  (if (< (* (lower-bound y) (upper-bound y)) 0)
      (error "Cannot divide by an interval that spans 0")
      (div-interval x y)))

;2.11
;(-3 2) (-4 8) -- both cross 0 -- only case that requires > 2 multiplicaitons
;(-5 -3) (-3 -2) -- (15 6)
(define (mult-interval-2 x y)
  (cond
   ((and (negative? (* (lower-bound x) (upper-bound x)))
         (negative? (* (lower-bound y) (upper-bound y))))
    (make-interval (min
                    (* (lower-bound x) (upper-bound y))
                    (* (lower-bound y) (upper-bound x)))
                   (max
                    (* (lower-bound x) (lower-bound y))
                    (* (upper-bound x) (upper-bound y)))))
   ((and (negative? (lower-bound x))
        (negative? (upper-bound x))
        (negative? (lower-bound y))
        (negative? (upper-bound y)))
   (make-interval (* (upper-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
   ((and (positive? (lower-bound x))
        (positive? (upper-bound x))
        (positive? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (lower-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
   ((and (negative? (lower-bound x))
        (positive? (upper-bound x))
        (positive? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (upper-bound y))))
   ((and (positive? (lower-bound x))
        (positive? (upper-bound x))
        (negative? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))
   ((and (negative? (lower-bound x))
        (negative? (upper-bound x))
        (positive? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (lower-bound x) (upper-bound y)) (* (upper-bound x) (lower-bound y))))
   ((and (positive? (lower-bound x))
        (positive? (upper-bound x))
        (negative? (lower-bound y))
        (negative? (upper-bound y)))
   (make-interval (* (upper-bound x) (lower-bound y)) (* (lower-bound x) (upper-bound y))))
   ((and (negative? (lower-bound x))
        (negative? (upper-bound x))
        (negative? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (lower-bound x) (upper-bound y)) (* (lower-bound x) (lower-bound y))))
   ((and (positive? (lower-bound x))
        (positive? (upper-bound x))
        (negative? (lower-bound y))
        (positive? (upper-bound y)))
   (make-interval (* (upper-bound x) (lower-bound y)) (* (upper-bound x) (upper-bound y))))))


(assert (= (lower-bound
            (mult-interval-2 (make-interval -4.0 10)
                           (make-interval 8 20.0)))
           -80))
(assert (= (upper-bound
            (mult-interval-2 (make-interval -4.0 10)
                           (make-interval 8 20.0)))
           200))

(assert (= (lower-bound
            (mult-interval-2 (make-interval -10.0 4)
                           (make-interval 9 20.0)))
           -200))
(assert (= (upper-bound
            (mult-interval-2 (make-interval -10 4)
                           (make-interval 9 20.0)))
           80))

(assert (= (lower-bound
            (mult-interval-2 (make-interval -4.0 10)
                           (make-interval -9 20.0)))
           -90))
(assert (= (upper-bound
            (mult-interval-2 (make-interval -4.0 10)
                           (make-interval -9 20.0)))
           200))

(assert (= (lower-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval 9 20.0)))
           36))
(assert (= (upper-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval 9 20.0)))
           200))

(assert (= (lower-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval -9 -20.0)))
           -90))
(assert (= (upper-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval -9 -20.0)))
           -80))

(assert (= (lower-bound
            (mult-interval-2 (make-interval -10.0 -4)
                           (make-interval 9 20.0)))
           -200))
(assert (= (upper-bound
            (mult-interval-2 (make-interval -10 -4)
                           (make-interval 9 20.0)))
           -36))

(assert (= (lower-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval -8 20.0)))
           -80))
(assert (= (upper-bound
            (mult-interval-2 (make-interval 4.0 10)
                           (make-interval -8 20.0)))
           200))

;2.12
