; helpers
; derivative
(define dx 0.0000001)
(define (deriv f)
  (lambda (x) (/
    (- (f (+ x dx)) (f x))
    dx)
    )
  )

(define sqrtp (deriv (lambda (y) (* y y))))
(newline)
(display (sqrtp 3)) ; should be 6
(newline)


; newton's method transform
(define (newton f) 
  (lambda (x) (-
    x
    (/ (f x)
       ((deriv f) x)))))

; fixed-point
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (let ((next (f guess)))
            (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

; fixed-point-transform
(define (fixed-point-transform g transform guess)
  (fixed-point (transform g) guess)
  )
;1.40 use newton's method to approximate zeros of x^3 + ax^2 + bx + c
(define (cubic a b c) (lambda (x) (+ (expt x 3) (* a (expt x 2)) (* b x) c)))
(display (fixed-point-transform (cubic 2 3 4) newton 1)) ; should be close to -1.6506
(newline)

;1.41 double higher order procedure
(define (double g) (lambda (x) (
  g (g x)
  )
))

(define (inc x) (+ x 1))
(display (((double (double double)) inc) 5)) ; should be 16
(newline)

;1.42 defining compose
(define (compose f g) (lambda (x)
  (f (g x))
  )
)
(display ((compose square inc) 6))  ; should be 49
(newline)

;1.43 nth repeated application
(define (repeated f n) 
  (if (= n 1) f
    (repeated (compose f f) (- n 1))
        )
  )
(display ((repeated square 2) 5)) ; should be 625
(newline)

;n-fold smoothing
(define (smooth f) (lambda (x)
  (/ (+ 
    (f (- x dx))
    (f x)
    (f (+ x dx))
    ) 3
  )
))

(define (n-foldsmooth f n) 
  ((repeated smooth n) f)
)
(define (impulse-maker a y)
(lambda (x)
(if (= x a)
y
0)))

(define my-impulse-function
 (impulse-maker 0 6.0))

(display (my-impulse-function 0))
(newline)

(display ((n-foldsmooth my-impulse-function 4) 0.0))
(newline)

(define (average x y) (/ (+ x y) 2))
(define (identity f) f)
(define (average-damp f) (lambda (x) (average (f x) x)))
(define (nth-root n y) 
  (fixed-point-transform
      (lambda (x) (/ y (expt x (- n 1))))
      (repeated average-damp 4)
      1.0
    )
)

(display "hi")
(newline)
(display (nth-root 5 32))
(newline)
(display (nth-root 6 64))
(newline)
(display (nth-root 7 128))
(newline)
(display (nth-root 8 256))
(newline)
(display (nth-root 9 512))
(newline)
(display (nth-root 10 1024))
(newline)
(display (nth-root 11 2048))
(newline)
(display (nth-root 12 4096))
(newline)
(display (nth-root 13 8192))
(newline)
(display (nth-root 14 16384))
(newline)
(display (nth-root 15 32768))
(newline)
(display (nth-root 16 65536))
(newline)
(display "should work with 3 dampings")
(newline)
(display (nth-root 31 2147483648))
(newline)
(display "should not work with fewer than 4 dampings")
(newline)
(display (nth-root 32 4294967296))
(newline)
;complete nth-root procedure
(define (logbase2 x) (/ (log x) (log 2)))
(define (nth-root-safe n y) 
  (let ((damp-repeats 
    (ceiling (- (logbase2 (/ (+ n 1) 2)) 1)))
  )
  (newline)
  (fixed-point-transform
      (lambda (x) (/ y (expt x (- n 1))))
      (repeated average-damp damp-repeats)
      1.0
    ))
)

(display "should work for all n")
(newline)
(display (nth-root-safe 32 4294967296))
(display (nth-root-safe 36 68719476736))

;1.46 iterative improvement generalized procedure
(define (iterative-improve close-enough improve)
  (define (iter guess)
    (if (close-enough guess) guess
      (iter (improve guess))
      )
    )
  iter
  )

; new sqrt proceudre
(define (sqrt-iter-improve x) 
  ((iterative-improve
    (lambda (guess) (< (abs (- (square guess) x)) 0.00001))
    (lambda (guess) (average guess (/ x guess)))
   ) 1.0)
)

; new fixed-point procedure
(define (fixed-point-new f) 
  ((iterative-improve
    (lambda (guess) (< (abs (- (f guess) guess)) 0.000001))
    f
   ) 1.0)
)

(define (new-sqrt y) (
  fixed-point-new (average-damp (lambda (x) (/ y x)))
))

(newline)
(display (sqrt-iter-improve 4))
(newline)
(display (new-sqrt 4))
(newline)
(display "done")
