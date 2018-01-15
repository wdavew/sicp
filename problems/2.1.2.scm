;procedure to generate rational number that normalizes sign
(define (make-rat n d)
  (let ((g (gcd n d)))
  (if 
    (< d 0) (cons (- (/ n g)) (- (/ d g)))
    (cons (/ n g) (/ d  g))
    )
  )
)

(define (num x) (car x))
(define (denom x) (cdr x))
(define (print-rat n)
  (newline)
  (display (num n))
  (display "/")
  (display (denom n))
  )

(print-rat (make-rat 10 4))
(print-rat (make-rat -10 4))
(print-rat (make-rat -10 -4))
(print-rat (make-rat 10 -4))
(print-rat (make-rat 10 -7))
