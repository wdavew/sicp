; golden ratio phi is fixed point of x -> 1 + 1/x
(define tolerance 0.0001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2) 
    (< (abs (- v1 v2)) tolerance))
    (define (try guess)
      (display guess)
      (newline)
      (let ((next (f guess)))
            (if (close-enough? guess next)
              next
              (try next))))
    (try first-guess))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
(newline)
(newline)

; use fixed-point to find solution to x^x = 1000
(fixed-point (lambda (x) (/ (log 1000) (log x))) 1.1)
(newline)
(newline)

; compare with average damping
(fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 1.1)
(newline)
(newline)

; infinite continued fraction with Ni and Di == 1 produces 1/phi
(define (cont-frac n d k)
  (define (term i) 
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (term (+ 1 i))))
      )
    )
  (term 1))

(display (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20))
(newline)
(newline)

; iterative process of above continued fraction procedure
(define (cont-frac-iterm n d k)
  (define (term-iter remainder k) 
    (if (= k 1)
      (/ (n k) (+ (d k) remainder))
      (term-iter (/ (n k) (+ (d k) (remainder))) (- k 1)) 
      )
    )
  (term-iter 0))

(display (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           20))
(newline)
(newline)

; use cont-frac to approximate e using euler's expansion
(define (euler-di i)
  (cond
    ((= (remainder i 3) 2) (+ 2 (* 2 (/ (- i 2) 3))))
    (else 1)
    )
  )
        
(display (euler-di 5))
(newline)
(display (cont-frac (lambda (i) 1.0) euler-di 20))


; use cont-frac to appproximate tangent function using lambert's formula
(define (tan-cf x k)
  (cont-frac
    (lambda (i)
      (if (= i 1) (expt x i) ( - (expt x i))))
    (lambda (i) (- (* 2 i) 1))
    k
    )
  )

(newline)
(newline)
(display (tan-cf (/ 3.141596 4) 20))


