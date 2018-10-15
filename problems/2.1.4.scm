;2.1.4
;alternate representation of pairs
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

(assert
 (= (car (cons 5 3))
    5))

(assert
 (= (cdr (cons 5 3))
    3))


;2.1.5
;represent pairs of nonnegative integers using just numbers
;and arithmetic operations so that the pair a b
;is the prodcut of 2^a*3^b

;2 and 3 are prime, so any a and b will result in unique number
; since it will be represented as a prime factorization

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (count-factors divisor num)
  (if
   (not (= (remainder num divisor) 0))
   0
   (+ 1 (count-factors divisor (/ num divisor)))))

(define (car z)
  (count-factors 2 z))

(define (cdr z)
  (count-factors 3 z))

;2.1.6
;Church numerals
;representation of numbers and arithmetic
;using just lambdas, no numbers
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

;fn to test with
(define (inc x)
  (+ x 1))

(assert
 (=
  ((zero inc) 0)
  0))

(assert
 (=
  ((one inc) 1)
  2))

(assert
 (=
  ((two inc) 2)
  4))

(assert
 (=
  (((add one one) inc) 2)
  4))

(assert
 (=
  (((add one two) inc) 3)
  6))

;just because
(define (church-mult a b)
  (lambda (f)
    (lambda (x) ((a (b f)) x))))

(define (church-expt a b)
  (lambda (f)
    (lambda (x) (((b a) f) x))))

(define three (add one two))

(assert
 (=
  (((church-mult two one) inc) 0)
  2))

(assert
 (=
  (((church-mult two two) inc) 0)
  4))

(assert
 (=
  (((church-expt two three) inc) 0)
  8))

(assert
 (=
  (((church-expt three three) inc) 0)
  27))
