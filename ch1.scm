; Exercise 1.1

10
12
8
3
10
; a = 3
; b = 4
21
false
4
16
6
16

; Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))
)

; Exercise 1.3
(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))
(define (sum-of-squares-largest-two x y z)
  (cond ((> x y) (if (> y z)
                     (sum-of-squares x y)
                     (sum-of-squares x z)
                 )
        )
        (else (if (> x z)
                  (sum-of-squares y x)
                  (sum-of-squares y z)
              )
        )
  )
)

(sum-of-squares-largest-two 1 2 3)
(sum-of-squares-largest-two 4 5 0)

; Exercise 1.4
(define (a-plus-abs-b a b)
  (if (> b 0)
    +
    -
  ) a b
)
; If b is positive, add a and b. If b is zero or negative, subtract a and b.

; Exercise 1.5
(define (p) (p))
(define (test x y)
  (if (= x 0)
      0
      y
  )
)

(test 0 (p))

; With applicative-order evaluation:
; (test 0 (p))
; (test 0 (p)) (p) evaluates to (p)
; (test 0 (p)) stuck here

; With normal-order evaluation:
; (test 0 (p))
; (if (= 0 0)
;     0
;     (p)
; )
; evaluates to 0

; Newton's method of successive approximations of finding square root
(define (square x) (* x x))
(define (abs x)
  (if (> x 0)
    x
    (- x)
  )
)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001)
)
(define (improved-guess guess x)
  (/ (+ (/ x guess) guess) 2)
)
(define (sqrt-inner guess x)
        (if (good-enough? guess x)
          guess
          (sqrt-inner (improved-guess guess x) x)
        )
)

(define (sqrt x) (sqrt-inner 1.0 x))

; 1.6
; Because of applicative-order evaluation, the arguments for new-if need to evaluate to a value before applying the procedure. Since one of the arguments is recursive, it will never evaluate to a value. We don't have this problem with if or cond because they are a special procedure where only the first expression might be evaluated if the predicate returns true. With new-if, the comparison is never even made since the argument has not evaluated into a value yet.

; 1.7
(define (abs x)
  (if (> x 0)
    x
    (- x)
  )
)
(define (good-enough? guess previous)
  (< (abs (- guess previous)) 0.1)
)
(define (improved-guess guess x)
  (/ (+ (/ x guess) guess) 2)
)
(define (sqrt-inner-diff guess previous x)
        (if (good-enough? guess previous)
          guess
          (sqrt-inner-diff (improved-guess guess x) guess x)
        )
)

(define (sqrt x) (sqrt-inner-diff 1.0 2 x))

; 1.8 cube root approximation
(define (square x) (* x x))
(define (cube x) (* x x x))
(define (abs x)
  (if (> x 0)
    x
    (- x)
  )
)
(define (good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001)
)
(define (improved-guess guess x)
  (/ (+ (/ x (square guess))
        (* guess 2)
     )
     3
  )
)
(define (cbrt-inner guess x)
        (if (good-enough? guess x)
          guess
          (cbrt-inner (improved-guess guess x) x)
        )
)

(define (cbrt x) (cbrt-inner 1.0 x))

; Exercise 1.9
(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))
  )
)

(+ 4 5)
(if (= 4 0)
    5
    (inc (+ (dec 4) 5))
)
(inc (+ (dec 4) 5))
(inc (+ 3 5))
(inc (inc (+ (dec 3) 5)))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ (dec 2) 5))))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ (dec 1) 5)))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
; Recursive

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))
  )
)

(+ 4 5)
(if (= 4 0)
    5
    (+ (dec 4) (inc 5))
)
(+ (dec 4) (inc 5))
(+ 3 6)
(+ (dec 3) (inc 6))
(+ 2 7)
(+ (dec 2) (inc 8))
(+ 1 8)
(+ (dec 1) (inc 9))
(+ 0 9)
9
; Iterative

; Exercise 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))
              )
        )
  )
)

A(x, y) = { 0    , y = 0
            2y   , x = 0
            2    , y = 1
            A(x - 1, A(x, y- 1)), otherwise



(A (- x 1) (A x (- y 1)))

(A 1 10)
(A (- 1 1) (A 1 (- 10 1)))
(A 0 (A 1 9))
(A 0 (A (- 1 1) (A 1 (- 9 1))))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 1 8)))
; ...
; 2^10 = 1024

(A 2 4)
(A (- 2 1) (A 2 (- 4 1)))
(A 1 (A 2 3))
(A 1 (A (- 2 1) (A 2 (- 3 1))))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A (- x 1) (A x (- y 1)))))
(A 1 (A 1 (A (- 2 1) (A 2 (- 2 1)))))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A (- 1 1) (A 1 (- 2 1)))))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 (* 2 2)))
(A 1 (A 1 4))
(A 1 (A (- 1 1) (A 1 (- 4 1))))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 A(1 2))))
(A 1 (A 0 (A 0 A( 0 A( 1 1 )))))
(A 1 (A 0 (A 0 A( 0 2))))
(A 1 (A 0 (A 0 (* 2 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 (* 2 4)))
(A 1 (A 0 8))
(A 1 (* 2 8))
(A 1 16)
(A 0 (A 1 15))

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 A(1 2))))
(A 1 (A 0 (A 0 A( 0 A( 1 1 )))))
(A 1 (A 0 (A 0 A( 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))

(A 1 2) -> (A 0 2)
(A 1 4) -> (A 0 8)
(A 1 16) -> (A 0 32768)

(A 1 2) -> 2^2
(A 1 4) -> 2^4
(A 1 16)-> 2^16

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (2^2)))
(A 1 (2^((2^2))))
2^(2^((2^2)))
2^(2^4)

(A 2 1) -> 2^1
(A 2 2) -> 2^2
(A 2 3) -> 2^4
(A 2 4) -> 2^16

(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
(A 2 (A 1 (A 2 1)))
(A 2 (A 1 2))
(A 2 (2^2))
(A 2 4)
2^16

(A 1 16) -> (A 2 4) -> A(3 3) = 2^16

(define (f, n) (A 0 n)) = 2n
(define (g, n) (A 1 n)) = 2^n
(define (h, n) (A 2 n)) = 2^(2^...) n times when n > 1
                          2, n = 1
                          0, n = 0
(A 2 2)
(A 1 (A 2 1))
(A 1 2)
2 ^ 2

(A 2 3)
(A 1 (A 2 2))
(A 1 4)
2 ^ (2 ^ 2)

(A 2 4)
2 ^ (2 ^ (2 ^ 2))



(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))
              )
        )
  )
)
