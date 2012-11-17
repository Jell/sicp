(ns sicp.chapter2.section1
  (use sicp.core))

;; 2.1
(defn make-rat [n d]
  (let [sign (if (pos? (* n d)) 1 -1)
        n' (abs n)
        d' (abs d)
        g (gcd n' d')]
    [(* sign (/ n' g)) (/ d' g)]))

(defn numer [[n d]] n)

(defn denom [[n d]] d)


(defn print-rat [[n d]]
  (println n "/" d))

(defn add-rat [x y]
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn sub-rat [x y]
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(defn mul-rat [x y]
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(defn div-rat [x y]
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(defn equal-rat [x y]
  (= (* (numer x) (denom y))
     (* (denom x) (numer y))))

;; 2.2

(defn make-point [x y] [x y])
(defn x-point [[x y]] x)
(defn y-point [[x y]] y)

(defn average-point [a b]
  (make-point (/ (+ (x-point a) (x-point b)) 2)
              (/ (+ (y-point a) (y-point b)) 2)))

(defn make-segment [start-segment end-segment] [start-segment end-segment])
(defn start-segment [[start end]] start)
(defn end-segment [[start end]] end)

(defn midpoint-segment [segment]
  (average-point (start-segment segment)
                 (end-segment segment)))

;; 2.3
(defn make-rectangle [point-a point-b] [point-a point-b])
(defn rectangle-width [[point-a point-b]]
  (abs (- (x-point point-a) (x-point point-b))))
(defn rectangle-height [[point-a point-b]]
  (abs (- (y-point point-a) (y-point point-b))))

(defn perimeter [rect]
  (* 2 (+ (rectangle-width rect)
          (rectangle-height rect))))

(defn area [rect]
  (* (rectangle-width rect)
     (rectangle-height rect)))

(defn make-rectangle [point width height] [point width height])
(defn rectangle-width [[point width height]] width)
(defn rectangle-height [[point width height]] height)

;; 2.4
(defn cons' [x y]
  (fn [m] (m x y)))
(defn car' [z]
  (z (fn [p q] p)))
(defn cdr' [z]
  (z (fn [p q] q)))

(comment
  (car' (cons' 1 2))
  (car' (fn [m] (m 1 2)))
  ((fn [m] (m 1 2)) (fn [p q] p))
  ((fn [p q] p) 1 2)
  1)

;; 2.5
;; 2 and 3 are prime numbers, and the fundamental theorem of
;; arithmetic states that for any pair a and b, 2^a x 3^b will be
;; unique.

(defn cons-int [a b]
  (bigint (* (Math/pow 2 a) (Math/pow 3 b))))
(defn car-int [n]
  (count (take-while even? (iterate #(/ % 2) n))))
(defn cdr-int [n]
  (let [odd-part (/ n (Math/pow 2 (car-int n)))]
    (count (take-while #(> % 1) (iterate #(/ % 3) odd-part)))))

;; 2.6
(defn church->int [church]
  ((church inc) 0))

(def zero (fn [f] (fn [x] x)))
(defn add-1 [n]
  (fn [f] (fn [x] (f ((n f) x)))))

(comment
  (add1 zero)
  (fn [f] (fn [x] (f ((zero f) x))))
  (fn [f] (fn [x] (f ((fn [x'] x') x))))
  (fn [f] (fn [x] (f x)))
  )
(def one (fn [f] (fn [x] (f x))))

(comment
  (add1 one)
  (fn [f] (fn [x] (f ((one f) x))))
  (fn [f] (fn [x] (f ((fn [x'] (f x')) x))))
  (fn [f] (fn [x] (f (f x))))
  )
(def two (fn [f] (fn [x] (f (f x)))))

(defn add [a b]
  (fn [f] (fn [x] ((a f) ((b f) x)))))

;; 2.7
(defn make-interval [a b] [a b])
(defn upper-bound [[a b]] (max a b))
(defn lower-bound [[a b]] (min a b))

;; 2.8
(defn add-interval [x y]
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(defn mul-interval [x y]
  (let [p1 (* (lower-bound x) (lower-bound y))
        p2 (* (lower-bound x) (upper-bound y))
        p3 (* (upper-bound x) (lower-bound y))
        p4 (* (upper-bound x) (upper-bound y))]
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(defn div-interval [x y]
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

(defn sub-interval [x y]
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; 2.9
(defn width [x]
  (/ (- (upper-bound x)
        (lower-bound x))
     2))

(comment
  (width (add-interval x y))
  = (width (make-interval (+ (lower-bound x) (lower-bound y))
                          (+ (upper-bound x) (upper-bound y))))

  = (/ (- (upper-bound (make-interval (+ (lower-bound x) (lower-bound y))
                                      (+ (upper-bound x) (upper-bound y))))
          (lower-bound (make-interval (+ (lower-bound x) (lower-bound y))
                                      (+ (upper-bound x) (upper-bound y)))))
       2)

  = (/ (- (+ (upper-bound x) (upper-bound y))
          (+ (lower-bound x) (lower-bound y)))
       2)

  = (/ (+ (- (upper-bound x) (lower-bound x))
          (- (upper-bound y) (lower-bound y)))
       2)

  = (+ (/ (- (upper-bound x) (lower-bound x)) 2)
       (/ (- (upper-bound y) (lower-bound y)) 2))

  = (+ (width x) (width y))
  )

;; It is not true for the multiplications or divisions:

(comment
  (width (make-interval  0 2)) = 1
  (width (make-interval -2 0)) = 1
  (width (make-interval -4 0)) = 2
  )

;; 2.10

(defn div-interval [x y]
  {:pre [(pos? (width y))]}
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

;; 2.11

;; 9 cases:

;; + + + +
;; + + - +
;; + + - -
;; - + + +
;; - + - +
;; - + - -
;; - - + +
;; - - - +
;; - - - -
(defn mul-interval [x y]
  (let [xmin (lower-bound x)
        xmax (upper-bound x)
        ymin (lower-bound y)
        ymax (upper-bound y)]
    (cond (and (>= xmin 0) (>= xmax 0) (>= ymin 0) (>= ymax 0))
          (make-interval (* xmin ymin) (* xmax ymax))

          (and (>= xmin 0) (>= xmax 0) (<= ymin 0) (>= ymax 0))
          (make-interval (* xmax ymin) (* xmax ymax))

          (and (>= xmin 0) (>= xmax 0) (<= ymin 0) (<= ymax 0))
          (make-interval (* xmax ymin) (* xmin ymax))

          (and (<= xmin 0) (>= xmax 0) (>= ymin 0) (>= ymax 0))
          (make-interval (* xmin ymax) (* xmax ymax))

          (and (<= xmin 0) (>= xmax 0) (<= ymin 0) (>= ymax 0))
          (make-interval (min (* xmax ymin) (* xmin ymax))
                         (max (* xmin ymin) (* xmax ymax)))

          (and (<= xmin 0) (>= xmax 0) (<= ymin 0) (<= ymax 0))
          (make-interval (* xmax ymin) (* xmin ymin))

          (and (<= xmin 0) (<= xmax 0) (>= ymin 0) (>= ymax 0))
          (make-interval (* xmin ymax) (* xmax ymin))

          (and (<= xmin 0) (<= xmax 0) (<= ymin 0) (>= ymax 0))
          (make-interval (* xmin ymax) (* xmin ymin))

          (and (<= xmin 0) (<= xmax 0) (<= ymin 0) (<= ymax 0))
          (make-interval (* xmax ymax) (* xmin ymin)))))

;; 2.12

(defn make-center-width [c w]
  (make-interval (- c w) (+ c w)))

(defn center [i]
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(defn width [i]
  (/ (- (upper-bound i) (lower-bound i)) 2))

(defn make-center-percent [c p]
  (make-center-width c (* c (/ p 100.0))))

(defn percent [i]
  (* 100 (/ (width i) (center i))))

;; 2.13

;; Let's define two intervals A and B, such as:
;; A = c_a - w_a, c_a + w_a
;; B = c_b - w_b, c_b + w_b
;; where all values are positive and w << c

;; Let C = A * B

;; from 2.10:
;; C = (c_a-w_a)(c_b-w_b),
;;     (c_a+w_a)(c_b+w_b)

;; C = c_a*c_b - c_a*w_b - w_a*c_b + w_a*wb,
;;     c_a*c_b + c_a*w_b + w_a*c_b + w_a*wb,

;; w << c, so:
;; C = c_a*c_b - c_a*w_b - w_a*c_b,
;;     c_a*c_b + c_a*w_b + w_a*c_b,

;; C = c_a*c_b - (c_a*w_b + w_a*c_b),
;;     c_a*c_b + (c_a*w_b + w_a*c_b),

;; (center C) = c_a*c_b
;; (width C)  = c_a*w_b + w_a*c_b

;; (percent C) = 100 * (width C) / (center C)

;; (percent C) = 100 * (c_a*w_b + w_a*c_b) / (c_a * c_b)

;; (percent C) = 100 * w_a / c_a + 100 * w_b / c_b

;; (percent C) = (percent A) + (percent B)

;; 2.14
(defn par1 [r1 r2]
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(defn par2 [r1 r2]
  (let [one (make-interval 1 1)]
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))

(def a (make-interval 100 10))
(def b (make-interval 100 5))

(par1 a a)
;; => [0.5 500]
(center (par1 a a))
;; => 250.25

(par2 a a)
;; => [0.5 50]
(center (par2 a a))
;; => 27.5

(div-interval a a)
;; =>  [0.1 10.0]
(div-interval a b)
;; =>  [0.1 20.0]

;; 2.15
;; par2 minimizes the numbers of operations that enlargen the width of
;; the interval. "one" has a width of zero, so the only operation
;; enlarging the interval with par2 is add-interval, whereas in par1
;; there are 3 operations enlarging the interval.

;; 2.16
;; Nope.
;; http://en.wikipedia.org/wiki/Interval_arithmetic#Dependency_problem
