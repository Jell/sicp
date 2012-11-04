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
