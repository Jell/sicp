(ns sicp.chapter1.section3
  (use sicp.core))

;; 1.29
(defn sum [term a succ b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (succ a) succ b))))

(defn integral [f a b dx]
  (* (sum f (+ a (/ dx 2.0)) #(+ % dx) b)
     dx))

(defn simpson-integral [f a b n]
  {:pre [(even? n)
         (> n 0)]}
  (let [h (/ (- b a) n)
        y (fn [k] (f (+ a (* k h))))
        succ (fn [i] (* (y i)
                        (cond (= 0 i) 1
                              (= n i) 1
                              (odd? i) 4
                              (even? i) 2)))]
    (* (/ h 3)
       (sum succ 0 inc n))))

(integral cube 0 1 0.001)
;; => 0.249999875000001
(simpson-integral cube 0 1 2)
;; => 1/4 !!!

;; 1.30
(defn sum [term a succ b]
  ((fn [a result]
      (if (> a b)
        result
        (recur (succ a) (+ result (term a)))))
    a 0))

;; 1.31
;; Recursive:
(defn product [term a succ b]
  (if (> a b)
    1
    (* (term a)
       (product term (succ a) succ b))))

;; Iterative:
(defn product [term a succ b]
  ((fn [a result]
      (if (> a b)
        result
        (recur (succ a) (* result (term a)))))
    a 1))

(def factorial (partial product identity 1 inc))

(defn quarter-pi [n]
  (let [term #(float (if (even? %)
                       (/ % (dec %))
                       (/ (dec %) %)))]
    (product term 3 inc (+ n 3))))

(* 4 (quarter-pi 100000))
;; => 3.1416048989792253

;; 1.32
;; Recursive
(defn accumulate [combiner null-value term a succ b]
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (succ a) succ b))))

;; Iterative
(defn accumulate [combiner null-value term a succ b]
  ((fn [a result]
      (if (> a b)
        result
        (recur (succ a) (combiner result (term a)))))
   a null-value))

(def sum (partial accumulate + 0))
(def product (partial accumulate * 1))

;; 1.33

(defn filtered-accumulate [combiner null-value term a succ b filter-fn]
  ((fn [a result]
     (if (> a b)
       result
       (if (filter-fn a)
         (recur (succ a) (combiner result (term a)))
         (recur (succ a) result))))
   a null-value))

(defn sum-square-primes [a b]
  (filtered-accumulate + 0 square a inc b prime?))

(reduce + (map square (filter prime? (range 1 10))))
;; => 88
(sum-square-primes 1 10)
;; => 88

(defn product-relative-primes [n]
  (filtered-accumulate * 1 identity 1 inc n #(= (gcd % n) 1)))

(reduce * (filter #(= (gcd % 10) 1) (range 1 10)))
;; => 189
(product-relative-primes 10)
;; => 189

;; 1.34

(defn f [g]
  (g 2))

(f square)
(f #(* % (+ % 1)))

(comment
  (f f))
;; => Try to call 2 as a procedure. Hence an error.

;; 1.35

(def tolerance 0.00001)
(defn cos [x] (Math/cos x))
(defn sin [x] (Math/sin x))

(defn fixed-point [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (< (Math/abs (- v1 v2)) tolerance))
          (try-value [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try-value first-guess)))

(fixed-point cos 1.0)
(fixed-point (fn [y] (+ (sin y) (cos y))) 1.0)

;; phi² = phi + 1 => phi = 1 + 1/phi

(defn golden-ratio []
  (fixed-point (fn [phi] (+ 1 (/ 1 phi))) 1.0))

(golden-ratio)
;; => 1.6180327868852458

;; 1.36
(defn fixed-point* [f first-guess]
  (letfn [(close-enough? [v1 v2]
            (prn v1)
            (< (Math/abs (- v1 v2)) tolerance))
          (try-value [guess]
            (let [next (f guess)]
              (if (close-enough? guess next)
                next
                (recur next))))]
    (try-value first-guess)))

(fixed-point* (fn [x] (/ (Math/log 1000) (Math/log x))) 10.0)
;; Without average damping: 33 steps

(fixed-point* (fn [x] (/ (+ x (/ (Math/log 1000) (Math/log x)))
                         2)) 10.0)
;; With average damping: 9 steps

;; 1.37

(defn cont-frac [n d k]
  (letfn [(f [j x]
            (float (/ (n j)
                      (+ (d j) x))))
          (cont-frac-iter [j result]
            (if (zero? j)
              (f j result)
              (recur (dec j) (f j result))))]
    (cont-frac-iter k 0))
  )

(map #(/ 1 (cont-frac (constantly 1)
                      (constantly 1)
                      %))
     (range 1 20))
;; about 9 steps needed for a 4 decimals approximation

;; 1.38

(defn euler-d [n]
  (if (zero? (mod (+ 2 n) 3))
    (* 2 (/ (+ 2 n) 3))
    1))

(map euler-d (range 11))
;; => (1 2 1 1 4 1 1 6 1 1 8)

(+ 2 (cont-frac (constantly 1)
                euler-d
                10))

;; => 2.718281865119934

;; 1.39

(defn tan-cf [x k]
  (cont-frac (fn [n] (if (zero? n) x (- (square x))))
             (fn [n] (inc (* 2 n)))
             k))

(Math/tan 10)
;; => 0.6483608274590866
(tan-cf 10 100)
;; => 0.6483607
