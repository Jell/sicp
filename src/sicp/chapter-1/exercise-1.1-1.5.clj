(ns sicp.chapter-1)
;; 1.1

10 ; => 10

(+ 5 3 4) ; => 12

(- 9 1) ; => 8

(/ 6 2) ; => 3

(+ (* 2 4) (- 4 6)) ; => 6

(def a 3) ; => 'a

(def b (+ a 1)) ; => 'b

(+ a b (* a b)) ; => 19

(= a b) ; => false

(if (and (> b a) (< b (* a b)))
  b
  a)
;; => 4

(cond (= a 4) 6
      (= b 4) (+ 6 7 a)
      :else 25)
;; => 16

(+ 2 (if (> b a) b a)) ; => 6

(* (cond (> a b) a
         (< a b) b
         :else -1)
   (+ a 1))
;; => 16

;; 1.2

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 3 5)))))
   (* 3 (- 6 2) (- 2 7)))

;; 1.3

(defn max-two [max1 max2 & args]
  (let [head (first args) more (rest args)]
    (cond (= head nil)  [max1 max2]
          (< max1 max2) (recur max2 max1 args)
          (< max1 head) (recur head max1 more)
          (< max2 head) (recur max1 head more)
          :else         (recur max1 max2 more))))

(defn max-two-square-and-sum [& args]
  (apply + (map #(* % %) (apply max-two args))))

;; 1.4

(defn a+abs-b [a b]
  ((if (> b 0) + -) a b))

;; Compound expression: choose to apply + or - based on the sign of b

;; 1.5

(defn p [] (p))

(defn t [x y]
  (if (= 0 x)
    0
    y))

;; then you don't want to run (t 0 (p)) in normal order evaluation (infinite
;; recursion), but it's ok in applicative order evalutation because only x needs to
;; be resolved.
