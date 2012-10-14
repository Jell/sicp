(ns sicp.core)

(defn abs [n]
  {:post [(<= 0 %)]}
  (if (< 0 n) n (- n)))

(defn average [& args]
  {:pre [(< 0 (count args))]}
  (/ (apply + args) (count args)))

(defn square [x]
  {:post [(<= 0 %)]}
  (* x x))

(defn cube [x]
  (* x x x))

(defn prime? [n]
  (letfn [(divides? [a b]
            (= (mod b a) 0))
          (find-divisor [n test-divisor]
            (cond (> (square test-divisor) n) n
                  (divides? test-divisor n) test-divisor
                  :else (recur n (inc test-divisor))))

          (smallest-divisor [n]
            (find-divisor n 2))]
    (= n (smallest-divisor n))))

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))
