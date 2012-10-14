(ns sicp.core)

(defn square [x]
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
