(ns sicp.chapter-1.section-2)

;; 1.9
(defn ++ [a b]
  (if (= a 0)
    b
    (inc (++ (dec a) b))))

;; This one is recursive. The last call of the function is `inc`, not
;; the function itself, so no TCO possible.
(comment

  (++ 2 2)
  (inc (++ 1 2))
  (inc (inc ++ 0 2))
  (inc (inc 2))
  (inc 3)
  4

  )


(defn ++ [a b]
  (if (= a 0)
    b
    (recur (dec a) (inc b))))

;; This one is iterative, because the last call of the function is the
;; function itself, so TCO is possible

(comment

  (++ 2 2)
  (++ 1 3)
  (++ 0 4)
  4

  )

;; 1.10

(defn A
  "Ackerman's function"
  [x y]
  (cond (= y 0) 0
        (= x 0) (* 2 y)
        (= y 1) 2
        :else (recur (dec x)
                     (A x (dec y)))))

(A 1 10)
;; => 1024

(A 2 4)
;; => 65536

(A 3 3)
;; => 65536

(defn f
  "2 * n"
  [n]
  (A 0 n))

(defn g
  "2^n"
  [n]
  (A 1 n))

(defn h
  "2^2^2... n times, can't compute for n > 4"
  [n]
  (A 2 n))

(defn k
  "5 * n^2"
  [n]
  (* 5 n n))
