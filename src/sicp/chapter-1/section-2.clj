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

;; 1.11

(defn f1
  "recursive definition"
  [n]
  (if (< n 3)
    n
    (+ (f1 (- n 1))
       (* 2 (f1 (- n 2)))
       (* 3 (f1 (- n 3))))))


;; f(0) = 0
;; f(1) = 1
;; f(2) = 2
;; f(3) = f(0) + 2*f(1) + 3*f(2)

(defn f2-iter [a b c count]
  (cond
   (= count 2) c
   (< count 2) count
   :else (recur b
                c
                (+ c
                   (* 2 b)
                   (* 3 a))
                (dec count))))

(defn f2
  "iterative definition"
  [n]
  (f2-iter 0 1 2 n))

;; 1.12

(defn pascal
  "pascal recursive"
  [n]
  { :pre [(< 0 n)] }
  (if (= n 1)
    [1]
    (let [previous (pascal (dec n))]
      (vec (map +
                (cons 0 previous)
                (conj previous 0))))))

;; 1.13

;; Let phi = (1 + sqrt(5)) / 2
;; Let psi = (1 - sqrt(5)) / 2
;; Axiom:
;; Fib(n) = (phi^n - psi^n) / sqrt(5)
;; Fib(n + 1) = (phi^(n + 1) - psi^(n + 1)) / sqrt(5)

;; Fib(n+2) = Fib(n+1) + Fib(n)
;; Fib(n+2) = (phi^n - psi^n) / sqrt(5) + (phi^(n+1) - psi^(n+1)) / sqrt(5)
;; Fib(n+2) = (phi^n + phi^(n+1) - psi^n - psi^(n+1)) / sqrt(5)
;; Fib(n+2) = (phi^n + phi^n * phi - psi^n - psi^n * psi) / sqrt(5)
;; Fib(n+2) = ((phi + 1) * phi^n - (psi + 1) * psi^n) / sqrt(5)
;; Fib(n+2) = (phi^2 * phi^n - psi^2 * psi^n) / sqrt(5)
;; Fib(n+2) = (phi^(n+2) - psi^(n+2)) / sqrt(5)

;; If true for n and n+1, true for n+2

;; True for Fib(0) and Fib(1), so true for all n > 1

;; |psi^n / sqrt(5)| < 1 for all n > 0, so
;; |Fib(n) - phi^n| < 1 for all n,
;; So Fib(n) is the closest integer to phi^n / sqrt(5)
