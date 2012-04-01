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
;; f(3) = f(2) + 2*f(1) + 3*f(0)

(defn f2-iter [n-3 n-2 n-1 count]
  (cond
   (= count 2) n-1
   (< count 2) count
   :else (recur n-2
                n-1
                (+ n-1
                   (* 2 n-2)
                   (* 3 n-3))
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

;; 1.14

;;                                        /(11 0)
;;                                 /(11 1)       /(10 0)
;;                                |       \(10 1)      /(9 0)
;;                                |              \(9 1)      /(8 0)
;;                                |                    \(8 1)      /(7 0)
;;                                |                          \(7 1)      /(6 0)
;;                                |                                \(6 1)      /(5 0)
;;                                |                                      \(5 1)      /(4 0)
;;                                |                                            \(4 1)      /(3 0)
;;                                |                                                  \(3 1)      /(2 0)
;;                                /                                                        \(2 1)      /(1 0)
;;                         /(11 2)                                                               \(1 1)
;;                         |      \                                                                    \(0 1)
;;                         |      |              /(6 0)
;;                         |      |        /(6 1)      /(5 0)
;;                         |      |       |      \(5 1)      /(4 0)
;;                         |      |       |            \(4 1)      /(3 0)
;;                         |      |       |                  \(3 1)      /(2 0)
;;                         |      |       |                        \(2 1)      /(1 0)
;;                         |      |       |                              \(1 1)
;;                         |      |       |                                    \(0 1)
;;                         |      |      /
;;                         |       \(6 2)
;;                         |             \             /(1 0)
;;                         |              |      /(1 1)
;;                         |              \ (1 2)      \(0 1)
;;                         |                    \             /(1 0)
;;                         |                     |      /(1 1)
;;                         |                      \(1 2)      \(0 1)
;;                         |                            \             /(1 0)
;;                         |                             |      /(1 1)
;;                         |                              \(1 2)      \(0 1)
;;                         |                                    \(-4 2)
;;                        /
;;                 /(11 3)
;;                 |      \
;;                 |       |      /(-9 3)
;;                 |        \(1 3)
;;                 |              \            /(1 0)
;;                 |              |      /(1 1)
;;                 |               \(1 2)      \(0 1)
;;                 |                     \             /(1 0)
;;                 |                      |      /(1 1)
;;                 |                       \(1 2)      \(0 1)
;;                 |                             \             /(1 0)
;;                 |                              |      /(1 1)
;;                 |                               \(1 2)      \(0 1)
;;                 |                                     \(-4 2)
;;                /
;;         /(11 4)
;;        |       \(-14 4)
;;       /
;; (11 5)
;;       \(-39 5)
;;

;; Space: O(n)
;; Time: O(n) for each coin, so O(n^5) in total

;; 1.15

(def counter (atom 0))

(defn cube [x]
  (* x x x))

(defn p [x]
  (swap! counter inc)
  (- (* 3 x) (* 4 (cube x))))

(defn sine [angle]
  (if (not (> (Math/abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)
(deref counter)
;; => 5

(def counter (atom 0))
(sine 10)
(deref counter)
;; => 5

(def counter (atom 0))
(sine 30)
(deref counter)
;; => 6

(def counter (atom 0))
(sine 90)
(deref counter)
;; => 7

(def counter (atom 0))
(sine 270)
(deref counter)
;; => 8

;; Conclusion: O(log(n) to the base 3)

;; 1.16
(defn square [n]
  (* n n))

(defn fast-exp-iter [a b n]
  (cond (= n 1) a
        (even? n) (recur (* a (square b)) b (/ n 2))
        :else (recur (* a b) b (- n 1))))

(defn fast-exp [b n]
  (fast-exp-iter 1 b n))

;; 1.17

(defn twice [n]
  (* 2 n))
(defn halve [n]
  (/ n 2))

(defn fast-*
  "recursive fast multiplication"
  [a b]
  (cond (= b 0) 0
        (even? b) (twice (fast-* a (halve b)))
        :else (+ a (fast-* a (- b 1)))))

;; 1.18
(defn fast-*-iter [accu a b]
  (cond (= b 0) accu
        (even? b) (recur accu (twice a) (halve b))
        :else (recur (+ accu a) a (- b 1))))

(defn fast-*-2
  "iterative fast multiplication"
  [a b]
  (fast-*-iter 0 a b))

;; 1.19

;; Tpq  = (a, b) -> bq + aq + ap , bp + aq
;; Tpq2 = (a, b) -> (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, (bp + aq)p + (bq + aq + ap)q
;;      = (a, b) -> bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2, bp^2 + apq + bq^2 + aq^2 + apq
;;      = (a, b) -> b(2pq + q^2) + a(2q^2 + 2pq + p^2 ), b(p^2 + q^2) + a(2pq + q^2)
;;      = (a, b) -> b(2pq + q^2) + a(p^2 + q^2) + a(2pq + q^2), b(p^2 + q^2) + a(2pq + q^2)

;; p' = (p^2 + q^2)
;; q' = (2pq + q^2)

(defn fib-iter [a b p q count]
  (cond (= count 0) b
        (even? count) (recur a
                             b
                             (+ (square p ) (square q))
                             (+ (* 2 p q) (square q))
                             (/ count 2))
        :else (recur (+ (* b q) (* a q) (* a p))
                     (+ (* b p) (* a q))
                     p
                     q
                     (- count 1))))

(defn fast-fib [n]
  (fib-iter 1 0 0 1 n))

;; 1.20

(defn gcd [a b]
  (if (= b 0)
    a
    (recur b (mod a b))))

;; Normal order
(comment

  (gcd 206 40)

  (if (= 40 0)
    206
    (recur 40 (mod 206 40)))

  (if (= (mod 206 40) 0)
    40
    (recur (mod 206 40)
           (mod 40 (mod 206 40))))
  ;; 1
  (if (= 6 0)
    40
    (recur (mod 206 40)
           (mod 40 (mod 206 40))))

  (if (= (mod 40 (mod 206 40)) 0)
    (mod 206 40)
    (recur (mod 40 (mod 206 40))
           (mod (mod 206 40) (mod 40 (mod 206 40)))))
  ;; 2
  (if (= 4 0)
    (mod 206 40)
    (recur (mod 40 (mod 206 40))
           (mod (mod 206 40) (mod 40 (mod 206 40)))))

  (if (= (mod (mod 206 40) (mod 40 (mod 206 40))) 0)
    (mod 40 (mod 206 40))
    (recur (mod (mod 206 40) (mod 40 (mod 206 40)))
           (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))
  ;; 4
  (if (= 2 0)
    (mod 40 (mod 206 40))
    (recur (mod (mod 206 40) (mod 40 (mod 206 40)))
           (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))))

  (if (= (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))) 0)
    (mod (mod 206 40) (mod 40 (mod 206 40)))
    (recur (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
           (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
  ;; 7
  (if (= 0 0)
    (mod (mod 206 40) (mod 40 (mod 206 40)))
    (recur (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40))))
           (mod (mod (mod 206 40) (mod 40 (mod 206 40))) (mod (mod 40 (mod 206 40)) (mod (mod 206 40) (mod 40 (mod 206 40)))))))
  ;; 4
  2
 )


;; Applicative order
(comment

  (gcd 206 40)

  (if (= 40 0) 206 (recur 40 (mod 206 40)))
  ;; 1
  (recur 40 6)

  (if (= 6 0) 40 (recur 6 (mod 40 6)))
  ;; 1
  (recur 6 4)

  (if (= 4 0) 6 (recur 4 (mod 6 4)))
  ;; 1
  (recur 4 2)

  (if (= 2 0) 4 (recur 2 (mod 4 2)))
  ;; 1
  (recur 2 0)

  (if (= 0 0) 2 (recur 0 (mod 2 0)))

  2

  )

;; Normal order: mod called 18 times
;; Applicative order: mod called 4 times

;; 1.21
(defn divides? [a b]
  (= (mod b a) 0))

(defn find-divisor [n test-divisor]
  (cond (> (square test-divisor) n) n
        (divides? test-divisor n) test-divisor
        :else (recur n (inc test-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(smallest-divisor 199)
;; => 199

(smallest-divisor 1999)
;; => 1999

(smallest-divisor 19999)
;; => 7

;; 1.22
(defn now []
  (. System nanoTime))

(defn prime? [n]
  (= n (smallest-divisor n)))

(defn report-prime [elapsed-time]
  (print " *** Elapsed time: " (/ elapsed-time 1000000.0) " msecs")
  true)

(defn start-prime-test [n start-time]
  (if (prime? n)
    (report-prime (- (now) start-time))))

(defn timed-prime-test [n]
  (newline)
  (print n)
  (start-prime-test n (now)))

(defn search-for-primes [min-value]
  (for [n (iterate inc min-value)
        :when (odd? n)
        :when (timed-prime-test n)]
    n))

(take 3 (search-for-primes 1000))
;; Elapsed time: 0.268 msec (too much jitter to be precise.)
;; (1009 1013 1019)

(take 3 (search-for-primes 10000))
;; Elapsed time: 0.936
;; (10007 10009 10037)

(take 3 (search-for-primes 100000))
;; Elapsed time: 1.81
;; (100003 100019 100043)

(take 3 (search-for-primes 1000000))
;; Elapsed time: 3.429
;; (1000003 1000033 1000037)

;; Waaaay too fast to draw any conclusion, too much jitter between
;; tests. Let's try with much larger numbers
(take 3 (search-for-primes (bigint 1e11)))
;; Elapsed time: 58ms
(take 3 (search-for-primes (bigint 1e12)))
;; Elapsed time: 187ms, ratio 3.22
(take 3 (search-for-primes (bigint 1e13)))
;; Elapsed time: 562ms, ratio 3.00

;; Conclusion: That's right, ordo sqrt(n)
