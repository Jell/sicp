(ns sicp.chapter2.section2
  (use sicp.core))

;; 2.17
(defn last-pair [[x & xs]]
  (if (empty? xs) x (recur xs)))

;; 2.18
(defn my-reverse [[x & xs]]
  (if (empty? xs)
    (list x)
    (concat (my-reverse xs) (list x))))

;; 2.19
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(defn no-more? [[c & cs]]
  (nil? c))

(defn first-denomination [[c & cs]] c)
(defn except-first-denomination [[c & cs]] cs)

(defn cc [amount coin-values]
  (cond (= amount 0) 1
        (or (< amount 0) (no-more? coin-values)) 0
        :else (+ (cc amount
                     (except-first-denomination coin-values))
                 (cc (- amount
                        (first-denomination coin-values))
                     coin-values))))

;; The order does not matter because we brute force test all the
;; combinations of coins.

;; 2.20
(defn same-parity [x & xs]
  (cons x (filter (if (odd? x) odd? even?) xs)))

;; 2.21
(defn square-list [items]
  (if (empty? items)
    items
    (cons (square (first items))
          (square-list (rest items)))))

(defn square-list [items]
  (map square items))

;; 2.22
(defn square-list [items]
  (letfn [(iter [things answer]
            (if (empty? things)
              answer
              (recur (rest things)
                     (cons (square (first things))
                           answer))))]
    (iter items '())))

;; The list is inversed because we build the answer by cons-ing the
;; squares one by one, so the first item ends up as the last one in
;; the answer.

;; Inversing the arguments of cons will not work, because we will
;; pair a list with a number, and no a number with a list, so we will
;; end up with nested lists instead.

;; 2.23
(defn for-each [f [x & xs]]
  (when x
    (f x)
    (recur f xs)))

;; -.-
(defn count-leaves [x]
  (cond (not (coll? x)) 1
        (empty? x) 0
        :else (+ (count-leaves (first x))
                 (count-leaves (rest x)))))

;; 2.24
(list 1 (list 2 (list 3 4)))

;;  +---+---+   +---+---+   +---+---+
;;  | o | o-+-->+ o | o-+-->| o | o |
;;  +-|-+---+   +-|-+---+   +-|-+-|-+
;;    |           |           |   |
;;    1           2           3   4


;;         X
;;        / \
;;       1   X
;;          / \
;;         2   X
;;            / \
;;           3   4

;; 2.25

((comp first rest first rest rest) '(1 3 (5 7) 8))

((comp first first) '((7)))

((apply comp (repeat 6 (comp first rest))) '(1 (2 (3 (4 (5 (6 7)))))))

;; 2.26

(def x '(1 2 3))
(def y '(4 5 6))

(concat x y)
;;=> (1 2 3 4 5 6)

(cons x y)
;;=> ((1 2 3) 4 5 6)

(list x y)
;; => ((1 2 3) (4 5 6))

;; 2.27
(defn deep-reverse [[x & xs]]
  (let [x' (if (coll? x) (deep-reverse x) x)]
    (if (empty? xs)
      (list x')
      (concat (deep-reverse xs) (list x')))))

(def x '((1 2) (3 4)))
(my-reverse x)
;;=> ((3 4) (1 2))
(deep-reverse x)
;;=> ((4 3) (2 1))

;; 2.28
(defn fringe [[x & xs]]
  (if (coll? x)
    (recur (concat x xs))
    (if (empty? xs)
      (list x)
      (cons x (fringe xs)))))

(def x '((1 2) (3 4)))
(fringe x)
;;=> (1 2 3 4)

;; 2.29
(defn make-mobile [left right]
  (list left right))

(defn make-branch [length structure]
  (list length structure))

;; a.
(defn left-branch [mobile]
  (first mobile))

(defn right-branch [mobile]
  (last mobile))

(defn branch-length [branch]
  (first branch))

(defn branch-structure [branch]
  (last branch))

;; b.
(def weight? (complement coll?))
(def mobile? coll?)

(defn total-weight [structure]
  (if (weight? structure)
    structure
    (let [lb (left-branch structure)
          ls (branch-structure lb)
          rb (right-branch structure)
          rs (branch-structure rb)]
      (+ (total-weight rs) (total-weight ls)))))

;; c.
(defn balanced? [mobile]
  (let [lb (left-branch mobile)
        ls (branch-structure lb)
        ll (branch-length lb)
        rb (right-branch mobile)
        rs (branch-structure rb)
        rl (branch-length rb)]
    (and (if (mobile? ls) (balanced? ls) true)
         (if (mobile? rs) (balanced? rs) true)
         (= (total-weight ls) (total-weight rs)))))

;; d.
;; No problemo!
