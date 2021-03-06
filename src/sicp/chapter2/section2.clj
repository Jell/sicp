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

;; 2.30
(defn square-tree [[x & xs]]
  (let [x' (if (coll? x)
             (square-tree x)
             (square x))]
    (if (empty? xs)
      (list x')
      (cons x' (square-tree xs)))))

(square-tree '(1 (2 (3 4) 5) (6 7)))
;;=> (1 (4 (9 16) 25) (36 49)))

(defn square-tree [xs]
  (map #(if (coll? %)
          (square-tree %)
          (square %))
       xs))

(square-tree '(1 (2 (3 4) 5) (6 7)))
;;=> (1 (4 (9 16) 25) (36 49)))

;; 2.31
(defn tree-map [f xs]
  (map #(if (coll? %)
          (tree-map f %)
          (f %))
       xs))

(tree-map square '(1 (2 (3 4) 5) (6 7)))
;;=> (1 (4 (9 16) 25) (36 49)))

;; 2.32
(defn subsets [s]
  (if (empty? s)
    (list ())
    (let [r (subsets (rest s))]
      (concat r (map (partial cons (first s)) r)))))

(subsets '(1 2 3))
;;=> (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;; 2.33

(defn my-map [f xs]
  (accumulate (fn [x accu] (cons (f x) accu)) '() xs))

(defn append [seq1 seq2]
  (accumulate cons seq2 seq1))

(defn my-length [seq]
  (accumulate (fn [_ accu] (inc accu)) 0 seq))

;; 2.34
(defn horner-eval [x coefficients]
  (accumulate (fn [this-coeff higher-terms]
                (+ this-coeff
                   (* higher-terms x)))
              0
              coefficients))

;; 2.35
(defn count-leaves [tree]
  (accumulate + 0 (map #(if (coll? %) (count-leaves %) 1) tree)))

;; 2.36
(defn accumulate-n [op init seqs]
  (if (empty? (first seqs))
    '()
    (cons (accumulate op init (map first seqs))
          (accumulate-n op init (map rest seqs)))))

;; 2.37
(defn dot-product [v w]
  (accumulate + 0 (map * v w)))

(defn matrix-*-vector [m v]
  (map #(accumulate + 0 (map * % v)) m))

(defn transpose [m]
  (accumulate-n cons '() m))

(defn matrix-*-matrix [m n]
  (let [cols (transpose n)]
    (map #(matrix-*-vector cols %) m)))

;; 2.38

(fold-right / 1 '(1 2 3))
;;=> 3/2
(fold-left / 1 '(1 2 3))
;;=> 1/6
(fold-right list '() '(1 2 3))
;;=> (1 (2 (3 ())))
(fold-left list '() '(1 2 3))
;;=> (((() 3) 2) 1)

;; For fold-left and fold-right to be equivalent, the operation should
;; be commutative.

;; 2.39
(defn my-reverse [sequence]
  (fold-left (fn [x y] (cons y x)) '() sequence))

(defn my-reverse [sequence]
  (fold-right (fn [x y] (concat y (list x))) '() sequence))

;; 2.40
(defn unique-pairs [n]
  (mapcat (fn [i]
            (map (fn [j] (list j i))
                 (range (inc i) (inc n))))
          (range 1 (inc n))))

(defn prime-sum? [pair]
  (prime? (+ (first pair)
             (second pair))))

(defn make-pair-sum [pair]
  (list (first pair)
        (second pair)
        (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

;; 2.41
(defn ordered-triples [n]
  (mapcat (fn [i]
            (mapcat (fn [j]
                      (map (fn [k]
                             (list i j k))
                           (range (inc j) (inc n))))
                    (range (inc i) (inc n))))
          (range 1 (inc n))))

(defn ordered-triples-that-sums-to [n]
  (filter #(= (reduce + %) n)
          (ordered-triples (- n 3))))

;; 2.42
(defn make-position [row column] [row column])
(defn position-row [[row column]] row)
(defn position-column [[row column]] column)

(def empty-board '())
(defn adjoin-position [row column queens]
  (cons (make-position row column) queens))

(defn diagonal? [position1 position2]
  (let [x1 (position-column position1)
        y1 (position-row position1)
        x2 (position-column position2)
        y2 (position-row position2)]
    (cond (and (= y1 y2) (= x1 x2)) true
          (or (= y1 y2) (= x1 x2)) false
          :else (= 1 (abs (/ (- x2 x1)
                             (- y2 y1)))))))

(defn safe? [column positions]
  (let [position (first (filter #(= column (position-column %))
                                positions))
        other-positions (remove #{position} positions)
        row (position-row position)]
    (not (or (some #{row} (map position-row other-positions))
             (some #{column} (map position-column other-positions))
             (some (partial diagonal? position) other-positions)))))

(defn queens [board-size]
  (letfn [(queen-cols [k]
            (if (zero? k)
              (list empty-board)
              (filter (partial safe? k)
                      (mapcat (fn [rest-of-queens]
                                (map (fn [new-row]
                                       (adjoin-position new-row
                                                        k
                                                        rest-of-queens))
                                     (range 1 (inc board-size))))
                              (queen-cols (dec k))))))]
    (queen-cols board-size)))

(defn print-queens [qs]
  (let [board-size (count qs)
        empty-board (vec (repeat board-size
                                 (vec (repeat (inc board-size) "|   "))))
        full-board (reduce #(assoc-in %1 (mapv dec %2) "| X ") empty-board qs)]
    (prn)
    (doseq [row full-board]
      (println (apply str (repeat (+ 2 board-size) "---")))
      (println (apply str row)))
    (println (apply str (repeat (+ 2 board-size) "---")))))

(map print-queens (queens 6))

;; 2.43
(comment

  (mapcat (fn [new-row]
            (map (fn [rest-of-queens]
                   (adjoin-position new-row
                                    k
                                    rest-of-queens))
                 (queen-cols (dec k))))
          (range 1 (inc board-size)))

;; this is much slower because queen-cols is recomputed for every row, instead of once per column.
;; If the first algorithm ran in T, the slow one would run in T to the
;; power of the board size.
  )

;; 2.44
(declare beside)
(declare below)

(defn right-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (right-split painter (- n 1))]
      (beside painter (below smaller smaller)))))

(defn up-split [painter n]
  (if (= n 0)
    painter
    (let [smaller (up-split painter (- n 1))]
      (below painter (beside smaller smaller)))))

;; 2.45
(defn split [dir1 dir2]
  (fn [painter n]
     (if (= n 0)
         painter
         (let [smaller ((split dir1 dir2) painter (- n 1))]
           (dir1 painter (dir2 smaller smaller))))))
(def right-split (split beside below))
(def up-split (split below beside))

;; 2.46

(defn make-vect [x y] [x y])
(defn xcor-vect [[x y]] x)
(defn ycor-vect [[x y]] y)
(defn add-vect [v1 v2]
  (make-vect
   (+ (xcor-vect v1) (xcor-vect v2))
   (+ (ycor-vect v1) (ycor-vect v2))))

(defn sub-vect [v1 v2]
  (make-vect
   (- (xcor-vect v1) (xcor-vect v2))
   (- (ycor-vect v1) (ycor-vect v2))))

(defn scale-vect [s v]
  (make-vect
   (* s (xcor-vect v))
   (* s (ycor-vect v))))

;; 2.47
(defn make-frame [origin edge1 edge2] [origin edge1 edge2])
(defn origin-frame [[origin edge1 edge2]] origin)
(defn edge1-frame [[origin edge1 edge2]] edge1)
(defn edge2-frame [[origin edge1 edge2]] edge2)

;; 2.48
(defn make-segment [start end] [start end])
(defn start-segment [[start end]] start)
(defn end-segment [[start end]] end)

;; 2.49
(defn frame-coord-map [frame]
  (fn [v]
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(declare draw-line) ; TODO: implement draw-line?
(defn segments->painter [segment-list]
  (fn [frame]
    (doseq [segment segment-list]
      (draw-line
       ((frame-coord-map frame) (start-segment segment))
       ((frame-coord-map frame) (end-segment segment))))))

;; a)
def outline-segments

(def outline
  (segments->painter
   (list (make-segment (make-vect 0 0)
                       (make-vect 0 1))
         (make-segment (make-vect 0 0)
                       (make-vect 1 0))
         (make-segment (make-vect 1 0)
                       (make-vect 1 1))
         (make-segment (make-vect 0 1)
                       (make-vect 1 1)))))

;; b)
(def x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0)
                  (make-vect 1 1))
    (make-segment (make-vect 0 1)
                  (make-vect 1 0)))))
;; c)
(def diamond-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0.5)
                  (make-vect 0.5 0))
    (make-segment (make-vect 0 0.5)
                  (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1)
                  (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5)
                  (make-vect 0.5 0)))))
;; d)
(def wave-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0.85)
                  (make-vect 0.15 0.6))
    (make-segment (make-vect 0 0.65)
                  (make-vect 0.15 0.4))
    (make-segment (make-vect 0.3 0.65)
                  (make-vect 0.15 0.6))
    (make-segment (make-vect 0.3 0.6)
                  (make-vect 0.15 0.4))
    (make-segment (make-vect 0.3 0.65)
                  (make-vect 0.4 0.65))
    (make-segment (make-vect 0.3 0.6)
                  (make-vect 0.35 0.5))
    (make-segment (make-vect 0.4 0.65)
                  (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.5)
                  (make-vect 0.25 0))
    (make-segment (make-vect 0.4 0)
                  (make-vect 0.5 0.3))
    (make-segment (make-vect 0.5 0.3)
                  (make-vect 0.6 0))
    (make-segment (make-vect 0.35 0.85)
                  (make-vect 0.4 1))
    (make-segment (make-vect 0.6 1)
                  (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85)
                  (make-vect 0.6 0.65))
    (make-segment (make-vect 0.6 0.65)
                  (make-vect 0.75 0.65))
    (make-segment (make-vect 0.75 0.65)
                  (make-vect 1 0.35))
    (make-segment (make-vect 0.75 0)
                  (make-vect 0.6 0.45))
    (make-segment (make-vect 0.6 0.45)
                  (make-vect 1 0.15)))))


;; 2.50

(defn transform-painter [painter origin corner1 corner2]
  (fn [frame]
    (let [m (frame-coord-map frame)
          new-origin (m origin)]
      (painter
       (make-frame new-origin
                   (sub-vect (m corner1) new-origin)
                   (sub-vect (m corner2) new-origin))))))

(defn beside [painter1 painter2]
  (let [split-point (make-vect 0.5 0.0)]
    (let [paint-left (transform-painter painter1
                                        (make-vect 0.0 0.0)
                                        split-point
                                        (make-vect 0.0 1.0))
          paint-right (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.0)
                                         (make-vect 0.5 1.0)) ]
      (fn [frame]
        (paint-left frame)
        (paint-right frame)))))


(defn rotate90 [painter]
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))

(def rotate180 (comp rotate90 rotate90))
(def rotate270 (comp rotate180 rotate90))

(defn flip-vert [painter]
  (transform-painter painter
                     (make-vect 0 1)
		     (make-vect 1 1)
		     (make-vect 0 0)))

(defn flip-horiz [painter]
  (transform-painter painter
                     (make-vect 1 0)
		     (make-vect 0 0)
		     (make-vect 1 1)))

;; 2.51

;; a)
(defn below [painter1 painter2]
  (let [split-point (make-vect 0.0 0.5)]
    (let [paint-below (transform-painter painter1
                                         (make-vect 0.0 0.0)
                                         (make-vect 1.0 0.0)
                                         split-point)
          paint-above (transform-painter painter2
                                         split-point
                                         (make-vect 1.0 0.5)
                                         (make-vect 0.0 1.0))]
      (fn [frame]
        (paint-below frame)
        (paint-above frame)))))

;; b)
(defn below [painter1 painter2]
  (rotate270 (beside (rotate90 painter2)
                     (rotate90 painter1))))

;; 2.52

;; a)

;; Add a frame around the dude
(def wave-painter
  (segments->painter
   (list
    (make-segment (make-vect 1 0)
                  (make-vect 0 0))
    (make-segment (make-vect 1 1)
                  (make-vect 1 0))
    (make-segment (make-vect 0 1)
                  (make-vect 1 1))
    (make-segment (make-vect 0 0)
                  (make-vect 0 1))
    (make-segment (make-vect 0 0.85)
                  (make-vect 0.15 0.6))
    (make-segment (make-vect 0 0.65)
                  (make-vect 0.15 0.4))
    (make-segment (make-vect 0.3 0.65)
                  (make-vect 0.15 0.6))
    (make-segment (make-vect 0.3 0.6)
                  (make-vect 0.15 0.4))
    (make-segment (make-vect 0.3 0.65)
                  (make-vect 0.4 0.65))
    (make-segment (make-vect 0.3 0.6)
                  (make-vect 0.35 0.5))
    (make-segment (make-vect 0.4 0.65)
                  (make-vect 0.35 0.85))
    (make-segment (make-vect 0.35 0.5)
                  (make-vect 0.25 0))
    (make-segment (make-vect 0.4 0)
                  (make-vect 0.5 0.3))
    (make-segment (make-vect 0.5 0.3)
                  (make-vect 0.6 0))
    (make-segment (make-vect 0.35 0.85)
                  (make-vect 0.4 1))
    (make-segment (make-vect 0.6 1)
                  (make-vect 0.65 0.85))
    (make-segment (make-vect 0.65 0.85)
                  (make-vect 0.6 0.65))
    (make-segment (make-vect 0.6 0.65)
                  (make-vect 0.75 0.65))
    (make-segment (make-vect 0.75 0.65)
                  (make-vect 1 0.35))
    (make-segment (make-vect 0.75 0)
                  (make-vect 0.6 0.45))
    (make-segment (make-vect 0.6 0.45)
                  (make-vect 1 0.15)))))

;; b)
(defn corner-split [painter n]
 (if (= n 0)
     painter
     (let [up (up-split painter (- n 1))
           right (right-split painter (- n 1))
           corner (corner-split painter (- n 1))]
       (beside (below painter up)
               (below right corner)))))

;; c)
(defn square-limit [painter n]
  (let [quarter (rotate180 (corner-split painter n))
        half (beside (flip-horiz quarter) quarter)]
    (below (flip-vert half) half)))
