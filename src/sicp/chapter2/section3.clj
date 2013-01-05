(ns sicp.chapter2.section3
  (use sicp.core clojure.string))

;; 2.53

(list 'a 'b 'c)
;;=> (a b c)
(list (list 'george))
;;=> ((george))
(first '((x1 x2) (y1 y2)))
;;=> (x1 x2)
(second '((x1 x2) (y1 y2)))
;;=> (y1 y2)
(coll? (first '(a short list)))
;;=> false
(memq 'red '((red shoes) (blue socks)))
;;=> false
(memq 'red '(red shoes blue socks))
;;=> true

;; 2.54
(defn equal? [a b]
  (and (= (coll? a)
          (coll? b))
       (if (not (coll? a))
         (= a b)
         (or (and (empty? a)
                  (empty? b))
             (and (equal? (first a)
                          (first b))
                  (equal? (rest a)
                          (rest b)))))))

;; 2.55
(first ''abracadabra)
;;=> quote
;; because ''abracadabra is just synctactic sugar for
;; (quote (quote abracadabra))

;; 2.56
(def variable? symbol?)

(defn same-variable? [v1 v2]
  (and (variable? v1)
       (variable? v2)
       (= v1 v2)))

(defn make-sum [a1 a2]
  (list '+ a1 a2))

(defn make-product [m1 m2]
  (list '* m1 m2))

(defn sum? [x]
  (and (coll? x)
       (= '+ (first x))))

(defn addend [s]
  (second s))

(defn augend [s]
  (last s))

(defn product? [x]
  (and (coll? x)
       (= '* (first x))))

(defn multiplier [p]
  (second p))

(defn multiplicand [p]
  (last p))

(defn deriv [expr var]
  (cond (number? expr) 0
        (variable? expr) (if (same-variable? expr var) 1 0)
        (sum? expr) (make-sum (deriv (addend expr) var)
                              (deriv (augend expr) var))
        (product? expr) (make-sum
                         (make-product (multiplier expr)
                                       (deriv (multiplicand expr) var))
                         (make-product (deriv (multiplier expr) var)
                                       (multiplicand expr)))
        :else "unknown expression"))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 2)) 'x)

(defn make-sum [a1 a2]
  (cond (= a1 0) a2
        (= a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list '+ a1 a2)))

(defn make-product [m1 m2]
  (cond (or (= m1 0) (= m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list '* m1 m2)))

;; 2.56
(defn make-exponentiation [base exponent]
  (cond (or (= base 1) (= base 0)) 1
        (= exponent 0) 1
        (= exponent 1) base
        (and (number? base) (number? exponent)) (Math/pow base exponent)
        :else (list '** base exponent)))

(defn exponentiation? [x]
  (and (coll? x)
       (= '** (first x))))

(defn base [e]
  (second e))

(defn exponent [e]
  (last e))

(defn deriv [expr var]
  (cond (number? expr) 0
        (variable? expr) (if (same-variable? expr var) 1 0)
        (sum? expr) (make-sum (deriv (addend expr) var)
                              (deriv (augend expr) var))
        (product? expr) (make-sum
                         (make-product (multiplier expr)
                                       (deriv (multiplicand expr) var))
                         (make-product (deriv (multiplier expr) var)
                                       (multiplicand expr)))
        (exponentiation? expr)
        (make-product (exponent expr)
                      (make-product
                       (make-exponentiation (base expr)
                                            (make-sum (exponent expr)
                                                      -1))
                       (deriv (base expr) var)))
        :else  "unknown expression"))

;; 2.57
(defn make-sum-list [l]
   (if (= (count l) 2)
       (list '+ (first l) (second l))
       (make-sum (first l) (make-sum-list (rest l)))))

(defn make-sum [a1 a2]
  (cond (= a1 0) a2
        (= a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (make-sum-list (list a1 a2))))

(defn make-product-list [l]
   (if (= (count l) 2)
       (list '* (first l) (second l))
       (make-product (first l) (make-product-list (rest l)))))

(defn make-product [m1 m2]
  (cond (or (= m1 0) (= m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (make-product-list (list m1 m2))))

(defn augend [s]
  (let [a (rest (rest s))]
     (if (= (count a) 1)
         (first a)
         (make-sum-list a))))

(defn multiplicand [p]
  (let [m (rest (rest p))]
     (if (= (count m) 1)
         (first m)
         (make-product-list m))))

;; 2.58
;; a)
(defn make-sum [a1 a2]
  (cond (= a1 0) a2
        (= a2 0) a1
        (and (number? a1) (number? a2)) (+ a1 a2)
        :else (list a1 '+ a2)))
(defn sum? [x]
  (and (coll? x)
       (= (second x) '+)))
(defn addend [s]
  (first s))
(defn augend [s] (nth s 2))

(defn make-product [m1 m2]
  (cond (or (= m1 0) (= m2 0)) 0
        (= m1 1) m2
        (= m2 1) m1
        (and (number? m1) (number? m2)) (* m1 m2)
        :else (list m1 '* m2)))
(defn product? [x]
  (and (coll? x) (= (second x) '*)))
(defn multiplier [p] (first p))
(defn multiplicand [p] (nth p 2))


(defn make-exponentiation [base exponent]
  (cond (or (= base 1) (= base 0)) 1
        (= exponent 0) 1
        (= exponent 1) base
        (and (number? base) (number? exponent)) (Math/pow base exponent)
        :else (list base '** exponent)))
(defn exponentiation? [x]
  (and (coll? x)
       (= '** (second x))))
(defn base [e]
  (first e))
(defn exponent [e]
  (nth e 2))

;; b)
(defn simplify [exp]
  (if (empty? (rest exp))
    (first exp)
    exp))
(defn augend [s]
  (simplify (rest (rest s))))
(defn multiplicand [p]
  (simplify (rest (rest p))))

;; 2.59
(defn element-of-set? [x s]
  (cond (empty? s) false
        (equal? x (first s)) true
        :else (element-of-set? x (rest s))))

(defn adjoin-set [x s]
  (if (element-of-set? x s)
    s
    (cons x s)))

(defn union-set [s1 s2]
  (if (empty? s2)
    s1
    (union-set (adjoin-set (first s2) s1)
               (rest s2))))

(union-set '(1 2 3) '(2 3 4))

;; 2.60

(defn element-of-set? [x s]
  (cond (empty? s) false
        (equal? x (first s)) true
        :else (element-of-set? x (rest s))))

(defn adjoin-set [x s]
  (cons x s))

(defn union-set [s1 s2]
  (if (empty? s2)
    s1
    (union-set (adjoin-set (first s2) s1)
               (rest s2))))

;; Faster adjoin, Fater union, slower tests.

;; 2.61

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (first s)) true
        (< x (first s)) false
        :else (element-of-set? x (rest s))))

(defn intersection-set [set1 set2]
  (if (or (empty? set1)
          (empty? set2))
    '()
    (let [x1 (first set1)
          x2 (first set2)]
      (cond (= x1 x2) (cons x1 (intersection-set (rest set1) (rest set2)))
            (< x1 x2) (intersection-set (rest set1) set2)
            (< x2 x1) (intersection-set set1 (rest set2))))))

(defn adjoin-set [x s]
  (cond (empty? s) (cons x '())
        (= x (first s)) s
        (< x (first s)) (cons x s)
        (> x (first s)) (cons (first s) (adjoin-set x (rest s)))))

;; 2.62
(defn union-set [set1 set2]
  (cond (empty? set1) set2
        (empty? set2) set1

        (= (first set1)
           (first set2))
        (cons (first set1)
              (union-set (rest set1) (rest set2)))

        (< (first set1)
           (first set2))
        (cons (first set1)
              (union-set (rest set1) set2))

        :else
        (cons (first set2)
              (union-set set1 (rest set2)))))

;; 2.63
(defn entry [tree]
  (first tree))
(defn left-branch [tree]
  (second tree))
(defn right-branch [tree]
  (last tree))
(defn make-tree [entry left right]
  (list entry left right))

(defn element-of-set? [x s]
  (cond (empty? s) false
        (= x (entry s)) true
        (< x (entry s)) (element-of-set? x (left-branch s))
        (> x (entry s)) (element-of-set? x (right-branch s))))

(defn adjoin-set [x s]
  (cond (empty? s) (make-tree x '() '())
        (= x (entry s)) s
        (< x (entry s)) (make-tree (entry s)
                                   (adjoin-set x (left-branch s))
                                   (right-branch s))
        (> x (entry s)) (make-tree (entry s)
                                   (left-branch s)
                                   (adjoin-set x (right-branch s)))))

(defn tree->list1 [tree]
  (if (empty? tree)
    '()
    (concat (tree->list1 (left-branch tree))
            (cons (entry tree)
                  (tree->list1 (right-branch tree))))))

(defn tree->list2 [tree]
  (letfn [(copy-to-list [tree result-list]
            (if (empty? tree)
              result-list
              (recur (left-branch tree)
                     (cons (entry tree)
                           (copy-to-list (right-branch tree)
                                         result-list)))))]
    (copy-to-list tree '())))

;; a

;; The two procedures return the exact same results.

(def tree1 '(7 (3 (1  ()
                      ())
                  (5  ()
                      ()))
               (9 ()
                  (11 ()
                      ()))))

(def tree2 '(3 (1 ()
                  ())
               (7 (5 ()
                     ())
                  (9 ()
                     (11 ()
                         ())))))

(def tree3 '(5 (3 (1  ()
                      ())
                  ())
               (9 (7  ()
                      ())
                  (11 ()
                      ()))))

(tree->list1 tree1)
;;=> (1 3 5 7 9 11)
(tree->list1 tree2)
;;=> (1 3 5 7 9 11)
(tree->list1 tree3)
;;=> (1 3 5 7 9 11)


(tree->list2 tree1)
;;=> (1 3 5 7 9 11)
(tree->list2 tree2)
;;=> (1 3 5 7 9 11)
(tree->list2 tree3)
;;=> (1 3 5 7 9 11)

;; b

;; Both procedures go through the tree once, so they are both at least
;; O(n). tree->list1 also uses append, which is O(n/2), but only
;; for half of each iteration, so tree-list1 is O(n x log(n)), which
;; is slower than tree-list2 that is O(n)

;; 2.64

(defn partial-tree [elts n]
  (if (= n 0)
    (cons '() elts)
    (let [left-size      (quot (dec n) 2)
          left-result    (partial-tree elts left-size)
          left-tree      (first left-result)
          non-left-elts  (rest left-result)
          right-size     (- n (inc left-size))
          this-entry     (first non-left-elts)
          right-result   (partial-tree (rest non-left-elts)
                                       right-size)
          right-tree     (first right-result)
          remaining-elts (rest right-result)]
      (cons (make-tree this-entry
                       left-tree
                       right-tree)
            remaining-elts))))

(defn list->tree [elements]
  (first (partial-tree elements (count elements))))

;; a

;; This procedure splits elements around a pivot which is the median
;; value of the elements. The median value becomes the entry of this
;; level, the elements before it the left branch and the elements
;; after it the right branch. The tree is build recursively until
;; there are no elements left.

(list->tree '(1 3 5 7 9 11))
(comment
  (5 (1 ()
        (3 ()
           ()))
     (9 (7 ()
           ())
        (11 ()
            ()))))


;;
;;           5
;;          / \
;;         /   \
;;        1     9
;;       /     / \
;;      /     /   \
;;     3     7     11
;;

;; b

;; The procedure goes through the list once and only uses constant
;; time operations, so this procedure is O(n)

;; 2.65
(def tree->list tree->list2)

(defn union-set-tree [set1 set2]
  (list->tree (union-set (tree->list set1)
                         (tree->list set2))))

(defn intersection-set-tree [set1 set2]
  (list->tree (intersection-set (tree->list set1)
                                (tree->list set2))))
