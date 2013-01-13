(ns sicp.chapter2.section4
  (use sicp.core))

;; 2.73
(def optable (atom {}))

(defn get [type op]
  (get-in @optable [type op]))

(defn put [type op function]
  (swap! optable assoc-in [type op] function))

(defn variable? [x] (symbol? x))
(defn same-variable? [v1 v2]
  (and (variable? v1) (variable? v2) (= v1 v2)))

(defn operator [exp] (first exp))
(defn operands [exp] (rest exp))

(defn deriv [exp var]
  (cond (number? exp) 0
        (variable? exp) (if (same-variable? exp var) 1 0)
        :else ((get 'deriv (operator exp)) (operands exp)
               var)))

(defn install-deriv-package []
  (letfn [(=number? [exp num]
            (and (number? exp) (= exp num)))

          ;; sum
          (make-sum [a1 a2]
            (cond (=number? a1 0) a2
                  (=number? a2 0) a1
                  (and (number? a1) (number? a2)) (+ a1 a2)
                  :else (list '+ a1 a2)))
          (addend [opds] (first opds))
          (augend [opds] (second opds))
          (deriv-sum [opds var]
            (make-sum (deriv (addend opds) var)
                      (deriv (augend opds) var)))

          ;; product
          (make-product [m1 m2]
            (cond (or (=number? m1 0) (=number? m2 0)) 0
                  (=number? m1 1) m2
                  (=number? m2 1) m1
                  (and (number? m1) (number? m2)) (* m1 m2)
                  :else (list '* m1 m2)))
          (multiplier [opds] (first opds))
          (multiplicand [opds] (second opds))
          (deriv-product [opds var]
            (make-sum
             (make-product (multiplier opds)
                           (deriv (multiplicand opds) var))
             (make-product (deriv (multiplier opds) var)
                           (multiplicand opds))))

          ;; exponentiation
          (make-exponentiation [base exp]
            (cond (=number? exp 0) 1
                  (=number? exp 1) base
                  :else (list '** base exp)))

          (base [opds]
            (first opds))

          (exponent [opds]
            (second opds))

          (deriv-exponentation [opds var]
            (make-product
             (exponent opds)
             (make-product
              (make-exponentiation (base opds)
                                   (make-sum (exponent opds) (- 1)))
              (deriv (base opds) var))))]

    ;; interface
    (put 'deriv '+ deriv-sum)
    (put 'deriv '* deriv-product)
    (put 'deriv '** deriv-exponentation)))

(install-deriv-package)

;; 2.74
;; a

(defn make-generic-file [division file]
  (cons division file))
(defn division [generic-file]
  (first generic-file))
(defn original-file [generic-file]
  (rest generic-file))

(defn get-record [employee generic-file]
  ((get 'get-record (division generic-file))
   employee (original-file generic-file)))

;; b
(defn make-generic-record [division record]
  (cons division record))
(defn division [generic-record]
  (first generic-record))
(defn original-record [generic-record]
  (rest generic-record))

(defn get-salary [generic-record]
  ((get 'get-salary (division generic-record))
   (original-record generic-record)))

;; c
(defn in-this-division? [employee division]
  ((get 'in-this-division? division) employee))

(defn find-employee-record [employee files]
  (cond (empty? files) (throw (Exception. "unknown employee") employee)
        (in-this-division? employee (division (first files)))
        (get-record employee (first files))
        :else (recur employee (rest files))))

;; 2.75
(defn make-from-real-imag [x y]
  (fn dispatch [op]
    (condp = op
      'real-part x
      'imag-part y
      'magnitude (Math/sqrt (+ (square x) (square y)))
      'angle (Math/atan2 y x)
      (throw (Exception. (str "Unknown op: " op))))))

(defn make-from-mag-ang [r a]
  (fn dispatch [op]
    (condp = op
      'real-part (* r (Math/cos a))
      'imag-part (* r (Math/sin a))
      'magnitude r
      'angle a
      (throw (Exception. (str "Unknown op: " op))))))

;; 2.76

;; With explicit dispatch:
;; new type = new functions for every generic interface procedure,
;; mind name conflicts
;; new operation = new generic interface procedure, mind name conflicts
;;
;; With data-directed system:
;; new type = update table
;; new operation = update table
;;
;; With message-passing:
;; new type = nothing spectial
;; new operation = change all types

;; 2.77

;; magnitude is not defined for the type "complex", but it is
;; defined for its subtypes "polar" and "rectangular". We just need
;; to feed the operation table with the complex type, as Alyssa does.
(comment
  (apply-generic 'magnitude '(complex rectangular 3 4))
  ((get 'magnitude '(complex)) '(rectangular 3 4))
  (magnitude '(rectangular 3 4))
  (apply-generic 'magnitude '(rectangular 3 4))
  ((get 'magnitude '(rectangular)) (3 4))
  (magnitude (3 4)))

;; 2.78
(defn type-tag [datum]
  (cond (coll? datum) (first datum)
        (number? datum) 'clojure-number
        :else (throw (Exception. (str "Bad tagged datum: ") datum))))

(defn contents [datum]
  (cond (coll? datum) (rest datum)
        (number? datum) datum
        :else (throw (Exception. (str "Bad tagged datum: " datum)))))

(defn attach-tag [type-tag contents]
  (if (number? contents)
      contents
      (cons type-tag contents)))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get op type-tags)]
    (if proc
      (apply proc (map contents args))
      (throw (Exception.
              (str "no method '" op "' for " (apply str type-tags)))))))

;; 2.79

(defn add [x y] (apply-generic 'add x y))
(defn sub [x y] (apply-generic 'sub x y))
(defn mul [x y] (apply-generic 'mul x y))
(defn div [x y] (apply-generic 'div x y))

(defn install-clojure-number-package []
  (letfn [(tag [x] (attach-tag 'clojure-number x))]
    (put 'add '(clojure-number clojure-number) (comp tag +))
    (put 'sub '(clojure-number clojure-number) (comp tag -))
    (put 'mul '(clojure-number clojure-number) (comp tag *))
    (put 'div '(clojure-number clojure-number) (comp tag /))
    (put 'make 'clojure-number tag)))
(install-clojure-number-package)
(defn make-clojure-number [n]
  ((get 'make 'clojure-number) n))

(defn numer [x] (first x))
(defn denom [x] (second x))

(defn install-rational-package []
  (letfn [(make [n d]
            (let [g (gcd n d)]
              [(/ n g) (/ d g)]))
          (add [x y]
            (make (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
          (sub [x y]
            (make (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
          (mul [x y]
            (make (* (numer x) (numer y))
                  (* (denom x) (denom y))))
          (div [x y]
            (make (* (numer x) (denom y))
                  (* (denom x) (numer y))))
          (tag [x] (attach-tag 'rational x))]
    (put 'add  '(rational rational) (comp tag add))
    (put 'sub  '(rational rational) (comp tag sub))
    (put 'mul  '(rational rational) (comp tag mul))
    (put 'div  '(rational rational) (comp tag div))
    (put 'make 'rational (comp tag make))))

(install-rational-package)
(defn make-rational [n d]
  ((get 'make 'rational) n d))

(defn install-polar-package []
  (letfn [(magnitude [z] (first z))
          (angle [z] (second z))
          (make-from-mag-ang [r a]
            [r a])
          (real-part [z] (* (magnitude z) (Math/cos (angle z))))
          (imag-part [z] (* (magnitude z) (Math/sin (angle z))))
          (make-from-real-imag [x y]
            [(Math/sqrt (+ (square x) (square y)))
             (Math/atan2 y x)])
          (tag [x] (attach-tag 'polar x))]
    (put 'real-part '(polar) (comp tag real-part))
    (put 'imag-part '(polar) (comp tag imag-part))
    (put 'magnitude '(polar) (comp tag magnitude))
    (put 'angle '(polar) (comp tag angle))
    (put 'make-from-real-imag 'polar (comp tag make-from-real-imag))
    (put 'make-from-mag-ang 'polar (comp tag make-from-mag-ang))))

(install-polar-package)

(defn install-rectangular-package []
  (letfn [(real-part [z] (first z))
          (imag-part [z] (second z))
          (make-from-real-imag [x y]
            [x y])
          (magnitude [z] (+ (square (real-part z))
                            (square (imag-part z))))
          (angle [z] (Math/atan2 (imag-part z) (real-part z)))
          (make-from-mag-ang [r a]
            [(* r (Math/cos a))
             (* r (Math/sin a))])
          (tag [x] (attach-tag 'rectangular x))]
    (put 'real-part '(rectangular) (comp tag real-part))
    (put 'imag-part '(rectangular) (comp tag imag-part))
    (put 'magnitude '(rectangular) (comp tag magnitude))
    (put 'angle '(rectangular) (comp tag angle))
    (put 'make-from-real-imag 'rectangular (comp tag make-from-real-imag))
    (put 'make-from-mag-ang 'rectangular (comp tag make-from-mag-ang))))

(install-rectangular-package)

(defn real-part [z] (apply-generic 'real-part z))
(defn imag-part [z] (apply-generic 'imag-part z))
(defn magnitude [z] (apply-generic 'magnitude z))
(defn angle [z] (apply-generic 'angle z))
(defn make-from-real-imag [x y]
  ((get 'make-from-real-imag 'rectangular) x y))
(defn make-from-mag-ang [x y]
  ((get 'make-from-mag-ang 'polar) x y))

(defn install-complex-package []
  (letfn [(make-from-real-imag [x y]
            ((get 'make-from-real-imag 'rectangular) x y))
          (make-from-mag-ang [r a]
            ((get 'make-from-mag-ang 'polar) r a))
          (real-part [z] (apply-generic 'real-part z))
          (imag-part [z] (apply-generic 'imag-part z))
          (angle [z] (apply-generic 'angle z))
          (magnitude [z] (apply-generic 'magnitude z))
          (add [z1 z2]
            (make-from-real-imag (+ (real-part z1) (real-part z2))
                                 (+ (imag-part z1) (imag-part z2))))
          (sub [z1 z2]
            (make-from-real-imag (- (real-part z1) (real-part z2))
                                 (- (imag-part z1) (imag-part z2))))
          (mul [z1 z2]
            (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                               (+ (angle z1) (angle z2))))
          (div [z1 z2]
            (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                               (- (angle z1) (angle z2))))
          (tag [z] (attach-tag 'complex z))]
    (put 'add '(complex complex) (comp tag add))
    (put 'sub '(complex complex) (comp tag sub))
    (put 'mul '(complex complex) (comp tag mul))
    (put 'div '(complex complex) (comp tag div))
    (put 'make-from-mag-ang 'complex (comp tag make-from-mag-ang))
    (put 'make-from-real-imag 'complex (comp tag make-from-real-imag))
    (put 'real-part '(complex) real-part)
    (put 'imag-part '(complex) imag-part)
    (put 'magnitude '(complex) magnitude)
    (put 'angle '(complex) angle)))

(install-complex-package)
(defn make-complex-from-real-imag [x y]
  ((get 'make-from-real-imag 'complex) x y))

(defn make-complex-from-mag-ang [x y]
  ((get 'make-from-mag-ang 'complex) x y))

(defn equ? [x y]
  (apply-generic 'equ? x y))

(put 'equ? '(clojure-number clojure-number) =)

(put 'equ? '(rational rational)
     (fn equ-rat? [x y]
       (and (= (numer x) (numer y))
            (= (denom x) (denom y)))))

(put 'equ? '(complex complex)
     (fn equ-complex [x y]
       (and (= (real-part x) (real-part y))
            (= (imag-part x) (imag-part y)))))

(equ? 1 1)
;;=> true
(equ? 1 2)
;;=> false
(equ? (make-rational 1 1)
      (make-rational 1 1))
;;=> true
(equ? (make-rational 1 1)
      (make-rational 1 2))
;;=> false
(equ? (make-complex-from-mag-ang 10 10)
      (make-complex-from-mag-ang 10 10))
;;=> true
(equ? (make-complex-from-mag-ang 10 10)
      (make-complex-from-mag-ang 10 20))
;;=> false
(equ? (make-complex-from-real-imag 10 10)
      (make-complex-from-real-imag 10 10))
;;=> true
(equ? (make-complex-from-real-imag 10 10)
      (make-complex-from-real-imag 10 20))
;;=> false

;; 2.80
(defn =zero? [x]
  (apply-generic '=zero? x))

(put '=zero? '(clojure-number)
     zero?)

(put '=zero? '(rational)
     (fn [x] (=zero? (numer x))))

(put '=zero? '(complex)
     (fn [z] (=zero? (magnitude z))))

(=zero? 0)
;;=> true
(=zero? 1)
;;=> false
(=zero? (make-rational 0 1))
;;=> true
(=zero? (make-rational 1 1))
;;=> false
(=zero? (make-complex-from-real-imag 0 0))
;;=> true
(=zero? (make-complex-from-real-imag 1 1))
;;=> false
(=zero? (make-complex-from-mag-ang 0 10))
;;=> true
(=zero? (make-complex-from-mag-ang 10 10))
;;=> false

;; 2.81

(def coercion-table (atom {}))
(defn get-coercion [type1 type2]
  (get-in @coercion-table [type1 type2]))

(defn put-coercion [type1 type2 function]
  (swap! coercion-table assoc-in [type1 type2] function))

(defn apply-generic [op & args]
  (let [type-tags (map type-tag args)
        proc (get op type-tags)
        raise-no-method-error
        #(throw (Exception.
                 (str "No method '" op "' for these tags: "
                      (apply str type-tags))))]
    (if proc
      (apply proc (map contents args))
      (if (= (count args) 2)
        (let [[type1 type2] type-tags
              [a1 a2] args
              t1->t2 (get-coercion type1 type2)
              t2->t1 (get-coercion type2 type1)]
          (cond (= type1 type2) (raise-no-method-error)
                t1->t2 (apply-generic op (t1->t2 a1) a2)
                t2->t1 (apply-generic op a1 (t2->t1 a2))
                :else (raise-no-method-error)))
        (raise-no-method-error)))))

(put-coercion 'clojure-number 'complex
              (fn [n] (make-complex-from-real-imag (contents n) 0)))

;; Nothing was really needed, since no coercion to same type were
;; available. But now we can support both.
