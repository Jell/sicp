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
    (cond (= op 'real-part) x
          (= op 'imag-part) y
          (= op 'magnitude) (Math/sqrt (+ (square x) (square y)))
          (= op 'angle) (Math/atan2 y x)
          :else (throw (Exception. (str "Unknown op: " op))))))

(defn make-from-mag-ang [r a]
  (fn dispatch [op]
    (cond (= op 'real-part) (* r (Math/cos a))
          (= op 'imag-part) (* r (Math/sin a))
          (= op 'magnitude) r
          (= op 'angle) a
          :else (throw (Exception. (str "Unknown op: " op))))))

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
