(ns sicp.chapter3.section1
  (use sicp.core))

;; 3.1

(defn make-accumulator [initial-value]
  (let [total (atom initial-value)]
    (fn [n]
      (swap! total + n))))

(def A (make-accumulator 5))

(A 10)
;; => 15

(A 10)
;; => 25

;; 3.2

(defn make-monitored [function]
  (let [calls-number (atom 0)]
    (fn [& args]
      (if (and (= (count args) 1)
               (= (first args) :how-many-calls?))
        @calls-number
        (do (swap! calls-number inc)
            (apply function args))))))

(def monitored-square (make-monitored sqrt))

(monitored-square 100)
;;=> 10
(monitored-square :how-many-calls?)
;;=> 1

;; 3.3
(defn make-account [initial-balance password]
  (let [balance (atom initial-balance)
        actions
        {:withdraw (fn [amount]
                     (swap! balance
                            (fn [current-balance]
                              (if (< current-balance amount)
                                (do (print "Insufficient amount!")
                                    current-balance)
                                (- current-balance amount)))))
         :deposit  (fn [amount]
                     (swap! balance + amount))
         :check    (fn []
                     @balance)}]
    (fn [action submitted-password & args]
      (if (= password submitted-password)
        (apply (actions action) args)
        "Wrong password!"))))

(def account (make-account 100 "password"))

(account :check "")
(account :deposit "password" 10)
(account :withdraw "password" 10)
(account :check "password")

;; 3.4
(defn make-account [initial-balance password]
  (let [balance (atom initial-balance)
        failed-attempts (atom 0)
        call-the-cops! (fn [] "Calling the cops!")
        actions
        {:withdraw (fn [amount]
                     (swap! balance
                            (fn [current-balance]
                              (if (< current-balance amount)
                                (do (print "Insufficient amount!")
                                    current-balance)
                                (- current-balance amount)))))
         :deposit  (fn [amount]
                     (swap! balance + amount))
         :check    (fn []
                     @balance)}]
    (fn [action submitted-password & args]
      (if (= password submitted-password)
        (apply (actions action) args)
        (do
          (swap! failed-attempts inc)
          (if (> @failed-attempts 7)
            (call-the-cops!)
            "Wrong password!"))))))

(def account (make-account 100 "password"))
(dotimes [n 6]
  (account :check ""))
(account :check "")
;;=> "Wrong password!"
(account :check "")
;;=> "Calling the cops!"

;; 3.5
(defn monte-carlo [trials experiment]
  (letfn[(iter [trials-remaining trials-passed]
           (cond (= trials-remaining 0)
                 (/ trials-passed trials)

                 (experiment)
                 (recur (dec trials-remaining)
                        (inc trials-passed))

                 :else
                 (recur (dec trials-remaining)
                        trials-passed)))]
    (iter trials 0)))

(defn random-in-range [low high]
  (let [range (- high low)]
    (+ low (rand range))))

(defn estimate-integral [predicate x1 x2 y1 y2 trials]
  (letfn [(test []
            (predicate (random-in-range x1 x2)
                       (random-in-range y1 y2)))]
    (* (* (- x2 x1) (- y2 y1))
       (monte-carlo trials test))))

(defn estimate-pi [trials]
  (letfn [(predicate [x y]
            (< (+ (square x)
                  (square y))
               1.0))]
    (estimate-integral predicate -1.0 1.0 -1.0 1.0 trials)))

(estimate-pi 1000000)
;;=> ≈ 3.14

;; 3.6
(defn random-init []
  0.42)

(defn rand-update [x]
  (let [a 7.5 b 10 m 3.0]
    (/ (rem (+ (* a x) b)
            m)
       m)))

(def my-rand
  (let [x (atom (random-init))
        actions {:generate (fn [] (swap! x rand-update))
                 :reset (fn [value] (reset! x value))}]
    (fn [action & args]
      (apply (actions action) args))))


;; 3.7
(defn make-account [initial-balance password]
  (let [balance (if (number? initial-balance)
                  (atom initial-balance)
                  initial-balance)
        failed-attempts (atom 0)
        call-the-cops! (fn [] "Calling the cops!")
        actions
        {:withdraw (fn [amount]
                     (swap! balance
                            (fn [current-balance]
                              (if (< current-balance amount)
                                (do (print "Insufficient amount!")
                                    current-balance)
                                (- current-balance amount)))))

         :deposit (fn [amount] (swap! balance + amount))

         :check (fn [] @balance)

         :make-joint-account (fn [new-password]
                               (make-account balance
                                             new-password))}]
    (fn [action submitted-password & args]
      (if (= password submitted-password)
        (apply (actions action) args)
        (do
          (swap! failed-attempts inc)
          (if (> @failed-attempts 7)
            (call-the-cops!)
            "Wrong password!"))))))

(def frank-acc (make-account 100 "open-sesame"))
(frank-acc :make-joint-account "wrong-pass" "rosebud")

(def paul-acc (frank-acc :make-joint-account "open-sesame" "rosebud"))

(frank-acc :check "open-sesame")
(paul-acc :check "rosebud")
(paul-acc :deposit "rosebud" 10)
(frank-acc :check "open-sesame")

;; 3.8
(defmacro +-> [a b]
  `(let [v1# ~a v2# ~b]
     (+ v1# v2#)))

(defmacro +<- [a b]
  `(let [v1# ~b v2# ~a]
     (+ v1# v2#)))

(defn make-f []
  (let [previous-state (atom nil)]
    (fn [n]
      (print n)
      (or @previous-state
          (do (reset! previous-state n)
              0)))))

(let [f (make-f)]
  (+-> (f 0) (f 1)))
;;=> 0

(let [f (make-f)]
  (+<- (f 0) (f 1)))
;;=> 1

;; 3.9

(comment
  global env -> [factorial -> (if (= n 1) 1 (* n (factorial (dec n))))]

  (factorial 6)

  E1 -> n: 6
  E2 -> n: 5
  E3 -> n: 4
  E4 -> n: 3
  E5 -> n: 2
  E6 -> n: 1

  )



(comment
  global env -> [factorial -> (fact-iter 1 1 n)
                 fact-iter -> (...)]

  (factorial 6)

  E1 -> n: 6
  E2 -> product: 1,   counter: 1, max-count: 6
  E3 -> product: 2,   counter: 2, max-count: 6
  E4 -> product: 6,   counter: 3, max-count: 6
  E5 -> product: 24,  counter: 4, max-count: 6
  E6 -> product: 120, counter: 5, max-count: 6
  E7 -> product: 720, counter: 6, max-count: 6

  )
