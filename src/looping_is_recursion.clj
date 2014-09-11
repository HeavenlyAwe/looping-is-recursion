(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (let [a (first seq1)
                       b (first seq2)
                       e1 (empty? seq1)
                       e2 (empty? seq2)]
                   (cond
                     (and e1 e2) true
                     (or e1 e2) false
                     (not= a b) false
                     :else (recur (rest seq1) (rest seq2)))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [seq1 a-seq
         index 0]
    (cond
      (empty? seq1) nil
      (pred (first seq1)) index
      :else (recur (rest seq1) (inc index)))))

(defn avg [a-seq]
  (if (empty? a-seq)
    0
    (loop [sum 0
           n 0
           seq1 a-seq]
      (if (empty? seq1)
        (/ sum n)
        (recur (+ sum (first seq1)) (inc n) (rest seq1))))))

(defn parity [a-seq]
  (let [add-or-remove (fn [res-set value]
                        (if (contains? res-set value)
                          (disj res-set value)
                          (conj res-set value)))]
    (loop [res-set #{}
           inp-seq a-seq]
      (if (empty? inp-seq)
        res-set
        (recur (add-or-remove res-set (first inp-seq)) (rest inp-seq))))))
                 

(defn fast-fibo [n]
  (loop [f-n1 1
         f-n0 0
         cur n]
    (if (zero? cur)
      f-n0
      (recur (+ f-n1 f-n0) f-n1 (dec cur)))))

(defn cut-at-repetition [a-seq]
  (loop [res-seq (empty seq)
         inp-seq a-seq
         key-set #{}]
    (let [a (first inp-seq)
          s (rest inp-seq)]
      (if (or (empty? inp-seq) (contains? key-set a))
        (reverse res-seq)
        (recur (conj res-seq a) s (conj key-set a))))))

