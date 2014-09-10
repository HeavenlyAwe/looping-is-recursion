(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (if (zero? exp)
                   acc
                   (recur (* base acc) (dec exp))))]
    (helper 1 exp)))

;(defn last-element [a-seq]
;  (if (empty? a-seq)
;    nil
;    (let [helper (fn [n size]
;                   (if )

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (let [a (first seq1)
                       b (first seq2)]
                   (cond
                     (and (nil? a) (nil? b)) true
                     (= a b) (recur (rest seq1) (rest seq2))
                     :else false)))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  ":(")

(defn avg [a-seq]
  -1)

(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

