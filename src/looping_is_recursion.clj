(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc e]
                 ;(prn (str acc " - " base " - " e))
                 (if (zero? e)
                   acc
                   (recur (* acc base) (dec e))
                   ))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [head (first a-seq)
        tail (rest a-seq)]
    ;(println (str head "-" tail))
    (if (empty? tail)
      head
      (recur tail))))

(defn seq= [seq1 seq2]
  ;(prn (str seq1 " - " seq2))
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [index 0
         seq a-seq]
    (cond
      (empty? seq) nil
      (pred (first seq)) index
      :else (recur (inc index) (rest seq)))))

(defn avg [a-seq]
  (loop [acc 0
         count 0
         seq a-seq]
    (if (empty? seq)
      (/ acc count)
      (recur (+ acc (first seq)) (inc count) (rest seq)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odds #{}
           seq a-seq]
      (if (empty? seq)
        odds
        (let [current (first seq)
              tail (rest seq)]
          (recur (toggle odds current) tail))))))

(defn fast-fibo [n]
  (loop [n-1 1
         n-2 0
         counter n]
    ;(println (str "counter : " counter " / n-1 : " n-1 " / n-2 : " n-2))
    (if (zero? counter)
      n-2
      (recur n-2 (+ n-1 n-2) (dec counter)))))

(defn cut-at-repetition [a-seq]
  (loop [seen #{}
         items []
         seq a-seq]
    (println (str "seen : " seen " / head : " (first seq) " / seq : " seq))
    (let [current (first seq)]
      (if (or
            (contains? seen current)
            (empty? seq))
        items
        (recur (conj seen current) (conj items current) (rest seq))))))