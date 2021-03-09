(ns magic-square.puzzle)

(def values
  [1.0 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0])

(defn- mete [v l]
       (if (empty? l)
         (conj [] v)
         (loop [n l
                res []]
               (if (empty? n)
                 res
                 (if (number? (first n))
                   (conj res (conj n v))
                   (recur (rest n) (conj res (mete v (first n)))))))))

(defn- drop-nth [n coll]
       (concat
         (take n coll)
         (drop (inc n) coll)))

(defn- permutations [values]
       (loop [n 0
              per []]
             (if (= n (count values))
               per
               (let [x values]
                    (recur (inc n) (conj per (mete (nth values n) (permutations (drop-nth n x)))))))))

(defn- sum
       [coll]
       (reduce + coll))

(defn- sum-rows [m]
      (map #(reduce + %) m))

(defn- sum-cols [m]
      [(reduce + (map first m))
       (reduce + (map second m))
       (reduce + (map last m))])

(defn- sum-diagonals
       [l]
       (let [left-to-right-diagonals [(first (first l)) (second (second l)) (last (last l))]
             right-to-left-diagonals [(first (last l)) (second (second l)) (last (first l))]]
            [(sum left-to-right-diagonals) (sum right-to-left-diagonals)]))

(defn- es-valido?
       [l]
       (= (set (sum-rows l))
          (set (sum-cols l))
          (set (sum-diagonals l))))


(defn magic-square
      [coll]
      (->> (permutations coll)
           flatten
           (partition 9)
           (map #(map vec (partition 3 %)))
           (filter #(es-valido? %))
           first
           vec))

