(ns sudoku-solver.game-rules.generator
  (:require [sudoku-solver.game-rules.board :as board]
            [sudoku-solver.game-rules.solver :as solver]
            [sudoku-solver.game-rules.validator :as validator]))



(defn- update-random [sudoku]
  (loop [sudoku sudoku]
    (let [[x y] (-> sudoku board/empty-coordinates shuffle first)
          possible-values (filter (fn [val]
                                    (validator/board-is-valid? (board/update-value-at sudoku x y val)))
                                  (range 1 (inc (board/num-of-rows sudoku))))]
      (if (empty? possible-values)
        ;; Shouldn't ever happen, but let's just be sure
        (recur sudoku)

        (board/update-value-at sudoku x y (rand-nth possible-values))))))

(defn randomly-filled-board
  ([] (randomly-filled-board 30))
  ([fail-threshold]
   (loop [sudoku (board/empty-sudoku)
          failures 0
          rounds 0]
     (if (validator/board-is-complete? sudoku)
       sudoku

       (let [new-board (update-random sudoku)]
         (println "Round #" rounds)
         (board/p-print new-board)
         (println)
         (cond
           (solver/only-one-solution? new-board {:max-rounds 30000})
           (recur new-board 0 (inc rounds))

           (>= failures fail-threshold)
           (do
             (println "Too many failures, taking a random solution board")
             (-> sudoku (solver/solve {:wanted-solutions 100}) rand-nth board/p-print))

           :else
           (do
             (println "Randomly generated a board with no valid solutions. Fail" (inc failures) "/" fail-threshold)
             (recur sudoku (inc failures) (inc rounds)))))))))

(defn fill-in-random-order []
  (let [sudoku (board/empty-sudoku)]
    (loop []
      (if-let [solved (-> sudoku
                          (solver/solve
                            {:free-coords (shuffle (board/empty-coordinates sudoku))
                             :max-rounds 50000})
                          first)]
        solved

        (recur)))))

(defn fill-with-random-numbers
  ([] (fill-with-random-numbers {}))
  ([opts]
   (let [sudoku (board/empty-sudoku)]
     (loop []
       (if-let [solved (-> sudoku
                           (solver/solve
                             (merge
                               opts
                               {:new-value-fn (fn [sudoku solution path]
                                               (->
                                                 (clojure.set/difference
                                                   (into #{} (range 1 (inc (board/num-of-rows sudoku))))
                                                   (into #{} (keys (get-in solution path))))
                                                 vec
                                                 rand-nth))
                               :mark-as-failure?-fn (fn [sudoku solution path]
                                                      (= (board/num-of-rows sudoku) (count (keys (get-in solution path)))))}))
                           first)]
         solved

         (recur))))))

(defn fill-with-no-randomization []
  (-> (board/empty-sudoku)
      (solver/solve)
      first))

(defn- generate-unique*
  ([] (generate-unique* nil))
  ([max-empty-spaces] (generate-unique* max-empty-spaces (fill-with-random-numbers)))
  ([max-empty-spaces filled-board]
   (let [sudoku filled-board
         positions (-> sudoku board/value-coordinates keys shuffle)]
     (println "Generated random board")
     (board/p-print sudoku)
     (if max-empty-spaces
       (println "Finding a unique solutions with" max-empty-spaces "empty spaces.")
       (println "Finding a unique solution the maximum amount of empty spaces."))
     (loop [sudoku sudoku
            [[x y :as pos] & p] positions
            uniques {}
            one-solution? false]
       (cond
         (and (nil? pos) one-solution?)
         uniques

         (nil? pos)
         uniques

         (= (count (board/empty-coordinates sudoku)) max-empty-spaces)
         (do
           (println "Unique board has" max-empty-spaces "empty spaces.")
           uniques)

         :else
         (do
           (println "** Round" (- (count positions) (count p)) "/" (count positions) ". Sudoku has" (count (board/empty-coordinates sudoku)) "empty spaces.")
           (let [new (board/update-value-at sudoku x y nil)]
             (if (solver/only-one-solution? new)
               (recur new p (assoc uniques (count (board/empty-coordinates new)) new) true)

               (recur sudoku p uniques false)))))))))

(defn generate-very-easy []
  (let [n 35]
    (get (generate-unique* n) n)))
(defn generate-easy []
  (let [n 43]
    (get (generate-unique* n) n)))
(defn generate-medium []
  (let [n 51]
    (get (generate-unique* n) n)))
(defn generate-hard []
  (let [n 56]
    (get (generate-unique* n) n)))
(defn generate-very-hard []
  (let [n 59]
    (get (generate-unique* n) n)))
(defn generate-extreme [& params]
  (let [uniques (apply generate-unique* params)
        most-empty-slots (apply max (keys uniques))]
    (get uniques most-empty-slots)))