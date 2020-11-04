(ns sudoku-solver.game-rules.solver
  (:require [sudoku-solver.game-rules.board :as board]
            [sudoku-solver.game-rules.validator :as validator]))

(defn- solve*
  ([sudoku]
    (solve* sudoku {}))
  ([sudoku {:keys [wanted-solutions free-coords max-rounds new-value-fn mark-as-failure?-fn]}]
   (let [free-coords (or free-coords (board/empty-coordinates sudoku))
         new-value-fn (or new-value-fn
                          (fn [sudoku solution path]
                            (inc (apply max (or (keys (get-in solution path)) [0])))))
         mark-as-failure?-fn (or mark-as-failure?-fn
                                 (fn [sudoku solution path]
                                   (= (apply max (or (keys (get-in solution path)) [0])) (board/num-of-rows sudoku))))
         wanted-solutions (or wanted-solutions 1)

         unique-solution? (fn [complete-boards sudoku]
                            (nil? ((into #{} complete-boards) sudoku)))]

     (loop [sudoku sudoku
            i 0
            complete-boards []
            solution {:start nil}
            path [:start]
            rounds 0]

       (when (= 0 (mod rounds 3000))
         (println rounds "-" path))

       (cond
         (and (some? max-rounds) (> rounds max-rounds))
         (do
           (binding [*out* *err*]
             (println rounds "- Terminating, maximum rounds was" max-rounds)
             [complete-boards solution]))

         (and (validator/board-is-complete? sudoku) (unique-solution? complete-boards sudoku))
         (let [complete-boards (conj complete-boards sudoku)]
           (if (and (not= wanted-solutions :all) (>= (count complete-boards) wanted-solutions))
             (do
               (println rounds "- Found" wanted-solutions "solution, as requested")
               [complete-boards (assoc-in solution path true)])

             (do
               (println rounds "- Found solution #" (count complete-boards))
               (let [[x y] (last free-coords)]
                 (recur (board/update-value-at sudoku x y nil) (dec (count free-coords)) complete-boards solution (vec (butlast path)) (inc rounds))))))

         (validator/board-is-complete? sudoku)
         (do
           (binding [*out* *err*]
             (println rounds "- Ended up with a solution that had been already found"))
           (let [[x y] (last free-coords)]
             (recur (board/update-value-at sudoku x y nil) (dec (count free-coords)) complete-boards solution (vec (butlast path)) (inc rounds))))

         (neg? i)
         (if (empty? complete-boards)
           (binding [*out* *err*]
             (println rounds "- Unable to find a solution")
             [complete-boards solution])
           (do
             (println rounds "- No more than" (count complete-boards) "solutions available!")
             [complete-boards solution]))

         :else
         (let [[x y] (get free-coords i)]
           (if (mark-as-failure?-fn sudoku solution path)
             (recur (board/update-value-at sudoku x y nil) (dec i) complete-boards solution (vec (butlast path)) (inc rounds))

             (let [new-val (new-value-fn sudoku solution path)
                   sudoku (board/update-value-at sudoku x y new-val)]
               (if (validator/board-is-valid? sudoku)
                 (recur sudoku (inc i) complete-boards (update-in solution path merge {new-val nil}) (conj path new-val) (inc rounds))

                 (recur sudoku i complete-boards (update-in solution path merge {new-val false}) path (inc rounds)))))))))))

(defn- solved-boards
  ([sudoku] (solved-boards sudoku {}))
  ([sudoku opts] (-> sudoku (solve* opts) first)))

(defn- solutions-count
  ([sudoku] (solutions-count sudoku {}))
  ([sudoku opts] (-> sudoku (solved-boards opts) count)))

(defn solve
  ([sudoku] (solve sudoku {}))
  ([sudoku opts] (when (validator/board-is-valid? sudoku) (solved-boards sudoku opts))))

(defn only-one-solution?
  ([sudoku] (only-one-solution? sudoku {}))
  ([sudoku opts] (and (validator/board-is-valid? sudoku) (= 1 (solutions-count sudoku (merge opts {:wanted-solutions 2}))))))

(defn many-solutions?
  ([sudoku] (many-solutions? sudoku {}))
  ([sudoku opts] (and (validator/board-is-valid? sudoku) (= 2 (solutions-count sudoku (merge opts {:wanted-solutions 2}))))))
