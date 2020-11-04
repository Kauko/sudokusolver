(ns sudoku-solver.game-rules.validator
  (:require [sudoku-solver.game-rules.board :as board]))

(def ^{:private true
       :dynamic true} *valid-vals* (into #{} (range 1 (inc board/*board-size*))))

(defn- value-is-valid? [val]
  (or (nil? val) (*valid-vals* val)))

(defn- is-valid? [vals]
  (and
    (or (every? nil? vals) (apply distinct? (remove nil? vals)))
    (every? value-is-valid? vals)))

(defn- is-complete? [vals]
  (and (is-valid? vals)
       (every? some? vals)))

(defn submatrix-is-valid? [sudoku x y]
  (let [valid? (is-valid? (board/submatrix sudoku x y))]
    ;(when-not valid? (println "Submatrix" (board/submatrix sudoku x y) "is not valid!"))
    valid?))

(defn submatrix-is-complete? [sudoku x y]
  (is-complete? (board/submatrix sudoku x y)))

(defn row-is-valid? [sudoku n]
  (let [valid? (is-valid? (board/row sudoku n))]
    ;(when-not valid? (println "Row" (board/row sudoku n) "is not valid!"))
    valid?))

(defn row-is-complete? [sudoku n]
  (is-complete? (board/row sudoku n)))

(defn col-is-valid? [sudoku n]
  (let [valid? (is-valid? (board/col sudoku n))]
    ;(when-not valid? (println "Column" (board/col sudoku n) "is not valid!"))
    valid?))

(defn col-is-complete? [sudoku n]
  (is-complete? (board/col sudoku n)))

(defn board-is-complete? [sudoku]
  (and
    (every? true? (map (fn [[x y]] (submatrix-is-complete? sudoku x y)) (board/submatrix-relative-coords sudoku)))
    (every? true? (map (partial row-is-complete? sudoku) (range (board/num-of-rows sudoku))))
    (every? true? (map (partial col-is-complete? sudoku) (range (board/num-of-cols sudoku))))))

(defn board-is-valid? [sudoku]
  (and
    (every? true? (map (fn [[x y]] (submatrix-is-valid? sudoku x y)) (board/submatrix-relative-coords sudoku)))
    (every?
      true?
      (map (partial row-is-valid? sudoku) (range (board/num-of-rows sudoku))))
    (every?
      true?
      (map (partial col-is-valid? sudoku) (range (board/num-of-cols sudoku))))))