(ns sudoku-solver.game-rules.board)

(def ^{:dynamic true} *num-of-submatrices-per-row* 3)
(def ^{:dynamic true} *num-of-submatrices-per-col* 3)
(def ^{:dynamic true} *board-size* 9)

(defprotocol MatrixOfMatrices
  (submatrix-width [this] "Returns a number telling how wide a submatrix should be")
  (submatrix-depth [this] "Returns a number telling how deep a submatrix should be")
  (num-of-submatrices-per-row [this] "Returns the number of submatrices per row")
  (num-of-submatrices-per-col [this] "Returns the number of submatrices per col")
  (submatrices [this] "Divides the Matrix into submatrices. Depth and width are defined by submatrix-width and -depth functions. Returns a list of lists")
  (submatrix [this x y] "Returns the submatrix at position (x,y). (0,0) is the top-left corner.")
  (submatrix-relative-coords [this] "Returns a list submatrix coordinates relative to other submatrices. The first (from the top, to the right) is 0,0, second is 1,0, etc")
  (submatrix-absolute-coords [this] "Returns a 2d list, where values are coordinate pairs contained in the submatrix")
  (p-print [sudoku] "Print a human readable representation of the state of the matrix"))

(defprotocol Matrix
  (rows [this] "Returns a list of rows")
  (row [this y] "Returns the row with coordinate y, zero being top left corner")
  (cols [this] "Returns a list of columns")
  (col [this x] "Returns the column with coordinate x, zero being top left corner")
  (dimensions [this] "Returns the number of columns and rows")
  (num-of-cols [this] "Returns the number of clumns")
  (num-of-rows [this] "Returns the number of rows")
  (update-value-at [this x y val] "Updates the value at (x,y)")
  (value-at [this x y] "Returns the value at (x,y)")
  (value-coordinates [this] "Returns a map of coordinate to value")
  (empty-coordinates [this] "Returns a list of coordinates, where the value is empty"))

(defrecord Sudoku [board])

(extend-type Sudoku
  MatrixOfMatrices
  (submatrix-depth [sudoku]
    (/ (num-of-rows sudoku) *num-of-submatrices-per-row*))
  (submatrix-width [sudoku]
    (/ (num-of-cols sudoku) *num-of-submatrices-per-col*))
  (submatrix-relative-coords [sudoku]
    (for [x (range (num-of-submatrices-per-row sudoku))
          y (range (num-of-submatrices-per-col sudoku))]
      [x y]))
  (submatrix-absolute-coords [sudoku]
    (for [[col row] (submatrix-relative-coords sudoku)]
      (for [x (range (submatrix-width sudoku))
            y (range (submatrix-depth sudoku))]
        [(+ x (* col (submatrix-width sudoku))) (+ y (* row (submatrix-depth sudoku)))])))
  (num-of-submatrices-per-row [sudoku]
    (/ (num-of-rows sudoku) (submatrix-width sudoku)))
  (num-of-submatrices-per-col [sudoku]
    (/ (num-of-cols sudoku) (submatrix-depth sudoku)))
  (submatrices [sudoku]
    (mapv
      (fn [i]
        (mapv
          #(apply concat %)
          (partition
           (submatrix-depth sudoku)
           (take-nth (num-of-submatrices-per-col sudoku)
                     (drop
                       i
                       (mapcat
                         (fn [sub-row]
                           (mapcat
                             (fn [row]
                               (partition (submatrix-width sudoku) row))
                             sub-row))
                         (partition (submatrix-depth sudoku) (rows sudoku))))))))
      (range (num-of-submatrices-per-col sudoku))))

  (submatrix [sudoku x y]
    (-> (submatrices sudoku)
        (nth x)
        (nth y)))
  (p-print [{:keys [board] :as sudoku}]
    (let [row->str (fn [sudoku divider row]
                     (str
                       "|"
                       (apply
                        str
                        (butlast (mapcat
                                   identity
                                   (interleave
                                     (partition (submatrix-width sudoku) (map #(or % ".") row))
                                     (repeat [divider])))))
                       "|\n"))
          submatrix-row->str (fn [sudoku rows]
                               (apply
                                 str
                                 (concat
                                  (map (partial row->str sudoku \|) (take (submatrix-depth sudoku) rows))
                                  (row->str sudoku \+ (take (num-of-cols sudoku) (repeat \-))))))]
      (print
        (apply
          str
          (concat
           [(row->str sudoku \+ (take (num-of-cols sudoku) (repeat \-)))]
           (map (partial submatrix-row->str sudoku) (partition (submatrix-depth sudoku) board)))))
      (newline)
      sudoku))

  Matrix
  (rows [{:keys [board]}]
    board)
  (row [sudoku y]
    (nth (rows sudoku) y))
  (cols [{:keys [board] :as sudoku}]
    (map
      (fn [n]
        (map #(nth % n) board))
      (range (num-of-cols sudoku))))
  (col [sudoku x]
    (nth (cols sudoku) x))
  (dimensions [{:keys [board] :as sudoku}]
    [(count (first board)) (count board)])
  (num-of-cols [sudoku]
    (first (dimensions sudoku)))
  (num-of-rows [sudoku]
    (second (dimensions sudoku)))
  (update-value-at [sudoku x y val]
    (update-in sudoku [:board y x] (constantly val)))
  (value-at [sudoku x y]
    (get-in sudoku [:board y x]))
  (value-coordinates [sudoku]
    (->> (for [x (range (num-of-cols sudoku))
               y (range (num-of-rows sudoku))]
           {[x y] (value-at sudoku x y)})
         (into {})))
  (empty-coordinates [sudoku]
    (vec (sort (mapv first (remove (comp some? val) (value-coordinates sudoku)))))))

(defn sudoku
  ([]
    (sudoku (range 1 (inc *board-size*))))
  ([row]
   (->Sudoku (->> row
                  vec
                  (repeat)
                  (take *board-size*)
                  vec))))

(defn empty-sudoku []
  (sudoku (take *board-size* (repeat nil))))