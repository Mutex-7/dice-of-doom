(ns dice.dice)

(def num-players 3)
(def max-dice 4)
(def board-size 4)
(def board-hexnum (* board-size board-size)) ;; Make sure that board-hexnum is always bigger then or equal to num-players!
(def ai-level 2)
(def fair-hexnum (inc (quot board-hexnum num-players)))
(def fair-dicenum (int (* (* (quot board-hexnum num-players) max-dice) 0.4)))
(def player-pool (atom (shuffle (range num-players))))
(def dice-probability [[0.84 0.97 1.0 1.0]
                       [0.44 0.78 0.94 0.99]
                       [0.15 0.45 0.74 0.91]
                       [0.04 0.19 0.46 0.72]
                       [0.01 0.06 0.22 0.46]])

(declare attacking-moves
         rate-position
         add-passing-move
         score-board
         largest-cluster-size
         attack-fail)

(defn new-hex
  "Creates a hexagon for use in a board."
  [player dice pos]
  {:player player
   :dice dice
   :pos pos})

(defn new-move
  "Creates a new map with the information needed to indicate what move was made."
  [src dst tree fail]
  {:src src
   :dst dst
   :tree tree
   :fail fail})

(defn game-tree
  "Builds the entire game tree below the current node/board."
  [board player spare-dice first-move]
  {:player player
   :board board
   :moves (add-passing-move board
                            player
                            spare-dice
                            first-move
                            (attacking-moves board player spare-dice))})

(defn random-board
  []
  (for [pos (range board-hexnum)]
    (new-hex (rand-int num-players)
             (inc (rand-int max-dice))
             pos)))

(defn blank-board
  []
  (vec (repeatedly board-hexnum #(new-hex nil 1 nil))))

(defn random-player
  "Pick a random player that hasn't been picked recently."
  []
  (when (empty? @player-pool)
    (reset! player-pool (shuffle (range num-players))))
  (let [player (first @player-pool)]
    (reset! player-pool (rest @player-pool))
    player))

(defn assign-positions
  "Assigns positions to each hex on the board."
  ([board]
   (assign-positions board 0))
  ([board pos]
   (if (= board-hexnum pos)
     board
     (recur (assoc-in board [pos :pos] pos) (inc pos)))))

(defn rand-unowned-hex
  "Returns the position of a random unowned hex."
  [board]
  (:pos (rand-nth (filter #(nil? (:player %)) board))))

(defn unassigned-hex?
  "Does board have a hex without a player?"
  [board]
  (some #(nil? (:player %)) board))

(defn assign-ownership
  "Distribute hexes to players."
  [board]
  (if (unassigned-hex? board)
    (recur (assoc-in board [(rand-unowned-hex board) :player] (random-player)))
    board))

(defn player-hexes
  "Which hexes on the board does a player own? Returns hexes."
  [board player]
  (filter #(== player (:player %)) board))

(defn rand-reinforceable
  "Return a random position this player can reinforce."
  [board player]
  {:pre [(not (nil? board))
         (not (empty? board))]
   :post [(number? %)]}
  (let [hexes (player-hexes board player)]
    (:pos (rand-nth (filter #(< (:dice %) max-dice) hexes)))))

(defn rand-reinforce
  "Randomly place reinforcements for a single player in owned positions."
  [board player spare-dice]
  {:pre [(not (nil? board))
         (not (empty? board))
         (number? player)
         (number? spare-dice)]}
  (if (zero? spare-dice)
    board
    (recur (update-in board [(rand-reinforceable board player) :dice] inc) player (dec spare-dice))))

(defn assign-start-dice
  "Add starting dice to board."
  ([board]
   (assign-start-dice board (dec num-players)))
  ([board player]
   (if (neg? player)
     board
     (recur (rand-reinforce board player fair-dicenum) (dec player)))))

(defn even-board
  "Generates a relatively even playing field."
  []
  (-> (blank-board)
      assign-positions
      assign-ownership
      assign-start-dice))

(defn player-letter
  "Convert from number to letter. A for zero, B for one, etc."
  [n]
  (char (+ 65 n)))

(defn neighbors
  "Given a position, which hexes are it's neighbors?"
  [pos]
  (let [up (- pos board-size)
        down (+ pos board-size)]
    (filter #(and (>= % 0) (< % board-hexnum))
         (concat (list up down)
               (when-not (zero? (mod pos board-size))
                 (list (dec up) (dec pos)))
               (when-not (zero? (mod (inc pos) board-size))
                 (list (inc pos) (inc down)))))))

(def neighbors (memoize neighbors))

(defn neighbor?
  "Are these two positions on the board adjacent to each other?"
  [src dst]
  (some src (neighbors dst)))

(defn add-new-dice-tco
  "TCO version of add-new-dice."
  ([board player spare-dice] ;; Spare dice here is only for function signature back-compat.
   (add-new-dice-tco board player (largest-cluster-size board player) '[]))
  ([eater-vec player spare-dice accum]
   (if (or (zero? spare-dice)
           (empty? eater-vec))
     (concat accum eater-vec)
     (let [hex (first eater-vec)]
       (if (and (== (:player hex) player)
                (< (:dice hex) max-dice))
         (recur (rest eater-vec)
                player
                (dec spare-dice)
                (conj accum (update hex :dice inc)))
         (recur (rest eater-vec)
                player
                spare-dice
                (conj accum hex)))))))

(defn board-attack
  "Moves dice from src to dst. Returns a new/updated board."
  [board player src dst]
  (map (fn [hex]
         (cond
           (== (:pos hex) (:pos src)) (assoc hex :dice 1)
           (== (:pos hex) (:pos dst)) (new-hex player (dec (:dice src)) (:pos dst))
           :else hex))
       board))

(defn add-passing-move
  "Add a passing move to the collection of moves, if passing is allowed."
  [board player spare-dice first-move moves]
  (if first-move
    moves
    (conj moves
          (new-move nil
                    nil
                    (game-tree (add-new-dice-tco board player (dec spare-dice))
                               (mod (inc player) num-players)
                               0
                               true)
                    nil))))

(defn attacking-moves
  "Adds valid attacking moves to the game tree."
  [board cur-player spare-dice]
  (filter some?
          (mapcat (fn [srchex]
                    (when (== cur-player (:player srchex))
                      (map (fn [dsthexnum]
                             (let [dsthex (nth board dsthexnum)]
                               (when (and (not= (:player dsthex) cur-player)
                                          (> (:dice srchex) 1))
                                 (new-move (:pos srchex)
                                           (:pos dsthex)
                                           (game-tree (board-attack board
                                                                    cur-player
                                                                    srchex
                                                                    dsthex)
                                                      cur-player
                                                      (+ spare-dice (:dice dsthex))
                                                      nil)
                                           (game-tree (attack-fail board (:pos srchex))
                                                      cur-player
                                                      spare-dice
                                                      nil)))))
                           (neighbors (:pos srchex)))))
                  board)))

(defn attack-fail
  "Generate a board where an attack failed."
  [board src]
  (map #(if (= src (:pos %))
          (assoc % :dice 1)
          %)
       board))

(defn roll-dice
  "Rolls a pile of dice."
  [dice-num]
  (reduce + (repeatedly dice-num #(inc (rand-int 6)))))

(defn roll-against
  "Roll two piles of dice"
  [src dst]
  (> (roll-dice src) (roll-dice dst)))

(defn pick-chance-branch
  [board move]
  (if (roll-against (:dice (nth board (:src move)))
                    (:dice (nth board (:dst move))))
    (:tree move)
    (:fail move)))

(defn valid-cluster?
  "Are all elements of this cluster owned by the same player?"
  [board cluster]
  (let [player (:player (nth board (first cluster)))]
    (every? true? (map #(== player (:player (nth board %)))
                       cluster))))

(defn cluster-neighbors
  "Returns the set of all positions neighboring the cluster."
  [cluster]
  (into #{} (mapcat #(neighbors %)
                    cluster)))

(defn can-join-cluster?
  "Given a position and a cluster, can this position be added to the cluster?"
  [board cluster pos]
  (and (contains? (cluster-neighbors cluster) pos) ;; pos is a neigbor to cluster?
       (== (:player (nth board pos)) (:player (nth board (first cluster)))))) ;; owned by the same player?

(defn get-connected
  "Which tiles are connected to this one?"
  ([board pos]
   (let [player (nth board pos)]
     (get-connected board player [pos] 0)))
  ([board player accum index]
   (cond (== index board-hexnum) board
         (can-join-cluster? board accum index) (recur board player (conj accum index) (inc index))
         :else (recur board player accum (inc index)))))

(defn largest-cluster-size
  "Obtain largest cluster size for a given player."
  [board player]
  (apply max (filter some? (for [i (range board-hexnum)]
                             (when (== player (:player (nth board i)))
                               (count (get-connected board i)))))))

(defn score
  "Get a vector with each index representing the player, and values representing the score each player has."
  ([board]
   (let [scores (vec (take num-players (repeat 0)))
         board-index 0]
     (score board scores board-index)))
  ([board scores board-index]
   (let [scores (update scores (:player (nth board board-index)) inc)]
     (if (== board-index (dec board-hexnum))
       scores
       (score board scores (inc board-index))))))

(defn winners
  "Which players captured the highest number of hexagons at the end of the game?"
  ([board]
   (let [scores (score board)
         best (apply max scores)
         n 0
         winlist []]
     (winners board scores best n winlist)))
  ([board scores best n winlist]
   (if (== n num-players)
     winlist
     (if (== best (nth scores n))
       (winners board scores best (inc n) (conj winlist n))
       (winners board scores best (inc n) winlist)))))

(defn threatened?
  "Is this hex in a weak position?"
  [hex board]
  (some true?
        (map (fn [nhexnum]
               (let [nhex (nth board nhexnum)]
                 (when (and (not= (:player hex) (:player nhex))
                            (> (:dice nhex) (:dice hex)))
                   true)))
             (neighbors (:pos hex)))))

(defn score-board
  "Better heuristic for board rating."
  [board player]
  (reduce + (map #(if (== player (:player %))
                    (if (threatened? % board)
                      1
                      2)
                    -1)
                 board)))

(defn get-ratings
  "Returns ratings for follow-up moves."
  [game-tree player]
  (map #(let [src (:src %)
              dst (:dst %)]
          (if (and src dst)
            (let [srchex (nth (:board game-tree) src)
                  dsthex (nth (:board game-tree) dst)
                  probability (nth (nth dice-probability (dec (:dice dsthex))) (- (:dice srchex) 2))]
              (+ (* probability (rate-position (:tree %) player))
                 (* (- 1 probability) (rate-position (:fail %) player))))
            (rate-position (:tree %) player)))
       (:moves game-tree)))

(def get-ratings (memoize get-ratings))

(defn rate-position
  "Minmax rating for a position."
  [game-tree player]
  (if-not (empty? (:moves game-tree))
    (apply (if (== (:player game-tree) player)
             max
             min)
           (get-ratings game-tree player))
    (score-board (:board game-tree) player)))

(defn trimmed-tree
  "Returns a tree from this node, trimmed at given depth."
  [tree depth]
  {:player (:player tree)
   :board (:board tree)
   :moves (if (zero? depth)
            '()
            (map #(new-move (:src %)
                            (:dst %)
                            (trimmed-tree (:tree %) (dec depth))
                            (trimmed-tree (:fail %) (dec depth)))
                 (:moves tree)))})

(defn attackable
  "What positions can be attacked given src as a starting position?"
  [board src]
  (let [srchex (nth board src)]
    (filter some?
            (map (fn [hexnum]
                   (let [dsthex (nth board hexnum)]
                     (when (and (not= (:player srchex) (:player dsthex))
                                (pos? (:dice srchex)))
                       hexnum)))
                   (neighbors src)))))

(defn capable
  "Which positions on this board are capable of mounting an attack?"
  [board player]
  (filter some?
          (map (fn [srchex]
                 (when (and (= (:player srchex) player)
                            (pos? (count (attackable board (:pos srchex))))
                            (> (:dice srchex) 1))
                   (:pos srchex)))
               board)))
