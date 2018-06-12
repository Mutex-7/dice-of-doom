(ns dice.svg
  (:require [clojure.string :as string]
            [clojure.core :as core]
            [clojure.string :as str]
            [dice.dice :as dice]))

(def stroke-width 1)
(def board-width 900)
(def board-height 500)
(def board-scale 64)
(def top-offset 3)
(def dice-scale 40)
(def dot-size 0.05)

(def dice-top [{:x 0 :y -1}
               {:x -0.6 :y -0.75}
               {:x 0 :y -0.5}
               {:x 0.6 :y -0.75}])

(def dice-left [{:x 0 :y -0.5}
                {:x -0.6 :y -0.75}
                {:x -0.6 :y 0}
                {:x 0 :y 0.25}])

(def dice-right [{:x 0 :y -0.5}
                 {:x 0.6 :y -0.75}
                 {:x 0.6 :y 0}
                 {:x 0 :y 0.25}])

(def dice-dots [{:x -0.05 :y -0.875}
                {:x 0.125 :y -0.8}
                {:x 0.3 :y -0.725}
                {:x -0.3 :y -0.775}
                {:x -0.125 :y -0.7}
                {:x 0.05 :y -0.625}
                {:x 0.2 :y -0.35}
                {:x 0.2 :y -0.05}
                {:x 0.45 :y -0.45}
                {:x 0.45 :y -0.15}
                {:x -0.45 :y -0.45}
                {:x -0.2 :y -0.05}])

(def polysquare [{:x -1 :y -1}
                 {:x -1 :y 1}
                 {:x 1 :y 1}
                 {:x 1 :y -1}])

(def hexpoints [{:x -1 :y -0.2}
                {:x 0 :y -0.5}
                {:x 1 :y -0.2}
                {:x 1 :y 0.2}
                {:x 0 :y 0.5}
                {:x -1 :y 0.2}])

(def chosen-color "yellow")

(def player-colors ["red" "blue" "green" "purple"])

(defn mk-tag
  "Builds arbitrary tags."
  [name attributes isClosing]
  (str "<"
       (when isClosing
         "/")
       (string/lower-case name)
       (string/join (for [[key value] attributes]
                      (str " " (string/lower-case (core/name key)) "=\"" (string/lower-case value) "\"")))
       ">"))

(defmacro tag
  "Used to create generic tag/xml like structures."
  [name attributes & body]
  `(str
     (mk-tag '~name ~attributes false)
     ~@body
     (mk-tag '~name () true)))

(defmacro svg
  "Used to create svg images."
  [width height & body]
  `(tag "svg" {:width ~width
               :height ~height
               :xmlns "http://www.w3.org/2000/svg"
               :xmlns:xlink "http://www.w3.org/1999/xlink"}
        ~@body))

(defn circle
  [cx cy radius fill stroke]
  (tag "circle" {:cx cx
                 :cy cy
                 :r radius
                 :fill fill
                 :stroke stroke
                 :stroke-width stroke-width}))

(defn polygon
  "Make sure you format the points properly first."
  [points fill stroke]
  (tag "polygon" {:points points
                  :fill fill
                  :stroke stroke
                  :stroke-width stroke-width}))

;;(defn random-walk
;;  "Use this to make a kinda random polygon."
;;  [value length]
;;  (when-not (zero? length)
;;    (cons value
;;          (random-walk (if (zero? (rand-int 2))
;;                         (dec value)
;;                         (inc value))
;;                       (dec length)))))

;;(defn gen-points
;;  "Generates a random series of points in the svg format."
;;  ([]
;;   (str "0,200" (gen-points "" 0 100 400) " 400,200"))
;;  ([accum x y length]
;;   (when-not (zero? length)
;;     (let [x (inc x)
;;           y (if (zero? (rand-int 2))
;;               (inc y)
;;               (dec y))]
;;       (str accum " " x "," y (gen-points accum x y (dec length)))))))

;;(defn financial-svg
;;  []
;;  (svg 400 200 (polygon (gen-points) "red" "black")))

(defn maps-to-points
  [maplist]
  (string/join " " (map #(str (:x %) "," (:y %))
                    maplist)))

(defn draw-die
  [x y color]
  (letfn [(calc-pt [point]
            {:x (+ x (* dice-scale (:x point)))
             :y (+ y (* dice-scale (:y point)))})]
    (str (polygon (maps-to-points (map calc-pt dice-top)) color "black")
         (polygon (maps-to-points (map calc-pt dice-left)) color "black")
         (polygon (maps-to-points (map calc-pt dice-right)) color "black")
         (string/join (map (fn [point] ;; Draw dots here
                             (polygon (maps-to-points (map #(calc-pt {:x (+ (:x point) (* (:x %) dot-size))
                                                                      :y (+ (:y point) (* (:y %) dot-size))})
                                                           polysquare))
                                      "white"
                                      "grey"))
                           dice-dots)))))

(defn draw-hex
  "This function just draws a hexagonal polygon. Not the entire hex."
  [xx yy color]
  (string/join (polygon (maps-to-points (map (fn [point]
                                               {:x (+ xx (* board-scale (:x point)))
                                                :y (+ yy (* board-scale (:y point)))})
                                             hexpoints))
                        color
                        "black")))

(defn draw-selector
  "This just draws a positional indicator for tiles."
  [x y selected]
  (tag "ellipse" {:cx x
                  :cy y
                  :rx (* board-scale 0.6)
                  :ry (* board-scale 0.3)
                  :fill "none"
                  :stroke chosen-color
                  :stroke-width (if selected
                                  4
                                  2)}))

(defn draw-tile
  [x y pos hex xx yy color chosen-tile highlighted]
    (str (draw-hex xx (+ yy 5) color)
         (draw-hex xx yy color)
         (when highlighted
           (draw-selector xx yy false))
         (when (= pos chosen-tile)
           (draw-selector xx yy true))
         (string/join (for [i (range (:dice hex))]
                        (draw-die (+ xx (* dice-scale 0.3 (if (odd? (+ x y i))
                                                            -0.3
                                                            0.3)))
                                  (- yy (* dice-scale i 0.8))
                                  color)))))

(defn make-game-link
  [pos]
  (str "/hexgame?chosen=" pos))

(defn draw-board
  [board chosen-tile legal-tiles]
  (string/join (for [y (range dice/board-size)]
                 (string/join (for [x (range dice/board-size)]
                                (let [pos (+ x (* dice/board-size y))
                                      hex (nth board pos)
                                      xx (* board-scale (+ (* 2 x) (- dice/board-size y))) ;; pixel coordinates
                                      yy (* board-scale (+ (* y 0.7) top-offset)) ;; pixel coordinates
                                      color (nth player-colors (:player hex))]
                                  (if (or (= pos chosen-tile)
                                          (some #(= pos %) legal-tiles))
                                    (tag "g" {}
                                         (tag "a" {"xlink:href" (make-game-link pos)}
                                              (draw-tile x y pos hex xx yy color chosen-tile true)))
                                    (draw-tile x y pos hex xx yy color chosen-tile false))))))))

;;(defn board-test
;;  [tree-atom]
;;  (svg board-width board-height (draw-board (:board @tree-atom) nil nil)))
