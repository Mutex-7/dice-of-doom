(ns dice.views
  (:require [clojure.string :as string]
            [dice.svg :as svg]
            [dice.dice :as dice]))

(def from-tile (atom nil))
(def cur-game-tree (atom nil))

(defn web-init
  []
  (reset! cur-game-tree (dice/game-tree (dice/even-board) 0 0 true))
  (reset! from-tile nil))

(defn help
  "Displays help page."
  []
  (str "<ol>"
       "<li>Don't panic."
       "<li>This isn't perfect. I'll probably change this a little before making the write-up."
       "<li>Viewing / will reset the game."
       "<li>Viewing /help will display this page."
       "<li>Viewing /todo will display additional features and bugfixes that could be added to this project."
       "<li>The game will end if the current player cannot make a move for their first action."
       "<li>Reinforcements are granted equal to the largest contiguous body of tiles owned by that player."
       "</ol>"))

(defn todo
  "Displays various ways this game could be improved."
  []
  (str "<h1>High priority:</h1>"
       "<ul>"
       "<li>Performance issues, seemingly when moving dice."
       "<li>Place reinforcement dice in random places."
       "</ul>"
       "<h1>Low priority:</h1>"
       "<ul>"
       "<li>Scale the board to match browser resolution."
       "<li>Add session capability. Right now anybody connecting will view the only game in progress."
       "<li>Moar testing. Or any testing for that matter :P"
       "<li>Additional defensive coding practices (such as more :pre and :post conditions)."
       "<li>Throw some CSS in here."
       "<li>I guess I should have used Hiccup for the HTML. Oh well."
       "<li>Text to indicate the result of a dice battle."
       "</ul>"))

(defn announce-winner
  [board]
  (let [w (dice/winners board)]
    (str (if (> (count w) 1)
           (str "The game is a tie between " (map dice/player-letter w))
           (str "The winner is " (dice/player-letter (first w)) ". "))
         (svg/tag "a" {:href "/"} "Play again"))))

(defn select-tree
  [game-tree src dst]
  (some #(when (and (= src (:src %))
                    (= dst (:dst %)))
           (if (and src dst)
             (dice/pick-chance-branch (:board game-tree) %) ;; Non-passing move
             (:tree %))) ;; Passing move
        (:moves game-tree)))

(defn handle-computer
  [game-tree]
  (let [ratings (dice/get-ratings (dice/trimmed-tree game-tree dice/ai-level) (:player game-tree))
        chosen-move (nth (:moves game-tree) (.indexOf ratings (apply max ratings)))]
    (reset! cur-game-tree (select-tree @cur-game-tree (:src chosen-move) (:dst chosen-move))))
  (string/join (svg/tag "script" () "window.setTimeout('window.location=\"hexgame\"',2000)")))

(defn draw-board
  [tree selected-tile]
  (svg/svg svg/board-width
           svg/board-height
           (svg/draw-board (:board tree)
                           selected-tile
                           (if selected-tile
                             (dice/attackable (:board tree) selected-tile)
                             (dice/capable (:board tree) (:player tree))))))

(defn handle-human
  [chosen]
  (cond (nil? chosen) (str "Please chose a hex to move from:") ;; Player hasn't chosen a hex yet
        (= chosen "pass") (do (reset! cur-game-tree (select-tree @cur-game-tree nil nil)) ;; Did the player pass? (add reinforcements)
                              (str "Reinforcements have been placed. " (svg/tag a {:href (svg/make-game-link nil)} "Continue")))
        (nil? @from-tile) (do (reset! from-tile chosen) ;; Player has chosen a tile to move from (prompt user for destination)
                              (str "Now choose a destination."))
        (= chosen @from-tile) (do (reset! from-tile nil) ;; Tile was selected twice (should deselect)
                                  (str "Move canceled."))
        :else (do (reset! cur-game-tree (select-tree @cur-game-tree @from-tile chosen)) ;; Valid attacking move has been completed. Move: from-tile->chosen
                  (reset! from-tile nil)
                  (str "You may now "
                       (svg/tag a {:href (svg/make-game-link "pass")} "Pass")
                       " or make another move."))))

(defn hex-game
  "Handles building of the entire page."
  [chosen]
   (when-not cur-game-tree
     (web-init))
   (str "<!doctype html>"
        (svg/tag "center" ()
                 "Welcome to dice of doom! View <a href=\"/help\">/help</a> or <a href=\"/todo\">/todo</a> for more information."
                 (svg/tag "br" ())
                 (cond (empty? (:moves @cur-game-tree)) (announce-winner (:board @cur-game-tree))
                       (zero? (:player @cur-game-tree)) (handle-human chosen)
                       :else (handle-computer @cur-game-tree))
                 (svg/tag "br" ())
                 (draw-board @cur-game-tree @from-tile))))
