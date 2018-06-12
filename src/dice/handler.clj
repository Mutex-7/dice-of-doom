(ns dice.handler
  (:require [dice.views :as views]
            [dice.dice :as dice]
            [dice.svg :as svg]
            [compojure.core :refer :all]
            [compojure.route :as route]
            [compojure.coercions :as coercions]
            [ring.adapter.jetty :as jetty]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]])
  (:gen-class))

(defroutes app-routes
  (GET "/" []
       (views/web-init)
       (views/hex-game nil))
  (GET "/hexgame" [chosen :<< coercions/as-int]
       (views/hex-game chosen))
  (GET "/hexgame" [chosen]
       (views/hex-game chosen))
  (GET "/hexgame" []
       (views/hex-game nil))
  (GET "/help" []
       (views/help))
  (GET "/todo" []
       (views/todo))
  (route/not-found "Page Not Found :P"))

(def app
  (wrap-defaults app-routes site-defaults))

(defn -main
  [& [port]]
  (let [port (Integer. (or port
                           (System/getenv "PORT")
                           5000))]
    (jetty/run-jetty #'app {:port port :join? false})))
