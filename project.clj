(defproject dice "0.1.0-SNAPSHOT"
  :description "Clojure port of Land of Lisp's dice of doom game"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [compojure "1.5.1"]
                 [ring/ring-defaults "0.2.1"]
                 [ring/ring-jetty-adapter "1.5.1"]]
  :plugins [[lein-ring "0.9.7"]]
  :ring {:handler dice.handler/app}
  :main dice.handler
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring/ring-mock "0.3.0"]]}})
