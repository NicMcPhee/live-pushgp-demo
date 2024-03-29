(defproject push-live-demo "0.8.0"
            :description "A live demo of Push and PushGP"
            :url "https://github.com/NicMcPhee/live-pushgp-demo"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.10.0"]
                           [org.clojure/core.async "0.4.500"]
                           [ring/ring-devel "1.7.1"]
                           [compojure "1.6.1"]
                           [ring-server "0.5.0"]
                           [cryogen-markdown "0.1.11"]
                           [cryogen-core "0.2.1"]]
            :plugins [[lein-ring "0.12.5"]]
            :main cryogen.core
            :ring {:init cryogen.server/init
                   :handler cryogen.server/handler})
