(defproject tetris "0.2.0"
  :description "Tetris in Reagent"
  :url "https://github.com/sunflowerseastar/tetris"
  :license {:name "GNU Affero General Public License v3"
            :url "http://www.gnu.org/licenses/agpl-3.0.html"}

  :min-lein-version "2.7.1"

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.520"]
                 [net.mikera/core.matrix "0.62.0"]
                 [org.clojure/math.combinatorics "0.1.6"]
                 [tupelo "0.9.201"]
                 [reagent "0.8.1"]]

  :source-paths ["src"]

  :aliases {"fig"       ["trampoline" "run" "-m" "figwheel.main"]
            "fig:build" ["trampoline" "run" "-m" "figwheel.main" "-b" "dev" "-r"]
            "fig:min"   ["run" "-m" "figwheel.main" "-O" "advanced" "-bo" "dev"]
            "fig:test"  ["run" "-m" "figwheel.main" "-co" "test.cljs.edn" "-m" "tetris.test-runner"]}

  :profiles {:dev {:dependencies [[com.bhauman/figwheel-main "0.2.3"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]}})
