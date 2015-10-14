(defproject yada "0.1.0-SNAPSHOT"
  :description "STAMP yada in Clojure"
  :url "http://example.com/TODO"
  ; Note: exclude org.clojure/clojure everywhere to make sure we're using our
  ; version of Clojure all the time.
  :dependencies [[org.clojure/tools.cli "0.3.3" :exclusions [org.clojure/clojure]]
                 [com.taoensso/timbre "4.1.4" :exclusions [org.clojure/clojure]]]
  :resource-paths ["resources/clojure-1.6.0.jar"]
  :main ^:skip-aot yada.main
  :target-path "target/%s"
  :profiles {
    :dev {
      :plugins [[lein-cloverage "1.0.6"]]
    }
    :uberjar {
      :aot :all
    }
  })