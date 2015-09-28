(defproject yada "0.1.0-SNAPSHOT"
  :description "STAMP yada in Clojure"
  :url "http://example.com/TODO"
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