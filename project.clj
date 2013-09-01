(defproject matrixtests "0.1.0-SNAPSHOT"
  :description "Speed tests on matrix manipulations"
  :url "http://fbmnds.blogspot.de/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/tools.macro "0.1.2"]
                 [incanter/incanter-core "1.5.2"]
                 [clatrix "0.3.0"]
                 [org.jblas/jblas "1.2.3"]
                 [net.mikera/core.matrix "0.7.2"]
                 [slingshot "0.10.3"]
                 [criterium "0.4.1"]]
  :profiles {:dev {:dependencies [[midje "1.5.1"]]
                   :plugins [[lein-midje "3.1.0"]]}}
  :jvm-opts ["-Xmx2048M"]
  :repl-options {:port 4555}
  )
