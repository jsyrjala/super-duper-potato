(defproject clojure-ces "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ch.qos.logback/logback-classic "1.2.3"]
                 [de.gurkenlabs/litiengine "0.4.17"]
                 [nrepl "0.6.0"]
                 ]
  :profiles {:dev {:dependencies [
                                  [criterium "0.4.5"]
                                  ]
                   }
            }
  ;; :injections [(.. System (setProperty "clojure.server.repl" ""{:port 5555 :accept clojure.core.server/repl}""))]
  :jvm-opts ["-XX:+PrintCommandLineFlags"
             ;; "-XstartOnFirstThread"
              "-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"
             ]
  ;; :repl-options {:init-ns  clojure-ces.example.input}
  ;; :main clojure-ces.example.input/start
  :main clojure-ces.example.litiengine

  )
