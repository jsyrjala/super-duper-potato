(defproject clojure-ces "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/tools.logging "0.4.1"]
                 [ch.qos.logback/logback-classic "1.2.3"]

                 [play-cljc "0.3.0"]

                 [nrepl "0.6.0"]

                 [org.lwjgl/lwjgl "3.2.2"]
                 [org.lwjgl/lwjgl "3.2.2"
                  :classifier "natives-macos"
                  ;;:native-prefix ""
                  ]

                 [org.lwjgl/lwjgl-glfw "3.2.2"]
                 [org.lwjgl/lwjgl-glfw "3.2.2"
                  :classifier "natives-macos"
                  ;;:native-prefix ""
                  ]

                 [org.lwjgl/lwjgl-opengl "3.2.2"]
                 [org.lwjgl/lwjgl-opengl "3.2.2"
                  :classifier "natives-macos"
                  ;;:native-prefix ""
                  ]

                 [org.lwjgl/lwjgl-stb "3.2.2"]
                 [org.lwjgl/lwjgl-stb "3.2.2"
                  :classifier "natives-macos"
                  ;;:native-prefix ""
                  ]
                 ]
  :profiles {:dev {:dependencies [
                                  [criterium "0.4.5"]
                                  ]
                   }
            }
  :jvm-opts ["-XX:+PrintCommandLineFlags"
             "-XstartOnFirstThread"
             ]
  :repl-options {:init-ns  clojure-ces.example.input
                 }
  :main clojure-ces.example.input/start
  )
