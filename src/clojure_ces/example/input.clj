(ns clojure-ces.example.input
  (:require [clojure.tools.logging :as log])
  (:import (de.gurkenlabs.litiengine.input Input IKeyboard)
           (java.util.function Consumer)
           (java.awt.event KeyEvent)))


(def keys-down (atom {}))


(defn key-pressed [^KeyEvent event]
  (swap! keys-down assoc (.getKeyCode event) true))

(defn key-released [^KeyEvent event]
  (swap! keys-down dissoc (.getKeyCode event)))

(defn register-key-handlers []
  (log/info "Register handlers")
  (let [^IKeyboard keyboard (Input/keyboard)]
    (.onKeyPressed keyboard
                   (reify Consumer
                     (accept [_ event]
                       (key-pressed event))))
    (.onKeyReleased keyboard
                    (reify Consumer
                      (accept [_ event]
                        (key-released event))))))