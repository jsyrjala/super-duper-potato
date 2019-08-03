(ns clojure-ces.example.graphics-swing
  (:require [clojure-ces.system :as system])
  (:import [java.awt Canvas Graphics]
           [javax.swing JFrame])
  )


(defn create-graphics [size]
  (let [frame (JFrame. "Asteroid")
        canvas (doto (Canvas.)
                 (.setSize (size 0) (size 1)))]
    (.add frame canvas)
    (.pack frame)
    (.setVisible frame true)
    {::frame frame ::canvas canvas}
    )
  )

(defn draw-actor [graphics entity]
  (let [canvas (::canvas graphics)
        name (-> (system/first-component entity :named)
                 :named/name)
        position (system/first-component entity :position)
        drawable (system/first-component entity :drawable)]
    (println "draw actor" name position drawable graphics)

    ;; (.fillOval canvas 100 100 200 200)
    nil
    )
  )
