(ns clojure-ces.example.vector)


(defn vector2 [x y]
  [(double x) (double y)])

(defn number [value]
  (or value 0))

(defn length [v]
  (let [[x y] v]
    (Math/hypot x y)))

(defn distance [v1 v2]
  (let [[x1 y1] v1
        [x2 y2] v2]
    (Math/hypot (- x1 x2) (- y1 y2))))

(defn scale [v s]
  (let [[x y] v]
    [(* s x) (* s y)]))

(defn negate [v]
  (scale v -1))

(defn normalize [v]
  (let [len (length v)]
    (if (< len 0.00000001)
      [1 0]
      (scale v (/ 1.0 len)))))

(defn clamp [v max-length]
  (let [d (length v)]
    (if (< d max-length)
      v
      (scale (normalize v) max-length))))

;; TODO very much assumes 2 component vectors
(defn add [v1 v2]
  {:pre [(vector? v1)
         (= (count v1) 2)
         (vector? v2)
         (= (count v2) 2)]}
  (let [[x1 y1] v1
        [x2 y2] v2]
    [(+ (number x1) (number x2))
     (+ (number y1) (number y2))]))

(defn rotate [v rad]
  (let [[x y] v
        c (Math/cos rad)
        s (Math/sin rad)]
    [(- (* x c) (* y s))
     (+ (* x s) (* y c))]))

(defn wrap-around [v bounding-box]
  (let [[min-x min-y max-x max-y] bounding-box
        [x y] v
        width (- max-x min-x)
        height (- max-y min-y)
        ;; TODO v could be multiple screenfulls avay..
        new-x (cond (< x min-x) (+ x width)
                    (> x max-x) (- x width)
                    :else x)
        new-y (cond (< y min-x) (+ y height)
                    (> y max-x) (- y height)
                    :else y)]
    [new-x new-y]
    ))