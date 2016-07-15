(ns livecode.core
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Rigidbody Vector3 Vector2 GameObject Component Transform]
           [clojure.lang Var]))

(defn demo []
  (position (v2 3))
  (position (v3 0 3 1))
  (position (object-named "Main Camera"))
  (map position (seq (.transform (object-named "Canvas"))))
  (def camera (object-named "Main Camera"))
  (position #'camera))

(defn force! [obj f]
  (.AddForce (ensure-component obj Rigidbody) f))