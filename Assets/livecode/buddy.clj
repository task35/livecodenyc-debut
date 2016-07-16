(ns livecode.buddy
  (:require [livecode.network :as net])
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Camera Rigidbody]))

(defn start [go]
  (set! (.. go transform position)
        (v3 (rand)
            (+ 10 (.. Camera/main transform position y))
            (rand))))

(def directions
  {"left"   (v3 -0.5 0 0)
   "right"  (v3 0.5 0 0)
   "top"    (v3 0 0 0.5)
   "bottom" (v3 0 0 -0.5)
   "center" (v3 0 0.5 0)})

(defn apply-movement [go dir]
  (.AddForce (cmpt go Rigidbody)
             dir
             ForceMode/Impulse))

(defn move [go]
  (let [buttons @net/buttons
        button (-> go .name buttons)]
    (if-let [dir (directions button)]
      (apply-movement go dir))))