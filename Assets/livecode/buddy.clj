(ns livecode.buddy
  (:require [livecode.network :as net])
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Camera Rigidbody Renderer Color GUIText]
           [UnityEditor EditorGUIUtility]))

(defn follow-buddy [bud]
  (fn [go]
    (if (null-obj? bud)
      (destroy go)
      (set! (.. go transform position)
            (.WorldToViewportPoint
              Camera/main
              (.. bud transform position))))))

(defn attach-name-tag [bud]
  (let [nt (GameObject. (str (.name bud) "-nametag"))]
    (cmpt+ nt GUIText)
    (with-cmpt nt [gt GUIText]
               (set! (.text gt) (last (re-find #"\.(.*)"(.name bud))))
               (set! (.fontSize gt) 40)
               (set! (.alignment gt) TextAlignment/Center)
               (set! (.anchor gt) TextAnchor/MiddleCenter)
               (set! (.pixelOffset gt) (v2 0 40)))
    (hook+ nt
           :update
           (follow-buddy bud))))

(defn start [go]
  (set! (.. go transform position)
        (v3 (rand)
            (+ 10 (.. Camera/main transform position y))
            (rand)))
  (set! (.. (cmpt go Renderer) material color)
        (EditorGUIUtility/HSVToRGB (rand) 1.0 1.0))
  (attach-name-tag go))

(def directions
  (atom
    {"left"   (v3 -0.5 0 0)
     "right"  (v3 0.5 0 0)
     "top"    (v3 0 0 0.5)
     "bottom" (v3 0 0 -0.5)
     "center" (v3 0 0 0)}))

(defn apply-movement [go dir]
  (.AddForce (cmpt go Rigidbody)
             dir
             ForceMode/Impulse))

(defn move [go]
  (let [buttons @net/buttons
        button (-> go .name buttons)]
    (if-let [dir (@directions button)]
      (apply-movement go dir))))