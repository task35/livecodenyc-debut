(ns livecode.core
  (:require [livecode.buddy :as bud]
            [clojure.string :as string]
            [arcadia.internal.name-utils :refer [camels-to-hyphens]])
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Rigidbody Vector3 Vector2 GameObject Component Transform]
           [clojure.lang Var]
           IEnumerator
           ArcadiaState))

(defn import-namespace [n]
  (->> AppDomain/CurrentDomain
       .GetAssemblies
       (mapcat #(.GetTypes %))
       (filter #(= (.Namespace %) n))
       (map #(try (.importClass *ns* %)
               (catch Exception e)))
       dorun))

(import-namespace "UnityEngine")

(def solids
  (->> (Resources/LoadAll "Polyhedra")
       (filter #(= (type %) GameObject))
       (mapcat (fn [s] [(keyword (-> s .name camels-to-hyphens string/lower-case))
                        s]))
       (apply hash-map)))

(defn solid
  ([position] (solid (-> solids keys rand-nth) position))
  ([name position]
   (let [p (instantiate (solids name) position)]
     (cmpt+ p MeshCollider)
     (cmpt+ p Rigidbody)
     p)))

(def coro-root (cmpt Camera/main ArcadiaState))

(defn animate [f]
  (.StartCoroutine
    coro-root
    (reify IEnumerator
      (MoveNext [this] (f))
      (get_Current [this]))))

(defn timeline [fns]
  (let [i (volatile! 0)
        v (volatile! nil)]
    (.StartCoroutine
      coro-root
      (reify IEnumerator
        (MoveNext [this]
                  (let [f (get fns @i)
                        r (f)]
                    (when-not r
                      (vswap! i inc)
                      (vreset! v r)))
                  (< @i (count fns)))
        (get_Current [this] @v)))))

(defn set-gravity [g]
  (doseq [rb (objects-typed Rigidbody)]
    (set! (.useGravity rb) g)))1

(defn set-confetti [c]
  (let [ps (cmpt (object-named "confetti") ParticleSystem)]
    (set! (.enableEmission ps) c)))

(defn set-flight [f]
  (swap! bud/directions
    assoc "center" (if f
                     (v3 0 0.5 0)
                     (v3 0))))
(def sun
  (object-named "Directional Light"))

(defn until-time [f ms]
  (let [stop-time (+ Time/time ms)]
    (livecode.core/animate
      (fn []
        (f)
        (< Time/time stop-time)))))