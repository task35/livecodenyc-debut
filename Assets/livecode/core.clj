(ns livecode.core
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

(state (.gameObject Camera/main))

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

(def sun
  (object-named "Directional Light"))

(defn until-time [f ms]
  (let [stop-time (+ Time/time ms)]
    (livecode.core/animate
      (fn []
        (f)
        (< Time/time stop-time)))))
