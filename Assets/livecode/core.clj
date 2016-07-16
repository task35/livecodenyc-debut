(ns livecode.core
  (:require [livecode.buddy :as bud]
            [clojure.string :as string]
            [arcadia.internal.name-utils :refer [camels-to-hyphens]])
  (:use arcadia.core
        arcadia.linear)
  (:import [UnityEngine Rigidbody Vector3 Vector2 GameObject Component Transform]
           [clojure.lang Var]
           IEnumerator
           ArcadiaState
           Helpers))

(defn tile-at [x y]
  (Helpers/Raycast
    (v3 x 10 y)
    (v3 0 -1 0)
    9))

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
   (instantiate (solids name) position)))

(defn mesh-collides [obj]
  (let [rb (cmpt+ obj Rigidbody)
        mc (cmpt+ obj MeshCollider)]
    (set! (.useGravity rb) false)
    (set! (.isKinematic rb) true)
    (set! (.sharedMesh mc)
          (-> obj children first (cmpt MeshFilter) .mesh))
    obj))

(defn impulse [o v]
  (.AddForce (cmpt o Rigidbody) v ForceMode/Impulse))

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

(defn once [f] (fn [] (f) false))

(defn translate [obj v]
  (if-let [rb (cmpt obj Rigidbody)]
    (.MovePosition rb (v3+ (.position rb) v))
    (.. obj translate (Translate v) Space/World)))

(defn move-to
  ([obj pos] (move-to obj pos 1))
  ([obj pos speed]
   (let [opos (.. obj transform position)
         distance (Vector3/Distance opos pos)
         dir (-> (v3- pos opos)
                 .normalized
                 (v3* speed))]
     (timeline
       [(fn []
          (translate obj (v3* dir Time/deltaTime))
          (> (Vector3/Distance (.. obj transform position) pos)
             (* Time/deltaTime speed)))
        (once #(set! (.. obj transform position) pos))]))))

(defn move-by
  ([obj offs] (move-by obj offs 1))
  ([obj offs speed]
   (move-to obj (v3+ (.. obj transform position) offs)
            speed)))

(defn set-gravity [g]
  (doseq [rb (objects-typed Rigidbody)]
    (set! (.useGravity rb) g)))

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