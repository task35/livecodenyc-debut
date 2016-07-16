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

(defn coordinates [o]
  ((juxt #(int (.x %)) #(int (.z %))) (.. o transform position)))

(def coordinates-map
  (let [ch (children (object-named "Tiles"))]
    (->> ch
         (interleave (map coordinates ch))
         (apply hash-map))))

(defn tile-at
  ([v]
   (tile-at (.x v) (.y v)))
  ([x y]
   (coordinates-map [(int x) (int y)])))

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

(defn physical [obj]
  (let [rb (cmpt+ obj Rigidbody)
        sc (cmpt+ obj SphereCollider)]
    (set! (.useGravity rb) true)
    (set! (.isKinematic rb) false)
    (set! (.radius sc)
          
          (-> obj children first (cmpt MeshFilter) .mesh .bounds .max .magnitude)
          )
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

(defn position! [obj p]
  (if-let [rb (cmpt obj Rigidbody)]
    (.MovePosition rb p)
    (set! (.. obj translate position) p)))

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

(def ^:dynamic *tile-speed* 5)

(defn raise-tile
  ([v h] (raise-tile (.x v) (.y v) h))
  ([x y h] (raise-tile x y h *tile-speed*))
  ([x y h speed]
   (let [t (tile-at x y)]
     (move-by t (v3 0 h 0) speed))))

(defn set-tile-height
  ([v h] (set-tile-height (.x v) (.y v) h))
  ([x y h]
   (let [t (tile-at x y)]
     (position! t (v3 x h y)))))

(defn reset-tile
  ([v] (reset-tile (.x v) (.y v)))
  ([x y] (reset-tile x y *tile-speed*))
  ([x y speed]
   (let [t (tile-at x y)]
     (move-by t (v3 0 (- 0 (.. t transform position y) 5.5) 0) speed))))

(defn goal-post [w z h]
  (raise-tile (- w) z h)
  (raise-tile w z h))

(defn reset-all-tiles []
  (dorun
    (for [x (range -20 20)
          y (range -20 20)]
      (reset-tile x y))))

(defn stairs
  [x0 x1 y0 y1 f]
  (for [x (range (min x0 x1) (max x0 x1))
        y (range (min y0 y1) (max y1 y1))]
    (raise-tile x y (f x y))))

(defn stop-coroutines []
  (.StopAllCoroutines coro-root))

(defn set-gravity [g]
  (doseq [rb (objects-typed Rigidbody)]
    (set! (.useGravity rb) g)))

(defn set-confetti [c]
  (let [ps (cmpt (object-named "confetti") ParticleSystem)]
    (if c (.Play ps))
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

(defn sleep-all []
  (doseq [rb (objects-typed Rigidbody)]
    (.Sleep rb)))

(defn stop-reset []
  (stop-coroutines)
  (reset-all-tiles))

(defn big-text [s]
  (set! (.text (cmpt (object-named "big-text") GUIText))
    s))

;; ============================================================
;; fun macros

(defmacro set-with! [obj [sym & props] & body]
  `(let [obj# ~obj
         ~sym (.. obj# ~@props)]
     (set! (.. obj# ~@props) (do ~@body))))

(defmacro when-set!
  ([obj k v]
   (let [objsym (gensym "obj__")
         access (if (symbol? k)
                  `(. ~objsym ~k)
                  `(.. ~objsym ~@k))]
     `(let [~objsym ~obj]
        (when-let [v# ~v]
          (set! ~access v#)))))
  ([obj k v & kvs]
   (assert (even? (count kvs)))
   (let [objsym (gensym "obj__")]
     `(let [~objsym ~obj]
        ~@(for [[k v] (partition 2 (list* k v kvs))]
            `(when-set! ~objsym ~k ~v))))))

;; yes but you too are gross
(defmacro if-in-let
  ([m [n k] then]
   `(when-let [e# (find ~m ~k)]
      (let [~n (val e#)]
        ~then)))
  ([m [n k] then else]
   `(if-let [e# (find ~m ~k)]
      (let [~n (val e#)]
        ~then)
      ~else)))

;; murp.
(defmacro when-in-set!
  ([obj map path key]
   (let [objsym (gensym "obj__")
         access (if (symbol? path)
                  `(. ~objsym ~path)
                  `(.. ~objsym ~@path))]
     `(let [~objsym ~obj] 
        (if-in-let ~map [v# ~key]
          (set! ~access v#)))))
  ([obj map path key & path-keys]
   (let [objsym (gensym "obj__")
         mapsym (gensym "map__")]
     `(let [~objsym ~obj
            ~mapsym ~map]
        ~@(for [[path key] (partition 2 (list* path key path-keys))]
            `(when-in-set! ~objsym ~mapsym ~path ~key))))))

;; ============================================================
;; physics!

;; not that we use this anywhere
(defn fm-convert [x]
  (case x
    :force ForceMode/Force
    :acceleration ForceMode/Acceleration
    :impulse ForceMode/Impulse
    :velocity-change ForceMode/VelocityChange
    x))

(defn pmc-convert [x]
  (case x
    :average PhysicMaterialCombine/Average
    :minimum PhysicMaterialCombine/Minimum
    :multiply PhysicMaterialCombine/Multiply
    :maximum PhysicMaterialCombine/Maximum
    x))

(defn physic-material ^PhysicMaterial [m]
  (let [^PhysicMaterial pm (PhysicMaterial.)
        m2 (as-> m m
             (if (contains? m :bounce-combine)
               (update m :bounce-combine pmc-convert)
               m)
             (if (contains? m :friction-combine)
               (update m :friction-combine pmc-convert)
               m))]
    (when-in-set! pm m2
      bounceCombine :bounce-combine
      bounciness :bounciness
      frictionCombine :friction-combine
      staticFriction :static-friction
      dynamicFriction :dynamic-friction)
    pm))

(def bouncer
  (physic-material
    {:bounciness 0.9
     :bounce-combine :average
     :dynamic-friction 0
     :friction-combine :minimum}))


;; ==================================================

(defn v2-dist [a b]
  (Vector2/Distance a b))

(defmacro v2dest [[x y v] & body]
  `(let [v# ~v
         ~x (.x v#)
         ~y (.y v#)]
     ~@body))

(definline v2-inv [v]
  `(let [v# ~v]
     (v2 (/ 1 (.x v#)) (/ 1 (.y v#)))))

(defn raise-tile-to
  ([v h] (raise-tile-to (.x v) (.y v) h))
  ([x y h] (raise-tile-to x y h *tile-speed*))
  ([x y h speed]
   (let [t (tile-at x y)]
     (with-cmpt t [tr Transform]
       (move-to t
         (v3
           (.. tr position x)
           (- h 5.5)
           (.. tr position z))
         speed)))))

(defn wall-to [start hop len height]
  (dorun
    (for [v (take len (iterate #(v2+ % hop) start))]
      (raise-tile-to v height))))

(defn box [start extension height]
  (let [box-breadth (.x extension)
        box-depth (.y extension)
        other-corner (v2+ start (v2+ start extension (v2 -1)))]
    (wall-to start        (v2  0  1) box-depth height)
    (wall-to start        (v2  1  0) box-breadth height)
    (wall-to other-corner (v2  0 -1) box-depth height)
    (wall-to other-corner (v2 -1  0) box-breadth height)))

(defn block [start extension height]
  (dorun (for [x (range (.x extension))
               z (range (.y extension))]
           (raise-tile-to (v2+ start (v2 x z)) height))))

(defn on-block [start extension f]
  (dorun
    (for [x (range (.x extension))
          y (range (.y extension))
          :let [pos (v2+ start (v2 x y))]]
      (raise-tile-to pos (f pos)))))

(defn block-t [start extension t f]
  (let [start-time Time/time]
    (animate
      (fn []
        (when (< Time/time (+ start-time t))
          (let [tdiff (- Time/time start-time)]
            (dotimes [xi (.x extension)]
              (dotimes [yi (.y extension)]
                (let [pos (v2+ start (v2 xi yi))]
                  (let [tile (tile-at pos)
                        tpos (.. tile transform position)]
                    (when-let [h (f (v2 (.x tpos) (.y tpos)) tdiff)]
                      (set! (.. tile transform position)
                        (v3 (.x tpos) h (.z tpos)))))))))
          true)))))

