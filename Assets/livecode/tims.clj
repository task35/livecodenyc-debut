(ns livecode.tims
  (:refer-clojure :exclude [aget])
  (:use [livecode core]
        [arcadia core linear
         ;;hydrate
         ]
        [clojure pprint repl])
  (:require [clojure.set :as set]
            [arcadia.introspection :as intro]
            [livecode.mesh :as m])
  (:import UpdateHook
           FixedUpdateHook
           [UnityEngine
            Time
            Color
            PhysicMaterial
            Transform
            BoxCollider
            ForceMode
            Rigidbody
            Time
            Collider
            Mesh
            CombineInstance
            PhysicMaterialCombine]
           [UnityEngine
            MeshFilter MeshRenderer Shader
           Material GameObject Component
            Color]
           [UnityEditor
            Selection]
           [UnityEngine
            Quaternion Vector2 Vector3 Transform GameObject Component
            Debug MeshFilter Mesh MeshRenderer Color
            LineRenderer Material Shader
            Gizmos Texture2D Resources Mathf
            Physics Ray RaycastHit
            Input
            Camera
            Application])
  (:use [seascape.core :exclude [shader-dir shader-material]]
        arcadia.core
        arcadia.linear
        ;;common.core
        gamma-tools.core
        clojure.pprint)
  (:require [gamma.api :as g]
            [gamma.program :as p]
            [arcadia.internal.map-utils :as mu]))

;; ============================================================
;; consts

;; !! THIS MIGHT NOT WORK IN EXPORTED REPL ON RAMSEYS COMPUTER !!

;; relativize this

(def shader-dir
  "/Users/timothygardner/code/livecodenyc-debut/Assets/Shaders/Resources/")



;; ==================================================
;; stuff

;; (defn kill! [x]
;;   (let [spec (dehydrate x)]
;;     (GameObject/Destroy x)
;;     spec))
  
;; (def cube-spec
;;   (kill! (create-primitive :cube)))

;; (defn children [obj]
;;   (for [tr (.transform (.gameObject obj))]
;;     (.gameObject tr)))

(defn rand*
  ([]
   (rand))
  ([max]
   (rand max))
  ([min max]
   (+ min (rand (- max min)))))

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

;; ============================================================
;; selection
;; this api is probably bullshit

(defn- ensure-set [x]
  (if (set? x) x (set x)))

(def global-selection
  (atom #{}))

(defn sel
  ([] @global-selection)
  ([x] (reset! global-selection #{x}))
  ([x & xs]
   (reset! global-selection (into #{} (conj x xs)))))

(defn sel+ [& xs]
  (swap! global-selection into xs))

(defn sel- [& xs]
  (swap! global-selection
    (fn [s]
      (persistent!
        (reduce disj! (transient s) xs)))))

(defn fsel []
  (first @global-selection))

(defn selall [coll]
  (reset! global-selection (into #{} coll)))

(defn sel? [x]
  (contains? @global-selection x))

;; ============================================================
;; naming

(defn bename [obj name]
  (set! (.. obj name) name)
  obj)

;; ============================================================
;; space


;; (defmacro def-setter [name & path]
;;   `(defn ~name [obj# ~@path]
;;      (set-with! obj# [_ path#]
;;        val#)
;;      obj#))

;; (defn set-pos [obj pos]
;;   (set! (.. obj transform position) pos)
;;   obj)

;; (defn set-scale [obj scl]
;;   (set! (.. obj transform localScale) scl)
;;   obj)

;; (defn set-rot [obj rot]
;;   (set! (.. obj transform rotation) rot)
;;   obj)

;; ============================================================
;; traversal

(defn game-object-seq [x]
  (tree-seq
    #(instance? GameObject %)
    (fn [^GameObject y]
      (map (fn [^Transform tr]
             (.gameObject tr))
        (.transform y)))
    x))

;; ============================================================
;; materials

(defn shader-material ^Material [^String name]
  (Material. (Shader/Find name)))

(defn- ensured-mesh-renderer ^MeshRenderer [^GameObject obj]
  (or (.GetComponent obj UnityEngine.MeshRenderer)
    (.AddComponent obj UnityEngine.MeshRenderer)))

(defn color ^Color
  ([^GameObject obj]
   (when-let [^MeshRenderer mr (.GetComponent obj UnityEngine.MeshRenderer)]
     (.. mr material color)))
  ([^GameObject obj, ^Color c]
   (let [^MeshRenderer mr (ensured-mesh-renderer obj)]
     (set! (.. mr material color) c))
   obj))

;; ============================================================
;; raycasting

(comment
  (defn raycast [^Ray ray]
    (when-let [^RaycastHit hit (first (RayCastHelper/raycast ray))]
      {:collider (.collider hit)
       :point (.point hit)
       :transform (.transform hit)}))

  (defn raycast-plane [^Plane plane, ^Ray ray]
    (first (RayCastHelper/raycastPlane plane ray)))

  (defn forward-ray ^Ray [x]
    (let [^Transform t (transform x)]
      (Ray. (.position t) (.forward t))))

  (defn raycast-forward [x]
    (raycast (forward-ray x))))

;; ============================================================
;; meshes

;; (defn combine-meshes ^Mesh
;;   ([meshes] (combine-meshes meshes nil))
;;   ([meshes {:keys [merge-submeshes use-matrices]
;;             :or {merge-submeshes true
;;                  use-matrices false}}]
;;    (let [f (fn [^Mesh m]
;;              (let [^CombineInstance c (CombineInstance.)]
;;                (set! (.mesh c) m)
;;                c))
;;          ^|UnityEngine.CombineInstance[]| cs (->> meshes
;;                                                (map f)
;;                                                (into-array UnityEngine.CombineInstance))
;;          ^Mesh mesh (Mesh.)]
;;      (.CombineMeshes mesh cs
;;        merge-submeshes
;;        use-matrices)
;;      mesh)))

(defn star-points [point-n, outer-r, inner-r]
  (let [point-n (int point-n)
        angstep (/ (* 2 Mathf/PI) point-n)
        inner-rot (aa (* Mathf/Rad2Deg (/ angstep 2)) 0 0 1)
        ps1 (->> (range point-n)
              (mapcat
                (fn [i]
                  (let [p1 (v3
                             (Mathf/Cos (* i angstep))
                             (Mathf/Sin (* i angstep))
                             0)
                        p2 (qv* inner-rot (v3* p1 (/ inner-r outer-r)))]
                    [p1 p2])))
              (map #(v3* % outer-r)))]
    ps1))

;; ============================================================
;;

;; (defn mesh-obj [mesh]
;;   (let [obj (hydrate cube-spec)
;;         mf (ensure-component obj MeshFilter)]
;;     (set! (.mesh mf) mesh)
;;     obj))

;; (defn scumpit [objs]
;;   (->> objs
;;     (map #(v3scale (position %) (v3 1 0 1)))
;;     m/triangulate
;;     m/backfaced-mesh
;;     mesh-obj))

;; (defn scumpit2 [objs]
;;   (->> objs
;;     (map #(v3scale (position %) (v3 1 0 1)))
;;     (m/polygon-extrude (v3 0 1 0))
;;     m/backfaced-mesh
;;     mesh-obj))


;; ============================================================
;; SHADERS
;; ============================================================

;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; ============================================================
;; SWAP TO ENABLE GAMMA EMISSION
(def ^:dynamic *emit-shaders*
  ;;false
  true
  )
;; ============================================================
;; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(require '[arcadia.introspection :as intro])

(defn mempr [& args]
  (pprint (apply intro/members args)))

(defmacro clobs [[name body] & bodies]
  `(as-> ~body ~name ~@bodies))

(defn sin-phase [x phase]
  (g/sin (gdiv (g* x 2 Mathf/PI) phase)))

(defn numdist [a b]
  (g/abs (g- a b)))

(defn modnrm [a b]
  (gdiv (g/mod a b) b))

(def pi Mathf/PI)

(def two-pi
  (float (* Mathf/PI 2)))
  
  ;; bit o sugar

(defmacro defshader
  ([name body]
   `(def ~name ~body))
  ([name shader-name shader-dir body]
   `(let [shader# ~body]
      (when *emit-shaders*
        (write-shader ~shader-name ~shader-dir shader#))
      (def ~name shader#))))

(defshader sphere-practice "demoShaderA" shader-dir
  (let [wobble (g/uniform "wobble" :vec4)
        object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))]
        (g/if (g/< dist 5)
          (g/vec4 0 1 0 1)
          (g/vec4 0.3 0.3 0.3 1)))}}))

(defshader spiral-practice "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))]
        (g/if (g/< dist 5)
          (g/vec4 0 1 0 1)
          (g/vec4 0.3 0.3 0.3 1)))}}))

(defshader concentro "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))]
        (g/* (g/vec4 1) (gdiv (g/mod dist 1) 5)))}}))

(defshader concentro-2 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            phase 1
            phase-dist (g/mod dist phase)
            val (g/abs (g/- phase-dist (g/div phase 2)))]
        (g/* (g/vec4 1) val))}}))

(defshader concentro-2 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [phase 0.1
            dist (clobs [dist (g/distance
                                position-in-world-space,
                                (g/vec4 0 0 0 1))]
                   (g/abs (g- (g/mod dist, phase)
                            (gdiv phase 2)))
                   (g/div dist
                     (gdiv phase 2)))
            val dist]
        (g/vec4 0 0 0 val))}}))

(defshader concentro-3 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [phase 0.1
            val (g+ Mathf/PI
                  (g/atan
                    (gx position-in-world-space)
                    (gy position-in-world-space)))
            scaled-val (gdiv val (g* Mathf/PI 2))]
        (g/vec4 0 0 0 scaled-val))}}))

(defshader radial-1 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [phase (float (/ 1 100))
            angnrm (clobs [ang (g/atan
                                 (gx position-in-world-space)
                                 (gy position-in-world-space))]
                     (g+ ang Mathf/PI)
                     (gdiv ang (g* Mathf/PI 2)))
            val (clobs [v (g/mod angnrm phase)]
                  (gdiv v phase)
                  (g/smoothstep 0 0.5
                    (g/abs (g- v 0.5))))]
        (g/vec4 0 0 0 val))}}))


(defshader radial-2 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [ang-phase (float (/ 1 10))
            rad-phase (float (/ 1 10))
            angnrm (clobs [ang (g/atan
                                 (gx position-in-world-space)
                                 (gy position-in-world-space))]
                     (g+ ang Mathf/PI)
                     (gdiv ang (g* Mathf/PI 2)))
            dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            val (clobs [v (g/mod
                            (g+ angnrm (sin-phase dist rad-phase))
                            ang-phase)]
                  (gdiv v ang-phase)
                  (g/smoothstep 0 0.5
                    (g/abs (g- v 0.5))))]
        (g/vec4 0 0 0 val))}}))

(defshader radial-3 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [ang-phase (float (/ 1 10))
            rad-phase (float (/ 1 10))
            angnrm (clobs [ang (g/atan
                                 (gx position-in-world-space)
                                 (gy position-in-world-space))]
                     (g+ ang Mathf/PI)
                     (gdiv ang (g* Mathf/PI 2)))
            dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            val-1 (clobs [v (g/mod
                              (g+ angnrm (sin-phase dist rad-phase))
                              ang-phase)]
                    (gdiv v ang-phase)
                    (g/smoothstep 0 0.5
                      (g/abs (g- v 0.5))))
            val-2 (let [subang (gdiv (g/mod angnrm ang-phase) ang-phase)
                        subsweep-length (g* (g* 2 Mathf/PI dist) rad-phase)
                        subang-2 (g/mod
                                   (g+ subang
                                     (gdiv
                                       (g/sin (sin-phase dist 1))
                                       subsweep-length))
                                   1)
                        mark-len 0.3
                        mark-len-nrm (gdiv mark-len subsweep-length)]
                    (clobs [v subang-2]
                      (g/smoothstep
                        (g- 1 mark-len-nrm)
                        1 v)
                      ;;(g+ val-1 v)
                      ))]
        (g/vec4 0 0 0 val-2))}}))


(defshader radial-4 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [ang-phase (float (/ 1 20))
            rad-phase (float (/ 1 1000))
            mark-len 0.5
            distortion-magnitude 2
            angnrm (clobs [ang (g/atan
                                 (gx position-in-world-space)
                                 (gy position-in-world-space))]
                     (g+ ang Mathf/PI)
                     (gdiv ang (g* Mathf/PI 2)))
            dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            val-1 (clobs [v (g/mod
                              (g+ angnrm (sin-phase dist rad-phase))
                              ang-phase)]
                    (gdiv v ang-phase)
                    (g/smoothstep 0 0.5
                      (g/abs (g- v 0.5))))
            subang (gdiv (g/mod angnrm ang-phase) ang-phase)
            subsweep-length (g* (g* 2 Mathf/PI dist)
                              ang-phase)
            subang-2 (clobs [sa
                             #_(noise (g/vec2
                                       (gx position-in-world-space)
                                       (gy position-in-world-space)))
                             (sin-phase dist 1)]
                       (gdiv sa subsweep-length)
                       (g* sa distortion-magnitude)
                       (g+ subang sa)
                       (g/mod sa 1))
            mark-len-nrm (gdiv mark-len subsweep-length)
            val-2 (clobs [v subang-2]
                    (g* (numdist v 0.5) 2)
                    (g/smoothstep
                      (g- 1 mark-len-nrm)
                      1 v)
                    ;;(g+ val-1 v)
                    )]
        (g/vec4 0 0 0 val-2))}}))


(defshader radial-5 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [ang-phase (float (/ 1 10 ;;120
                               ))
            rad-phase 1
            mark-len 1
            distortion-magnitude 2
            angnrm (-> (g/atan
                         (gx position-in-world-space)
                         (gy position-in-world-space))
                     (g+ Mathf/PI)
                     (gdiv (g* Mathf/PI 2)))
            dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            val-1  (modnrm dist rad-phase)
            subang (modnrm angnrm ang-phase)
            subsweep-length (g* (g* 2 Mathf/PI dist)
                              ang-phase)
            subang-2 (-> (noise
                           (g* 0.9
                             (g/vec2
                               (gx position-in-world-space)
                               (gy position-in-world-space))))
                       (gdiv subsweep-length)
                       (g* distortion-magnitude)
                       (g+ subang)
                       (g/mod 1))
            mark-len-nrm (gdiv mark-len subsweep-length)
            val-2 (clobs [v subang-2]
                    (g* (numdist v 0.5) 2)
                    (g/smoothstep
                      (g- 1 mark-len-nrm)
                      1 v)
                    (g- val-1 v))]
        (g/vec4 0 0 0 val-2))}}))

(defshader radial-6 "demoShaderA" shader-dir
  (let [object->world (not-prop (g/uniform "_Object2World" :mat4))
        time (not-prop (g/uniform "_Time" :vec4))
        vtime (g/varying "vtime" :vec4)
        sphere-center (g/uniform "sphere_center" :vec4)
        position-in-world-space (g/varying "position_in_world_space" :vec4)]
    {:transparent true
     :vertex-shader
     {position-in-world-space (g* object->world glv)
      ;; (g- (g* object->world glv)
                              ;;   sphere-center)
      vtime time
      (g/gl-position) (g* glmvpm glv)}
     
     :fragment-shader
     {(g/gl-frag-color)
      (let [ang-phase (float (/ 1 120))
            rad-phase 13
            t (g/aget vtime (g/int 1))
            mark-len 1
            distortion-magnitude 2
            angnrm (-> (g/atan
                         (gx position-in-world-space)
                         (gy position-in-world-space))
                     (g+ Mathf/PI)
                     (gdiv (g* Mathf/PI 2)))
            dist (g/distance position-in-world-space, (g/vec4 0 0 0 1))
            val-1  (modnrm dist rad-phase)
            subang (modnrm angnrm ang-phase)
            subsweep-length (g* (g* 2 Mathf/PI dist)
                              ang-phase)
            subang-2 (-> (noise
                           (g* 0.9
                             (g/vec2
                               (gx position-in-world-space)
                               (gy position-in-world-space))))
                       (gdiv subsweep-length)
                       (g* distortion-magnitude)
                       (g+ subang)
                       (g/mod 1))
            mark-len-nrm (gdiv mark-len subsweep-length)
            val-2 (clobs [v subang-2]
                    (g* (numdist v 0.5) 2)
                    (g/smoothstep
                      (g- 1 mark-len-nrm)
                      1 v)
                    (g- val-1 v))]
        (g/vec4 0 0 0
          (g* (gdiv t 100)
           val-2)))}}))

;; demo fun
(comment
  (load-level "shaders")
  (use 'arcadia.core)
  
  ;; clone cube
  (set! (.name (instantiate (object-named "Cube")))
    "Cube 2")
  
  ;; rotate both
  (do
    ;; rotate cube
    (hook+ (object-named "Cube")
      :update
      #(.. % transform (Rotate 1 0 0)))
    
    ;; rotate cube 2
    (hook+ (object-named "Cube 2")
      :update
      #(.. % transform (Rotate 0 1 0))))
  
  ;; reset rotations
  (doseq [o (objects-named #"Cube.*")]
    (set! (.. o transform rotation) Quaternion/identity))
  
  ;; next scene
  (load-level "space"))
