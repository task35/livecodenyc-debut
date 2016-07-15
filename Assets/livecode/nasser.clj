(ns livecode.nasser
  (:use arcadia.core
        arcadia.linear
        livecode.core)
  (:require [livecode.on-screen-text :as ost])
  (:import UpdateHook
           FixedUpdateHook
           ArcadiaBehaviour
           [UnityEngine Rigidbody GameObject MeshRenderer Color]))

(require '[livecode.tims :as tims])
(require '[livecode.mesh :as mesh])

(defn mesh
  ([vs] (mesh vs (v3 0 1 0)))
  ([vs v-extrude] 
   (-> (mesh/polygon-extrude
         v-extrude vs)
       mesh/backfaced-mesh
       tims/mesh-obj)))

(defn always-forward [go]
  (let [forward (.. go transform forward)]
    (force! go forward)))

(defn camera-rotater [go]
  (let [{:keys [speed offset target-position look-at]
         :or {offset (v3 0)
              target-position (v3 0)
              look-at (v3 0)
              speed 1.0}}
        (state go)
        radius (.magnitude offset)]
    (set! (.. go transform position)
          (v3 (+ (.x (position target-position)) (* radius (Mathf/Cos (* speed Time/time))))
              (+ (.y (position target-position)) (.y offset))
              (+ (.z (position target-position)) (* radius (Mathf/Sin (* speed Time/time))))))
    (.. go transform (LookAt (position look-at)))))

(defn camera-control [go]
  ;; lerp to target-position
  (set! (.. go transform position)
        (v3+ (or (-> go state :offset)
                 (v3 0))
          (Vector3/Lerp (.. go transform position)
                      (position (-> go state :target-position))
                      0.5)
           ))
  ;; lerp to look-at
  (let [{:keys [look-at current-look-at]
         :or {current-look-at (v3 0)}}
        (state go)
        lerped-look-at (Vector3/Lerp
                         (position current-look-at)
                         (position look-at)
                         0.5)]
    (swap-state! go assoc :current-look-at lerped-look-at)
    (.. go transform (LookAt (position lerped-look-at)))))

(defn cube-mover [go]
  (.. go transform (Rotate 0.5 0 0) )
  (.. go transform (Translate (v3 (* 1 (Math/Sin (Time/time)))
                                  0
                                  0))))

(defmacro ∆ [& args]
  `(* Time/deltaTime ~@args))

(defn control
  ([go] (control go {:look-at (v3 0) :target-position (v3 0)}))
  ([go s]
   (hook! go FixedUpdateHook #'camera-control)
   (state! go s)))

(defn rotate
  ([go] (control go {:look-at (v3 0) :target-position (v3 0)}))
  ([go s]
   (hook! go FixedUpdateHook #'camera-rotater)
   (state! go s)))

(defn add-forces [f objs]
  (doseq [p objs]
    (.AddForce (ensure-component p Rigidbody)
               f)))

(defn push-towards [pos f objs]
  (doseq [p objs]
    (.AddForce (ensure-component p Rigidbody)
               (v3* (v3- (position pos) (position p)) f))))

(defn push-away [pos f objs]
  (doseq [p objs]
    (.AddForce (ensure-component p Rigidbody)
               (v3* (v3- (position p) (position pos)) f))))

(defn objects-near [pos radius]
  (->> (objects-typed GameObject)
       (filter #(< (Vector3/Distance (position %)
                                     (position pos))
                   radius))))

(defn centroid [objs]
  (v3div (apply v3+ (map position objs))
         (count objs)))

(defn rand
  ([a] (UnityEngine.Random/Range 0 a))
  ([a b] (UnityEngine.Random/Range a b)))

(defn geo-merge [objs]
  (let [parent (GameObject. (str (gensym "merge")))
        centroid (v3div (apply v3+ (map position objs))
                        (count objs))]
    (set! (.. parent transform position) centroid)
    (ensure-component parent Rigidbody)
    (doseq [o objs]
      (set! (.. o transform parent) (.transform parent))
      (remove-component o Rigidbody)
      (doseq [ac (.GetComponents o ArcadiaBehaviour)]
        (destroy ac)))
    parent))

(defn geo-unmerge [parent]
  (let [children (vec (.transform parent))
        rb (.GetComponent parent Rigidbody)]
    (doseq [child children]
      (let [child-rb (ensure-component (.gameObject child) Rigidbody)]
        (set! (.velocity child-rb)
              (.velocity rb) ))
      (set! (.. child parent) nil))
    (destroy parent)
    children))

(defn named! [o n]
  (set! (.. o name) n))

(defn color! [o c]
  (set! (.. o (GetComponent MeshRenderer) material color)
        c))

(defn colored? [o c]
  (= (.. o (GetComponent MeshRenderer) material color)
     c))

(defn line-up [x z objs]
  (->> objs
       (map (fn [i o]
              (set! (.. o transform position)
                    (v3 x (+ 10 (* 2 i)) z)))
            (range))
       doall))
