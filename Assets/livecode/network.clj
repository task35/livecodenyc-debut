(ns livecode.network
  (:use arcadia.core
        arcadia.linear)
  (:require [clojure.edn :as edn])
  (:import 
    [System.Threading Thread ThreadStart]
    [System.Net WebClient WebException ServicePointManager SecurityProtocolType
     Security.RemoteCertificateValidationCallback]))

; (def base-url "http://localhost:3000/")
(def base-url "http://livecodenyc.herokuapp.com/")
(def web-client (WebClient.))

;; state -> {"id" "button"}
;; ids   -> ["id" ...]
(defn from-server [path]
  (edn/read-string
    (.. web-client (DownloadString (str base-url path)))))

(def buttons (atom {}))
(def ids (atom #{}))

(defn buttons! []
  (reset! buttons (from-server "state")))

(defn ids! []
  (reset! ids (into #{} (from-server "ids"))))

(defn poll-network [go]
  (.Start (Thread.
            (gen-delegate
              ThreadStart []
              (while true
                (try
                  (buttons!)
                  (ids!)
                  (Thread/Sleep 10)
                  (catch WebException e)
                  ))))))


(defn populate [go]
  (let [ids @ids
        transform (.. go transform)]
    ;; add new buddies
    (doseq [id ids]
      (when-not (.Find transform id)
        (let [buddy* (if-let [prefab (state go ::buddy-prefab)]
                       (instantiate prefab)
                       (GameObject.))]
          (set! (.. buddy* transform parent) transform)
          (set! (.. buddy* name) id))))
    ;; remove old buddies
    (doseq [child transform]
      (when-not (ids (.name child))
        (destroy (.gameObject child))))))

(defn buddy-root [go]
  (doto go
    (set-state! ::buddy-prefab nil)
    (hook+ :start #'poll-network)
    (hook+ :update #'populate)))