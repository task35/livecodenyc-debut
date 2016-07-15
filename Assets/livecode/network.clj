(ns livecode.network
  (:require [clojure.edn :as edn])
  (:import [System.Net WebClient ServicePointManager SecurityProtocolType
            Security.RemoteCertificateValidationCallback]))

(def web-client (WebClient.))
(def state-url "http://livecodenyc.herokuapp.com/state")

(defn state-string []
  (.. web-client (DownloadString state-url)))

(defn state* []
  (edn/read-string (state-string)))

(def state (atom {}))

(defn update-state! []
  (reset! state (state*)))