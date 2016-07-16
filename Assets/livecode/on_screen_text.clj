(ns livecode.on-screen-text
  (:use arcadia.core)
  (:require [clojure.string :as string])
  (:import 
    [UnityEngine GameObject]
    [UnityEngine.UI Text]))

(def max-characters 10000)
(def max-text-height 60)

(defn left-text [] (.. (object-named "Left Text") (GetComponent Text)))
(defn right-text [] (.. (object-named "Right Text") (GetComponent Text)))

(defn text [txt]
  (.text txt))

(defn text! [txt s]
  (set! (.text txt) s))

(defn clear [txt]
  (text! txt ""))

(defn lines [txt]
  (-> (.text txt)
      (string/split #"\n")
      count))

(defn drop-top [txt n]
  (->> (string/split (.text txt) #"\n")
       (drop n)
       (string/join "\n")
       (text! txt)))

(defn drop-bottom [txt n]
  (->> (string/split (.text txt) #"\n")
       (drop-last n)
       (string/join "\n")
       (text! txt)))

(defn clean-up [txt]
  (text! txt (string/replace (text txt) #"^</color>\s+" "")))

(defn push [txt s]
  (let [s (if (> (count s) max-characters)
            "..."
            s)]
    (let [s (string/replace s #"=> " "=>\n")]
      (text! txt (str (text txt) "\n" s))
      (drop-top txt (inc (- (lines txt) max-text-height)))
      (clean-up txt))))

(def default-theme
  [
   ;; comments
   [#"(;;.*\n)" "#747369FF"]
   ;; specials
   [#"\b(str|def|defn|if|if-not)\b" "#CD98CDFF"]
   ;; core
   [#"\b(join|map)\b" "#61CCCDFF"]
   ;; parens
   [#"(\(|\)|\[|\]|\{|\})" "#bbbbbbff"]
   ;; numbers
   [#"\b(\d+)\b" "#FB9150FF"]
   ;; keywords
   [#"(:[^\s]+)" "#FB9150FF"]
   ;; strings
   [(re-pattern "(\"[^\"]+\")") "#98CD97FF"]
   ])


(defn highlight-code
  ([s] (highlight-code s default-theme))
  ([s theme]
   (reduce
     (fn [code [pattern color]]
       (string/replace code pattern
                       (str "<color=" color ">$1</color>")))
     s
     theme)))