(ns url.core
  (:require [clojure.string :as str :only [join replace split]]))

(def ^:private host #"(host)\((.+?)\);")
(def ^:private path #"(path)\((.+?)\);")
(def ^:private query #"(queryparam)\((.+?)\);")
(def ^:private param #"\?(\w+)")

(defn- make-host-re
  [host]
  (when-not (empty? host)
    (str ".+://" (str/replace host "." "\\.") "/")))

(defn- make-path-re
  [path]
  (when-not (empty? path)
    (str/replace path param "(.+)")))

(defn- make-query-re
  [query]
  (when-not (empty? query)
    (->> query
         (map #(str/replace % param "(\\\\w+)"))
         (map #(str "(?=.*" % ")"))
         str/join
         (#(str "\\?.*" % ".+")))))

(defn- make-pattern-regex
  [pattern]
  (re-pattern
   (str
    (make-host-re (:host pattern))
    (make-path-re (:path pattern))
    (make-query-re (:query pattern)))))

(defn- path-keys
  [pattern]
  (when-let [path (:path pattern)]
    (map (comp keyword last) (re-seq param path))))

(defn- query-keys
  [pattern]
  (when-let [query (:query pattern)]
    (map (comp keyword last #(str/split % #"=\?")) query)))

(defn- extract-keys
  [pattern]
  (concat
   (path-keys pattern)
   (query-keys pattern)))

(defn- try-int
  [val]
  (if-let [int-value (re-find #"^[1-9]\d*$" val)]
    (read-string int-value)
    val))

(defn- recognize-pattern
  [pattern url]
  (when-let [values (re-seq (make-pattern-regex pattern) url)]
    (->> (zipmap (extract-keys pattern) (rest (first values)))
         (into [])
         (map (fn [[k v]] [k (try-int v)]))
         sort)))

(defn new-pattern
  [template]
  {:host (last (re-find host template))
   :path (last (re-find path template))
   :query (map last (re-seq query template))})

(defn recognize
  [pattern url]
  (let [url-parts (recognize-pattern pattern url)]
    (when-not (empty? url-parts)
      url-parts)))
