(ns hyperfiddle.transit
  (:require [cognitect.transit :as t])
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream]))

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]}]
  (let [type (or type :json)
        in   (ByteArrayInputStream. (.getBytes s "UTF-8"))
        rdr  (t/reader in type opts)]
    (t/read rdr)))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]}]
  (let [type   (or type :json)
        out    (ByteArrayOutputStream.)
        writer (t/writer out type opts)]
    (t/write writer x)
    (.toString out)))
