(ns maildir-tools.util
  (:require [clojure.string :as str])
  (:import [java.security MessageDigest]
           ))

(def get-pid
  (memoize
   (fn []
     (-> (java.lang.management.ManagementFactory/getRuntimeMXBean)
         (.getName)
         (str/split #"@")
         (first)))))

(defn get-hostname []
  (.. java.net.InetAddress getLocalHost getHostName))

(defn hexify [s]
  (format "%x" (new java.math.BigInteger s)))

(defn md5 [string]
  (let [md (MessageDigest/getInstance "MD5")]
    (.update md (.getBytes string))
    (->> md
        .digest
        (map #(format "%02x" %))
        (apply str))))
