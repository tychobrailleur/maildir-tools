(ns maildir-tools.core
  (:require [clojure.java.io :as io]
            [maildir-tools.util :as util]
            [clojure.string :as str]))

(def timehash (atom (hash-map)))
(defrecord MaildirFolder [root name])

(defn create-time-seq []
  (let [time (quot (System/currentTimeMillis) 1000)
        v (swap! timehash #(update-in % [time] (fnil inc 0)))]
    [time (get v time)]))

(defn folder-full-path [folder]
  (-> (io/file (:root folder) (:name folder))
      .getCanonicalPath))

(defn parse-filename [filename]
  (let [[_ time timeseq pid host uid fmd5 flags]
        (re-matches #"(\d+)_(\d+)\.(\d+)\.([^,]+),U=(\d+),FMD5=([^\:]+):2,(\w*)" filename)]
    {:time time
     :timeseq timeseq
     :pid pid
     :host host
     :uid uid
     :fmd5 fmd5
     :flags flags}))

(defn create-unique-message-name [folder uid flags]
  (let [[timeval timeseq] (create-time-seq)]
    (format "%d_%d.%s.%s,U=%d,FMD5=%s%s2,%s"
            timeval
            timeseq
            (util/get-pid)
            (util/get-hostname)
            uid
            (util/md5 (:name folder))
            ":"
            (str/join (sort flags)))))


(defmulti get-uid class)

(defmethod get-uid nil [filename] nil)
(defmethod get-uid String [filename]
  (when-let [m (re-matches #".*,U=(\d+),.*" filename)]
    (last m)))
(defmethod get-uid java.io.File [file]
  (get-uid (.getName file)))


(defn save-to-tmp [folder name content]
  (let [path (io/file (folder-full-path folder) "tmp")]
    (spit (io/file path name) content)))

(defn save-message [uid folder content flags])

(defn list-messages [folder]
  (apply concat
         (for [d ["new" "cur"]]
           (let [full-path (io/file (folder-full-path folder) d)]
             (filter #(.isFile %) (file-seq full-path))))))

(defn count-messages [folder]
  (count (list-messages folder)))


(defn file->message [folder file]
  (let [info (parse-filename (.getName file))
        content (slurp file)]
    (-> info
        (assoc :content content)
        (assoc :folder folder)
        (assoc :filename file))))

(defn get-message [folder uid]
  (let [msgs (list-messages folder)]
    (loop [msg msgs]
      (if (= (get-uid (first msg)) uid)
        (file->message folder (first msg))
        (when (not (empty? (rest msg)))
          (recur (rest msg)))))))


(defn -main [& args]
  (let [maildir (->MaildirFolder "/tmp/" "INBOX")
        msgs (list-messages maildir)
        name (create-unique-message-name maildir 45 ["R"])]
    (println (get-uid (first msgs)))
    (save-to-tmp maildir name "Hello this is a nice message")))
