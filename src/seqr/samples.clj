(ns seqr.samples
  (:require [clojure.string :as string]
            [seqr.osc :as osc]
            [clojure.java.io :as io]
            [seqr.connections :as conn]
            [seqr.interpreters :as interp]))

(defonce ^:private buffer-nums (agent #{}))

(def MAX-BUFFERS 1024)

(def ^:private b-alloc (osc/builder "/b_allocRead ?num ?path"))

(def ^:private b-free (osc/builder "/b_free ?num"))

(def drum-kits nil)

(defn free-all-buffers []
  (doseq [n (range MAX-BUFFERS)]
    (conn/send! "sc" (b-free {"num" n})))
  (send buffer-nums (constantly #{})))


(defn- sample [path]
  (let [buf-num (inc (if (empty? @buffer-nums) -1 (apply max @buffer-nums)))]
    (conn/send! "sc" (b-alloc {"num" buf-num "path" path}))
    (send buffer-nums conj buf-num)
    buf-num))

(defn create-sample-map [path & [nest filter]]
  (let [directory (io/file path)
        tree (reduce (fn [tree listing]
                       (if (and (.isFile listing)
                                (or (.contains (.getName listing) "wav") (.contains (.getName listing) "aif"))
                                (and (if (fn? filter) (filter listing) true)))
                         (let [nested-path
                               (map (fn [part]
                                      (keyword (string/replace (string/replace part " " "") ":" "")))
                                    (string/split  (subs (.getAbsolutePath listing) (inc (count path)))  #"\\"))
                               disk-path (string/replace (.getAbsolutePath listing) "\\" "\\\\")]

                           (if (= nest true)
                             (update-in tree nested-path (fn [_] (sample disk-path)))
                             (assoc tree
                                    (-> (.getName listing)
                                        (string/replace " " "")
                                        (string/replace ":" "")
                                        (string/replace #"\..*" "")
                                        keyword)
                                    (sample disk-path))
                             )
                           )
                         tree
                         )
                       )
                     {}
                     (file-seq directory)
                     )
        ]
    tree
    )
  )

(defn group-samples [samples]
  (let [sample-names (sort (map name (keys samples)))]
    (reduce (fn [g sample]
              (let [group (-> (re-seq #"([A-Za-z]+)[^\dA-Za-z]?[\d]+\..*$" sample) first last keyword)
                    inst (keyword sample)]
                (assoc-in g [group inst] (samples inst))
                )) {} sample-names)
    )
  )

(defn reset-drum-kits []
  (free-all-buffers)
  (Thread/sleep 1000)
  (alter-var-root (var drum-kits)
                  (constantly
                   (create-sample-map
                    (str (System/getProperty "user.home")
                         "\\samples\\Drum Kits") true))))

(defn drum [{:strs [kit] :keys [action] :as data}]
  (let [kit (first (filter #(re-matches (re-pattern (str "(?i)" kit)) (name %))
                           (keys drum-kits)))
        [_ t n] (re-find #"([a-z]+)([0-9]+)" action)
        buf-num (some (fn [[s b]]
                        (when (re-matches (re-pattern (str "(?i).*" t ".*" n ".*"))
                                          (name s))
                          b))
                      (get drum-kits kit))]
    (if buf-num
      (assoc data :args ["num" buf-num])
      data)))

(interp/register-interpreter "drum" drum)
