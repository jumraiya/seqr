(ns seqr.samples
  (:require [clojure.string :as string]
            [seqr.osc :as osc]
            [clojure.java.io :as io]
            [seqr.connections :as conn]
            [seqr.interpreters :as interp])
  (:import (java.nio.file Paths)
           (javax.sound.midi MidiMessage ShortMessage)))

(defonce ^:private buffer-nums (agent #{}))

(def MAX-BUFFERS 1024)

(def ^:private b-alloc (osc/builder "/b_allocRead ?num ?path"))

(def ^:private b-free (osc/builder "/b_free ?num"))

(defonce drum-kits nil)

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
                          (let [p (.toPath listing)
                                kit-name (string/replace (.toString (.getName p (- (.getNameCount p) 2))) " " "")
                                sample-name (string/replace (.toString (.getFileName p)) " " "")
                                disk-path (string/replace (.getAbsolutePath listing) "\\" "\\\\")]
                            (assoc-in tree [kit-name sample-name] (sample disk-path)))
                          tree))

                      {}
                      (file-seq directory))]

     tree))

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
                         "/samples/Drum Kits") true))))

(defn- next-token [text]
  (if text
    (let [text (.trim text)
          [match letters nums other]
          (re-find #"([a-zA-Z]+)|([0-9]+)|([^a-zA-Z0-9]+)" text)]
      [(cond
         (some? letters) {:type :alpha :val letters}
         (some? nums) {:type :num :val nums}
         (some? other) {:type :other :val other}
         :else {:type :eof})
       (if (and text match (> (.length text) (.length match)))
         (.substring text (.length match)))])
    [{:type :eof}]))

(defn- gen-sample-pat
  ([in]
   (let [[f-tok] (next-token in)]
     (gen-sample-pat in {:exp (StringBuilder. "(?i)")})))
  ([in state]
   (let [[{:keys [type val]} text] (next-token in)]
     (condp = type
       :alpha (gen-sample-pat
               text
               (update state :exp #(.append % (str ".*" val))))
       :num (gen-sample-pat
             text
             (update state :exp #(.append % (str ".*" val))))
       :other (gen-sample-pat text state)
       :eof (re-pattern (.toString (.append (:exp state) ".*")))))))

(defn drum [{:strs [kit] :keys [action] :as data}]
  (let [k-pat (gen-sample-pat kit)
        a-pat (gen-sample-pat action)
        kit (first (filter #(re-matches k-pat %)
                           (keys drum-kits)))
        buf-num  (some (fn [[s b]]
                         (when (re-matches a-pat (name s))
                           b))
                       (get drum-kits kit))]
    (if buf-num
      (assoc data :args
             (reduce into
                     ["num" buf-num]
                     (dissoc data :action :action-str "kit")))
      data)))

(interp/register-interpreter "drum" drum)

(defn midi->drum
  ([msg]
   (midi->drum msg {}))
  ([msg {:keys [args]}]
   (let [cmd (.getCommand msg)
         k-pat (gen-sample-pat (get args "kit"))
         kit (first (filter #(re-matches k-pat %)
                           (keys drum-kits)))
         pos (- (.getData1 msg) 48)
         samples (keys (get drum-kits kit))]
     (when (and kit
                (= cmd ShortMessage/NOTE_ON)
                (>= pos 0)
                (< pos (count samples)))
       {:action (nth samples pos)}))))

(interp/register-midi-interpreter "drum" midi->drum)
