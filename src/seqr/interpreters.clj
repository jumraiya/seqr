(ns seqr.interpreters
  (:require [seqr.music :as mu]
            [clojure.string :as str])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(defonce interpreters (atom {}))

(defonce midi-interpreters (atom {}))

(defn register-interpreter [key f]
  (swap! interpreters assoc key f))

(defn interpret [{:keys [args interpreter] :as cl} {:keys [action] :as ac} & [b n]]
  (let [f (or (get @interpreters interpreter) identity)
        data (-> (merge args ac)
                 (dissoc :action-str)
                 (update-vals #(if (and (string? %)
                                        (.startsWith ^String % "$"))
                                 (get cl %)
                                 %))
                 (update-vals #(if (map? %) (:val %) %))
                 (update-vals #(if (fn? %) (% b n) %)))]
    (when (:action data)
      (f data))))

(defn register-midi-interpreter [key f]
  (swap! midi-interpreters assoc key f))

(defn interpret-midi [{:keys [interpreter] :as clip} msg]
  ((get @midi-interpreters interpreter) msg clip))

(defn get-interpreters []
  (keys @interpreters))

(defn note [{:keys [action] :as data}]
  (try
    (let [n (mu/note action)
          freq (mu/midi->hz n)
          args (vec (flatten
                     (into []
                           (assoc
                            (reduce dissoc data [:action :action-str "synth" :node-id "add-action" "target"])
                            "freq" freq
                            "note" n))))
          params (merge data {:action action
                              :args args})]
      params)
    (catch Exception ex
        ;(prn ex)
      action)))

(register-interpreter "note" note)

(defn midi->note
  ([msg]
   (midi->note msg {}))
  ([msg clip]
   (let [gated? (contains? (:args clip) "gate")
         cmd (.getCommand msg)
         ret (if (or (= cmd ShortMessage/NOTE_ON)
                     (and gated?
                          (= cmd ShortMessage/NOTE_OFF)))
               {:action (name (mu/find-note-name (.getData1 msg)))}
               {})]
     (if gated?
       (condp = cmd
         ShortMessage/NOTE_ON (assoc ret "gate" 1)
         ShortMessage/NOTE_OFF (assoc ret "gate" 0)
         ret)
       (when (= cmd ShortMessage/NOTE_ON)
           ret)))))

(register-midi-interpreter "note" midi->note)

#_(defn scale [{:keys [action] :strs [scale] :as data}]
  (when scale
    (let [[root type] (re-seq #"[^\s]+" scale)
          scale (mu/scale (keyword root) (keyword type))
          [_ degree mods] (if (string? action)
                            (re-find #"(\d+)([b#<>]*)?" action)
                            [nil action nil])
          apply-mods #(reduce (fn [n m]
                                (condp = m
                                  \b (dec n)
                                  \# (inc n)
                                  \> (+ 12 n)
                                  \< (- n 12)
                                  :else n))
                              %1 %2)
          idx (if (string? degree)
                (-> degree Integer/parseInt dec)
                degree)
          idx (if (< idx (count scale)) idx (rem idx (count scale)))
          n (apply-mods (nth scale idx) mods)]
      (assoc data :args
             (flatten
              (into []
                    (-> data
                        (dissoc "scale")
                        (dissoc :action)
                        (dissoc :action-str)
                        (assoc "note" n "freq" (mu/midi->hz n)))))))))

(defn scale [{:keys [action] :strs [scale] :as data}]
  (when scale
      (let [[root type] (re-seq #"[^\s]+" scale)
            scale (mu/scale (keyword root) (keyword type))
            scale (if (= 0 (mod (last scale) (first scale)))
                    (butlast scale) scale)
            pitches (cycle scale)
            [s no modify oct] (first (re-seq
                                      #"([0-9]+)([b#><]+)*\|?([1-9]+)?"
                                      (str action)))
            no (Integer/parseInt no)
            n (nth pitches
                   (dec
                    no))
            n (+ n (* 12 (int (/ no (count scale)))))
            n (if (not (nil? oct))
                (-> n mu/find-pitch-class-name name (str oct) keyword mu/note)
                n)
            n (if (not (nil? modify))
                (reduce (fn [n m]
                          (cond (= \b m) (dec n)
                                (= \# m) (inc n)
                                (= \> m) (+ 12 n)
                                (= \< m) (- n 12)
                                true n
                                ))
                        n modify)
                n)
            freq (mu/midi->hz n)]
        (assoc data :args
               (flatten
                (into []
                      (-> data
                          (dissoc "scale")
                          (dissoc :action)
                          (dissoc :action-str)
                          (assoc "note" n "freq" (mu/midi->hz n)))))))))

(register-interpreter "scale" scale)

(defn- midi->scale-note
  ([msg]
   (midi->scale-note msg {}))
  ([msg {:keys [args]}]
   (when-let [scale (get args "scale")]
       (let [gated? (contains? args "gate")
             [root type] (re-seq #"[^\s]+" scale)
             cmd (.getCommand msg)
             midi-note (.getData1 msg)
             root-note (mu/note (keyword root))
             scale (mu/scale root type (range 1 (-> mu/SCALE (get (keyword type)) count)))
             scale-mul (- (last scale) (first scale))
             intervals (get mu/SCALE type)
             start (loop [start root-note]
                     (cond
                       (and (<= start midi-note)
                            (<= midi-note (+ start scale-mul))) start
                       (> midi-note start) (recur (+ start 12))
                       :else (recur (- start 12))))
             degree (loop [degree 1 cur start]
                      (cond
                        (= cur midi-note) (str degree)
                        (> cur midi-note) (str degree "b")
                        :else (recur (inc degree) (+ cur (nth intervals degree)))))
             octave-mods (str/join (repeat (/ (abs (- start root-note)) 12)
                                           (cond (> start root-note) ">"
                                                 (< start root-note) "<"
                                                 :else "")))
             ret (if (or (= cmd ShortMessage/NOTE_ON)
                         (and gated?
                              (= cmd ShortMessage/NOTE_OFF)))
                   {:action (str degree octave-mods)}
                   {})]
         (if gated?
           (condp = cmd
             ShortMessage/NOTE_ON (assoc ret "gate" 1)
             ShortMessage/NOTE_OFF (assoc ret "gate" 0)
             ret)
           (when (= cmd ShortMessage/NOTE_ON)
             ret))))))

(register-midi-interpreter "scale" midi->scale-note)
