(ns seqr.interpreters
  (:require [seqr.music :as mu])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(defonce interpreters (atom {}))

(defonce midi-interpreters (atom {}))

(defn register-interpreter [key f]
  (swap! interpreters assoc key f))

(defn interpret [{:keys [args interpreter]} action & [b n]]
  (let [f (get @interpreters interpreter)]
    (dissoc
     (if (fn? (:action action))
       (f (merge args {:action ((:action action) b n)}))
       (f (merge args action)))
     :action :action-str)))

(defn register-midi-interpreter [key f]
  (swap! midi-interpreters assoc key f))

(defn interpret-midi [{:keys [interpreter] :as clip} msg]
  ((get @midi-interpreters interpreter) msg clip))

(defn get-interpreters []
  (keys @interpreters))

(defn note [{:keys [action] :as data}]
  (let [n (mu/note action)
        freq (mu/midi->hz n)
        args (vec (flatten
                   (into []
                         (assoc
                          (reduce dissoc data [:action "synth" :node-id "add-action" "target"])
                          "freq" freq
                          "note" n))))
        params (merge data {:action action
                            :args args})]
    params))

(register-interpreter "note" note)

(defn midi->note
  ([msg]
   (midi->note msg {}))
  ([msg clip]
   (let [gated? (get-in clip [:args :gated])
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
       ret))))

(register-midi-interpreter "note" midi->note)

(defn scale [{:keys [action] :strs [scale] :as data}]
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
                      (assoc "note" n "freq" (mu/midi->hz n))))))))

(register-interpreter "scale" scale)
