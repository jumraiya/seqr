(ns seqr.sequencer
  (:require
   [seqr.clip :as clip]
   [seqr.helper :as helper])
  (:import
   (java.util Arrays)))

(defonce buffer
  (make-array Byte/TYPE 10 1000 512))

(defonce counter (volatile! 0))

(defonce state (agent {:period 1000
                       :div 4
                       :size 4
                       :playing #{}
                       :clip-slots {}
                       :slots (set (range 10))}))

(defn add-clip [clip]
  (when-let [slot (first (:slots @state))]
    (send state #(-> %
                     (update :slots disj slot)
                     (update :clip-slots assoc (:name clip) slot)))
    (let [new-div (helper/lcmv (:div clip) (:div @state))
          new-size (reduce max (mapv ))])))

(defn rm-clip [name]
  (when-let [slot (get-in @state [:clip-slots name])]
    (send state #(-> %
                     (update :clip-slots dissoc name)
                     (update :slots conj slot)))))

(def counter-thread
  (proxy [Thread]
      (run []
        (vswap! counter inc)
        (Thread/sleep (:period @state)))))
