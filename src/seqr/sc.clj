(ns seqr.sc
  (:require [seqr.osc :as osc]
            [seqr.sequencer :as sequencer]
            [seqr.connections :as conn]
            [seqr.serializers :as se])
  (:import [javax.sound.midi MidiMessage ShortMessage]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(def query-nodes
  (osc/builder "/g_queryTree ?group:0 ?flag:0"))

(def stop-gated
  (osc/builder "/stop_gated 0"))

(def eval-sc-code (osc/builder "/eval_code ?code"))

(defonce available-audio-buses (atom #{}))

(def rm-group (osc/builder "/g_freeAll ?id:-12"))
                                        ;0	add the new group to the head of the group specified by the add target ID.
                                        ;1	add the new group to the tail of the group specified by the add target ID.
                                        ;2	add the new group just before the node specified by the add target ID.
                                        ;3	add the new group just after the node specified by the add target ID.
                                        ;4	the new node replaces the node specified by the add target ID. The target node is freed.
(def new-group (osc/builder "/g_new ?id:-1 ?action:0 ?target:0"))
;Replies to the sender with a /g_queryTree.reply
;int	flag: if synth control values are included 1, else 0
;; int	node ID of the requested group
;; int	number of child nodes contained within the requested group
;; then for each node in the subtree:	
;; int	node ID
;; int	number of child nodes contained within this node. If -1 this is a synth, if >=0 it's a group
;; then, if this node is a synth:
;; symbol	the SynthDef name for this node.
;; then, if flag (see above) is true:
;; int	numControls for this synth (M)
;; M *	symbol or int	control name or index
;;      float or symbol	value or control bus mapping symbol (e.g. 'c1')


(defonce ^:private clip-groups (agent {}))

(defonce clip-group-num (atom 99))

(defonce ^:private clip-bus-map (agent {}))

(defonce ^:private node-counter (atom 100))

(defonce ^:private gated-nodes (atom {}))

(defonce ^{:dynamic true} *node-tree-data* nil)


(defn- parse-synth-tree
  [id ctls?]
  (let [sname (first *node-tree-data*)]
    (if ctls?
      (let [n-ctls              (second *node-tree-data*)
            [ctl-data new-data] (split-at (* 2 n-ctls) (nnext *node-tree-data*))
            ctls                (apply hash-map ctl-data)]
        (set! *node-tree-data* new-data)
        {:type :synth
         :name sname
         :id id
         :controls ctls})
      (do
        (set! *node-tree-data* (next *node-tree-data*))
        {:type :synth
         :name sname
         :id id}))))

(defn- parse-node-tree-helper [ctls?]
  (let [[id n-children & new-data] *node-tree-data*]
    (set! *node-tree-data* new-data)
    (cond
      (neg? n-children)
      (parse-synth-tree id ctls?)       ; synth

      (= 0 n-children)
      {:type :group
       :id id
       :children nil}

      (pos? n-children)
      {:type     :group
       :id       id
       :children (doall (map (fn [i] (parse-node-tree-helper ctls?))
                             (range n-children)))})))

(defn- parse-node-tree
  [data]
  (let [ctls? (= 1 (first data))]
    (binding [*node-tree-data* (next data)]
      (parse-node-tree-helper ctls?))))


(defn query-group [group-id & [ctls?]]
  (conn/send! "sc" (if ctls?
                     (query-nodes {"group" group-id "flag" 1})
                     (query-nodes {"group" group-id})))
  (loop [i 0]
    (let [[_flag id n-type :as data] (:data (conn/receive-osc-message "/g_queryTree.reply"))]
        (if (and (not= n-type -1) (= id group-id))
          (parse-node-tree data)
          (if (< i 10)
            (do
              (Thread/sleep 100)
              (recur (inc i)))
            (do
              (prn "Could not query group")))))))

(defn- ensure-mixer-node [group-id audio-bus]
  (let [{:keys [children]} (query-group group-id)]
    (when-not (some #(when (= (:name %) "clipMixer")
                       %)
                    children)
      (conn/send!
       "sc" (se/sc-new-synth
             {"synth" "clipMixer"
              "add-action" 1
              "target" group-id
              :args ["audioBus" audio-bus]})))))

(defn- setup-mixer [{:keys [name dest] :as clip}]
  (when (= "sc" dest)
    (let [existing-gid (get @clip-groups name)
          exists (when existing-gid
                   (query-group existing-gid))]
      (if (nil? exists)
        (loop [i 0 g-id (swap! clip-group-num inc)]
          (conn/send! "sc" (new-group {"id" g-id}))
          (if (query-group g-id)
            (when-let [bus (first @available-audio-buses)]
              (send clip-groups assoc name g-id)
              (ensure-mixer-node g-id bus)
              (send clip-bus-map assoc name bus)
              (update clip :args
                      assoc
                      "target" g-id
                      "outBus" bus)
              (swap! available-audio-buses disj bus))
            (if (> i 9)
              (do
                (prn (str "could not assign group for " name))
                clip)
              (recur (inc i) (swap! clip-group-num inc)))))
        (do
          (ensure-mixer-node existing-gid (get clip-bus-map name))
          (update clip :args
                  assoc
                  "target" existing-gid
                  "outBus" (get @clip-bus-map name)))))))


(defn- delete-clip-group [{:keys [name dest]}]
  (when (= "sc" dest)
    (when-let [g-id (get @clip-groups name)]
      (conn/send! "sc" (rm-group {"id" g-id}))
      (send clip-groups dissoc name)
      (swap! available-audio-buses conj (get @clip-bus-map name))
      (send clip-bus-map dissoc name))))

(defn- register-callbacks []
  (sequencer/register-callback sequencer/clip-saved :setup-mixer setup-mixer)
  (sequencer/register-callback sequencer/clip-deleted :rm-sc-group delete-clip-group))

(defn- reset-audio-buses []
  (doseq [b @available-audio-buses]
    (conn/send! "sc-lang"
     (eval-sc-code {"code" (str "b = Bus.new('audio', " b ", 2);b.free;")})))
  (let [cmd "~busses=11.collect{b = Bus.audio(s, 2); b.index;};\"{:busses \" + ~busses.asString + \"}\"; "
        _ (conn/send! "sc-lang" (eval-sc-code {"code" cmd}))
        {:keys [data]} (conn/receive-osc-message "/response")]
    (reset! available-audio-buses (:busses (read-string (first data))))))

(defn- listen-notifications []
  (conn/send! "sc" ((osc/builder "/notify 1") {})))

(defn- release-gated-nodes []
  (doseq [g (vals @clip-groups)]
    (when-let [mixer-id (some #(when (= (:name %) "clipMixer") %)
                              (:children (query-group g)))]
      (conn/send! "sc" ((osc/builder "/n_set ?n-id ...?args") {"n-id" mixer-id "args" ["startRelease" 1]})))
    ))

(defn setup []
  (register-callbacks)
  (reset-audio-buses)
  (listen-notifications))
