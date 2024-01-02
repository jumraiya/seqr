(ns seqr.sc
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [seqr.helper :refer [get-pos]]
            [seqr.music :as mu]
            [seqr.osc :as osc]
            [seqr.sequencer :as sequencer]
            [seqr.connections :as conn]
            [seqr.serializers :as se])
  (:import [java.lang ProcessHandle]
           [javax.sound.midi MidiMessage ShortMessage]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(def query-nodes
  (osc/builder "/g_queryTree ?group:0 ?flag:0"))

(def stop-gated
  (osc/builder "/stop_gated 0"))

(def n-set (osc/builder "/n_set ?node-id ?control ?val"))

(def n-query (osc/builder "/n_query ?node-id"))

(def eval-sc-code (osc/builder "/eval_code ?code"))

(def sc-sync (osc/builder "/sync ?syncId"))

(def sc-status-req ((osc/builder "/status") {}))

(defonce available-audio-buses (atom #{}))

(defonce gated-nodes (atom {}))

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


(defonce clip-group-num (atom 99))

(defonce ^:private clip-mixer-data (agent {}))

(defonce ^:private node-counter (atom 100))

(defonce ^:private gated-nodes (atom {}))

(defonce ^:private sync-msg-counter (atom 0))

(defonce ^:private current-sc-synth (atom nil))

(defonce ^{:dynamic true} *node-tree-data* nil)

(defonce ^:private monitoring-sc? (atom false))

(def ^:private sc-synth-monitor-thread nil)


(defn- make-sync-request [req & [resp-matcher resp-handler]]
  "Ensures that the request has been processed before returning"
  (let [sync-id (swap! sync-msg-counter inc)
        synced (promise)
        res (promise)]
    (when (and resp-matcher resp-handler)
      (conn/register-one-off-listener
       (keyword (str "req-" sync-id))
       resp-matcher
       (fn [data]
         (deliver res (resp-handler data)))))
    (conn/send! "sc" req)
    (conn/register-one-off-listener
     (keyword (str "sync-" sync-id))
     (fn [url data]
       (and (= url "/synced") (= (first data) sync-id)))
     (fn [_]
       (deliver synced true)))
    (conn/send! "sc" (sc-sync {"syncId" sync-id}))
    (when (deref synced 500 false)
      (deref res 1000 nil))))

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
  (make-sync-request
   (if ctls?
     (query-nodes {"group" group-id "flag" 1})
     (query-nodes {"group" group-id}))
   (fn [url data]
     (and (= url "/g_queryTree.reply")
          (let [[_ id n-type] data]
            (and (not= n-type -1) (= id group-id)))))
   parse-node-tree))

(defn- ensure-mixer-node [group-id clip-name audio-bus]
  (let [{:keys [children]} (query-group group-id)]
    (when-not (some #(when (= (:name %) "clipMixer")
                       %)
                    children)
      (make-sync-request
       (se/sc-new-synth
             {"synth" "clipMixer"
              "add-action" 1
              "target" group-id
              :args ["audioBus" audio-bus]})))
    (let [{:keys [children]} (query-group group-id)]
      (if-let [node (some #(when (= (:name %) "clipMixer")
                             %)
                          children)]
        (send clip-mixer-data assoc-in
              [clip-name :mixer]
              (:id node))
        (prn "Could not find mixer node")))))

(defn- setup-mixer [{:keys [div name dest args] :as clip}]
  (if (contains? #{"sc" "sc-lang"} dest)
    (let [existing-gid (get-in @clip-mixer-data [name :group])
          exists (when existing-gid
                   (query-group existing-gid))
          ;; is-gated? (or (contains? args "gate")
          ;;               (some?
          ;;                (let [x (transient [])]
          ;;                  (postwalk #(if (and (map? %)
          ;;                                      (contains? % :action)
          ;;                                      (contains? % "gate"))
          ;;                               (do (conj! x %) nil)
          ;;                               %)
          ;;                            clip)
          ;;                  (first (persistent! x)))))
          ]
      (if (nil? exists)
        (loop [i 0 g-id (swap! clip-group-num inc)]
          (conn/send! "sc" (new-group {"id" g-id}))
          (if (query-group g-id)
            (when-let [bus (first @available-audio-buses)]
              (ensure-mixer-node g-id name bus)
              (send clip-mixer-data update name
                    assoc :bus bus :group g-id
                    ;:gated? is-gated?
                    )
              (swap! available-audio-buses disj bus)
              (update clip :args
                      assoc
                      "target" g-id
                      "outBus" bus))
            (if (> i 9)
              (do
                (prn (str "could not assign group for " name))
                clip)
              (recur (inc i) (swap! clip-group-num inc)))))
        (do
          (ensure-mixer-node existing-gid name (get-in @clip-mixer-data [name :bus]))
          (update clip :args
                  assoc
                  "target" existing-gid
                  "outBus" (get-in @clip-mixer-data [name :bus])))))
    clip))


(defn- delete-clip-group [{:keys [name dest]}]
  (when (= "sc" dest)
    (when-let [data (get @clip-mixer-data name)]
      (conn/send! "sc" (rm-group {"id" (:group data)}))
      (send clip-mixer-data dissoc name)
      (swap! available-audio-buses conj (get-in @clip-mixer-data [name :bus])))))

(defn update-clip-vol [name add]
  (when-let [group-id (get-in @clip-mixer-data [name :group])]
    (when-let [{:keys [children]} (query-group group-id true)]
      (let [{:keys [id controls]}
            (some #(when (= (:id %) (get-in @clip-mixer-data [name :mixer]))
                     %)
                  children)]
        (conn/send! "sc" (n-set {"node-id" id "control" "volume" "val"
                                 (+ (get controls "volume") add)}))))))


(defn- register-callbacks []
  (sequencer/register-callback sequencer/clip-saved :setup-mixer setup-mixer)
  (sequencer/register-callback sequencer/clip-deleted :rm-sc-group delete-clip-group))

(defn- reset-audio-buses []
  (doseq [b @available-audio-buses]
    (conn/send! "sc-lang"
                (eval-sc-code {"code" (str "b = Bus.new('audio', " b ", 2);b.free;")})))
  (let [cmd "~busses=11.collect{b = Bus.audio(s, 2); b.index;};\"{:busses \" + ~busses.asString + \"}\"; "
        _ (conn/send! "sc-lang" (eval-sc-code {"code" cmd}))
        _ (Thread/sleep 100)
        {:keys [data]} (conn/receive-osc-message "/response")]
    (reset! available-audio-buses (-> data first read-string :busses set))))

(defn- listen-for-notifications []
  (conn/send! "sc" ((osc/builder "/notify 1") {}))
  #_(conn/register-listener :node-go
                          (fn [url _]
                            (= url "/n_go"))
                          (fn [[id group]]
                            (when (some )))))

(defn- release-gated-nodes []
  (doseq [{:keys [mixer]} (vals @clip-mixer-data)]
    (conn/send! "sc" (n-set {"node-id" mixer "control" "startRelease" "val" 1}))))

(defn- mk-sc-synth-monitor []
  (proxy [Thread] ["sc-synth-receiver"]
    (run []
      (while @monitoring-sc?
        (try
          (let [proc (some
                      #(when (str/includes?
                              (-> % (.info) (.command) (.orElse ""))
                              "scsynth")
                         %)
                      (-> (ProcessHandle/allProcesses)
                          (.iterator)
                          iterator-seq))]
            (when (and proc
                       (.isAlive proc)
                       (not= (.pid proc) @current-sc-synth))
              (Thread/sleep 2000) ;; process might still be booting
              (reset! current-sc-synth (.pid proc))
              (sequencer/reprocess-clips))
            (Thread/sleep 1000))
          (catch Exception e
            (prn "Error monitoring sc-synth " (.getMessage e))))))))

(defn- reset-sc-synth-monitor []
  (reset! monitoring-sc? false)
  (loop [i 0]
    (if (< i 2)
     (if (and sc-synth-monitor-thread
              (not= Thread$State/TERMINATED
                    (.getState ^Thread sc-synth-monitor-thread)))
       (do
         (Thread/sleep 1000)
         (recur (inc i))))
     (prn "could not terminate sc-synth monitor")))
  (reset! monitoring-sc? true)
  (let [t (mk-sc-synth-monitor)]
    (alter-var-root (var sc-synth-monitor-thread)
                    (constantly t))
    (.start t)))


(defn setup []
  (register-callbacks)
  (reset-audio-buses)
  (reset-sc-synth-monitor)
  ;(listen-for-notifications)
  )

