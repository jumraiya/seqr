(ns seqr.sc
  (:require [clojure.string :as str]
            [clojure.walk :refer [postwalk]]
            [seqr.helper :refer [get-pos]]
            [seqr.music :as mu]
            [seqr.osc :as osc]
            [seqr.samples :as samples]
            [seqr.sequencer :as sequencer]
            [seqr.connections :as conn]
            [seqr.serializers :as se]
            [seqr.ui.clip-config :as clip-config])
  (:import [java.lang ProcessHandle]
           [javax.sound.midi MidiMessage ShortMessage]))

(def s-new
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))

(def query-nodes
  (osc/builder "/g_queryTree ?group:0 ?flag:0"))

(def stop-gated
  (osc/builder "/stop_gated"))

(def stop-gated-synth
  (osc/builder "/stop_gated ?target"))

(def n-set (osc/builder "/n_set ?node-id ?control ?val"))

(def n-free (osc/builder "/n_set ?node-id"))

(def n-query (osc/builder "/n_query ?node-id"))

(def group-free-all (osc/builder "/g_freeAll ?group-id"))

(def eval-sc-code (osc/builder "/eval_code ?code"))

(def sc-sync (osc/builder "/sync ?syncId"))

(def sc-status-req ((osc/builder "/status") {}))

(def sc-new-synth
  (osc/builder "/s_new ?synth ?node-id:-1 ?add-action:0 ?target:0 ...?args"))


(def get-controls (osc/builder "/get_controls ?synth:sin"))

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

(def ^:private default-args
  {[:dest] {"sc" {"synth" "sin"}
            "sc-lang" {"synth" "sin" "gate" 1}}
   [:interpreter] {"scale2" {"scale" "d4 major"}
                   "scale-chord" {"scale" "d4 major"}
                   "drum" {"synth" "sample" "kit" "kit4-electro"}}})

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
    (when (deref synced 100 false)
      (deref res 100 nil))))

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
       (sc-new-synth
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
                    (query-group existing-gid))]
       ;(Thread/sleep 500) ;; Try to avoid overwhelming sc
       (if (nil? exists)
         (loop [i 0 g-id (swap! clip-group-num inc)]
           (make-sync-request
            (new-group {"id" g-id}))
           (if (query-group g-id)
             (when-let [bus (first @available-audio-buses)]
               (ensure-mixer-node g-id name bus)
               (send clip-mixer-data update name assoc :bus bus :group g-id)
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
  (when (contains? #{"sc" "sc-lang"} dest)
    (when-let [{:keys [mixer bus]} (get @clip-mixer-data name)]
      ;(conn/send! "sc" (group-free-all {"group-id" (:group data)}))
      (conn/send! "sc" (n-set {"node-id" mixer "control" "startRelease" "val" 1}))
      (swap! available-audio-buses conj bus)
      (send clip-mixer-data dissoc name))))

(defn update-clip-vol [name value & [reset?]]
  (when-let [group-id (get-in @clip-mixer-data [name :group])]
    (when-let [{:keys [children]} (query-group group-id true)]
      (let [{:keys [id controls]}
            (some #(when (= (:id %) (get-in @clip-mixer-data [name :mixer]))
                     %)
                  children)
            new-vol (if reset?
                      value
                      (+ (get controls "volume") value))]
        (when (or reset? (>= new-vol 0.000001))
         (conn/send! "sc" (n-set {"node-id" id "control" "volume" "val" new-vol})))))))

(defn- get-synth-args [synth]
  (let [synth-args (promise)
        _ (conn/register-one-off-listener
           (keyword "controls" synth)
           (fn [url data] (and (= "/synth_controls" url)
                               (= synth (first data))))
           (fn [data] (deliver synth-args (into {} (map vec) (partition 2 (rest data))))))
        _ (conn/send! "sc-lang" (get-controls {"synth" synth}))]
    (deref synth-args 100 nil)))


(defn- register-callbacks []
  (sequencer/register-callback sequencer/clip-saved :setup-mixer #'setup-mixer)
  (sequencer/register-callback sequencer/clip-deleted :rm-sc-group #'delete-clip-group)
  (sequencer/register-callback
   sequencer/sequencer-paused :stop-gated
   #(conn/send! "sc-lang" (stop-gated {})))
  (sequencer/register-callback
   sequencer/clip-made-inactive
   :stop-gated #(when-let [target (-> % last :args (get "target"))]
                  (conn/send! "sc-lang" (stop-gated-synth {"target" target}))))

  (clip-config/register-callback
   clip-config/clip-arg-set-event
   :sc-synth-args
   (fn [clip arg old-value value]
     (if (= arg [:args "synth"])
       (if-let [synth-args (get-synth-args value)]
         (assoc clip :args (merge synth-args
                                  (assoc
                                   (reduce dissoc (:args clip)
                                           (keys (get-synth-args old-value)))
                                   "synth" value)))
         clip)
       clip))))

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
              (when (loop [i 0]
                      (if (< i 10)
                        (if (some? (query-group 0))
                          true
                          (do
                            (Thread/sleep 1000)
                            (recur (inc i))))
                        (prn "Waiting for sc boot timed out")))
                (reset! current-sc-synth (.pid proc))
                (send clip-mixer-data (constantly {}))
                (conn/send! "sc" (group-free-all {"group-id" 0}))
                (reset-audio-buses)
                (samples/reset-drum-kits)
                (await-for 1000 clip-mixer-data)
                (sequencer/reprocess-clips)))
            (Thread/sleep 1000))
          (catch Exception e
            (prn "Error monitoring sc-synth " (.getMessage e))))))))

(defn- reset-sc-synth-monitor []
  (reset! monitoring-sc? false)
  (when
   (loop [i 0]
     (if (< i 3)
       (if (and sc-synth-monitor-thread
                (not= Thread$State/TERMINATED
                      (.getState ^Thread sc-synth-monitor-thread)))
         (do
           (Thread/sleep 1000)
           (recur (inc i)))
         true)
       (prn "could not terminate sc-synth monitor")))
    (reset! monitoring-sc? true)
    (let [t (mk-sc-synth-monitor)]
      (alter-var-root (var sc-synth-monitor-thread)
                      (constantly t))
      (.start t))))


(defn- action->s-new [{:keys [args] :as action}]
  (let [synth-args (get-synth-args (get action "synth"))
        action (update action :args
                       #(into []
                              (comp
                               (filter (fn [[arg value]]
                                         (or
                                          (= arg "gate") ;; Always send gate
                                          (not= value
                                                (get synth-args arg)))))
                               cat)   
                              (partition 2 %)))]
    (sc-new-synth action)))

(defn setup []
  (conn/add-destination! "localhost" 57110 "sc" action->s-new)

  (conn/add-destination! "localhost" 57120 "sc-lang" action->s-new)

  (samples/reset-drum-kits)
  (register-callbacks)
  (reset-audio-buses)
  (reset-sc-synth-monitor)
  
  (clip-config/register-callback
   clip-config/clip-arg-set-event
   :default-args
   (fn [clip arg _old value]
     (if-let [args (get-in default-args [arg value])]
       (update clip :args merge args)
       clip)))
                                        ;(listen-for-notifications)
  )

