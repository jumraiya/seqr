(ns seqr.scheduler
  "Functions used for scheduling and executing actions on supercollider"
  (:import [java.util.concurrent ScheduledThreadPoolExecutor TimeUnit ThreadPoolExecutor]
           [java.io Writer]
           [java.nio.channels AsynchronousSocketChannel]
           [java.net InetSocketAddress]
           [java.nio ByteBuffer]))

(defonce pool (atom nil))
(defrecord PoolInfo [thread-pool jobs-ref id-count-ref])
(defrecord MutablePool [pool-atom])
(defrecord RecurringJob [id ms-period
                         job pool-info
                         scheduled?])
(declare mk-pool)
(declare schedule-job)
(declare cancel-job-id)
(declare shutdown-pool-now!)

(defn start-sched []
  "Creates or recreates the thread pool to execute events"
  (if (or (nil? @pool) (.isShutdown (:thread-pool @pool)))
    (swap! pool (fn [_] (mk-pool))))
  )

(defn stop-job [id]
  "Stops the scheduled job in the thread pool"
  (cancel-job-id id pool true)
  )

(defn start-job
  "Given a bpm, configures the thread pool to execute at a certain rate"
  ([period play-fn]
   (let [pool-info @pool
         id-count-ref  (:id-count-ref pool-info)]
     (dosync
      (commute id-count-ref inc))
    (start-job period play-fn @id-count-ref)))
  ([period play-fn id]
   (let [job (schedule-job id
                           @pool
                           play-fn
                           period)]
     (:id job))))

(defn schedule-task [cmd delay-ms]
  (let [^ScheduledThreadPoolExecutor t-pool (:thread-pool @pool)]
    
    (.schedule t-pool cmd delay-ms TimeUnit/MILLISECONDS)))


(defn is-job-running? [id]
  (let [jobs-ref @(get-in @pool [:jobs-ref])
        job (get-in jobs-ref [id :job])]
    (if (and job (not (.isDone job)) (not (.isCancelled job))) true false)))


(defmethod print-method PoolInfo
  [obj ^Writer w]
  (.write w (str "#<PoolInfo: " (:thread-pool obj) " "
                 (count @(:jobs-ref obj)) " jobs>")))

(defmethod print-method MutablePool
  [obj ^Writer w]
  (.write w (str "#<MutablePool - "
                 "jobs: "(count @(:jobs-ref @(:pool-atom obj)))
                 ">")))

(defmethod print-method RecurringJob
  [obj ^Writer w]
  (.write w (str "#<RecurringJob id: " (:id obj)
                 ", ms-period: " (:ms-period obj)
                 ", initial-delay: " (:initial-delay obj)
                 ", scheduled? " @(:scheduled? obj) ">")))


(defn- switch!
  "Sets the value of atom to new-val. Similar to reset! except returns the
  immediately previous value."
  [atom new-val]
  (let [old-val  @atom
        success? (compare-and-set! atom old-val new-val)]
    (if success?
      old-val
      (recur atom new-val))))

(defn- cpu-count
  "Returns the number of CPUs on this machine."
  []
  (.availableProcessors (Runtime/getRuntime)))



(defn- schedule-job
  "Schedule the fun to execute periodically in pool-info's pool with the
  specified initial-delay and ms-period. Returns a RecurringJob record."
  [id pool-info fun ms-period]
  (let [ms-period     (long ms-period)
        ^ScheduledThreadPoolExecutor t-pool (:thread-pool pool-info)
        start-time    (System/currentTimeMillis)
        jobs-ref      (:jobs-ref pool-info)]
    (dosync
     (let [job-info (map->RecurringJob {:id id
                                        :ms-period ms-period})
           job  (.scheduleAtFixedRate t-pool
                                      #(fun id)
                                      0
                                      ms-period
                                      TimeUnit/MILLISECONDS)]
       (commute jobs-ref update id #(merge % {:job job :info job-info}))
       job-info))))

(defn- shutdown-pool-now!
  "Shut the pool down NOW!"
  [pool-info]
  (.shutdownNow (:thread-pool pool-info))
  (doseq [job (vals @(:jobs-ref pool-info))]
    (reset! (:scheduled? job) false)))


(defn shutdown []
  "Shutdown thread pool"
  (if (and (not (nil? @pool))
           (not (.isShutdown (:thread-pool @pool))))
    (shutdown-pool-now! @pool)
    )
  )



(defn- mk-sched-thread-pool
  "Create a new scheduled thread pool containing num-threads threads."
  [num-threads]
  (let [t-pool (ScheduledThreadPoolExecutor. num-threads)]
    t-pool))

(defn- mk-pool-info
  [t-pool]
  (PoolInfo. t-pool (ref {}) (ref 0N)))

(defn- mk-pool
  "Returns MutablePool record storing a mutable reference (atom) to a
  PoolInfo record which contains a newly created pool of threads to
  schedule new events for. Pool size defaults to the cpu count + 2."
  [& {:keys [cpu-count stop-delayed? stop-periodic?]
      :or {cpu-count (+ 2 (cpu-count))}}]
  (mk-pool-info (mk-sched-thread-pool cpu-count))
  ;(MutablePool. (atom (mk-pool-info (mk-sched-thread-pool cpu-count))))
  )



(defn stop-and-reset-pool!
  "Shuts down the threadpool of given MutablePool using the specified
  strategy (defaults to :stop). Shutdown happens asynchronously on a
  separate thread.  The pool is reset to a fresh new pool preserving
  the original size.  Returns the old pool-info.

  Strategies for stopping the old pool:
  :stop - allows all running and scheduled tasks to complete before
          waiting
  :kill - forcefully interrupts all running tasks and does not wait

  Example usage:
  (stop-and-reset-pool! pool)            ;=> pool is reset gracefully
  (stop-and-reset-pool! pool
                        :strategy :kill) ;=> pool is reset forcefully"
  [pool & {:keys [strategy]
           :or {strategy :stop}}]
  (when-not (some #{strategy} #{:stop :kill})
    (throw (Exception. (str "Error: unknown pool stopping strategy: " strategy ". Expecting one of :stop or :kill"))))
  (let [pool-atom pool
        ;pool-atom      (:pool-atom pool)
        ^ThreadPoolExecutor tp-executor (:thread-pool @pool-atom)
        num-threads   (.getCorePoolSize tp-executor)
        new-t-pool    (mk-sched-thread-pool num-threads)
        new-pool-info (mk-pool-info new-t-pool)
        old-pool-info (switch! pool-atom new-pool-info)]
    (shutdown-pool-now! old-pool-info)
    old-pool-info))

(defn- cancel-job-id
  [id pool cancel-immediately?]
  (let [jobs-ref (:jobs-ref @pool)
        job      (get-in @jobs-ref [id :job])]
    (when (is-job-running? id)
      (.cancel job cancel-immediately?)
      (dosync
       (commute jobs-ref dissoc id)
       (true? (and job (nil? (get @jobs-ref id))))))))

(defn get-job-info [id]
  (get-in @(:jobs-ref @pool) [id :info]))
