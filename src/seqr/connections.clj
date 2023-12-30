(ns seqr.connections
  (:require
   [seqr.osc :as osc])
  (:import
   [java.net InetSocketAddress DatagramSocket InetAddress DatagramPacket]
   [java.nio ByteBuffer]
   [java.nio.channels SocketChannel]
   [java.util.concurrent.locks ReentrantLock]
   [java.util.concurrent TimeUnit]))


(defonce ^:private destinations (atom {}))

(defonce ^:private messages (agent {}))

(def MAX-MESSAGES 20)

(defonce ^:private data-socket (atom (DatagramSocket. 6814)))

(defonce ^:private connections (atom {}))

(defonce ^:private serializers (atom {}))

(defonce ^:private osc-msg-listeners (agent {}))

(defonce lock (ReentrantLock. true))

(def receiver-thread nil)

(defonce receiving? (agent false))

(defn register-one-off-listener [key matcher f]
  (send osc-msg-listeners assoc key [matcher f true]))

(defn register-listener [key matcher f]
  (send osc-msg-listeners assoc key [matcher f false]))

(defn- run-listeners [url data]
  (loop [listeners @osc-msg-listeners]
    (let [[k [m f oneoff?]] (first listeners)]
      (if (and m (m url data))
        (do
          (f data)
          (when oneoff?
           (send osc-msg-listeners dissoc k))
          (recur (rest listeners)))
        (if (not (empty? listeners))
          (recur (rest listeners)))))))

(defn- mk-receiver-thread []
  (proxy [Thread] ["osc-receiver"]
    (run []
      (while @receiving?
        (try
          (let [buf (byte-array 4096)
                p (DatagramPacket. buf 4096)
                _ (.receive @data-socket p)
                {:keys [url data] :as msg} (osc/read-msg (.getData p))]
            (run-listeners url data)
            (send messages update url
                  (fn [m]
                    (conj
                     (or (if (>= (count m) MAX-MESSAGES)
                           (vec (rest m))
                           m)
                         [])
                     msg))))
          (catch Exception e
            (prn "Error receiving" (.getMessage e))))))))

(defn reset-receiver []
  (send receiving? (constantly false))
  (send messages (constantly nil))
  (Thread/sleep 100)
  (let [;builder (.name (Thread/ofVirtual) "osc-receiver")
        t (mk-receiver-thread)]
    (send receiving? (constantly true))
    (alter-var-root (var receiver-thread)
                    (constantly
                     t
                     ;(.start builder recv-msg)
                     ))
    (.start t)))

(defn receive-osc-message [url]
  (let [m (peek (get @messages url))]
    (when m
      (send messages update url #(try (pop %) (catch Exception _))))
    m))

(defn get-serializer [dest]
  (get @serializers dest))

(defn add-destination! [^String host ^Integer port name & [serializer]]
  (swap! destinations assoc name (InetSocketAddress. host port))
  (when serializer
    (swap! serializers assoc name serializer)))

(defn rm-destination! [^String host ^Integer port name]
  (when-let [dest (get @destinations name)]
    (.close dest)
    (swap! destinations dissoc name)))

(defn get-destinations []
  (keys @destinations))

(defn connect! [^String host ^Integer port name]
  (try
    (let [ch (doto (SocketChannel/open)
               (.connect (InetSocketAddress. host port)))]
      (.setSoTimeout (.socket ch) 2000)
      (swap! connections assoc name ch))
    (catch Exception e
      (prn "Could not form persistent connection to " name host port (.getMessage e)))))

(defn disconnect! [name]
  (when-let [conn (get @connections name)]
    (try
      (.close conn)
      (swap! connections dissoc name)
      (catch Exception e
        (prn "Could not disconnect" name)))))

(defn send!
  ([conn bytes]
   (send! conn bytes 0 (alength bytes)))
  ([conn bytes offset length]
   (when-let [dest (get @destinations conn)]
     (.send @data-socket
            (DatagramPacket. bytes offset length
                             dest)))))

(defn try-send!
  ([conn bytes]
   (when (.tryLock lock 10 TimeUnit/MILLISECONDS)
       (send! conn bytes 0 (alength bytes))
       (.unlock lock)))
  ([conn bytes offset length]
   (when-let [dest (get @destinations conn)]
     (when (.tryLock lock 10 TimeUnit/MILLISECONDS)
         (.send @data-socket
                (DatagramPacket. bytes offset length
                                 dest))
         (.unlock lock)))))

#_(defn send-and-receive! [^clojure.lang.Keyword conn bytes]
    (when-let [conn (get @connections conn)]
      (let [write-buf (ByteBuffer/wrap bytes)]
        (sched/schedule-task
         #(try
            (let [buf (ByteBuffer/allocate 4096)
                  _ (.read conn buf)
                  data (into [] (.array buf))]
              (prn data)
              data)
            (catch Exception e
              (prn "Nothing received after 2 sec" (.getMessage e))))
         0)
        (.write conn write-buf))))

(defn send-and-receive! [conn bytes]
  (send! conn bytes)
  (future
    (try
      (let [buf (byte-array 4096)
            p (DatagramPacket. buf 4096)
            _ (.receive @data-socket p)]
        (.getData p))
      (catch Exception e
        (prn "Nothing received after 2 sec" (.getMessage e))))))
