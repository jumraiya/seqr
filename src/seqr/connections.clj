(ns seqr.connections
  (:require
   [seqr.scheduler :as sched])
  (:import
   [java.net InetSocketAddress DatagramSocket InetAddress DatagramPacket]
   [java.nio ByteBuffer]
   [java.nio.channels SocketChannel]))


(defonce ^:private destinations (atom {}))

(defonce ^:private data-socket (atom (doto (DatagramSocket. 6814)
                                       (.setSoTimeout 2000))))

(defonce ^:private connections (atom {}))



(defn add-destination! [^String host ^Integer port name]
  (swap! destinations assoc name (InetSocketAddress. host port)))

(defn rm-destination! [^String host ^Integer port name]
  (when-let [dest (get @destinations name)]
    (.close dest)
    (swap! destinations dissoc name)))

(defn connect! [^String host ^Integer port name]
 (try
    (let [ch (doto (SocketChannel/open)
               (.connect (InetSocketAddress. host port)))]
      (.setSoTimeout (.socket ch) 2000)
      (swap! connections update name ch))
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
  (let [res (future
              (try
                (let [buf (byte-array 4096)
                      p (DatagramPacket. buf 4096)
                      _ (.receive @data-socket p)]
                  (.getData p))
                (catch Exception e
                  (prn "Nothing received after 2 sec" (.getMessage e)))))]
    (send! conn bytes)
    res))
