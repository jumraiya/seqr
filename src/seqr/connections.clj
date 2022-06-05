(ns seqr.connections
  (:import
   [java.net InetSocketAddress DatagramSocket InetAddress DatagramPacket]))


(defonce ^:private connections (atom {}))

(defonce ^:private data-socket (atom (DatagramSocket. 6814)))

(defn connect! [^String host ^Integer port ^clojure.lang.Keyword name]
  (swap! connections assoc name (InetSocketAddress. host port)))

(defn disconnect! [^clojure.lang.Keyword name]
  (when (contains? @connections name)
    (.close (get @connections name))
    (swap! connections dissoc name)))

(defn send! [^clojure.lang.Keyword conn bytes]
  (when-let [conn (get @connections conn)]
    (.send @data-socket
           (DatagramPacket. bytes 0 (alength bytes)
                            conn))))
