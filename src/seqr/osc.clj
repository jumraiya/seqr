(ns seqr.osc
  (:require [clojure.string :refer [split]]
            [seqr.helper :as helper])
  (:import [java.nio ByteBuffer]))


(def t-word ::word)

(def t-param ::param)

(def t-url ::url)

(def t-spread ::spread)

(def t-int ::int)

(def t-float ::float)

(def t-unknown ::unknown)


(defn- padding-4 [len]
  (if (< len 4)
    (- 4 len)
    (- 4 (mod len 4))))

(defn push-string [{:keys [data tags] :as msg} ^String string]
  (let [bytes (into [] (.getBytes string))
        padding (padding-4 (count bytes))]
    (merge msg
           {:data (concat data bytes (repeat padding 0))
            :tags (conj tags (byte \s))})))

(defn push-int [{:keys [data tags] :as msg} val]
  (let [val (int val)
        bytes (map #(-> val (unsigned-bit-shift-right %) unchecked-byte) [24 16 8 0])]
   (merge msg
          {:data (concat data bytes)
           :tags (conj tags (byte \i))})))

(defn push-float [{:keys [data tags] :as msg} val]
  (let [val (Float/floatToIntBits (float val))
        bytes (map #(-> val (unsigned-bit-shift-right %) unchecked-byte) [24 16 8 0])]
    (merge msg
           {:data (concat data bytes)
            :tags (conj tags (byte \f))})))

(defn push-val [{:keys [data tags] :as msg} val]
  (condp = (type val)
    java.lang.Long (push-int msg val)
    java.lang.Integer (push-int msg val)
    java.lang.Double (push-float msg val)
    java.lang.Float (push-float msg val)
    java.lang.String (push-string msg val)
    clojure.lang.Keyword (push-string msg (name val))))

(defn push-vals [msg values]
  (reduce #(push-val %1 %2) msg values))

(defn osc-msg [^String url]
  {:url url
   :tags [(byte \,)]
   :data []}
  )

(defn get-packet [{:keys [url tags data] :as msg}]
  (let [url (into [] (.getBytes url))
        url-padding (padding-4 (count url))
        url (concat url (repeat url-padding 0))
        tags-padding (padding-4 (count tags))
        tags (concat tags (repeat tags-padding 0))]
    (byte-array (concat url tags data))
    )
  )


(defn- next-token [text]
  (let [text (.trim text)
        [match url param word f i spread]
        (re-find #"(/[^\s]+)|(\?[a-zA-Z]+[a-zA-Z0-9]*:?[^\s]*)|([a-zA-Z]+[a-zA-Z0-9]*)|(-?[0-9]+\.[0-9]+)|(-?[0-9]+)|(\.\.\.\?[a-zA-Z]+[a-zA-Z0-9]*)" text)]
    [(cond
       (-> url nil? not) {:type t-url :val url}
       (-> param nil? not) {:type t-param :val (let [[var default] (split param #":" 2)
                                                     val (if default
                                                           (-> default next-token first :val))]
                                                 [(.substring var 1)
                                                  val])}
       (-> word nil? not) {:type t-word :val word}
       (-> f nil? not) {:type t-float :val (Float/parseFloat f)}
       (-> i nil? not) {:type t-int :val (Integer/parseInt i)}
       (-> spread nil? not) {:type t-spread :val (keyword (.substring spread 4))}
       true {:type ::unkown})
     (if (and text match (> (.length text) (.length match)))
       (.substring text (.length match)))]))


(defmacro builder [template]
  (let [[url text] (next-token template)
        url (:val url)
        data-sym (gensym)
        msg-sym (gensym)
        ops (if-not (empty? (.trim (str text)))
              (loop [ops [] text text]
                (let [[data text] (next-token text)
                      t (:type data)
                      v (:val data)
                      [k d] (if (= t t-param)
                              v [nil nil])
                      op (condp = t
                           t-int    `(push-int ~v)
                           t-float  `(push-float ~v)
                           t-word   `(push-string ~v)
                           t-param  `(push-val (get data ~k (or ~d -666)))
                           t-spread `(push-vals (get data ~v [])))]
                  (if text
                    (recur (conj ops op) text)
                    (conj ops op `(get-packet)))))
              `((get-packet)))
        xargs {'data data-sym 'msg msg-sym}
        ops (mapv #(helper/replace-syms xargs %) ops)]
    `(fn [~data-sym]
       (let [~msg-sym (osc-msg ~url)]
         (-> ~msg-sym
             ~@ops)))))

(defn osc-align
  "Jump the current position to a 4 byte boundary for OSC compatible alignment."
  [buf]
  (.position buf (bit-and (bit-not 3) (+ 3 (.position buf)))))

(defn- decode-string
  "Decode string from current pos in buf. OSC strings are terminated by a null
  char."
  [buf]
  (let [start (.position buf)]
    (while (not (zero? (.get buf))) nil)
    (let [end (.position buf)
          len (- end start)
          str-buf (byte-array len)]
      (.position buf start)
      (.get buf str-buf 0 len)
      (osc-align buf)
      (String. str-buf 0 (dec len)))))

(defn- decode-blob
  "Decode binary blob from current pos in buf. Size of blob is determined by the
  first int found in buffer."
  [buf]
  (let [size (.getInt buf)
        blob (byte-array size)]
    (.get buf blob 0 size)
    (osc-align buf)
    blob))

(defn read-msg [data]
  (let [buf (ByteBuffer/wrap data)
        path (decode-string buf)
        type-tag (decode-string buf)
        args (reduce (fn [mem t]
                       (conj mem
                             (case t
                               \i (.getInt buf)
                               \h (.getLong buf)
                               \f (.getFloat buf)
                               \d (.getDouble buf)
                               \b (decode-blob buf)
                               \s (decode-string buf))))
                     []
                     (rest type-tag))]
    {:url path :tags type-tag :data args}))
