(ns seqr.osc
  (:use [clojure.string :refer [split]]))


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
    java.lang.String (push-string msg val)))

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
                                                 [(keyword (.substring var 1))
                                                  val])}
       (-> word nil? not) {:type t-word :val word}
       (-> f nil? not) {:type t-float :val (Float/parseFloat f)}
       (-> i nil? not) {:type t-int :val (Integer/parseInt i)}
       (-> spread nil? not) {:type t-spread :val (keyword (.substring spread 4))}
       true {:type ::unkown})
     (if (and text match (> (.length text) (.length match)))
       (.substring text (.length match)))]))


(defn builder [template]
  "Usage (builder \"/s_new ?node-id:-1 ?target:0\" {:target 22})"
  (let [[url text] (next-token template)
        url (:val url)
        osc-msg (osc-msg url)]
    (fn [args]
      (if text
        (loop [msg osc-msg text text]
          (let [[data text] (next-token text)
                t (:type data)
                v (:val data)
                [k d] (if (= t t-param)
                        v [nil nil])
                m (condp = t
                    t-int    (push-int msg v)
                    t-float  (push-float msg v)
                    t-word   (push-string msg v)
                    t-param  (push-val
                              msg
                              (if (contains? args k)
                                (get args k)
                                (or d -666)))
                    t-spread (reduce
                              #(push-val %1 %2)
                              msg (if (contains? args v)
                                    (get args v)
                                    [])))]
            (if text
              (recur m text)
              (get-packet m))))
        (get-packet osc-msg)))))
