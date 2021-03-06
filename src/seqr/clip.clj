(ns seqr.clip
  (:use [clojure.string :refer [replace join]]
        [clojure.data :refer [diff]]
        [seqr.helper :refer [get-pos get-point]]
        [seqr.midi :as midi]
        [seqr.sc :as sc]))

(def t-word ::word) ;; [0-9A-Za-z]+

(def t-params ::params) ;; {:param1 word...}

(def t-rest ::rest) ;; :[0-9]+

(def t-rest-bar ::rest-bar) ;; |

(def t-brace-open ::brace-open) ;; {

(def t-brace-close ::brace-close) ;; }

(def t-bracket-open ::bracket-open) ;; [

(def t-bracket-close ::bracket-close) ;; ]

(def t-paren-open ::paren-open) ;; (

(def t-paren-close ::paren-close) ;; )

(def t-options-boundary ::options-boundary)

(def t-unknown ::unknown)

(def t-eof ::eof)

(declare add-action)

(declare add-rest)

(declare add-params)

(declare add-param-val)

(declare add-map-key)

(declare add-map-val)


(defn- next-token [text]
  (if text
    (let [text (.trim text)
          [match options rest bar bracket-open bracket-close brace-open brace-close word]
          (re-find #"(%)|(:[0-9]+)|(\|)|(\[)|(\])|(\{)|(\})|([^\s\[\]\{\}]+)" text)]
      [(cond
         (-> options nil? not) {:type t-options-boundary :val "%"}
         (-> rest nil? not) {:type t-rest :val (Integer/parseInt (.substring rest 1))}
         (-> bar nil? not) {:type t-rest :val "|"}
         (-> bracket-open nil? not) {:type t-bracket-open :val "["}
         (-> bracket-close nil? not) {:type t-bracket-close :val "]"}
         (-> brace-open nil? not) {:type t-brace-open :val "{"}
         (-> brace-close nil? not) {:type t-brace-close :val "}"}
         (-> word nil? not) {:type t-word
                             :val (cond
                                    (re-matches #"-?[0-9]+\.[0-9]+" word)
                                    (Float/parseFloat (re-matches #"-?[0-9]+\.[0-9]+" word))
                                    (re-matches #"-?[0-9]+" word)
                                    (Integer/parseInt (re-matches #"-?[0-9]+" word))
                                    true word)
                             }
         true (if (> (.length text) 0)
                {:type t-unknown}
                {:type t-eof}))
       (if (and text match (> (.length text) (.length match)))
         (.substring text (.length match)))])
    [{:type t-eof}]))



(defn- add-rest [token text & {:keys [point div] :as clip}]
  (let [rest (:val token)
        [token text] (next-token text)
        bar? (= "|" rest)
        [bar note] (get-pos (dec point) div)
        delta (if bar?
                (cond
                  (= "|" (:val token)) div
                  (= note div) 0
                  true (- div note))
                rest)
        clip (update clip :point #(+ % delta))]
    (condp = (:type token)
      t-rest (add-rest token text clip)
      t-word (add-action token text clip false)
      t-bracket-open (add-action token text clip true)
      t-eof clip
      t-unknown (throw (Exception. (str "unknown token " token " : " text))))))

(defn- add-map-key [data text]
  (let [[token text] (next-token text)
        k (:val token)
        k (if (and (string? k) (> (.length k) 0) (= (.charAt k 0) \:))
            (keyword (.substring k 1))
            k)]
    (condp = (:type token)
      t-word (add-map-val data text k)
      t-brace-close [data text])))

(defn- add-map-val [data text key]
  (let [[token text] (next-token text)]
    (condp = (:type token)
      t-word (add-map-key (assoc data key (:val token)) text)
      t-brace-open (let [[sub text] (add-map-key {} text)]
                       (add-map-key (assoc data key sub) text)))))

(defn- add-params [token text {:keys [eval point div] :as clip} in-group? after-group?]
  (let [[params text] (add-map-key {} text)
        pos (get-pos point div)
        clip (if after-group?
               (update-in clip pos
                          (fn [actions]
                            (vec (map #(merge % params) actions))))
               (update-in clip pos
                          (fn [actions]
                            (conj (-> actions butlast vec)
                                  (merge (last actions) params)))))
        [token text] (next-token text)
        clip (if (not in-group?)
               (update clip :point inc)
               clip)]
    (condp = (:type token)
      t-word (add-action token text clip in-group?)
      t-bracket-open (if in-group?
                       (throw (Exception. "Cannot nest group actions"))
                       (add-action token text clip true))
      t-bracket-close (add-action token text clip false)
      t-rest (add-rest token text clip)
      t-eof clip)))


(defn- add-action [token text {:keys [eval point div args] :or {args {}} :as clip} group?]
  (let [pos (get-pos point div)
        after-group? (= t-bracket-close (:type token))
        clip (if (= t-word (:type token))
               (update-in clip pos #(conj (vec %) (merge args {:action (:val token)})))
               clip)
        [token text] (next-token text)
        clip (update clip :point (if (or group?
                                         (= t-brace-open (:type token))
                                         #_(and after-group?
                                              (= t-brace-open (:type token))))
                                   identity
                                   inc))]
    (condp = (:type token)
      t-word (add-action token text clip group?)
      t-rest (add-rest token text clip)
      t-bracket-open (if group?
                       (throw (Exception. "Cannot nest group actions"))
                       (add-action token text clip true))
      t-bracket-close (add-action token text clip after-group?)
      t-brace-open (add-params token text clip group? after-group?)
      t-eof clip)))

(defn as-str [{:keys [div args point] :as clip}]
  (let [text (StringBuilder.)
        mk-action-str (fn [actions]
                        (let [multi-action? (> (count actions) 1)
                              [common-params] (if multi-action?
                                                (diff
                                                 (reduce #(nth (diff (dissoc %1 :action) (dissoc %2 :action)) 2) actions)
                                                 args))
                              action-str (->> actions
                                              (map
                                               #(let [a-params (-> (dissoc % :action) (diff common-params) first (diff args) first)]
                                                  (str (:action %) (if a-params (str " " (-> a-params str (clojure.string/replace "\"" "")) " ")) " ")))
                                              join)]
                          (.trim
                           (if multi-action?
                             (str "[" (.trim action-str) "]" (if common-params (str " " (-> common-params str (clojure.string/replace "\"" "")))))
                             action-str))))
        [action-strs col-lens] (loop [actions {} lens {} p 1 last-action 0]
                                 (let [[bar note] (get-pos p div)
                                       has-action? (-> (get-in clip [bar note]) empty? not)
                                       rest-val (- p (max last-action (dec (get-point bar 1 div))) 1)
                                       rest-str (if (and has-action?
                                                         (> rest-val 0)
                                                         (not (= 1 note)))
                                                  (str ":" rest-val " ")
                                                  "")
                                       s (mk-action-str (get-in clip [bar note] []))
                                       [actions lens] (if has-action?
                                                        [(assoc actions p s)
                                                         (update lens note #(max (or % 0) (.length s)))]
                                                        [actions lens])
                                       [actions lens] (if (-> rest-str empty? not)
                                                        [(assoc actions (dec p) rest-str)
                                                         (update lens (-> p dec (get-pos div) second)
                                                                 #(max (or % 0) (.length rest-str)))]
                                                        [actions lens])]
                                   (if (< p point)
                                     (recur actions lens (inc p) (if has-action? p last-action))
                                     [actions lens])))
        pad-str #(str (get action-strs % " ")
                      (apply str (repeat (- (get col-lens
                                                 (second (get-pos % div)) 1)
                                            (.length (get action-strs % " ")))
                                         " ")))
        fn-to-str #(str (-> % meta :ns) "/" (-> % meta :name))
        options (into {} (map (fn [[k v]]
                                (when (and (not (number? k))
                                           (not (= k :point))
                                           (not (= v clojure.core/identity)))
                                  [k (cond
                                       (map? v) (into {} (map (fn [[k v]] [k (if (fn? v) (fn-to-str v) v)]) v))
                                       (fn? v) (fn-to-str v)
                                       true v)]))
                              clip))
        options-str (str (-> options
                             str
                             (clojure.string/replace "," "\n")
                             (clojure.string/replace "\"" ""))
                         "\n\n")]

    (.append text options-str)
    (loop [positions {} offset (.length options-str) p 1]
      (let [[bar note] (get-pos p div)
            has-action? (-> clip (get-in [bar note]) empty? not)
            s (str
               (pad-str p) " "
               (if (= div note)
                 "|\n\n"))
            end (+ offset (.length s))
            positions (if has-action?
                        (assoc-in positions [bar note] [offset end])
                        positions)]
        (.append text s)
        (if (< p point)
          (recur positions end (inc p))
          [positions (.toString text)])))))

(defn parse-clip [text & [clip]]
  (let [clip (merge {:div 4 :args {} :outs {} :group "default"} clip)
        [token text] (next-token text)
        clip (assoc clip :point 1)
        clip (condp = (:type token)
               t-brace-open (let [[options text] (add-map-key {} text)]
                              (parse-clip text (merge clip options)))
               t-brace-close (parse-clip text clip)
               t-bracket-open (add-action token text clip true)
               t-word (add-action token text clip false)
               t-rest (add-rest token text clip)
               t-eof clip)
        get-fn #(condp = (type %)
                  java.lang.String (with-meta
                                     (-> % symbol find-var var-get)
                                     (meta (-> % symbol find-var)))
                  clojure.lang.Symbol (with-meta
                                        (var-get (find-var %))
                                        (-> % find-var meta))
                  %)
        outs (into {} (map (fn [[k v]] [k (get-fn v)]) (:outs clip)))
        eval-fn (or (:eval clip) identity)
        eval-fn (get-fn eval-fn)]
    (assoc clip :eval eval-fn :outs outs)))

(defmacro clip [actions & {:keys [div args outs eval group] :or {div 4 args {} group "default"} :as clip}]
  (let [data (parse-clip (reduce #(str %1 (clojure.string/replace %2 "," "") " ") "" actions)
                         (merge clip {:div div
                                      :args args}))]
    data))


(defn calc-size [{:keys [size div point] :as clip} & [player-div]]
  "Returns the size of a clip"
  (let [[bar note] (or size (get-pos (dec point) div))
        step (if player-div (/ player-div div) 1)
        clip-size (* div (dec bar) step)
        clip-size (+ clip-size (* step note))]
    clip-size))

(defn midi-interpreter [clip]
  (let [{:keys [name ns]} (meta (:eval clip))
        e (str ns "/" name)
        midi-fn (condp = e
                  "seqr.sc/note" seqr.sc/midi->note
                  false)]
    midi-fn))


(defn build-from-midi [bpm {:keys [div] :as clip}]
  (let [buf (midi/get-quantized-buffer bpm div)
        midi-fn (midi-interpreter clip)
        clip (reduce dissoc clip (filter number? (keys clip)))
        clip (reduce
              (fn [clip [p msgs]]
                (let [actions (vec (filter (comp not empty?) (map #(midi-fn % clip) msgs)))]
                  (if (not (empty? actions))
                    (assoc-in clip (get-pos p div) actions)
                    clip)))
              clip buf)
        max-bar (apply max (filter number? (keys clip)))
        max-note (apply max (keys (get clip max-bar)))
        point (get-point max-bar max-note div)]
    (assoc clip :point (inc point))))
