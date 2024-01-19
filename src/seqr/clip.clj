(ns seqr.clip
  (:require [clojure.string :refer [join]]
            [clojure.data :refer [diff]]
            [seqr.helper :refer [replace-syms get-pos get-point]]
            [seqr.music :as m]
            [seqr.midi :as midi]
            [seqr.interpreters :as interp]))

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

(def t-single-quote ::single-quote)

(def t-options-boundary ::options-boundary)

(def t-action-var ::action-var)

(def t-unknown ::unknown)

(def t-eof ::eof)

(declare add-action)

(declare add-action-var)

(declare add-rest)

(declare add-params)

(declare add-param-val)

(declare add-map-key)

(declare add-map-val)

(declare parse-clip)

(defn eval-clj [in]
  (eval (read-string (str "(do (require '[seqr.music :refer :all]) " in ")"))))

(defn- next-token [text]
  (if text
    (let [text (.trim text)
          [match options rest bar bracket-open bracket-close brace-open brace-close paren-open paren-close single-quote action-var word]
          (re-find #"(%)|(:[0-9]+)|(\|)|(\[)|(\])|(\{)|(\})|(\()|(\))|(\')|(\$[a-z]+)|([^\s\[\]\{\}\(\)']+)" text)]
      [(cond
         (-> options nil? not) {:type t-options-boundary :val "%"}
         (-> rest nil? not) {:type t-rest :val (Integer/parseInt (.substring rest 1))}
         (-> bar nil? not) {:type t-rest :val "|"}
         (-> bracket-open nil? not) {:type t-bracket-open :val "["}
         (-> bracket-close nil? not) {:type t-bracket-close :val "]"}
         (-> brace-open nil? not) {:type t-brace-open :val "{"}
         (-> brace-close nil? not) {:type t-brace-close :val "}"}
         (-> paren-open nil? not) {:type t-paren-open :val "("}
         (-> paren-close nil? not) {:type t-paren-close :val ")"}
         (-> single-quote nil? not) {:type t-single-quote :val "'"}
         (-> action-var nil? not) {:type t-action-var :val action-var}
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

(defn is-action-var? [v]
  (and (string? v) (.startsWith ^String v "$")))

(defn- strs-eq? [str1 str2]
  (loop [data1 (next-token str1) data2 (next-token str2)]
    (let [[tok1 text1] data1
          [tok2 text2] data2]
      (if (and (= (:type tok1) t-eof)
               (= (:type tok2) t-eof))
        true
       (if (and (= (:type tok1) (:type tok2))
                (= (:val tok1) (:val tok2)))
         (recur (next-token text1) (next-token text2))
         false)))))

(defn- find-matching-action-var [action-vars action-str]
  (some (fn [[v data]]
          (when (strs-eq? data action-str)
            v))
        action-vars))

(defn- mk-params-str [params]
  (-> (clojure.walk/postwalk
           #(cond
              (and (string? %)
                   (clojure.string/includes? % " "))
              (str "'" % "'")
              (and (map? %) (contains? % :param-str))
              (:param-str %)
              :else %)
           params)
      str
      (clojure.string/replace "," "\n")
      (clojure.string/replace "\"" "")))


(defn mk-action-str [actions {:keys [args] :as clip}]
  (let [action-vars (into {}
                          (comp
                           (filter #(string? (key %)))
                           (filter
                            (fn [[k v]]
                              (.startsWith ^String k "$"))))
                          clip)]
    (if-let [action-var-str
             (find-matching-action-var
              action-vars
              (str "[" (reduce #(str %1 " " (:action-str %2)) "" actions) "]"))]
      action-var-str
      (let [multi-action? (> (count actions) 1)
            [common-params]
            (if multi-action?
              (diff
               (reduce #(nth (diff (dissoc %1 :action :action-str)
                                   (dissoc %2 :action :action-str))
                             2) actions)
               args))
            action-str (->> actions
                            (map
                             #(let [a-params (-> (dissoc % :action :action-str)
                                                 (diff common-params)
                                                 first
                                                 (diff args)
                                                 first)
                                    params-str (if (not (empty? a-params))
                                                 (str " " (mk-params-str a-params) " "))]
                                (if-let [ret (find-matching-action-var
                                              action-vars (clojure.string/replace
                                                           (str (:action-str %)
                                                                params-str)
                                                           "'" ""))]
                                  ret
                                  (str
                                   (or
                                    (find-matching-action-var
                                     action-vars (:action-str %))
                                    (:action-str %) (:action %))
                                   params-str
                                   " "))))
                            join)]
        (.trim
         (if multi-action?
           (str "[" (.trim action-str) "]"
                (if (not (empty? common-params))
                  (str " " (mk-params-str common-params))))
           action-str))))))

(defn- get-after-sexp [text]
  "Get the text after a sexp, this allows the parsing to continue after we encounter a clj expression"
  (let [s-expr (StringBuilder.)]
    (loop [lvl 0 text text]
      (let [[token text] (next-token text)
            lvl (condp = (:type token)
                  t-paren-open (inc lvl)
                  t-paren-close (dec lvl)
                  lvl)]
        (if (= lvl 0)
          [(-> (doto s-expr
                 (.append ")"))
               (.toString)
               read-string
               str)
           text]
          (do
            (.append s-expr (str (:val token) " "))
            (recur lvl text)))))))

(defn- read-quoted-value [text]
  (let [v (re-find #"(?)[^\']+" text)]
    [v (subs text (inc (.length ^String v)))]))

(defn- add-rest [token text & {:keys [point div] :as clip}]
  (let [rest (:val token)
        [token text] (next-token text)
        bar? (= "|" rest)
        [bar note] (get-pos (dec point) div)
        delta (if bar?
                (cond
                  (= point 1) div
                  (= "|" (:val token)) div
                  (= note div) 0
                  true (- div note))
                rest)
        clip (update clip :point #(+ % delta))]
    (condp = (:type token)
      t-rest (add-rest token text clip)
      t-paren-open (add-action token text clip false)
      t-action-var (add-action-var token text clip)
      t-word (add-action token text clip false)
      t-bracket-open (add-action token text clip true)
      t-eof clip
      t-unknown (throw (Exception. (str "unknown token " token " : " text))))))

(defn- add-map-key [data text & [dynamic?]]
  (let [[token text] (next-token text)
        k (:val token)
        k (if (and (string? k) (> (.length k) 0) (= (.charAt k 0) \:))
            (keyword (.substring k 1))
            k)]
    (condp = (:type token)
      t-word (add-map-val data text k dynamic?)
      t-action-var (add-map-val data text k dynamic?)
      t-brace-close [data text dynamic?])))

(defn- add-map-val [data text key & [dynamic?]]
  (let [[token text] (next-token text)]
    (condp = (:type token)
      t-single-quote (let [[v text] (read-quoted-value text)
                           [value] (if (and (string? key) (.startsWith key "$"))
                                     [{"x" v}]
                                     (add-map-key {} (str "x " v "}") false))]
                       (add-map-key (assoc data key (get value "x")) text dynamic?))
      t-word (add-map-key (assoc data key (:val token)) text dynamic?)
      t-paren-open (let [[a-str after] (get-after-sexp (str "( " text))]
                     (add-map-key (assoc data key
                                         {:val (eval-clj (str "(fn [bar note]" a-str ")"))
                                          :param-str a-str})
                                  after true))
      t-brace-open (let [[sub text] (add-map-key {} text)]
                     (add-map-key (assoc data key sub) text dynamic?)))))

(defn- add-params [token text {:keys [eval point div] :as clip} in-group? after-group?]
  (let [[params text dynamic?] (add-map-key {} text)
        pos (get-pos point div)
        clip (if after-group?
               (update-in clip pos
                          (fn [actions]
                            (vec (map #(merge % params) actions))))
               (update-in clip pos
                          (fn [actions]
                            (conj (-> actions butlast vec)
                                  (merge (last actions) params)))))
        clip (if dynamic?
               (update clip :dynamic conj point)
               clip)
        [token text] (next-token text)
        clip (if (not in-group?)
               (update clip :point inc)
               clip)]
    (condp = (:type token)
      t-word (add-action token text clip in-group?)
      t-action-var (add-action-var token text clip)
      t-paren-open (add-action token text clip in-group?)
      t-bracket-open (if in-group?
                       (throw (Exception. "Cannot nest group actions"))
                       (add-action token text clip true))
      t-bracket-close (add-action token text clip false)
      t-rest (add-rest token text clip)
      t-eof clip)))

#_(defmacro ^:private mk-dynamic-action [xform]
    (let [bar-sym (gensym)
          note-sym (gensym)
          body (replace-syms {'bar bar-sym 'note note-sym} xform)]
      `(fn [~bar-sym ~note-sym]
         (replace-syms
          ~body))))

(defn- add-action [token text {:keys [point div args parse-until] :or {args {}} :as clip} group?]
  (let [pos (get-pos point div)
        after-group? (= t-bracket-close (:type token))
        action-var-str (:action-var-str token)
        [is-action? action text action-str dynamic?]
        (condp = (:type token)
          t-word [true (:val token) text (:val token) false]
          t-paren-open (let [[a-str after] (get-after-sexp (str "( " text))]
                         [true (eval-clj (str "(fn [bar note]" a-str ")")) after a-str true])
          [false nil text nil false])
        clip (cond-> clip
               is-action?
               (update-in pos
                          #(conj (vec %)
                                 (merge args {:action action
                                              :action-str (str action-str)}
                                        (when action-var-str
                                          {:action-var-str action-var-str}))))
               (or dynamic?
                   (and is-action?
                        (some #(when (and (map? %) (fn? (:val %))) true) (vals args))))
               (update :dynamic conj point))
        [token text] (next-token text)
        token (if (and (not is-action?) action-var-str)
                (assoc token :action-var-str action-var-str)
                token)
        clip (update clip :point (if (or group?
                                         (= t-brace-open (:type token)))
                                   identity
                                   inc))]
    (if (and parse-until (>= (:point clip) parse-until))
      [clip text token]
      (condp = (:type token)
        t-word (add-action token text clip group?)
        t-action-var (add-action-var token text clip group?)
        t-paren-open (add-action token text clip group?)
        t-rest (add-rest token text clip)
        t-bracket-open (if group?
                         (throw (Exception. "Cannot nest group actions"))
                         (add-action token text clip true))
        t-bracket-close (add-action token text clip after-group?)
        t-brace-open (add-params token text clip group? after-group?)
        t-eof clip))))

(defn- add-action-var [token text clip & [group?]]
  (let [ac (get clip (:val token))
        [n-token text] (next-token (str ac " " text))
        group? (or group? (= (:type n-token) t-bracket-open))]
    (add-action n-token #_(assoc n-token :action-var-str (:val token))
                text clip group?)))

(defn as-str [{:keys [div args point] :as clip} & {:keys [exclude-preamble? no-args-diff]}]
  (let [text (StringBuilder.)
        [action-strs col-lens] (loop [actions {} lens {} p 1 last-action 0]
                                 (let [[bar note] (get-pos p div)
                                       has-action? (-> (get-in clip [bar note]) empty? not)
                                       rest-val (- p (max last-action (dec (get-point bar 1 div))) 1)
                                       rest-str (cond
                                                  (and has-action?
                                                       (> rest-val 0)
                                                       (not (= 1 note))) (str ":" rest-val " ")
                                                  (and (= p point)
                                                       (> (dec p) last-action)
                                                       (not (= 1 note))) (str ":" (- p last-action 1) " ")
                                                  :else "")
                                       s (mk-action-str (get-in clip [bar note] []) clip)
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
        ;; pad-str #(str (get action-strs % " ")
        ;;               (apply str (repeat (- (get col-lens
        ;;                                          (second (get-pos % div)) 1)
        ;;                                     (.length (get action-strs % " ")))
        ;;                                    " ")))
        fn-to-str #(str (-> % meta :ns) "/" (-> % meta :name))
        options (into {} (map (fn [[k v]]
                                (when (and (not (number? k))
                                           (not (contains? #{:point :dynamic} k))
                                           (not (= v clojure.core/identity)))
                                  [k (cond
                                       (map? v) (into {} (map (fn [[k v]] [k (if (fn? v) (fn-to-str v) v)]) v))
                                       (fn? v) (fn-to-str v)
                                       true v)]))
                              clip))
        options-str (str (mk-params-str options) "\n\n")]
    (when-not exclude-preamble?
      (.append text options-str))
    (loop [positions {} offset (if exclude-preamble? 0
                                   (.length options-str)) p 1]
      (let [[bar note] (get-pos p div)
            has-action? (-> clip (get-in [bar note]) empty? not)
            s  (get action-strs p " ") ;(pad-str p)
            padded-s (str
                      s " "
                      (if (= div note)
                        "|\n\n"))
            end (+ offset (.length s))
            positions (if has-action?
                        (assoc-in positions [bar note] [offset end])
                        positions)
            offset (+ offset (.length padded-s))]
        (.append text padded-s)
        (if (< p (dec point))
          (recur positions offset (inc p))
          [positions (.toString text)])))))


(defn wrap-eval [ev args]
  (ev (reduce (fn [m [k v]]
                (if (list? v)
                  (assoc m k (clojure.core/eval v))
                  m))
              args args)))

(defn parse-clip [text & [clip]]
  (let [clip (merge {:div 4 :args {} :dynamic #{}}
                    (reduce dissoc clip (filter number? (keys clip))))
        [token text] (next-token text)
        clip (assoc clip :point 1)
        clip (condp = (:type token)
               t-brace-open (let [[options text] (add-map-key {} text)]
                              (parse-clip text (merge clip options)))
               t-brace-close (parse-clip text clip)
               t-bracket-open (add-action token text clip true)
               t-paren-open (add-action token text clip false)
               t-action-var (add-action-var token text clip)
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
        eval-fn (get-fn (or (:eval clip) (with-meta identity {:ns "clojure.core" :name "identity"})))
        eval-fn (with-meta (partial wrap-eval eval-fn) (meta eval-fn))]
                                        ;(assoc clip :eval eval-fn :outs outs)
    clip))

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


(defn build-from-midi [bpm {:keys [div] :as clip}]
  (let [buf (midi/get-quantized-buffer bpm div)
        clip (reduce dissoc clip (filter number? (keys clip)))
        clip (reduce
              (fn [clip [p msgs]]
                (let [actions (vec (filter (comp not empty?)
                                           (map #(interp/interpret-midi clip %) msgs)))]
                  (if (not (empty? actions))
                    (assoc-in clip (get-pos p div) actions)
                    clip)))
              clip buf)
        max-bar (apply max (filter number? (keys clip)))
        max-note (apply max (keys (get clip max-bar)))
        point (get-point max-bar max-note div)]
    (assoc clip :point (inc point))))

(defn shift-left [{:keys [div point] :as clip} at]
  (reduce
   (fn [cl p]
     (let [to (get-pos (dec p) div)
           action (get-in clip (get-pos p div))]
       (if action
         (assoc-in cl to action)
         (update cl (first to) dissoc (second to)))))
   clip (range at (inc point))))

(defn shift-right [{:keys [div point] :as clip} at]
  (reduce
   (fn [cl p]
     (let [from (get-pos p div)
           to (get-pos (inc p) div)
           action (get-in clip from)
           ;; need to bump point if moving action at the end
           cl (if (and (= (inc p) point)
                       action)
                (update cl :point inc)
                cl)]
       (if action
         (-> cl
             (assoc-in to action)
             (update (first from) dissoc (second from)))
         cl)))
   clip (range (dec point) (dec at) -1)))

(defn parse-actions [action-str clip]
  (get-in (parse-clip action-str clip) [1 1]))
