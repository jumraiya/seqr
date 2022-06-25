(ns seqr.helper)


(defn get-wrapped-point [point clip-div clip-size player-div]
  (if (> clip-size 0)
      (let [scale (/ player-div clip-div)
            scaled (inc (/ (dec point) scale))
            point (if (> scaled clip-size)
                    (let [r (mod scaled clip-size)]
                      (if (= r 0)
                        clip-size
                        r))
                    scaled)]
        (when (int? point)
          point))))

(defn get-pos [beat div & {:keys [size player-div]}]
  "Given a offset in the sequence and a subdivision of pattern and sequencer, returns the corresponding bar and beat"
  (let [step (if player-div (/ player-div div) div)
        beat (cond (= size 1) 1
                   (and size (> beat size))
                   (if (= (mod beat size) 0)
                     (min size div)
                     (mod beat size))
                   true beat)
        ret-beat (if player-div
                   (or (= 1 beat) (= 0 (mod (dec beat) step)))
                   true)
        bar (cond player-div (inc (int (/ (dec beat) player-div)))
                  (= 0 (mod beat div)) (/ beat div)
                  true (inc (int (/ beat div))))
        n (if player-div
            (inc (int (/ (dec (mod beat player-div)) step)))
            (mod beat div))
        n (if (= 0 n) div n)]
    (if ret-beat
      [(int bar) (int n)]
      [0 0])))

(defn get-point [bar note div]
  "Reverse of get-pos, converts [bar note] into a single number in the sequence"
  (+ (* (dec bar) div) note)
  )


(defn gcd
      [a b]
      (if (zero? b)
      a
      (recur b, (mod a b))))

(defn lcm [a b]
  "Lowest common multiple"
      (/ (* a b) (gcd a b)))
;; to calculate the lcm for a variable number of arguments
(defn lcmv [& v] (reduce lcm v))

(defn calc-period [bpm div]
  "Given a bpm, division and size of a sequencer, calculate period in ms"
  (/ 60000 bpm div))
