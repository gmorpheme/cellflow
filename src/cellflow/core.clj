(ns cellflow.core
  (:use [clojure.set :only [difference union]])
  (:use [clojure.walk :only [postwalk]])
  (:use [clj-time.core :exclude 'extend])
  (:require [clj-time.format :as fmt]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv])
  (:import [org.apache.commons.math.analysis UnivariateRealFunction]
           [org.apache.commons.math.analysis.solvers UnivariateRealSolverFactory]))

;; -- lazy-cell
(defmacro cell
  ([expr]
     `(atom (fn [] ~expr)))
  ([]
     `(cell '_)))

(defn cv [cell]
  (if (instance? clojure.lang.IDeref cell) (@cell) cell))

(defn alter-cell [cell v]
  (swap! cell (fn [_] (constantly v))))

(defn cv* [eseq]
  (postwalk cv eseq))

(defn events [& eseq]
  (sort-by :date (apply concat eseq)))

;; event creation

(defn once [date amount]
  [{:date date :amount amount}])

(defn event-seq [name seq]
  (map #(assoc % :name name) seq))

(defmacro cashflow [& seqs]
  `(events
    ~@(for [s seqs]
        `(event-seq ~(keyword (first s)) ~@(rest s)))))

;; cashflow event profiles

(defn n-monthly [n start]
  (map #(plus start (months (* n %))) (range)))

(defn monthly [start]
  (n-monthly 1 start))

(defn quarterly [start]
  (n-monthly 3 start))

(defn yearly [start]
  (n-monthly 12 start))

(defn n-weekly [n start]
  (iterate #(plus % (weeks n)) start))

(defn weekly [start]
  (n-weekly 1 start))

(defn month-end [dt]
  (.getMaximumValue (.dayOfMonth dt)))

(defn at-month-end
  "Return the date with is at the end of the month containing dt"
  [dt]
  (= (day dt) (month-end dt)))

(defn to-month-start [dt]
  (.withDayOfMonth dt 1))

(defn to-month-end [dt]
  (.withDayOfMonth dt (month-end dt)))

(defn n-monthly-at-end [n start]
  (map to-month-end (n-monthly n start)))

(defn monthly-at-end [start]
  (n-monthly-at-end 1 start))

(defn quarterly-at-end [start]
  (n-monthly-at-end 3 start))

;; intervals for date sequence
(defn to-intervals [dates]
  (map #(apply interval %) (partition 2 1 (cons (date-time 0) dates))))

;; spreads

(defn indexed [s]
  (map vector (drop 1 (range)) s))

(defn each [amount dateseq]
  (for [date dateseq]
    {:amount amount
     :date date}))

(defn straight-line [total dateseq]
  (let [amount (/ total (count dateseq))]
    (for [date dateseq]
      {:amount amount
       :date date})))

(defn triangle [n]
  (/ (* n (inc n)) 2))

(defn rule-of-78 [total dateseq]
  (let [denom (triangle (count dateseq))]
    (for [[i date] (indexed dateseq)]
      {:amount (* total (/ i denom))
       :date date})))

(defn spread [value spread-fn dateseq]
  (spread-fn value dateseq))

;; capitalisation event constructors

(defn capitalise [rates dates]
  (map (fn [d] {:date d :capitalise rates}) dates))

;; any place where a value is specified, a cell may be
;; used instead
;;
;; basic cashflow events have date and value:
;; {:date (date 2011 11 11) :amount 123.35}
;;
;; balance "events" may be inserted with running totals:
;; {:date (date 2012 05 04) :balance 12421.22}
;;
;; they may also carry uncrystallised interest amounts in :carried
;; 
;; rate sequences are sequences of dated rates
;; [{:date (date 0) :rate 5.0} {:date (date 2010 10 12) :rate 5.2}]
;; the rates fn allows easy construction:
;; (rates 5.0 (date 2010 10 12) 5.2)
;;
;; a period function takes a joda interval and turns it into a
;; fraction of whatever time period the rate is expressed in.
;;
;; an interest spec is a sequence of a name, rate seq and period
;; function

(defn prepare-bals
  "Prepare balances for adding to an event sequence ready for pivoting into a report."
  [bseq]
  (let [compact (fn [bs] (map last (partition-by :date bs)))
        denormalise (fn [bs] (apply concat (for [{:keys [date carried capitalised] :as b} bs]
                                             (if carried
                                               (concat [(dissoc b :carried :capitalised)]
                                                       (map #(assoc % :date date :name (keyword (str (name (:name %)) "-carried"))) carried)
                                                       (map #(assoc % :date date :name (keyword (str (name (:name %)) "-capitalised"))) capitalised))
                                               [b]))))]
    (-> bseq compact denormalise)))

(defn pivot [eseq]
  (let [scrunch (fn [[date es]] (reduce #(merge-with + %1 {(:name %2) (:amount %2)}) {:date date} es))]
    (drop-while #(= (:date %) (date-time 0)) (sort-by :date (map scrunch (group-by :date eseq))))))

(defn dump-report
  ([report file]
     (dump-report report (vec (reduce #(union %1 (set (keys %2))) #{} report)) file))
  ([report cols file]
     (let [date-formatter (fmt/formatter "dd/MM/yyyy")
           data (map #(update-in % [:date] (partial fmt/unparse date-formatter)) report)]
       (with-open [out-file (io/writer file)]
         (csv/write-csv out-file (cons cols (for [record data] (map record cols))))))))

;; -- present value calcs

;; These functions represent a time period as a fraction of a year
;; according to a variety of strategies.

(defn annualise-actual-over-365 [i]
  (/ (in-days i) 365))

(defn annualise-interval-monthly [i]
  (/ (in-months i) 12))

(defn annualise-interval-yearly [i]
  (in-years i))

(defn annualise-interval-apr 
  "Crude approximation of UK APR interval calc."
  [i]
  (let [s (start i)
        e (end i)
        months-line-up (or (= (day s) (day e))
                           (and (at-month-end s) (at-month-end e))
                           (and (at-month-end e) (>= (day s) (day e))))
        years-line-up (and months-line-up (= (month s) (month e)))]
    (if months-line-up
      (if years-line-up
        (in-years i)
        (/ (in-months i) 12))
      (/ (in-days i) (+ 365 (/ 1 4))))))

;; Functions for dealing with rates and rate periods

(defn rates 
  "Allows simply specification of set of rate periods.
   Pass inital rate followed by alternating date and rate to specify dated rate changes.
   Returns ordered seq of :date & :rate maps."
  [rate & dated-rates]
  (cons {:date (date-time 0) :rate rate}
        (for [[d r] (partition 2 dated-rates)]
          {:date d :rate r})))

(defn rate-slice 
  "Turns an unbounded seq of :date / :rate maps into a ordered seq
   with an initial entry for start date and a final entry for end date."
  [start end rates]
  (let [[pre rates] (split-with #(not (after? (:date %) start)) rates)
        rates (take-while #(before? (:date %) end) rates)
        start-rate (:rate (last pre))
        end-rate (or (:rate (last rates)) start-rate)]
    (concat [{:date start :rate start-rate}] rates [{:date end :rate end-rate}])))

(defn rate-at [date rates]
  (:rate (last (take-while #(before? (:date %) date) rates))))

(defn pv
  "Determine the present value of a cashflow event at a given date in the
   context of rate sequence and annualisation function
   at - basis date
   event - map of :date and :amount
   rates - seq of {:date :rate} maps
   period-fn - period resolution function, clj-time's in-months or in-years "
  ([at event rates]
     (pv at event rates annualise-interval-monthly))
  ([at {:keys [date amount] :as event} rates period-fn]
     (let [rate-vals (rate-slice at date rates)
           multiplier (reduce * 1 (for [[a b] (partition 2 1 rate-vals)]
                                    (Math/pow (inc (:rate a)) (- (period-fn (interval (:date a) (:date b)))))))]
       (* amount multiplier))))

(defn interest
  "Determine an interest amount based on dated balance due at a given date, based on rate sequence

  balance - balance event
  at - end of period at balance
  rates - rate sequence
  period-fn - fn to turn interval into fraction of a year"
  [{:keys [date amount] :as balance} at rates period-fn]
  (let [rate-vals (rate-slice date at rates)
        multiplier (reduce + (for [[a b] (partition 2 1 rate-vals)]
                               (* (:rate a) (period-fn (interval (:date a) (:date b))))))]
    (* (:amount balance) multiplier)))

(defn starting-balance
  "Create a seed starting balance that can be progressed forwards using interest specs and events."
  ([name interest-specs]
     {:name name
      :date (date-time 0)
      :amount 0
      :carried (for [[name rates annualise] interest-specs]
                 {:name name
                  :amount 0})})
  ([interest-specs]
     (starting-balance :balance interest-specs)))

(defn progress-balance
  "From previous balance, process a dated event to calculate interest and deliver new balance,
   (capitalising if the event say so)."
  [prior {:keys [date amount capitalise]} interest-specs]
  {:pre [(not (nil? date))
         (contains? prior :date)
         (and (contains? prior :amount) (number? (:amount prior)))
         (contains? prior :carried)
         (every? #(= (count %) 3) interest-specs)]}
  (let [icalc (fn [[name rates period-fn]] {:name name
                                            :amount (interest prior date rates period-fn)})
        capitalise? (fn [isum] (and capitalise (capitalise (:name isum))))
        icalcs (map icalc interest-specs)
        isums (for [[i p] (map vector icalcs (:carried prior))]
                (assoc i :amount (+ (:amount i) (:amount p))))
        carried (map #(if (capitalise? %) (assoc % :amount 0) %) isums)
        capitalised (filter capitalise? isums)
        bal (apply +
                   (:amount prior)
                   (or amount 0)
                   (map :amount capitalised))]
    (merge prior {:date date
                  :amount bal
                  :carried carried
                  :capitalised capitalised})))

(defn find-balances
  "Calculate balance events for the supplied events and interest specifications."
  ([eseq ispecs]
     (find-balances :balance eseq ispecs))
  ([name eseq ispecs]
     (let [progress #(progress-balance %1 %2 ispecs)]
       (reductions progress (starting-balance name ispecs) eseq))))

;; -- solver
(defn solve [vary-cell target-cell range-start range-end]
  (let [function (reify UnivariateRealFunction
                   (value [self x] (do
                                     (alter-cell vary-cell x)
                                     (cv target-cell))))]
    (do
      (-> (UnivariateRealSolverFactory/newInstance)
          (.newBrentSolver)
          (.solve function (double range-start) (double range-end)))
      (cv vary-cell))))
