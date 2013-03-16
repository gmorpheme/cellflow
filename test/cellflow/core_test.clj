(ns cellflow.core-test
  (:use [cellflow.core])
  (:use [clojure.test])
  (:use [midje.sweet])
  (:require [clojure.set :as set])
  (:require [clojure.pprint :as pr])
  (:use [clj-time.core :only [date-time in-years interval before? after? plus minus months]]))

;; test support

(defn compound-to-yearly [r]
  (dec (Math/pow (inc (/ r 12)) 12)))

(defn near 
  "Check two floating point numbers are near enough."
  [x y]
  (<= (Math/abs (- x y)) 0.001))

;; test date patterns

(facts "Just check spans"
  (let [dates (take 12 (quarterly (date-time 2012 04 23)))]
    ((juxt first last) dates) => [(date-time 2012 04 23) (date-time 2015 01 23)])
  (let [dates (take 36 (monthly (date-time 2012 04 23)))]
    ((juxt first last) dates) => [(date-time 2012 04 23) (date-time 2015 03 23)])
  (let [dates (take 156 (weekly (date-time 2012 04 23)))]
    ((juxt first last) dates) => [(date-time 2012 04 23) (date-time 2015 04 13)]))

;; test event seq creators

(fact
  (let [dates (take 156 (weekly (date-time 2012 04 23)))
        events (spread 100 each dates)]
    (for [e events]
      (:amount e) => 256)))

;; test conversion of intervals into period-fractions

(fact
  (let [yeardays (+ 365 (/ 1 4))
        frac (fn [y1 m1 d1 y2 m2 d2]
               (annualise-interval-apr (interval (date-time y1 m1 d1) (date-time y2 m2 d2))))]
    (frac 2013 01 15 2013 01 20) => (/ 5 yeardays)
    (frac 2013 01 15 2013 02 10) => (/ 26 yeardays)
    (frac 2013 01 15 2013 02 16) => (/ 32 yeardays)
    (frac 2013 01 31 2013 02 28) => (/ 1 12)
    (frac 2013 01 31 2013 03 31) => (/ 2 12)
    (frac 2013 01 31 2013 04 30) => (/ 3 12)
    (frac 2013 01 31 2013 05 01) => (/ 90 yeardays)
    (frac 2013 02 28 2013 03 31) => (/ 1 12)
    (frac 2013 04 30 2013 05 31) => (/ 1 12)
    (frac 2012 01 31 2012 02 28) => (/ 28 yeardays)
    (frac 2012 01 31 2012 02 29) => (/ 1 12)))

(fact
  (let [frac (fn [y1 m1 d1 y2 m2 d2]
               (annualise-interval-monthly (interval (date-time y1 m1 d1) (date-time y2 m2 d2))))]
    (frac 2001 01 15 2001 01 20) => 0
    (frac 2001 01 15 2001 02 10) => 0
    (frac 2001 01 15 2001 02 16) => (/ 1 12)
    (frac 2001 01 31 2001 02 28) => (/ 1 12)
    (frac 2001 01 31 2001 03 01) => (/ 1 12)
    (frac 2001 01 31 2001 03 31) => (/ 2 12)
    (frac 2001 01 31 2001 04 30) => (/ 3 12)
    (frac 2001 01 31 2001 05 01) => (/ 3 12)
    (frac 2001 02 28 2001 03 31) => (/ 1 12)
    (frac 2001 04 30 2001 05 31) => (/ 1 12)
    (frac 2004 01 31 2004 02 28) => 0
    (frac 2004 01 31 2004 02 29) => (/ 1 12))  )

;; tests rates helpers

(facts
  (rates 0.1) => [{:date (date-time 0) :rate 0.1}]
  (rates 0.1 (date-time 2000 1 1) 0.2) => [{:date (date-time 0) :rate 0.1}
                                           {:date (date-time 2000 1 1) :rate 0.2}]
  (rates 0.1 (date-time 2013 4 23) 0.2 (date-time 2023 4 23) 0.5) => [{:date (date-time 0) :rate 0.1}
                                                                      {:date (date-time 2013 4 23) :rate 0.2}
                                                                      {:date (date-time 2023 4 23) :rate 0.5}])

(facts
  (rate-slice (date-time 0) (date-time 2012 1) (rates 0.032))
  =>
  [{:date (date-time 0) :rate 0.032}
   {:date (date-time 2012 1) :rate 0.032}]
  
  (rate-slice (date-time 2011) (date-time 2012) (rates 0.02))
  =>
  [{:date (date-time 2011) :rate 0.02}
   {:date (date-time 2012) :rate 0.02}])

;; present value calc

(facts
  (let [base (date-time 2011 1 1)
        t1 (date-time 2012 1 1)
        t2 (date-time 2013 1 1)]
    ; 10% for a year - rates yearly
    (pv base {:date t1 :amount 110} (rates 0.1) in-years) => (roughly 100)
    ; 20% for a year, 10% for a year - rates yearly
    (pv base {:date t2 :amount 132} (rates 0.2 t1 0.1) in-years) => (roughly 100)))

;; balance progression for principal outstanding based solves

(defn balance-equal [a b]
  (and (= (:date a) (:date b))
       (near (:amount a) (:amount b))
       (every? (fn [[l r]] (and (= (:name l) (:name r))
                               (near (:amount l) (:amount r))))
               (map vector (:carried a) (:carried b)))))

(fact "Progress interest to balance event, carrying or capitalising as necessary"
  (let [ispecs [[:funding (rates 0.02) annualise-interval-monthly]
                [:profit (rates 0.035) annualise-interval-monthly]]

        sb (starting-balance ispecs)
        fee {:name :fee
             :amount -320
             :date (date-time 2012 1)}
        bal1 (progress-balance sb fee ispecs)
        
        principal {:name :principal
                   :amount -1200
                   :date (date-time 2012 2)}
        fi1 (* (/ 1 12) 0.02 -320)
        profit1 (* (/ 1 12) 0.035 -320)
        bal2 (progress-balance bal1 principal ispecs)

        instalment {:name :instalment
                    :amount 200
                    :date (date-time 2012 3)
                    :capitalise #{:profit :funding}}
        fi2 (* (/ 1 12) 0.02 -1520)
        profit2 (* (/ 1 12) 0.035 -1520)
        bal3 (progress-balance bal2 instalment ispecs)]
    
    (balance-equal bal1 {:date (date-time 2012 1)
                         :amount -320
                         :carried [{:name :funding :amount 0}
                                   {:name :profit :amount 0}]}) => true
                                   
    (balance-equal bal2 {:date (date-time 2012 2)
                         :amount -1520
                         :carried [{:name :funding :amount fi1}
                                   {:name :profit :amount profit1}]}) => true
                                                                  
    (balance-equal bal3 {:date (date-time 2012 3)
                         :amount (+ -1520 fi1 profit1 fi2 profit2 200)
                         :carried [{:name :funding :amount 0}
                                   {:name :profit :amount 0}]})))

;; present value solves

(fact "NPV solve"
  (let [rental (cell)
        start (date-time 2013 1 1)
        events (cashflow
                (principal (once start -10000))
                (instalment (spread rental each (take 12 (monthly (date-time 2013 01 28))))))
        rate (rates 0.51162)
        pv (cell (apply + (for [e events] (pv start (cv* e) rate))))]
    (solve rental pv 0 10000) => (roughly 1000)))

(fact "NPV solve - 3 & 33"
  (let [start (date-time 2013 1 1)
        standard-rental (cell)
        rate (rates (compound-to-yearly (/ 1 10)))
        events (cashflow
                (principal (once start -10000))
                (initial (once (date-time 2013 1 28) (cell (* 3 (cv standard-rental)))))
                (instalment (spread standard-rental each (take 33 (monthly (date-time 2013 2 28))))))
        pv (cell (apply + (for [e events] (pv start (cv* e) rate))))]
    (solve standard-rental pv 0 100000) => (roughly 314.9862)))

(fact "Simple bond pricing"
  ; example from http://users.wfu.edu/palmitar/Law&Valuation/chapter%204/4-2-2.htm
  (let [start (date-time 2000)
        maturity (date-time 2010)
        prevailing-rate (rates 0.08)
        events (cashflow
                (interest (spread 86 each (drop 1 (take 11 (yearly start)))))
                (return (once maturity 1000)))]
    (reduce +
            (for [e events]
              (pv start e prevailing-rate annualise-interval-yearly)))
    => (roughly  1040.26)))

(fact "IRR calc"
  ; example from http://en.wikipedia.org/wiki/Internal_rate_of_return
  (let [events (cashflow
                (y0 (once (date-time 2001) -4000))
                (y1 (once (date-time 2002) 1200))
                (y2 (once (date-time 2003) 1410))
                (y3 (once (date-time 2004) 1875))
                (y4 (once (date-time 2005) 1050)))
        rate (cell 0)
        pv (cell
            (apply +
                   (for [e events]
                     (pv (date-time 2001) e (rates (cv rate))))))]
    (solve rate pv 0 1) => (roughly 0.143)))

(fact "NPV Solve - quarterly w/ multiple rates"
  ; https://docs.google.com/spreadsheet/pub?key=0Agz9i5hxEc76dGxHdFFlOXlYNlo1NnVNOWl5SnJWdXc&output=html
  (let [start (date-time 2010 1 1)
        rental (cell)
        rate (rates (compound-to-yearly 0.05)
                    (date-time 2010 12 1) (compound-to-yearly 0.03))
        [before-dates after-dates] (split-with #(before? % (date-time 2010 12 1))
                                               (take 7 (quarterly (date-time 2010 4 1))))
        events (cashflow
                (drawdown (once start -100000))
                (fixed-rentals (spread 12000 each before-dates))
                (var-rentals (spread rental each after-dates)))
        pv (cell (apply + (for [e events] (pv start (cv* e) rate))))]
    (solve rental pv 0 100000) => (roughly 17214.6549)))

(fact "APR Calc - simple 12 month"
  (let [start (date-time 2007 1 21)
        rate (cell)
        events (cashflow
                (initial (once start -10000))
                (instalments (spread 1000 each (take 12 (monthly (plus start (months 1)))))))
        pv (cell (apply + (for [e events] (pv start e (cv* (rates rate)) annualise-interval-apr))))]
    (solve rate pv 0 1) => (roughly  0.413)))

;; capital outstanding based lease-type solves

(fact
  (interest {:date (date-time 2001 1 1) :amount 100}
            (date-time 2002 1 1)
            (rates 0.15)
            annualise-interval-monthly) => (roughly 15))

(fact "Actual/365 based profit rate"
  ; https://docs.google.com/spreadsheet/pub?key=0Agz9i5hxEc76dGxHdFFlOXlYNlo1NnVNOWl5SnJWdXc&output=html
  (let [rate (rates 0.035)
        rental (cell) ; solve for rental
        start (date-time 2013 4 12)
        rental-dates (take 35 (monthly (date-time 2013 5 12)))
        eseq (cashflow
              (drawdown (once start -20000))
              (rentals (spread rental each rental-dates))
              (interest (capitalise #{:profit} rental-dates)))
        ispecs [[:profit rate annualise-actual-over-365]]
        balances (cell (find-balances (cv* eseq) ispecs))
        balance (cell (:amount (last (cv balances))))]
    (solve rental balance 0 10000) => (roughly 601.95)
    ;(dump-report (pivot (events (cv* eseq) (prepare-bals (cv ;balances)))) "test.csv")
    ))