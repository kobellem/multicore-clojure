(ns flight-reservation
  (:require [clojure.string]
            [clojure.pprint]
            ;[input-simple :as input]
            [input-random :as input]
						[flights :as flights]))

(def logger (agent nil))
(defn log [& msgs] (send logger (fn [_] (apply println msgs))))
;(defn log [& msgs] nil)

(defn- book [flight price seats]
  "Updates `flight` to book `seats` at `price`."
  (update flight :pricing
    (fn [pricing]
      (for [[p a t] pricing]
        (if (= p price)
          [p (- a seats) (+ t seats)]
          [p a t])))))

(defn- process-customer [customer]
  "Try to book a flight from `flights` for `customer`, returning the updated
  flight if found, or nil if no suitable flight was found."
  (if-let [{:keys [flight price]} (flights/find-flight customer)]
    (do
			(log "Customer" (:id customer) "booked" (:seats customer)
        "seats on flight" (flight :id) "at $" price " (< budget of $"
        (:budget customer) ").")
      flight)
    (do
      (log "Customer" (:id customer) "did not find a flight.")
      nil)))

(def finished-processing?
  "Set to true once all customers have been processed, so that sales process
  can end."
  (atom false))

(defn process-customers [customers]
  (doall (pmap process-customer customers))
  (reset! finished-processing? true))

(defn sales-process []
  "The sales process starts and ends sales periods, until `finished-processing?`
  is true."
  (loop []
    (let [discounted-carrier (rand-nth input/carriers)]
      (Thread/sleep input/TIME_BETWEEN_SALES)
      (flights/start-sale discounted-carrier log)
      (Thread/sleep input/TIME_OF_SALES)
      (flights/end-sale discounted-carrier log))
    (if (not @finished-processing?)
      (recur))))

(defn main []
;  (flights/initialize-flights input/flights)
  (let [f1 (future (time (process-customers input/customers)))
        f2 (future (sales-process))]
    @f1
    @f2)
  (log "Flights:")
  (flights/print-flights log))

(main)
(shutdown-agents)
