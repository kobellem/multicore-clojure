(ns flights
	(:require
		[clojure.string]
		[input-random :as input]))

(def flights input/flights)

(defn print-flights [log]
	(log flights))
	
(defn filter-flights-by-carrier [carrier]
	(filter (fn [flight]
		((flight :carrier) == carrier)
		flights)))

(defn filter-flights-by-route [from to]
	(filter (fn [flight]
		(and (= (flight :from) from) (= (flight :to)  to)))
		flights))

(defn start-sale [carrier log]
	(let [affected-flights (filter-flights-by-carrier carrier)]
		(dosync (for [flight affected-flights]
			(ref-set flight :pricing (* 0.8)))))
	(log "Sale started for " carrier))

(defn end-sale [carrier log]
	(let [affected-flights (filter-flights-by-carrier carrier)]
		(dosync (for [flight affected-flights]
			(ref-set flight :pricing (* 1.25)))))
	(log "Sale ended for " carrier))

(defn sort-pricing [pricing]
  (sort-by first pricing))

(defn filter-pricing-with-n-seats [pricing seats]
  (filter #(>= (second %) seats) pricing))

(defn lowest-available-price [flight seats]
	(-> (flight :pricing)
		(filter-pricing-with-n-seats seats)
		(sort-pricing)
		(first)
		(first)))

(defn update-flight [flight price seats]
	(ref-set flight
		{:id (flight :id)
		:from (flight :from) :to (flight :to)
		:carrier (flight :carrier)
		:pricing ((fn [pricing]
			(for [[p a t] pricing]
				(if (= p price)
					[p (- a seats) (+ t seats)]
					[p a t])))
			(flight :pricing))}))

(defn find-flight [customer]
	(let [{:keys [_id from to seats budget]} customer
	     flights-on-route (filter-flights-by-route from to)]
		(dosync 
			(let 
				[flights-and-prices 
					(for [flight flights-on-route
							:let [lowest-price (lowest-available-price flight seats)]
							:when (and (some? lowest-price) (<= lowest-price budget))]
						{:flight flight :price lowest-price})
				cheapest-flight-and-price	(first (sort-by :price flights-and-prices))]
			(if (some? cheapest-flight-and-price)
				(update-flight (cheapest-flight-and-price :flight) (cheapest-flight-and-price :price) seats))
			cheapest-flight-and-price))))
