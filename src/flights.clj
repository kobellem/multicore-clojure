(ns flights)

(def flights (list))

(defn initialize-flights [initial-flights]
	(reset! flights (list initial-flights)))

(defn print-flights []
	(for [flight flights] (print flight)))
	
(defn filter-flights-by-carrier [carrier]
	(filter (fn [flight]
		((flight :carrier) == carrier)
		flights)))

(defn filter-flights-by-route [from to]
	(filter (fn [flight]
		(and ((flight :from) == from) ((flight :to) == to)))
		flights))

(defn start-sale [carrier]
	(let [affected-flights (filter-flights-by-carrier)]
		(dosync (for [flight affected-flights]
			(ref-set flight :pricing (* 0.8))))))

(defn end-sale [carrier]
	(let [affected-flights (filter-flights-by-carrier)]
		(dosync (for [flight affected-flights]
			(ref-set flight :pricing (* 1.25))))))

(defn sort-pricing [pricing]
  (sort-by first pricing))

(defn filter-pricing-with-n-seats [pricing seats]
  (filter #(>= (second %) seats) pricing))

(defn lowest-available-price [flight seats]
	(-> (:pricing flight)
		(filter-pricing-with-n-seats seats)
		(sort-pricing)
		(first)
		(first)))

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
			cheapest-flight-and-price))))
