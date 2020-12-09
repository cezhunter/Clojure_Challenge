(ns pov)

(defn f1 [a b]
	(if (empty? (rest a))
		[]
		(first (keep
		#(if (some (fn [x] (= x (first %))) b) %)
		(rest a)))))

(defn f2 [a b]
	(into [] (cons (first a) (keep
		#(if (not (some (fn [x] (= x (first %))) b)) %)
		(rest a)))))

(defn path
  	([t tree] (path t tree []))
  	([t tree res]
  		(cond
  			(= t (first tree))
  			(conj res (first tree))
  			(> (count tree) 1)
  			(loop [[child & children] (rest tree) s []]
  				(if	(or (not (empty? s)) (empty? child))
  					s
  					(recur children (path t child (conj res (first tree)))))))))

(defn path-from-to [a b tree]
  (let [path_to_a (path a tree) path_to_b (path b tree)]
  	(if (or (nil? path_to_a) (nil? path_to_b))
  		nil
  		(distinct (concat (reverse path_to_a) path_to_b)))))

(defn of
	([x a]
		(if (or (nil? (path x a)) (empty? a)) 
				nil
				(of x (f1 a (path x a)) (f2 a (path x a)))))
	([x a r]
		(cond
			(= (first a) x)
				(conj a r)
			(empty? a)
				r
			:else
				(of x (f1 a (path x a)) (conj (f2 a (path x a)) r)))))
