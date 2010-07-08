(ns test-json-schema
  (:use json-schema
	lazytest.describe))
 

(defmacro raised?
  "Returns true if body raises a condition that matches with (using the keys of) the expected condition in the argument."
  [condition & body]
  `(try ~@body false
        (catch clojure.contrib.condition.Condition e#
          (let [actual# (select-keys (meta e#) (keys ~condition))]
            (= actual# ~condition)))))


(describe validate-type
  (it "should validate based on a single type"
    (validate-type {:type "object"} {}))

  (it "should perform union validation when multiple types are defined"
    (validate-type {:type ["object" "array"]} [])
    (validate-type {:type ["object" "array"]} {})) 

  (it "should raise condition if object does not match schema"
    (raised? {:type :invalid-type} (validate-type {:type "object"} [])))) 



(def product-schema
     {:name "Product"
      :properties {:id    {:type "number" :description "Product identifier"}
                   :name  {:description "Name of the product" :type "string"}
                   :price {:type "number" :minimum 0}
                   :tags  {:optional true :type "array" :items {:type "string"}}}})

(def p1 {:id 1 :name "Shoe" :price 400})

(describe validate "with correct simple object"
  (it "should return true"
    (validate product-schema p1)))


(describe validate "array"
  (it ""))



