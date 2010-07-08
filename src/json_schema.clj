(ns json-schema
  "This is JSON Schema in Clojure. See http://tools.ietf.org/html/draft-zyp-json-schema-02
 Main purposed is to allow object validation, but schema metadata can be used for exposing contracts as well."
  (:use clojure.walk
        clojure.contrib.condition
        (clojure.contrib [ns-utils :only (immigrate)])))


;; (defprotocol Schema
;;   (validate [this data]))
;extend for String, for Map and for Array?



(defmacro check
  "raise, keep context to output various validations"
  [condition & args]
  (let [key (or (first args) :generic)]
    `(or ~condition
         (raise :type ~key))))


(def type-validations
     { "object" #(map? %)
       "array" #(or (vector? %) (list? %))
       "string" #(string? %)
       "number" #(number? %)
       "integer" #(integer? %)
       "boolean" #(instance? Boolean %)       
       "null" #(nil? %)
       "any" (fn [] true)})


(defn validate-type
  "Single type definition or a collection indicating a union of type definitions."
  [schema data]
  (let [type (:type schema)
        type (if (coll? type) type (vector type))]
    (check (reduce #(or %1 %2)
                   (map (fn [t] ((type-validations t) data)) type))
           :invalid-type {:expected "object" :actual (type data)})))



(defmulti validate
  "Dispatch on object type for validation. If not implemented, performs only type validation."
  (fn [schema data] (:type schema)))


(defmethod validate "object" [schema data]
  (do 
    (validate-type schema data)
    
    (for [[property-name property-schema] (:properties schema)]
      (let [property (data property-name)]
        (check (or property
                   (:optional property-schema)))

        (when (:requires property-schema)
          (check (data (keyword (:requires property-schema)))))
                
        (validate property-schema property)))))


(defmethod validate nil [schema data]
  (validate (merge schema {:type "object"}) data)) 


(defmethod validate "array" [schema data])


(defmethod validate :default [schema data]
  (do 
    (validate-type schema data)))

