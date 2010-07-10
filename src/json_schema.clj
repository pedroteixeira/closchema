(ns closchema
  "This is JSON Schema in Clojure. See http://tools.ietf.org/html/draft-zyp-json-schema-02
 Main purposed is to allow object validation, but schema metadata can be used for exposing contracts as well."
  (:use clojure.walk
        clojure.template
        clojure.contrib.condition
        (clojure.contrib [ns-utils :only (immigrate)]))
  (:require [clojure.set :as set]))



(def ^{:doc "Allow validation errors to be captured."}
     *validation-context* nil)

(def ^{:doc "When walking an object, we keep a binding to current parent."}
     *parent* nil)


(defn process-errors
  "Default processing just outputs a boolean return."
  [errors]  
  (= (count errors) 0))

(defmacro with-validation-context  
  "Defines a binding to allow access to the root object and to enable invalidations to be captured. This strategy removes the need of raising exceptions at every single invalid point, and allows context information to be used when reporting about errors. Nested contexts are just ignored."
  [& body]  
  `(let [body# #(do ~@body
                    (process-errors @(:errors *validation-context*)))]
     (if-not *validation-context*  
       (binding [*validation-context* {:errors (ref '())
                                   :path (ref [])}]                  
         (body#))
       (body#))))


(defmacro walk-in
  "Step inside a relative path, from a previous object. This information is useful for reporting."
  [parent rel-path & body]
  `(binding [*parent* ~parent]
     (if-let [{path# :path} *validation-context*]
       (do 
         (dosync alter path# conj ~rel-path)
         ~@body
         (dosync alter path# pop))
       (do ~@body))))


(defmacro invalid
  "Register an invalid piece of data according to schema."
  [& args]
  (let [[path args] (if (keyword? (first args)) [nil args] [(first args) (rest args)])
        key (first args)
        data (second args)] 
    `(let [error# {:ref ~path :key ~key :data ~data}]
       (if-let [{errors# :errors path# :path} *validation-context*]
         (dosync (alter errors# conj
                        (merge {:path (conj @path# ~path)} error#))))
       (process-errors (list error#)))))


(defmacro report
  "Returns all errors, instead of simple boolean."
  [& args]
  `(binding [process-errors (fn [errors#] errors#)]
     (with-validation-context    
       (do ~@args))))


(defmulti validate*
  "Dispatch on object type for validation. If not implemented, performs only basic type validation."
  (fn [schema instance] (:type schema)))


(defn validate [schema instance]
  (with-validation-context
    (validate* schema instance)))



(def default-type "object")


(defmethod validate* nil [schema instance]
  (validate (merge schema {:type default-type}) instance))


(def ^{:doc "Known basic types."}
     basic-type-validations
     { "object" #(map? %)
       "array" #(or (vector? %) (list? %))
       "string" #(string? %)
       "number" #(number? %)
       "integer" #(integer? %)
       "boolean" #(instance? Boolean %)       
       "null" #(nil? %)
       "any" (fn [] true)})


(defn check-basic-type
  "Validate basic type definition for known types."
  [{t :type :as schema} instance]
  (let [t (or t default-type)
        types (if (coll? t) t (vector t))]
    
    (or (reduce #(or %1 %2)
                (map (fn [t] ((basic-type-validations t) instance)) types))
        (invalid :type {:expected types :actual (type instance)}))))


(defn common-validate [schema instance]
  (check-basic-type schema instance)

  (comment TODO
           disallow
           extends)
  )


(defmethod validate* :default [schema instance]
  (common-validate schema instance))


(defmethod validate* "object"
  [{properties-schema :properties
    additional-schema :additionalProperties
    :as schema} instance]

  (common-validate schema instance)

  #_ "validate properties defined in schema"
  (doseq [[property-name
           {optional :optional :as property-schema}] properties-schema]    
    (let [property (instance property-name)]
      (when-not (or property optional)
        (invalid property-name :required)))) 
  

  #_ "validate instance properties (using invidivual or addicional schema)"
  (doseq [[property-name property] instance]
    (if-let [{requires :requires :as property-schema}
             (or (and (map? properties-schema) (properties-schema property-name))
                 (and (map? additional-schema) additional-schema))]
      (do        
        (when (and requires property
                   (not (instance (keyword requires))))
          (invalid requires :required {:required-by property-name}))
      
        (walk-in instance property-name
                 (validate property-schema property)))))

    
  #_ "check additional properties"
  (when (false? additional-schema)
    (if-let [additionals (set/difference (set (keys instance))
                                      (set (keys properties-schema)))]
      (when (> (count additionals) 0)
        (invalid :addicional-properties-not-allowed {:properties additionals}))))) 




(defmethod validate* "array"
  [{items-schema :items
    unique? :uniqueItems :as schema} instance]
  
  (common-validate schema instance)

  #_ "specific array validation"
  (let [total (count instance)]
    (do-template
     [key op]     
     (if-let [expected (key schema)]
       (when (and (op total expected))
         (invalid key {:expected expected :actual total}))
       
       :minItems <
       :maxItems >)))

  (if-let [unique? (:uniqueItems schema)]
    (reduce (fn [l r] (when-not (= l r)
                        (invalid :uniqueItems {:l l :r r}))) instance))
  
  
  #_ "treat array as object for further common validation"
  (when items-schema
    (let [obj-array (zipmap (range (count instance)) instance)
          obj-schema (cond (and (map? items-schema)(:type items-schema))
                           {:type "object"
                            :additionalProperties items-schema}
                           
                           (coll? items-schema)
                           (merge schema {:type "object"
                                          :properties (zipmap (range (count items-schema)) items-schema)}))]
      (validate obj-schema obj-array))))




(defmethod validate* "string"
  [schema instance]
  (common-validate schema instance)
;minLength
;maxLength
;pattern  
  )


(defmethod validate* "number"
  [schema instance]
  (common-validate schema instance)
  (comment
    minimum
    maximum
    miniumCanEqual
    maximumCanEqual
    divisibleBy
    )
  )



