(ns closchema
  (:use closchema
	lazytest.describe))  


(defmacro ctx
  "Use simple value contexts with the :given options."
  [v] `(context [] ~v))
 

(defmacro raised? 
  "Returns true if body raises a condition that matches with (using the keys of) the expected condition in the argument."
  [condition & body]
  `(try ~@body false
        (catch clojure.contrib.condition.Condition e#
          (or (= ~condition :any) 
              (do (assert (map? ~condition))
                  (let [actual# (select-keys (meta e#) (keys ~condition))]
                    (= actual# ~condition)))))))

 

(def base-schema {:type "object"
                  :properties {:id    {:type "number"}
                               :name  {:type "string"}
                               :description {:optional true :type "string"}}})

 
(describe validate "object"
    
  (describe "with properties schema"
    :given [s (ctx base-schema)]
 
    (it "should accept object with only required properties"
      (validate s {:id 1 :name "shoe"})) 

    (it "should accept object with optional properties"
      (validate s {:id 1 :name "shoe" :description "style"}))
    
    (it "should not accept when required property is not present" 
      (not (validate s {:id 1})))
    
    (it "should not accept when property type is incorrect"
      (not (validate s {:id "1" :name "bag"}))))
  
   
  (describe "with additional properties schema"

    (describe "set to false"
      :given [s (ctx (merge base-schema {:additionalProperties false}))]

      (it "should not allow any properties that are not defined in schema"
        (not (validate s {:id 1 :name "wine" :age "13 years"}))))

    (describe "defining a schema"
      :given [s (ctx (merge base-schema {:additionalProperties {:type "string"}}))]

      (it "should enforce that all extra properties conform to the schema"
        (and 
         (validate s {:id 1 :name "wine" :age "13 years" :country "france"})
         (not (validate s {:id 1 :name "wine" :age 13})))))))

 

(describe validate "items"

  (describe "with no items definition"
    (it "should allow any"
      (and 
       (validate {:type "array"} [ 1 2 3])
       (validate {:type "array" :items []} [1 "2" 3]))))
  
  (describe "with object schema" :given [s1 (ctx {:type "array" :items {:type "string"}})
                                         s2 (ctx {:type "array" :items {:type "object" :properties {:name {:type "string"}}}})]     
    (it "should accept empty array"
      (validate s1 []))
     
    (it "should accept homogenous array"
      (and  
       (validate s1 ["a" "b" "c"])
       (validate s2 [{:name "half"} {:name "life"}])))
 
    (it "should not accept heterogenous array"
      (not (validate s1 ["a" "b" 3])))

    (it "should not accept if inner item does not follow item schema" 
      (not (validate s2 [{:name "quake"} {:id 1}]))))


    
  (describe validate "with array of schemas"      
      
    (describe "with single object definitions"
      :given [s (ctx {:type "array" :items [{:type "string"} {:type "string"}]})]
      
      (it "should enforce tuple typing"
        (and  
         (validate s ["a" "b"])
         (not (validate s ["a"]))
         (not (validate s ["a" 1]))))

      (it "should allows additional properties to be defined by default"
        (and 
         (validate s ["a" "b" "c"])
         (validate s ["a" "b" 1 2 3 {}])))

      )

    (describe "with additional properties bounded"
      :given [s (ctx {:type "array" :items [{:type "number"}] :additionalProperties {:type "string"}})]

      (it "should ensure type schema for additional properties"
        (and 
         (validate s [1 "a" "b" "c"])
         (not (validate s [1 2 "a"]))))

      (it "should still enforce tuple typing"
        (not (validate s ["a" 1])))) 

      
    (describe "with aditional properties disabled"
      :given [s (ctx {:type "array" :items [{:type "number"}] :additionalProperties false})] 
      
      (it "should be strict about tuple typing"
        (validate s [1])) 

      (it "should be strict about tuple typing"  
        (not (validate s [1 "a"]))))))
