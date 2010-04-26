(ns com.slyphon.utils.update-bean
  (:use [clojure.contrib
         [except     :only (throw-arg)]
         [str-utils  :only (re-gsub)]
         [java-utils :only (as-str wall-hack-method)]]))


(defn #^String upcase
  "upcases the initial character in s"
  [s]
  (let [string (as-str s)]
    (apply str (Character/toUpperCase (first string)) (rest string))))

(defn #^String camelize [s]
  (re-gsub #"(?:^|_|-)(.)" (fn [[_ ch & rest]] (.toUpperCase ch)) (as-str s)))

#_
(defmacro update-bean
  ([obj props-map]
     (let [&env  clojure.lang.Compiler/LOCAL_ENV
           props (cond (map? props-map)     (seq props-map)
                       (get &env props-map) (seq (&env props-map))
                       :else                (seq (eval props-map)))
           
           ks    (map (comp symbol #(str "set" (-> (as-str %) camelize upcase)) first) props)
           vs    (map second props)
           dots  (map (fn [k v] `(. ~k ~v)) ks vs)]
       `(doto ~obj ~@dots))))

;; largely adopted from the core_proxy.clj "bean" function
(defn update-bean-using-reflection [#^Object obj props-map]
  (let [prop-descr (seq (.. java.beans.Introspector
                            (getBeanInfo (class obj))
                            (getPropertyDescriptors)))

        write-map  (reduce (fn [hsh #^java.beans.PropertyDescriptor pd]
                             (let [name     (. pd (getName))
                                   w-method (. pd (getWriteMethod))]
                               (if w-method
                                 (let [param-types (vec (.getParameterTypes w-method))
                                       w-meth-name (keyword (.getName w-method))]
                                   (assoc hsh (keyword name)
                                          (fn [& args] (apply wall-hack-method (class obj) w-meth-name param-types obj args))))
                                 hsh)))
                           {}
                           prop-descr)]

    ;; (pprint write-map)

    (doseq [[k v] props-map]
      (let [meth-name (-> k as-str (camelize :initial-cap false) keyword)
            _         (printf "k: '%s'\n" k)
            set-f     (get write-map meth-name)] 
        (if set-f
          (apply set-f (if (seq? v) v [v]))
          (throw-arg "no method found for meth-name %s and key %s\n" meth-name k))))))



