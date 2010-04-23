(ns com.slyphon.utils.update-bean
  (:use [clojure.contrib
         [str-utils :only (re-gsub)]
         [java-utils :only (as-str)]]))


(defn #^String upcase
  "upcases the initial character in s"
  [s]
  (let [string (as-str s)]
    (apply str (Character/toUpperCase (first string)) (rest string))))

(defn #^String camelize [s]
  (re-gsub #"(?:^|_|-)(.)" (fn [[_ ch & rest]] (.toUpperCase ch)) (as-str s)))

(defmacro update-bean
  ([obj props-map]
     (let [props (cond (map? props-map)     (seq props-map)
                       (get &env props-map) (seq (&env props-map))
                       :else                (seq (eval props-map)))
           
           ks    (map (comp symbol #(str "set" (-> (as-str %) camelize upcase)) first) props)
           vs    (map second props)
           dots  (map (fn [k v] `(. ~k ~v)) ks vs)]
       `(doto ~obj ~@dots))))

