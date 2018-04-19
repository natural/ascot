(ns natural.ascot.json-schema
  (:import [java.sql Types])
  (:require [clojure.string :refer [join split]]))


(defn yes? [v]
  (= :yes (-> (or v "") (.toLowerCase) keyword)))


(def date?
  (partial = "date"))

(def timestamp?
  (fn [a] (boolean (some #{a} #{"timestamptz" "timestamp"}))))

(defn stringish? [v]
  (get #{Types/CHAR Types/VARCHAR} v))

(defn extract-string-default [v]
  (join (->> (first (split v #"::")) (drop 1) (drop-last 1))))


(def prop-fns  [ 
   (fn [{:keys [nullable]}]
     {:json-schema/x-nullable (= nullable 1)})

   (fn [{:keys [type_name]}]
     {:json-schema/x-type-name type_name})

   (fn [{:keys [column_def is_autoincrement]}]
     (if-let [default (and (not (yes? is_autoincrement)) column_def)]
       {:json-schema/default (str (read-string (extract-string-default default)))}))

   (fn [{:keys [ordinal_position]}]
     {:json-schema/x-ordinal-position ordinal_position})

   (fn [{:keys [is_autoincrement]}]
     (if (yes? is_autoincrement)
       {:json-schema/x-auto-increment true}))

   (fn [{:keys [type_name]}]
     (if (timestamp? type_name)
       {:json-schema/format    "date-time"
        :json-schema/type      "string"}))

   (fn [{:keys [type_name]}]
     (if (date? type_name)
       {:json-schema/format    "date"
        :json-schema/type      "string"}))

   (fn [{:keys [column_size data_type]}]
     (if (stringish? data_type)
       {:json-schema/type      "string"
        :json-schema/maxLength column_size
        :json-schema/minLength 0}))
   ])


(defn col->schema [col]
  (apply merge (map (fn [f] (f col)) prop-fns)))




;; (def int=       (fn [a] (boolean (some #{a} #{"int" "int4" "int8" "integer"}))))
;; (def serial=    (partial = "serial"))




;; (defn common-props [{:keys [is_autoincrement column_def column_name ordinal_position type_name
;;                             nullable table_name] :as col} pks fks]

;;   (let [is-auto (= "YES" is_autoincrement)]

;;      (let [ref (map #(keyword table_name (:column_name %))
;;                     (filter #(= (:column_name %) column_name) pks))]
;;        (if (seq ref)
;;          {:json-schema/x-primary-key ref}))
;;      (let [ref (map #(keyword (:pktable_name %) (:pkcolumn_name %))
;;                     (filter
;;                      #(= (:fkcolumn_name %) column_name)
;;                      fks))]
;;        (if (seq ref)
;;          (merge
;;           {:json-schema/x-foreign-key ref}
;;           (if (int= type_name)
;;             {:spec (if nullable (s/nilable pos-int?) pos-int?)}))))




;; ;;
;; ;;
;; (defn string-props [{:keys [column_def column_size] :as col} pks fks]
;;   (merge
;;    (common-props col pks fks)
;;    {:json-schema/type      "string"}
;;    {:json-schema/maxLength column_size}
;;    {:json-schema/minLength 0}
;;    (if column_def
;;      {:json-schema/default (extract-string-default column_def)})))


;; (def date?      (spec-tools/spec {:spec #(or (instance? java.sql.Date %) (pos-int? %) (string? %))
;;                           :type :date}))

;; (def timestamp? (spec-tools/spec {:spec #(or (instance? java.sql.Timestamp %) (pos-int? %) (string? %))
;;                           :type :timestamp}))

;; (def dbl?       (spec-tools/spec {:spec #(or spec/double? spec/int?)
;;                           :type :double}))

;; (def interval?  (spec-tools/spec {:spec string?
;;                           :type :interval}))

;; (def other?     (spec-tools/spec {:spec string?
;;                           :type :other}))

;; (def bytea?     (spec-tools/spec {:spec string?
;;                           :type :bytea}))




;; ;; of course you shouldn't do anything like this.  but it's this way
;; ;; on purpose, because somewhere in the schema/spec conversions, the
;; ;; specs are interpreted all as 'object' if we're not careful.
;; ;;
;; (defmacro null? [sp] `(if 'nullable (s/nilable ~sp) ~sp))


;; (defn old-col->spec [{:keys [column_size nullable data_type type_name column_name] :as column} pks fks]
;;   #_(warn :col column)
;;   (let [type-props {Types/VARCHAR string-props}
;;         type-specs {Types/BIT       (null? spec/boolean?)
;;                     Types/BOOLEAN   (null? spec/boolean?)
;;                     Types/CHAR      (null? spec/char?)
;;                     Types/DATE      (null? date?)
;;                     Types/DOUBLE    (null? spec/double?)
;;                     Types/FLOAT     (null? spec/float?)
;;                     Types/INTEGER   (if (serial= type_name) (null? pos-int?) (null? spec/int?))
;;                     Types/NUMERIC   (null? spec/double?)
;;                     Types/REAL      (null? spec/double?)
;;                     Types/TIMESTAMP (null? timestamp?)

;;                     ;; includes enums
;;                     Types/VARCHAR   (null? (s/and spec/string? #(<= (.length %) column_size)))

;;                     ;; partial support:
;;                     Types/ARRAY     (null? other?)
;;                     Types/OTHER     (null? other?) ;; interval,
;;                     -2              bytea?
;;                     }
;;         sp (get type-specs data_type)
;;         ex (get type-props data_type common-props)]
;;     ;; (warn :col-spec column sp data_type type_name)
;;     (spec-tools/create-spec (merge {:spec sp} (ex column pks fks)))))

