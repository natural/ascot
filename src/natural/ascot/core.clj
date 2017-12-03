;; adapted from https://github.com/viesti/table-spec
;;
(ns natural.ascot.core
  (:require [clojure.java.jdbc :refer [metadata-result with-db-metadata] :as jdbc]
            [clojure.spec.alpha :as s]
            [clojure.string :as string]
            
            [spec-tools.core :as st :refer [create-spec]]
            [spec-tools.spec :as spec])
  (:import [java.sql Types]))




(def date=      (partial = "date"))
(def int=       (fn [a] (boolean (some #{a} #{"int" "int4" "int8" "integer"}))))
(def serial=    (partial = "serial"))
(def timestamp= (fn [a] (boolean (some #{a} #{"timestamptz" "timestamp"}))))


;;
;;
(defn common-props [{:keys [is_autoincrement column_def column_name ordinal_position type_name
                            nullable table_name] :as col} pks fks]

  ;; (warn :WW column_name fks)
  (let [is-auto (= "YES" is_autoincrement)]
    (merge
     {:json-schema/x-nullable (= nullable 1)}

     (if type_name
       {:json-schema/x-type-name type_name})
     (if-let [default (and (not is-auto) column_def)]
       {:json-schema/default (read-string default)})
     (if ordinal_position
       {:json-schema/x-ordinal-position ordinal_position})
     (let [ref (map #(keyword table_name (:column_name %))
                    (filter #(= (:column_name %) column_name) pks))]
       (if (seq ref)
         {:json-schema/x-primary-key ref}))
     (let [ref (map #(keyword (:pktable_name %) (:pkcolumn_name %))
                    (filter
                     #(= (:fkcolumn_name %) column_name)
                     fks))]
       (if (seq ref)
         (merge
          {:json-schema/x-foreign-key ref}
          (if (int= type_name)
            {:spec (if nullable (s/nilable pos-int?) pos-int?)}))))

     (if (timestamp= type_name)
       {:json-schema/format "date-time"
        :json-schema/type "string"})
     (if (date= type_name)
       {:json-schema/format "date"
        :json-schema/type "string"})
     (if (= column_name "phone")
       {:json-schema/format "phone"})
     (if (= column_name "email")
       {:json-schema/format "email"})
     (if (= column_name "photo")
       {:json-schema/format "photo"})     
     (if is-auto
       {:json-schema/x-auto-increment true}))))


;;
;;
(defn extract-string-default [v]
  (string/join (->> (first (string/split v #"::")) (drop 1) (drop-last 1))))

;;
;;
(defn string-props [{:keys [column_def column_size] :as col} pks fks]
  (merge
   (common-props col pks fks)
   {:json-schema/type      "string"}
   {:json-schema/maxLength column_size}
   {:json-schema/minLength 0}
   (if column_def
     {:json-schema/default (extract-string-default column_def)})))


(def date?      (st/spec {:spec #(or (instance? java.sql.Date %) (pos-int? %) (string? %))
                          :type :date}))

(def timestamp? (st/spec {:spec #(or (instance? java.sql.Timestamp %) (pos-int? %) (string? %))
                          :type :timestamp}))

(def dbl?       (st/spec {:spec #(or spec/double? spec/int?)
                          :type :double}))

(def interval?  (st/spec {:spec string?
                          :type :interval}))

(def other?     (st/spec {:spec string?
                          :type :other}))

(def bytea?     (st/spec {:spec string?
                          :type :bytea}))




;; of course you shouldn't do anything like this.  but it's this way
;; on purpose, because somewhere in the schema/spec conversions, the
;; specs are interpreted all as 'object' if we're not careful.
;;
(defmacro null? [sp] `(if 'nullable (s/nilable ~sp) ~sp))


(defn col->spec [{:keys [column_size nullable data_type type_name column_name] :as column} pks fks]
  #_(warn :col column)
  (let [type-props {Types/VARCHAR string-props}
        type-specs {Types/BIT       (null? spec/boolean?)
                    Types/BOOLEAN   (null? spec/boolean?)
                    Types/CHAR      (null? spec/char?)
                    Types/DATE      (null? date?)
                    Types/DOUBLE    (null? spec/double?)
                    Types/FLOAT     (null? spec/float?)
                    Types/INTEGER   (if (serial= type_name) (null? pos-int?) (null? spec/int?))
                    Types/NUMERIC   (null? spec/double?)
                    Types/REAL      (null? spec/double?)
                    Types/TIMESTAMP (null? timestamp?)

                    ;; includes enums
                    Types/VARCHAR   (null? (s/and spec/string? #(<= (.length %) column_size)))

                    ;; partial support:
                    Types/ARRAY     (null? other?)
                    Types/OTHER     (null? other?) ;; interval,
                    -2              bytea?
                    }
        sp (get type-specs data_type)
        ex (get type-props data_type common-props)]
    ;; (warn :col-spec column sp data_type type_name)
    (create-spec (merge {:spec sp} (ex column pks fks)))))


;;
;;
(defn tables [{:keys [schema] :as db-spec}]
  (let [grouped-by (fn [a] (fn [b] (group-by a b)))
        keyed      (fn [a] (into {} (map (fn [[k v]] [(keyword k) v]) a)))
        reorg      (fn [item]
                     (let [tables (keys (:columns item)) types (keys item)]
                       (into {}
                             (for [n tables]
                               [n (into {}
                                        (for [k types]
                                          [k (-> item k n)]))]))))]
    (with-db-metadata [md db-spec]
      (reorg {:columns
              (-> md
                  (.getColumns nil schema nil nil)
                  (metadata-result)
                  ((grouped-by :table_name))
                  (keyed))
              :primary-keys
              (-> md
                  (.getPrimaryKeys nil schema nil)
                  (metadata-result)
                  ((grouped-by :table_name))
                  (keyed))
              :foreign-keys
              (-> md
                  (.getImportedKeys nil schema nil)
                  (metadata-result)
                  ((grouped-by :fktable_name))
                  (keyed))}))))


;; table-specs -> rel-specs!
;;
(defn table-specs [tables & {:keys [ns] :or {ns :table}}]
  (for [[rel {:keys [columns primary-keys foreign-keys] :as table}] tables
        :let [rel-name    (name rel)
              rel         (keyword (name ns) rel-name)
              select-cols (fn [v]
                            (map #(keyword rel-name %)
                                 (map :column_name
                                      (filter #(= v (:nullable %))
                                              columns))))
              req-un      (select-cols 0)
              opt-un      (select-cols 1)]]
    [rel `(s/keys :req-un ~req-un :opt-un ~opt-un)]))


;;
;;
(defn column-specs [tables]
  (for [[rel   {:keys [columns primary-keys foreign-keys]}] tables]
    [rel (for [{:keys [column_name] :as column} columns
               :let   [k (keyword (name rel) column_name)
                       s (col->spec column primary-keys foreign-keys)]]
           [k s])]))


;;
;;
(defn register [specs]
  (doseq [[k s] specs]
    (eval `(s/def ~k ~s))))
