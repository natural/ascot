(ns natural.ascot.core
  (:import [java.sql Types])
  (:require [clojure.java.jdbc :refer [metadata-result with-db-metadata]]
            [clojure.spec.alpha :as s]
            [clojure.string :refer [join split]]
            [spec-tools.core :as spec-tools]
            [spec-tools.spec :as spec]))


(defn relations [{:keys [catalog schema tables types] :as db-spec}]
  (let [tables  (or tables "%")
        types   (or types #{:table :view})

        by-type (fn [{:keys [table_type]}]
                  (and table_type (get types (-> table_type (.toLowerCase) keyword))))

        meta    #(with-db-metadata [md db-spec] (metadata-result (% md)))
        
        rels    (filter by-type (meta #(.getTables % catalog schema tables nil)))

        cols    (fn [{:keys [table_name]}]
                  {table_name (meta #(.getColumns % catalog schema table_name nil))})

        pks     (fn [{:keys [table_name]}]
                  {table_name (meta #(.getPrimaryKeys % catalog schema table_name))})

        fks     (fn [{:keys [table_name]}]
                  {table_name (meta #(.getImportedKeys % catalog schema table_name))})]
    
    {:columns      (into {} (map cols rels))
     :foreign-keys (into {} (map fks  rels))
     :primary-keys (into {} (map pks  rels))
     :relations    rels}))


(defmulti col->spec
  (fn [{:keys [data_type nullable]}]
    [(Integer. data_type) (= nullable 1)]))


(defmacro def-col->spec [type predicate]
  (let [name (-> type str (.split "/") (aget 1) (.toLowerCase) keyword)]
    `(do
       (defmethod col->spec [~type true] [col#]
         (spec-tools/create-spec {:spec (s/nilable ~predicate) :type ~name}))
       (defmethod col->spec [~type false] [col#]
         (spec-tools/create-spec {:spec ~predicate :type ~name})))))


(def-col->spec Types/ARRAY     spec/seq?)
(def-col->spec Types/BIGINT    spec/int?)
(def-col->spec Types/BINARY    spec/string?)
(def-col->spec Types/BIT       spec/string?)
(def-col->spec Types/BOOLEAN   spec/boolean?)
(def-col->spec Types/DATE      spec/inst?)
(def-col->spec Types/DECIMAL   spec/float?)
(def-col->spec Types/DOUBLE    spec/decimal?)
(def-col->spec Types/FLOAT     spec/float?)
(def-col->spec Types/INTEGER   spec/int?)
(def-col->spec Types/NUMERIC   spec/number?)
(def-col->spec Types/OTHER     spec/inst?)
(def-col->spec Types/REAL      spec/float?)
(def-col->spec Types/SMALLINT  spec/int?)
(def-col->spec Types/SQLXML    spec/string?)
(def-col->spec Types/TIME      spec/inst?)
(def-col->spec Types/TIMESTAMP spec/inst?)
(def-col->spec Types/TINYINT   spec/int?)


(defmethod col->spec [Types/CHAR true] [col]
  (let [sz (:column_size col)
        sp (s/nilable spec/string?)
        sp (if sz
             (s/and sp #(<= (.length %) sz))
             sp)]
    (spec-tools/create-spec {:spec sp :type :char})))


(defmethod col->spec [Types/CHAR false] [col]
  (let [sz (:column_size col)
        sp (s/nilable spec/string?)
        sp (if sz
             (s/and sp #(<= (.length %) sz))
             sp)]
    (spec-tools/create-spec {:spec sp :type :char})))


(defmethod col->spec [Types/VARCHAR true] [col]
  (col->spec (assoc col :data_type Types/CHAR)))


(defmethod col->spec [Types/VARCHAR false] [col]
  (col->spec (assoc col :data_type Types/CHAR)))
