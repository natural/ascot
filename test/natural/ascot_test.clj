(ns natural.ascot-test
  (:import [java.sql Types])
  (:use midje.sweet)
  (:require [clojure.java.jdbc  :as jdbc]
            [clojure.spec.alpha :as s]
            [natural.ascot.core :as ascot]
            [natural.ascot.json-schema :as ascot-json]
            ))


(fact
 "specs account for null columns"
 (let [col (ascot/col->spec {:data_type Types/INTEGER :nullable 1})]
   (s/valid? col 0)   => true
   (s/valid? col 1)   => true
   (s/valid? col nil) => true))


(fact
 "specs account for non-null columns"
 (let [col (ascot/col->spec {:data_type Types/INTEGER :nullable 0})]
   (nil? (:spec col)) => false
   (nil? (:type col)) => false
   (s/valid? col 0)   => true
   (s/valid? col 1)   => true
   (s/valid? col nil) => false))


(fact
 "specs allow for varchar columns"
 (let [col (ascot/col->spec {:data_type Types/VARCHAR :nullable 0})]
   (s/valid? col 0)   => false
   (s/valid? col "")  => true
   (s/valid? col "1") => true))


(fact
 "specs allow for char columns"
 (let [col (ascot/col->spec {:data_type Types/CHAR :nullable 0})]
   (s/valid? col 0)   => false
   (s/valid? col "")  => true
   (s/valid? col "1") => true))


(fact
  "specs allow for int columns"
  (let [col (ascot/col->spec {:data_type Types/INTEGER :nullable 0})]
    (s/valid? col -1)  => true
    (s/valid? col  0)  => true
    (s/valid? col -1)  => true        
    (s/valid? col "0") => false
    (s/valid? col "1") => false
    (s/valid? col nil) => false))


;; sqlite ":memory:" db type doesn't work with jdbc meta data methods.  :(
;;
(let [temp-db (java.io.File/createTempFile "ascot-test" ".db")
      temp-name (.getAbsolutePath temp-db)
      source {:dbtype "sqlite" :dbname temp-name}
      
      _ (jdbc/execute! source "CREATE TABLE T (a INT PRIMARY KEY, b INT)")
      _ (jdbc/execute! source "CREATE TABLE U (c INT REFERENCES T (b))")
      
      rels (ascot/relations source)]
  (fact
    "relations found via metadata method calls"
    (count (-> rels :relations))              => 2
    (count (-> rels :foreign-keys (get "T"))) => 0
    (count (-> rels :primary-keys (get "T"))) => 1
    (count (-> rels :columns      (get "T"))) => 2
    (count (-> rels :foreign-keys (get "U"))) => 1

    (.delete temp-db) => true))


(fact "about json schema bits"
  (ascot-json/date? "date") => true
  ;; json: default, position, null, type
  ;; 
  )

