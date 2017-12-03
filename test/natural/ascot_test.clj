(ns natural.ascot-test
  (:use midje.sweet) 
  (:require [natural.ascot.core :as ascot]))


(fact "about some partial equality fns"
      (ascot/date= "date")   => true
      (ascot/date= "int")    => false

      (ascot/int= "int")      => true
      (ascot/int= "integer")  => true
      (ascot/int= "int4")     => true
      (ascot/int= "int8")     => true
      (ascot/int= "date")     => false
      (ascot/int= "int2")     => false

      (ascot/serial= "serial") => true
      (ascot/serial= "nope")   => false

      (ascot/timestamp= "timestamptz") => true
      (ascot/timestamp= "timestamp")   => true
      (ascot/timestamp= "datetime")    => false
      )


(fact "about common column properties"
      (let [type-name "any"
            default-value "default"
            ordinal-pos 37]

        (-> (ascot/common-props {:column_def default-value} {} {})     :json-schema/default)            => (read-string default-value)
        (-> (ascot/common-props {:ordinal_position ordinal-pos} {} {}) :json-schema/x-ordinal-position) => ordinal-pos
        (-> (ascot/common-props {:nullable 1} {} {})                   :json-schema/x-nullable)         => true
        (-> (ascot/common-props {:type_name type-name} {} {})          :json-schema/x-type-name )       => type-name))


(fact "about column default extract"
      (ascot/extract-string-default "\"ok\"::nope")  =>     "ok"
      (ascot/extract-string-default "\"nope\"::yep") =not=> "ok"
      (ascot/extract-string-default "ok::nope")      =not=> "ok"
      )
