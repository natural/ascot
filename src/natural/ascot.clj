(ns natural.ascot
  (:require [natural.ascot.core]))


#_(seq
 (map (fn [[a b c]] (intern a b c))
      [['natural.ascot 'column-specs natural.ascot.core/column-specs]
       ['natural.ascot 'register     natural.ascot.core/register]
       ['natural.ascot 'table-specs  natural.ascot.core/table-specs]
       ['natural.ascot 'tables       natural.ascot.core/tables]]))
