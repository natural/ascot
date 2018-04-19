(defproject natural.ascot "0.1.1"
  :description "Specs from JDBC relations."
  :url "https://github.com/natural/ascot"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/java.jdbc "0.7.5"]
                 [org.postgresql/postgresql "42.2.2"]
                 [org.xerial/sqlite-jdbc "3.21.0.1"]

                 [metosin/spec-tools "0.6.1"]
                 [midje "1.9.1"]
                 ])
