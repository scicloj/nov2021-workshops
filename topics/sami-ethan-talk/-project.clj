(defproject reclojure-2021 "0.1.0-SNAPSHOT"
  :description ""
  :url ""
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [scicloj/notespace "4-alpha-13"]
                 [scicloj/tablecloth "6.025"]
         ;;        [org.scicloj/tablecloth.time "1.00-alpha-4-SNAPSHOT"]
                 ]
  :repl-options {:nrepl-middleware [scicloj.notespace.v4.nrepl/middleware]})


