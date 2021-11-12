(defproject inferme-tutorials "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.3"]
                 [scicloj/notespace "4-alpha-10"]
                 [org.scicloj/kindly "1-alpha3"]
                 [generateme/inferme "0.0.2-SNAPSHOT"]
                 [generateme/fitdistr "1.0.4"]
                 [aerial.hanami "0.12.9"]]
  :repl-options {:nrepl-middleware [scicloj.notespace.v4.nrepl/middleware]})
