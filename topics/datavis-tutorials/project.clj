(defproject datavis-tutorials "0.1.0-SNAPSHOT"
  :description "data visualization tutorials"
  :url "https://github.com/scicloj/nov2021-workshops"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :middleware [lein-tools-deps.plugin/resolve-dependencies-with-deps-edn]
  :plugins [[lein-tools-deps "0.4.5"]]
  :lein-tools-deps/config {:config-files [:install :user :project]}
  :repl-options {:nrepl-middleware [scicloj.notespace.v4.nrepl/middleware]})
