(ns fiddle-now)

(require '[clojure.tools.deps.alpha.repl :refer [add-libs]])

(add-libs '{scicloj/notespace {:mvn/version "4-alpha-13"}})

(require '[scicloj.notespace.v4.api :as notespace]
         '[scicloj.kindly.kind :as kind])

(comment
  (notespace/restart!)
  ,)


(range 100)

(def raw-data (read-string
               (slurp "/home/sakalli/syncs/Dropbox/projects/nov2021-workshops/topics/sami-ethan-talk/quick-start/zulip-scicloj.txt")))

;; stream->topic->message



