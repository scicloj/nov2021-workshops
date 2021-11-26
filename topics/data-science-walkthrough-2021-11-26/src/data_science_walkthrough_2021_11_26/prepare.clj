(ns data-science-walkrhrough-2021-11-26.preare
  (:require [tablecloth.api :as tc]
           [tech.v3.datatype :refer [emap] :as dtype]
           [tech.v3.datatype.functional :as fun]
           [tech.v3.datatype.datetime :as dtype-dt]
           [clojure.string :as s]))

;; (defonce raw-data (read-string
;;                    (slurp "data/scicloj-zulip.edn")))
(defonce raw-data (read-string
                   (slurp "/home/sakalli/projects/nov2021-workshops/zulip-scicloj.txt")))

(def messages (-> raw-data
                  (tc/dataset)
                  (tc/select-columns [:display_recipient
                                      :subject
                                      :content
                                      :sender_id
                                      :timestamp
                                      :content])
                  (tc/rename-columns {:display_recipient :stream
                                      :subject           :topic
                                      :sender_id         :sender-id})
                  (tc/add-columns {:keyword-? (fn [ds]
                                                (emap #(s/includes? % "?")
                                                      :boolean
                                                      (:content ds)))})
                  #_(tc/drop-columns :content)))



(tc/write! messages "data/prepared-messages.csv")

