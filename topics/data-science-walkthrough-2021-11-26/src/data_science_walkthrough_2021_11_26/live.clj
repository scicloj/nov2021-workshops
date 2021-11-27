(ns data-science-walkthrough-2021-11-26.live
  (:require [tablecloth.api :as tc]
            [tablecloth.time.api :as time]
            [tech.v3.datatype :refer [emap] :as dtype]
            [tech.v3.datatype.rolling :as rolling]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind]
            [tech.v3.datatype.functional :as fun]
            [clojure.string :as s]
            [data-science-walkthrough-2021-11-26.sentiments :as sentiments]))

(comment
  (notespace/restart! #_{:open-browser? true})

  (notespace/restart-events!))

(defonce raw-data (read-string
                   (slurp "data/scicloj-zulip.edn")))

(def messages (-> raw-data
                  tc/dataset
                  (tc/select-columns [:display_recipient
                                      :subject
                                      :sender_id
                                      :timestamp
                                      :content])
                  (tc/rename-columns {:display_recipient :stream
                                      :subject           :topic
                                      :sender_id         :sender-id})))

(tc/shape messages)

(-> messages
    first
    first)

(-> messages
    :sender-id
    set
    count)
;;248

(defn seconds-since-different-sender [sender gap-duration]
  (->> (map vector sender gap-duration)
       (reduce (fn [acc current]
                 (let [[last-sender last-gap-duration] (first acc)
                       [current-sender current-gap-duratiion] current]
                   (if (= current-sender last-sender)
                     (conj (rest acc)
                           [last-sender nil] ;; fixme
                           [current-sender (+ current-gap-duratiion
                                              last-gap-duration)])
                     (conj acc current)))) '())
       reverse
       (map second)))

(def seconds-in-a-day
  (* 24 60 60))

(def seconds-in-a-week
  (* 7 24 60 60))

(def quick-response-threshold
  (* 10 60))



^kind/dataset
(def messages-with-features
  (-> messages
     (tc/group-by [:stream :topic])
     (tc/add-column :date-time (fn [ds]
                                 (->> ds
                                      :timestamp
                                      (emap (partial * 1000) :int64)
                                      (emap #(time/milliseconds->anytime % :local-date-time) :local-date-time))))
    
     (tc/add-columns {:local-date #(emap
                                    (fn [t]
                                      (time/convert-to t :local-date))
                                    :local-date
                                    (:date-time %))
                      :month      (fn [ds]
                                    (emap #(time/month % {:as-number? true})
                                          :int32
                                          (:date-time ds)))
                      :dayofweek  (fn [ds]
                                    (map #(-> %
                                              (time/dayofweek {:as-number? true})
                                              dec
                                              ([:Mon :Tue :Wed :Thu :Fri :Sat :Sun]))
                                         (:date-time ds)))
                      :hour       #(emap time/hour :int32 (:date-time %))
                      :year       #(emap time/year :int32 (:date-time %))})
    
     (tc/add-column :same-sender-as-last? #(let [sender-id (:sender-id %)]
                                             (fun/eq sender-id (fun/shift sender-id 1))))

     (tc/add-column :seconds-since-last #(let [timestamp (:timestamp %)]
                                           (fun/- timestamp (fun/shift timestamp 1))))

     (tc/add-column :response-time #(seconds-since-different-sender
                                     (:sender-id %) (:seconds-since-last %)))

     (tc/drop-rows (complement :response-time))
                                        ; dropping messages which are not the firsts
                                        ; in a sequence by the same sender

     (tc/add-column :prev-response-time
                    #(-> %
                         :response-time
                         (fun/shift 1)))

     (tc/add-column :next-response-time
                    #(-> %
                         :response-time
                         (fun/shift -1)
                         (fun/min seconds-in-a-week)))

     (tc/add-column :active?
                    #(-> %
                         :next-response-time
                         (fun/< quick-response-threshold)))

     (tc/add-column :ma-of-log-response-time
                    (fn [ds]
                      (-> ds
                          :response-time
                          fun/log10
                          (rolling/fixed-rolling-window
                           3
                           fun/mean
                           {:relative-window-position :left}))))

     (tc/ungroup)

     (tc/add-column :safe-sender-id
                    (fn [ds]
                      (-> ds
                          :sender-id
                          ((fn [values]
                             (let [frequent-values (->> values
                                                        frequencies
                                                        (sort-by val)
                                                        reverse
                                                        (take 30)
                                                        (map key)
                                                        set)]
                               (->> values
                                    (map (fn [value]
                                           (-> value
                                               frequent-values
                                               (or -1)))))))))))

     (tc/select-rows (fn [row]
                       (-> row :year (>= 2019))))
     sentiments/add-sentiments))



(-> messages-with-features
    tc/column-names)
^kind/dataset
(-> messages-with-features
    tc/head)

