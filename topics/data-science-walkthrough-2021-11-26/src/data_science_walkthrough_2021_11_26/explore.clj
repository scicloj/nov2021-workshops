(ns data-science-walkrhrough-2021-11-26.explore
  (:require [tablecloth.api :as tc]
            [tablecloth.time.api :as time]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind]
            [scicloj.ml.core :as ml]
            [scicloj.metamorph.ml.loss :as loss]
            [scicloj.ml.metamorph :as mm]
            [tablecloth.pipeline :as tc-pipe]
            [tech.v3.datatype :as dtype :refer [emap]]
            [tech.v3.datatype.functional :as fun]
            [tech.v3.datatype.rolling :as rolling]
            [tech.v3.dataset :as tmd]
            [tech.v3.dataset.modelling :as tmd-model]
            [tech.v3.datatype.statistics :as stats]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [scicloj.viz.api :as viz]
            [scicloj.viz.dataset]))


(comment
  (notespace/restart! {:open-browser? true})
  (notespace/restart-events!))

(defonce prepared-messages
  (-> "data/prepared-messages.csv"
      (tc/dataset {:key-fn keyword})))

(defn secs-since-different-sender [sender gap-duration]
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

(defonce messages-with-features
  (-> prepared-messages
      (tc/group-by [:stream :topic])
      (tc/add-column :same-sender-as-last? #(let [sender-id (:sender-id %)]
                                              (fun/eq sender-id (fun/shift sender-id 1))))
      (tc/add-column :secs-since-last #(let [timestamp (:timestamp %)]
                                         (fun/- timestamp (fun/shift timestamp 1))))
      (tc/add-column :secs-since-diff-sender #(secs-since-different-sender
                                               (:sender-id %) (:secs-since-last %)))
      (tc/drop-rows (complement :secs-since-diff-sender))
                                        ; dropping messages which are not the firsts
                                        ; in a sequence by the same sender
      (tc/add-column :date-time #(emap
                                  (fn [seconds-ts]
                                    (time/milliseconds->anytime (* 1000 seconds-ts) :local-date-time))
                                  :local-date-time
                                  (:timestamp %)))
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
                                     (emap #(time/dayofweek % {:as-number? true})
                                           :int32
                                           (:date-time ds)))
                       :hour       #(emap time/hour :int32 (:date-time %))
                       :year       #(emap time/year :int32 (:date-time %))})
      (tc/add-column :next-response-time #(fun/shift (:secs-since-diff-sender %) -1))
      (tc/ungroup)
      (tc/select-rows (fn [row]
                        (-> row :year (>= 2019))))))

(defonce topic-date-split
  (-> messages-with-features
      (tc/group-by [:topic :local-date])
      (tc/without-grouping-> (tc/split :holdout)
                             (tc/add-column :data #(map (fn [data split-name]
                                                          (tc/add-column data :$split-name split-name))
                                                        (:data %)
                                                        (:$split-name %))))
      tc/ungroup
      (tc/group-by :$split-name {:result-type :as-map})))


(defn enrich-features [messages]
  (-> messages
      (tc/group-by [:subject])
      (tc/add-column :ma-of-secs-since-diff-sender
                     (fn [ds]
                       (-> ds
                           :secs-since-diff-sender
                           (rolling/fixed-rolling-window
                            30
                            fun/mean
                            {:relative-window-position :left}))))
      tc/ungroup))



(-> topic-date-split
    :train
    (tc/group-by [:keyword-?])
    (tc/aggregate {:median-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))}))



(-> topic-date-split
    :train
    (tc/group-by [:year])
    (tc/aggregate {:median-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))}))


(-> topic-date-split
    :train
    (tc/group-by [:year :dayofweek])
    (tc/aggregate {:median-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    (tc/order-by [:year :dayofweek]))


(-> topic-date-split
    :train
    (tc/group-by [:hour])
    (tc/aggregate {:n tc/row-count
                   :median-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    (tc/order-by [:hour]))


