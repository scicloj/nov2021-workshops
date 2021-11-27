;; # walkthrough

;; Nov. 26th, 2021

;; Sami Kallinen, Ethan Miller, Daniel Slutsky

;; ## the stack

;; - notespace / oz / clerk / goldly / saite -- literate programming

;; - tablecloth / tech.v3.dataset

;; - dtype-next

;; - tablecloth.time

;; - statistics: tech.v3.datatype.statistics (dtype-next) / fastmath / kixistats

;; - datavis: tech.viz, hanami, viz.clj, cljplot


;; ## scicloj

:_

;; # setup

(ns data-science-walkrhrough-2021-11-26.explore
  (:require [tablecloth.api :as tc]
            [tablecloth.time.api :as time]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.api :as kindly]
            [scicloj.ml.core :as ml]
            [scicloj.metamorph.ml.loss :as loss]
            [scicloj.ml.metamorph :as mm]
            [tablecloth.pipeline :as tc-pipe]
            [tech.v3.datatype :as dtype :refer [emap]]
            [tech.v3.datatype.functional :as fun]
            [tech.v3.datatype.rolling :as rolling]
            [tech.v3.dataset :as tmd]
            [tech.v3.dataset.print :as dataset-print]
            [tech.v3.dataset.modelling :as tmd-model]
            [tech.v3.datatype.statistics :as stats]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [scicloj.viz.api :as viz]
            [scicloj.viz.dataset]
            [tech.viz.vega]
            [fastmath.core :as math]
            [fastmath.stats]
            [fastmath.random :as random]
            [data-science-walkthrough-2021-11-26.sentiments :as sentiment]))

(comment
  (notespace/restart! {:open-browser? true})

  (notespace/restart-events!))

(def prepared-messages
  (-> "data/prepared-messages.csv"
      (tc/dataset {:key-fn keyword
                   :dataset-name "prepared messages"})))

prepared-messages

;; # features

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

(def messages-with-features
  (-> prepared-messages
      (tc/group-by [:stream :topic])
      (tc/add-column :same-sender-as-last? #(let [sender-id (:sender-id %)]
                                              (fun/eq sender-id (fun/shift sender-id 1))))
      (tc/add-column :seconds-since-last #(let [timestamp (:timestamp %)]
                                         (fun/- timestamp (fun/shift timestamp 1))))
      (tc/add-column :seconds-since-diff-sender #(seconds-since-different-sender
                                                  (:sender-id %) (:seconds-since-last %)))
      (tc/drop-rows (complement :seconds-since-diff-sender))
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
                                     (map #(-> %
                                               (time/dayofweek {:as-number? true})
                                               dec
                                               ([:Mon :Tue :Wed :Thu :Fri :Sat :Sun]))
                                          (:date-time ds)))
                       :hour       #(emap time/hour :int32 (:date-time %))
                       :year       #(emap time/year :int32 (:date-time %))})
      (tc/add-column :next-response-time
                     #(-> %
                          :seconds-since-diff-sender
                          (fun/shift -1)
                          (fun/min seconds-in-a-week)))
      (tc/add-column :ma-of-log-seconds-since-diff-sender
                     (fn [ds]
                       (-> ds
                           :seconds-since-diff-sender
                           fun/log10
                           (rolling/fixed-rolling-window
                            3
                            fun/mean
                            {:relative-window-position :left}))))
      (tc/ungroup)
      (tc/select-rows (fn [row]
                        (-> row :year (>= 2019))))))

messages-with-features


(-> prepared-messages
    (tc/group-by [:stream :topic])
    :data
    first)

;; # ml preparations

(-> {:x (range 9)}
    tc/dataset
    (tc/split :holdout {:seed 1}))

(def topic-date-split
  (-> messages-with-features
      (tc/select-rows (fn [row]
                        (-> row
                            :next-response-time
                            pos?)))
      (tc/group-by [:topic :local-date])
      (tc/without-grouping-> (tc/split :holdout {:seed 1})
                             (tc/add-column
                              :data
                              #(map (fn [data split-name]
                                      (tc/add-column data :$split-name split-name))
                                    (:data %)
                                    (:$split-name %))))
      tc/ungroup
      (tc/group-by :$split-name {:result-type :as-map})))


;; # exploring-train

(-> topic-date-split
    :train
    :next-response-time
    fastmath.stats/mean)

(-> topic-date-split
    :train
    :next-response-time
    fastmath.stats/stddev)

(-> topic-date-split
    :train
    :next-response-time
    fun/mean)

(-> topic-date-split
    :train
    :next-response-time
    tech.v3.datatype.statistics/standard-deviation)


^kind/vega
(-> topic-date-split
    :train
    :next-response-time
    (tech.viz.vega/histogram :next-response-time))

(-> topic-date-split
    :train
    :next-response-time
    fun/log10
    (->> (map (fn [x] (Double/isNaN x))))
    frequencies)

(-> topic-date-split
    :train
    :next-response-time
    fun/log10
    (->> (filter pos?)
         (map (fn [x] (Double/isNaN x))))
    frequencies)

^kind/vega
(-> topic-date-split
    :train
    :next-response-time
    fun/log10
    (->> (filter pos?))
    (tech.viz.vega/histogram :log-next-response-time))

(> Double/NaN 0)

(-> topic-date-split
    :train
    (tc/group-by [:year])
    (tc/aggregate {:median-next-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    (tc/order-by [:year])
    viz/data
    (viz/y :median-next-response-time)
    (viz/x :year {:type :nominal})
    (viz/type ht/bar-chart)
    viz/viz)

^kind/hiccup
(-> [:keyword-?
     :year
     :joy :positive :anticipation :anger :sadness
     :stream]
    (->> (map (fn [grouping]
                [:div [:h1 [:p/code (pr-str grouping)]]
                 [:p/vega
                  (-> topic-date-split
                      :train
                      (tc/group-by [grouping])
                      (tc/order-by [grouping])
                      (tc/aggregate {:median-next-response-time
                                     (fn [ds]
                                       (-> ds
                                           :next-response-time
                                           fun/median))})
                      viz/data
                      (viz/y :median-next-response-time)
                      (viz/x grouping {:type :nominal})
                      (viz/type ht/bar-chart)
                      viz/viz)]]))
         (into [:div])))


^kind/hiccup
(-> [:keyword-?
     :year
     :joy :positive :anticipation :anger :sadness
     :stream]
    (->> (map (fn [grouping]
                [:div [:h1 [:p/code (pr-str grouping)]]
                 [:p/vega
                  (-> topic-date-split
                      :train
                      (tc/order-by [grouping])
                      (tc/add-column :log-next-response-time #(-> %
                                                                  :next-response-time
                                                                  fun/log10))
                      viz/data
                      (viz/y :log-next-response-time)
                      (viz/x grouping {:type :nominal})
                      (viz/type scicloj.viz.templates/boxplot-chart)
                      viz/viz)]]))
         (into [:div])))



^kind/hiccup
(-> [:afinn-sentiment :trust :surprise]
    (->> (map (fn [grouping]
                [:div [:h1 [:p/code (pr-str grouping)]]
                 [:p/vega
                  (-> topic-date-split
                      :train
                      (tc/order-by [grouping])
                      (tc/add-column :log-next-response-time #(-> %
                                                                  :next-response-time
                                                                  fun/log10))
                      viz/data
                      (viz/y :log-next-response-time)
                      (viz/x grouping)
                      (viz/type ht/point-chart)
                      viz/viz)]]))
         (into [:div])))



(-> topic-date-split
    :train
    (tc/group-by [:keyword-?])
    (tc/aggregate {:median-next-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                        fun/median))})
    delay)


(-> topic-date-split
    :train
    (tc/group-by [:year])
    (tc/aggregate {:median-next-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    delay)


(-> topic-date-split
    :train
    (tc/group-by [:year :dayofweek])
    (tc/aggregate {:median-next-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    (tc/order-by [:year :dayofweek])
    delay)


(-> topic-date-split
    :train
    (tc/group-by [:hour])
    (tc/aggregate {:n tc/row-count
                   :median-next-response-time
                   (fn [ds]
                     (-> ds
                         :next-response-time
                         fun/median))})
    (tc/order-by [:hour])
    delay)


;; # ml


(def pipe1
  (ml/pipeline
   (tc-pipe/add-column :log-next-response-time #(-> %
                                                    :next-response-time
                                                    fun/log10))
   (tc-pipe/add-column :safe-ma #(-> %
                                     :ma-of-log-seconds-since-diff-sender
                                     (->> (map (fn [x]
                                                 (if (Double/isFinite x)
                                                   x
                                                   0))))))
   (mm/select-columns [:hour
                       :keyword-?
                       :year
                       :joy :positive :anticipation
                       :stream
                       :sender-id
                       :safe-ma
                       :log-next-response-time])
   (ml/lift tmd/categorical->one-hot [:hour])
   (ml/lift tmd/categorical->one-hot [:year])
   (ml/lift tmd/categorical->one-hot [:stream])
   (mm/set-inference-target :log-next-response-time)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/elastic-net
              :lambda1 10.0
              :lambda2 10.0})))

(def trained-ctx1
  (pipe1 {:metamorph/data (:train topic-date-split)
          :metamorph/mode :fit}))

(def test-ctx1
  (pipe1
   (assoc trained-ctx1
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(-> trained-ctx1
    :model
    ml/explain)


(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> topic-date-split
                    :train
                    :next-response-time
                    fun/mean
                    (->> (repeat (count actual))))]
  (loss/mae actual predicted))
;; 77419.71823544911

(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> test-ctx1
                    :metamorph/data
                    :log-next-response-time
                    (->> (fun/pow 10)))]
  (loss/mae actual predicted))
;; 51997.16641663653



(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> test-ctx1
                    :metamorph/data
                    :log-next-response-time
                    (->> (fun/pow 10)))]
  (-> {:actual actual
       :predicted predicted}
      tc/dataset
      viz/data
      (viz/x "actual")
      (viz/y "predicted")
      (viz/type "point")
      (assoc :XSCALE {:type :log}
             :YSCALE {:type :log})
      viz/viz))


(-> topic-date-split
    :train
    (tc/select-rows (fn [row]
                      (-> row
                          :next-response-time
                          (> 10000000)))))


;; :bye

