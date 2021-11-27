;; # machine learning pipelines

;; Nov. 27th, 2021

;; Sami Kallinen, Ethan Miller, Daniel Slutsky

;; # setup

(ns data-science-walkrhrough-2021-11-26.ml
  (:require [tablecloth.api :as tc]
            [tablecloth.time.api :as time]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.api :as kindly]
            [scicloj.ml.core :as ml]
            [scicloj.metamorph.ml.loss :as loss]
            [scicloj.ml.metamorph :as mm]
            [scicloj.ml.dataset]
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

(def quick-response-threshold
  (* 10 60))

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
      (tc/add-column :active?
                     #(-> %
                          :next-response-time
                          (fun/< quick-response-threshold)))
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
                        (->> row
                             ((juxt :seconds-since-diff-sender :next-response-time))
                             (every? pos?))))
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



(-> messages-with-features
    (tc/select-columns [:seconds-since-diff-sender :next-response-time])
    (tc/select-rows (fn [row]
                      (->> row
                           vals
                           (every? pos?))))
    viz/data
    (viz/x :seconds-since-diff-sender)
    (viz/y :next-response-time)
    (viz/type ht/point-chart)
    (assoc :XSCALE {:type "log"}
           :YSCALE {:type "log"})
    viz/viz)



;; # ml


(def pipe1
  (ml/pipeline
   (tc-pipe/add-columns {:log-seconds-since-diff-sender #(-> %
                                                             :seconds-since-diff-sender 
                                                             fun/log10)
                         :log-next-response-time #(-> %
                                                      :next-response-time
                                                      fun/log10)})
   ;; (tc-pipe/add-column :safe-ma #(-> %
   ;;                                   :ma-of-log-seconds-since-diff-sender
   ;;                                   (->> (map (fn [x]
   ;;                                               (if (Double/isFinite x)
   ;;                                                 x
   ;;                                                 0))))))
   (mm/select-columns [;; :hour
                       ;; :keyword-?
                       ;; :year
                       ;; :joy :positive :anticipation
                       ;; :stream
                       ;; :safe-ma
                       :log-seconds-since-diff-sender
                       :log-next-response-time])
   ;; (ml/lift tmd/categorical->one-hot [:hour])
   ;; (ml/lift tmd/categorical->one-hot [:year])
   ;; (ml/lift tmd/categorical->one-hot [:stream])
   (mm/set-inference-target :log-next-response-time)
   {:metamorph/id :model}
   (mm/model {:model-type ;; :smile.regression/random-forest
              :smile.regression/ordinary-least-square
              ;; :lambda1 10.0
              ;; :lambda2 10.0
              })))

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
  (loss/mse actual predicted))


(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> test-ctx1
                    :metamorph/data
                    :log-next-response-time
                    (->> (fun/pow 10)))]
  (loss/mse actual predicted))




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
      (viz/x "predicted")
      (viz/y "actual")
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


;; # classification

(defn number->category [column]
  (let [lookup (->> column
                    meta
                    :categorical-map
                    :lookup-table
                    (map (comp vec reverse))
                    (into {}))]
    (-> column
        fun/round
        (->> (map lookup)))))

(def pipe2
  (ml/pipeline
   (tc-pipe/add-columns {:log-seconds-since-diff-sender #(-> %
                                                             :seconds-since-diff-sender 
                                                             fun/log10)})
   (mm/select-columns [;; :hour
                       ;; :keyword-?
                       :year
                       ;; :joy :positive :anticipation
                       ;; :stream
                       :log-seconds-since-diff-sender
                       :active?])
   ;; (mm/categorical->one-hot [:keyword-?])
   (mm/categorical->one-hot [:year])
   ;; (mm/categorical->one-hot [:hour])
   ;; (mm/categorical->one-hot [:stream])
   (mm/categorical->number [:active?])
   (mm/set-inference-target :active?)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.classification/logistic-regression})))


(def trained-ctx2
  (pipe2 {:metamorph/data (:train topic-date-split)
          :metamorph/mode :fit}))

(-> trained-ctx2
    :model
    ml/explain)

(-> trained-ctx2
    :model
    ml/thaw-model
    (.coefficients)
    vec)

(def test-ctx2
  (pipe2
   (assoc trained-ctx2
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))


(let [actual    (-> topic-date-split :test :active?)
      predicted (-> test-ctx2
                    :metamorph/data
                    :active?
                    number->category)]
  {:confusion (->> (map vector actual predicted)
                   frequencies)
   :accuary   (fun// (-> (fun/eq actual predicted)
                       (fun/sum))
                   (tc/row-count actual))})



:bye

