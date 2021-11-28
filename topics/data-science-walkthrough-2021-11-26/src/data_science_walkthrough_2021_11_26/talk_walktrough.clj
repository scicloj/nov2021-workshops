(ns data-science-walkthrough-2021-11-26.talk-walktrough
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
            ;; 
            [data-science-walkthrough-2021-11-26.sentiments :as sentiment]
            ;;[data-science-walkrhrough-2021-11-26.reddit-benchmark :as r]
            ))


(comment
  (notespace/restart! {:open-browser? true})
  (notespace/restart-events!))

(def prepared-messages
  (-> "data/prepared-messages.csv"
      (tc/dataset {:key-fn keyword
                   :dataset-name "prepared messages"})))

;; # initial exploring dataset

prepared-messages

(-> prepared-messages
    tmd/shape)

(-> prepared-messages
    :stream
    set)

(-> prepared-messages
    :topic
    set)

(-> prepared-messages
    :topic
    set
    count)

(-> prepared-messages
    :sender-id
    set
    count)

;; visualize user posting activity, e.g. how many users post very frequently
^kind/vega
(-> prepared-messages
     :sender-id
     frequencies
     (->> (map second))
     frequencies
     (->> (map (fn [[post-count user-count]]
                 {:num-posts post-count
                  :num-users user-count})))
     tc/dataset
     viz/data
     (viz/y :num-posts)
     (viz/x :num-users)
     (viz/type ht/point-chart)
     (assoc :XSCALE {:type :log}
            :YSCALE {:type :log} ;; dropping this log may be clearer
            )
     (viz/viz)
     )

;; visualize distribution of number of posts in topics
(-> prepared-messages
    :topic
    frequencies
    (->> (map second))
    frequencies
    (->> (map (fn [[post-count topic-count]]
                {:posts post-count
                 :topic topic-count})))
    tc/dataset
    viz/data

    (viz/y :posts)
    (viz/x :topic)
    (viz/type ht/point-chart)
    (assoc :XSCALE {:type :log}
           :YSCALE {:type :log})
    viz/viz)

;; # prepared sentiment features


(sentiment/aggregate-mean-afinn-score mess-w-sentiments)
;; | :word-count | :afinn-sentiment | :sentiment |
;; |------------:|-----------------:|-----------:|
;; |    876432.0 |          35121.0 |  0.0400727 |

(aggregate-mean-sentiments  mess-w-sentiments)


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


(-> messages
    (tc/add-column :date-time (fn [ds]
                                (->> ds
                                     :timestamp
                                     (map (partial * 1000))
                                     (map #(time/milliseconds->anytime % :local-date-time)))))
    tc/head)

(def messages-with-features
  (-> prepared-messages
      (tc/group-by [:stream :topic])
      (tc/add-column :same-sender-as-last? #(let [sender-id (:sender-id %)]
                                              (fun/eq sender-id (fun/shift sender-id 1))))
      (tc/add-column :seconds-since-last #(let [timestamp (:timestamp %)]
                                            (fun/- timestamp (fun/shift timestamp 1))))
      (tc/add-column :response-time #(seconds-since-different-sender
                                      (:sender-id %) (:seconds-since-last %)))
      (tc/drop-rows (complement :response-time))
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
                        (-> row :year (>= 2019))))))

(-> messages-with-features
    tc/column-names)

(-> messages-with-features
    :safe-sender-id
    frequencies)

(-> messages-with-features
    (tc/group-by [:stream :topic])
    (tc/select-columns [:prev-response-time
                        :response-time
                        :next-response-time])
    :data
    first)

messages-with-features


;; visualize message volume month-by-month for different years
^kind/hiccup
[:div
 [:h2 "monthly message volume by year"]
 [:p/vega
  (-> messages-with-features
      (tc/group-by [:month :year])
      (tc/aggregate {:num-messages tc/row-count})
      (tc/order-by :month)
      (viz/data)
      (viz/type ht/line-chart)
      (viz/x "month")
      (viz/y "num-messages")
      (viz/color "year")
      (viz/viz)
      )]]

;; visualizing messsage volume at different frequencies
(-> messages-with-features
    (time/index-by :date-time)
    (time/adjust-frequency time/->days
                           {:ungroup? false
                            :include-columns [:year]})
    (tc/aggregate {:num-messages tc/row-count})
    (tc/update-columns {:date-time (partial map str)})
    (viz/data)
    (viz/type ht/line-chart)
    (viz/x "date-time" {:type :temporal})
    (viz/y "num-messages")
    (viz/color "year")
    (viz/viz))


;; # ml preparations

(-> {:x (range 9)
     :y (repeatedly 9 rand)}
    tc/dataset
    (tc/split :holdout {:seed 1}))

(def topic-date-split
  (-> messages-with-features
      (tc/select-rows (fn [row]
                        (and (->> row
                                  ((juxt :response-time
                                         :next-response-time
                                         :prev-response-time))
                                  (every? pos?))
                             (not= (:response-time row)
                                   (:next-response-time row)))))
      (tc/group-by [:stream :topic :local-date])
      (tc/without-grouping-> (tc/split :holdout {:seed 1})
                             (tc/add-column
                              :data
                              #(map (fn [data split-name]
                                      (tc/add-column data :$split-name split-name))
                                    (:data %)
                                    (:$split-name %))))
      tc/ungroup
      (tc/group-by :$split-name {:result-type :as-map})))

(->> topic-date-split
     vals
     (map (comp frequencies :safe-sender-id)))

;; # exploring-train

^kind/hiccup
[:div
 [:p/vega
  (-> topic-date-split
      :train
      :next-response-time
      (tech.viz.vega/histogram :next-response-time))]
 [:p/vega
  (-> topic-date-split
      :train
      :next-response-time
      fun/log10
      (tech.viz.vega/histogram :log-next-response-time))]]


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
(-> [:year
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
(-> [:year
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



(-> topic-date-split
    :train
    (tc/select-columns [:response-time :next-response-time])
    (tc/select-rows (fn [row]
                      (->> row
                           vals
                           (every? pos?))))
    viz/data
    (viz/x :response-time)
    (viz/y :next-response-time)
    (viz/type ht/point-chart)
    (assoc :XSCALE {:type "log"}
           :YSCALE {:type "log"})
    viz/viz)

(->> topic-date-split
     :train
     ((juxt :response-time :next-response-time))
     (map fun/log)
     (apply fastmath.stats/correlation))


;; # scicloj.ml

;; - experience the basic use of scicloj.ml on a real data problem

;; - learn about pipelines (metamorph, metamorph.ml)

;; - see some advanced uses of scicloj.ml (e.g., tuning over whole pipelines)


;; # regression

;; fit / transform

(def regression-pipe1
  (ml/pipeline
   (mm/add-columns {:log-response-time      #(-> %
                                                 :response-time 
                                                 fun/log10)
                    :log-next-response-time #(-> %
                                                 :next-response-time
                                                 fun/log10)})
   (mm/select-columns [:log-response-time
                       :log-next-response-time])
   (mm/set-inference-target :log-next-response-time)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/ordinary-least-square})))

(def regression-trained-ctx1
  (regression-pipe1 {:metamorph/data (:train topic-date-split)
                     :metamorph/mode :fit}))

(-> regression-trained-ctx1
    :model
    ml/explain)

(-> regression-trained-ctx1
    (update-in [:model :model-data] dissoc :model-as-bytes))

(def regression-test-ctx1
  (regression-pipe1
   (assoc regression-trained-ctx1
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(defn regression-test-ctx->measures [regression-test-ctx]
  (let [actual        (-> topic-date-split
                          :test
                          :next-response-time)
        log-actual    (-> actual
                          fun/log10)
        log-predicted (-> regression-test-ctx
                          :metamorph/data
                          :log-next-response-time)
        predicted     (->> log-predicted
                           (fun/pow 10))
        mse-of-logs   (loss/mse log-actual log-predicted)
        mse           (loss/mse actual predicted)]
    {:mse-of-logs mse-of-logs
     :mse         mse
     :R2-of-logs  (fun/- 1
                         (-> mse-of-logs
                             (fun// (stats/variance log-actual))))
     :R2          (fun/- 1
                         (fun// mse
                                (stats/variance actual)))}))

(regression-test-ctx->measures regression-test-ctx1)


(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> regression-test-ctx1
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



(let [actual    (-> topic-date-split
                    :test
                    :next-response-time)
      predicted (-> regression-test-ctx1
                    :metamorph/data
                    :log-next-response-time
                    (->> (fun/pow 10)))]
  (-> {:actual    actual
       :predicted predicted}
      tc/dataset
      viz/data
      (viz/x "predicted")
      (viz/y "actual")
      (viz/type "point")
      viz/viz))


(def regression-pipe2
  (ml/pipeline
   (tc-pipe/add-columns {:log-response-time #(-> %
                                                             :response-time 
                                                             fun/log10)
                         :log-next-response-time #(-> %
                                                      :next-response-time
                                                      fun/log10)})
   (mm/select-columns [:year
                       :stream
                       :surprise
                       :joy
                       :sadness
                       :anger
                       :anticipation
                       :log-response-time
                       :log-next-response-time])
   (mm/categorical->one-hot [:stream])
   (mm/categorical->one-hot [:year])
   (mm/set-inference-target :log-next-response-time)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/ridge})))

(def regression-trained-ctx2
  (regression-pipe2 {:metamorph/data (:train topic-date-split)
          :metamorph/mode :fit}))

(def regression-test-ctx2
  (regression-pipe2
   (assoc regression-trained-ctx2
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(-> regression-trained-ctx2
    :model
    ml/explain
    (update :coefficients #(sort-by first %)))

(regression-test-ctx->measures regression-test-ctx2)


;; # classification

(def classification-pipe1
  (ml/pipeline
   (tc-pipe/add-columns {:log-response-time #(-> %
                                                             :response-time 
                                                             fun/log10)})
   (mm/select-columns [:log-response-time
                       :active?])
   (mm/categorical->number [:active?])
   (mm/set-inference-target :active?)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.classification/logistic-regression})))

(def classification-trained-ctx1
  (classification-pipe1 {:metamorph/data (:train topic-date-split)
                         :metamorph/mode :fit}))

(def classification-test-ctx1
  (classification-pipe1
   (assoc classification-trained-ctx1
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(defn numbers->categories [column]
  (let [lookup (->> column
                    meta
                    :categorical-map
                    :lookup-table
                    (map (comp vec reverse))
                    (into {}))]
    (-> column
        fun/round
        (->> (map lookup)))))

(defn classification-test-ctx->measures [classification-test-ctx]
  (let [actual    (-> topic-date-split :test :active?)
        predicted (-> classification-test-ctx
                      :metamorph/data
                      :active?
                      numbers->categories)]
    {:confusion (->> (map vector actual predicted)
                     frequencies)
     :accuracy   (fun// (-> (fun/eq actual predicted)
                           (fun/sum))
                       (tc/row-count actual))}))

(classification-test-ctx->measures
 classification-test-ctx1)



(def classification-pipe2
  (ml/pipeline
   (tc-pipe/add-columns {:log-response-time #(-> %
                                                 :response-time 
                                                 fun/log10)
                         :log-prev-response-time #(-> %
                                                      :prev-response-time 
                                                      fun/log10)})
   (mm/select-columns [:log-response-time
                       :log-prev-response-time
                       :joy :positive :anticipation :anger :sadness
                       :hour
                       :stream
                       :safe-sender-id
                       :active?])
   (mm/categorical->one-hot [:stream :safe-sender-id
                             :hour])
   (mm/categorical->number [:active?])
   (mm/set-inference-target :active?)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.classification/logistic-regression
              :lambda 1.0})))

(def classification-trained-ctx2
  (classification-pipe2 {:metamorph/data (:train topic-date-split)
                         :metamorph/mode :fit}))

(def classification-test-ctx2
  (classification-pipe2
   (assoc classification-trained-ctx2
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(defn numbers->categories [column]
  (let [lookup (->> column
                    meta
                    :categorical-map
                    :lookup-table
                    (map (comp vec reverse))
                    (into {}))]
    (-> column
        fun/round
        (->> (map lookup)))))

(classification-test-ctx->measures
 classification-test-ctx2)

;;:bye

