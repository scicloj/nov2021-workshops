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
(require '[scicloj.viz.api :as viz])

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

(require '[aerial.hanami.templates :as ht]
         '[scicloj.viz.api :as viz])

(-> (tc/dataset {:x (range 100)
                 :y (repeatedly 100 #(rand-int 10))})
    (viz/data)
    (viz/type ht/point-chart)
    (viz/viz))

(-> messages-with-features
    (time/index-by :date-time)
    (time/adjust-frequency time/->months-end
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

^kind/hiccup
[:div
 [:h2 "Monthly message volume by year"]
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
      (viz/viz))]]

(-> [{:name "scicloj"
      :afinn-sentiment 0.04007270387206309,
      :surprise 0.00433005641053727,
      :message-word-count 1.0,
      :joy 9.127918651988973E-6,
      :positive 6.731840005841868E-5,
      :negative 3.422969494495865E-6,
      :anticipation 1.2550888146484839E-5,
      :anger 5.24855322489366E-5,
      :sadness 1.4376471876882633E-4,
      :trust 0.14719453420231118,}
     {:name "ds subreddit"
      :afinn-sentiment 0.0344561594893045,
      :surprise 0.005903947765825354,
      :message-word-count 1.0,
      :joy 2.142707806026334E-5,
      :positive 1.1233460777182179E-4,
      :negative 6.144529737869635E-6,
      :anticipation 1.5928511705092823E-4,
      :anger 8.23997193052774E-5,
      :sadness 2.5176816720809425E-4,
      :trust 0.19036887502640967}]
    tc/dataset
    (tc/drop-columns [:message-word-count])
    (tc/update-columns {:afinn-sentiment #(map (partial * 100) %)
                        :positive #(map (partial * 100000) %)
                        :sadness #(map (partial * 100000) %)
                        :joy #(map (partial * 1000000) %)
                        :anticipation #(map (partial * 1000000) %)
                        :anger #(map (partial * 1000000) %)
                        :negative #(map (partial * 1000000) %)
                        :surprise #(map (partial * 1000000) %)


                        }
                       )
    (tc/pivot->longer (complement #{:name}))
    (tc/pivot->wider :name :$value))

;; splitting the data
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


^kind/hiccup
 [:p/vega
  (-> topic-date-split
      :train
      :next-response-time
      (tech.viz.vega/histogram :next-response-time))]


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


;; relationship features next response times
^kind/hiccup
(-> [
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

;; # ml




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


(require '[scicloj.ml.core :as ml]
         '[scicloj.metamorph.ml.loss :as loss]
         '[scicloj.ml.metamorph :as mm])

(def regression-pipe
  (ml/pipeline
   (mm/add-columns {:log-response-time #(-> % :response-time fun/log10)
                    :log-next-response-time #(-> % :next-response-time fun/log10)})
   (mm/select-columns [:log-response-time
                       :log-next-response-time])
   (mm/set-inference-target :log-next-response-time)
   {:metamorph/id :model}
   (mm/model {:model-type :smile.regression/ordinary-least-square}))
  )

(def regression-trained-ctx1
  (regression-pipe {:metamorph/data (:train topic-date-split)
                     :metamorph/mode :fit}))

(-> regression-trained-ctx1
    :model
    ml/explain)


(def regression-test-ctx1
  (regression-pipe
   (assoc regression-trained-ctx1
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(-> regression-trained-ctx1
    (update-in [:model :model-data] dissoc :model-as-bytes))



(require '[tech.v3.datatype.statistics :as stats])

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

(def classification-pipe1
  (ml/pipeline
   (mm/add-columns {:log-response-time #(-> %
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
   (mm/add-columns {:log-response-time #(-> %
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
 classification-test-ctx1)

(classification-test-ctx->measures
 classification-test-ctx2)

