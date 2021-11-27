(ns analysis)

(require '[tablecloth.api :as tc]
         '[scicloj.notespace.v4.api :as notespace]
         '[scicloj.kindly.kind :as kind])

(comment
  (notespace/restart! {:open-browser? true})

  (notespace/restart-events!)
  ,)

(def messages (tc/dataset "prepped-data.csv" {:key-fn keyword}))

(tc/head messages)

(tc/shape messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def topic-date-split
  (-> messages
      (tc/group-by [:topic :local-date])
      (tc/drop-rows (comp #(= % 2018) :year))
      (tc/without-grouping->
       (tc/split :holdout {:seed 1})
       (tc/add-column :data
                      #(map (fn [data split-name]
                              (tc/add-column data :$split-name split-name))
                            (:data %)
                            (:$split-name %))))
      (tc/ungroup)
      (tc/group-by :$split-name {:result-type :as-map})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exploring & Visualizing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[scicloj.viz.api :as viz]
         '[aerial.hanami.templates :as ht])

(-> topic-date-split
    :train
    (tc/group-by [:keyword-?])
    (tc/aggregate {:median-response-time
                   (fn [ds]
                     (-> ds
                         :secs-until-next-response
                         fun/median))}))


(defn show-median-bar-chart [ds feature-column]
  (-> ds
      (tc/group-by [feature-column])
      (tc/aggregate
       {:median #(fun/median (:secs-until-next-response %))})
      (tc/order-by feature-column)
      (viz/data)
      (viz/type ht/bar-chart)
      (viz/x feature-column {:type :nominal})
      (viz/y :median)
      (viz/viz)))

(show-median-bar-chart (:train topic-date-split) :keyword-?)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modelling 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[scicloj.ml.core :as ml]
         '[scicloj.ml.metamorph :as mm]
         '[tablecloth.pipeline :as tc-pipe]
         '[tech.v3.datatype :as dtype]
         '[tech.v3.datatype.functional :as fun]
         '[tech.v3.dataset :as tmd]
         '[tech.v3.dataset.modelling :as tmd-model]
         '[tech.v3.datatype.statistics :as stats])

;; pipeline that works but no scoring yet
(def ols-pipe1
  (ml/pipeline
   (tc-pipe/select-columns [:year :secs-until-next-response])
   (mm/categorical->one-hot [:year])
   (mm/set-inference-target :secs-until-next-response)
   {:metamorph/id :trained-model}
   (mm/model {:model-type :smile.regression/ordinary-least-square})))


(def trained-ctx
  (ols-pipe1 {:metamorph/data (:train topic-date-split) 
             :metamorph/mode :fit}))

(-> trained-ctx
    :trained-model
    ml/explain)

(def test-ctx
  (ols-pipe1
   (assoc trained-ctx
          :metamorph/data (:test topic-date-split)
          :metamorph/mode :transform)))

(-> test-ctx
    :metamorph/data)

(let [actual    (-> topic-date-split
                    :test
                    :secs-until-next-response)
      predicted (-> test-ctx
                    :metamorph/data
                    :secs-until-next-response)]
  (loss/mae predicted actual))

;; (* 60 60 24)

;; regularization





