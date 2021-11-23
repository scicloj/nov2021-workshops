(ns analysis)

(require '[tablecloth.api :as table]
         '[scicloj.notespace.v4.api :as notespace])

(comment
  (notespace/restart! {:open-browser? true})

  (notespace/restart-events!)
  ,)

(def messages (table/dataset "prepped-data.csv" {:key-fn keyword}))

(table/head messages)

(table/shape messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Splitting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn our-split [ds]
  (let [sds (-> ds
                (table/group-by [:subject])
                (table/without-grouping-> (table/split :holdout)))
        select-split (fn [split-name split-ds]
                      (-> split-ds
                          (table/without-grouping->
                           (table/select-rows
                            (comp (partial = split-name) :$split-name)))
                          (table/ungroup)))]
    {:train (select-split :train sds) 
     :test  (select-split :test sds)}))

(def split-pair
  (our-split messages))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modelling 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require '[scicloj.ml.core :as ml]
         '[tech.v3.datatype.statistics :as stats]
         '[tech.v3.dataset.modelling :refer [set-inference-target]])

(defn score [split-pair features target-column model-type]
  (let [train-ds (table/select-columns (:train split-pair) features)
        test-ds  (table/select-columns (:test split-pair) features)
        model (-> train-ds
                  (set-inference-target target-column)
                  (ml/train {:model-type model-type}))
        predictions (ml/predict test-ds model)]
    (ml/mae (target-column predictions) (target-column test-ds))))

(score
 split-pair
 [:year]
 :active?
 :smile.classification/decision-tree)


(def model(-> split-pair
              :train
              (table/select-columns [:month :year :active?])
              (set-inference-target :active?)
              (ml/train {:model-type :smile.classification/decision-tree})
              ;; (table/columns)
              ))


(ml/predict (-> split-pair :test (table/select-columns [:year :month :active?]))
            model)










