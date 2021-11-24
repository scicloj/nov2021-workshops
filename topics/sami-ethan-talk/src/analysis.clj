(ns analysis)

(require '[tablecloth.api :as table]
         '[scicloj.notespace.v4.api :as notespace]
         '[scicloj.kindly.kind :as kind])

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
         '[tech.v3.dataset :as tmd]
         '[tech.v3.datatype.statistics :as stats]
         '[tech.v3.dataset.modelling :refer [set-inference-target] :as tmd-model])

(defn score [split-pair features target-column model-type]
  (let [all-columns (conj features target-column)
        train-ds (table/select-columns (:train split-pair) all-columns)
        test-ds  (table/select-columns (:test split-pair) all-columns)
        model (-> train-ds
                  (tmd/categorical->number [:active?])
                  (set-inference-target target-column)
                  (ml/train {:model-type model-type}))
        predictions (ml/predict test-ds model)]
    (table/bind test-ds predictions)))

;; this yields a ds with predictions and i think related probabilities
(score
 split-pair
 [:year]
 :active?
 :smile.classification/decision-tree)


