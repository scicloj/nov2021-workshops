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

(defn our-split [ds]
  (let [sds (-> ds
                (tc/group-by [:subject])
                (tc/without-grouping-> (tc/split :holdout)))
        select-split (fn [split-name split-ds]
                      (-> split-ds
                          (tc/without-grouping->
                           (tc/select-rows
                            (comp (partial = split-name) :$split-name)))
                          (tc/ungroup)))]
    {:train (select-split :train sds) 
     :test  (select-split :test sds)}))

(def split-pair
  (our-split (-> messages
                 (tc/select-columns (comp (partial not= 2018) :year)))))


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

(defn score [split-pair features target-column model-type]
  (let [all-columns (conj features target-column)
        train-ds (tc/select-columns (:train split-pair) all-columns)
        test-ds  (tc/select-columns (:test split-pair) all-columns)
        model (-> train-ds
                  (tmd/categorical->number [:active?])
                  (tmd-model/set-inference-target :active?)
                  (ml/train {:model-type model-type}))
        predicted (ml/predict test-ds model)]
    (let [actual (-> test-ds :active? dtype/->float-array)
          predictions (:active? predicted)]
      (fun//
       (-> (fun/eq actual predictions)
           (fun/sum))
       (tc/row-count test-ds)))))

;; this yields a ds with predictions and i think related probabilities
;; how do we analyze this further. need to join this result again with
;; original data?
(score
 split-pair
 [:year #_:dayofmonth #_:dayofweek]
 :active?
 :smile.classification/decision-tree)


;; pipeline that works but no scoring yet
(def mypipe
  (ml/pipeline
   (tc-pipe/select-columns [:year :active?])
   (mm/categorical->number [:active?])
   (mm/set-inference-target [:active?])
   (mm/model {:model-type :smile.classification/decision-tree})))

(def trained-ctx
  (mypipe {:metamorph/data (:train split-pair)
           :metamorph/mode :fit}))

trained-ctx


(def test-ctx
  (mypipe
   (assoc trained-ctx
          :metamorph/data (:test split-pair)
          :metamorph/mode :transform)))

(-> test-ctx
    :metamorph/data
    (tech.v3.dataset.modelling/column-values->categorical :active?)
    (#(tc/add-column (:test split-pair) :predicted %)))


