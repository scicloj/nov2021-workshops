(ns analysis)

(require '[tablecloth.api :as table]
         '[scicloj.notespace.v4.api :as notespace])

;; (notespace/restart-events!)

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
