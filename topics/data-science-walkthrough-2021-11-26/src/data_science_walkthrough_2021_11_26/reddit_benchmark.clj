(ns reddit-benchmark
  (:require [tablecloth.api :as tc]
            [data-science-walkthrough-2021-11-26.sentiments :as sentiments]))

(def reddit (->
             (tc/dataset "/home/sakalli/downloads/reddit/reddit_database.csv"  {:key-fn keyword})
             (tc/drop-rows (complement :post))
             (tc/rename-columns  {:post :content})
             (tc/select-columns [:content :subreddit])
             (tc/random 50000)
             sentiments/add-sentiments))


(sentiments/aggregate-mean-afinn-score reddit)

;; | :word-count | :afinn-sentiment | :sentiment |
;; |------------:|-----------------:|-----------:|
;; |   6347109.0 |         218697.0 | 0.03445616 |


(sentiments/aggregate-mean-sentiments reddit)

;; {:afinn-sentiment 0.0344561594893045,
;;  :surprise 0.005903947765825354,
;;  :message-word-count 1.0,
;;  :joy 2.142707806026334E-5,
;;  :positive 1.1233460777182179E-4,
;;  :negative 6.144529737869635E-6,
;;  :anticipation 1.5928511705092823E-4,
;;  :anger 8.23997193052774E-5,
;;  :sadness 2.5176816720809425E-4,
;;  :trust 0.19036887502640967,
;;  :fear 0.0}





