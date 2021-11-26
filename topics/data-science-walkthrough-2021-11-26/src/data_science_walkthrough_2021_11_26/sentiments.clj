(ns sentiments
  (:require [tablecloth.api :as tc]
            [tech.v3.datatype :refer [emap] :as dtype]
            [tech.v3.datatype.functional :as fun]
            [tech.v3.dataset :as tmd]
            [tech.v3.datatype.datetime :as dtype-dt]
            [clojure.string :as s]))

(def afinn-lexicon
  (->>  "lexicons/AFINN/AFINN-111.txt"
        slurp
        (#(s/split %  #"\n"))
        (map #(s/split % #"\t"))
        (reduce (fn [acc [k v]]
                  (assoc acc k (read-string v))) {})))

(def nrc-lexicon
 (->> "lexicons/NRC-Emotion-Lexicon/NRC-Emotion-Lexicon-v0.92/NRC-Emotion-Lexicon-Wordlevel-v0.92.txt"
      slurp 
      (#(s/split % #"\t0\r\n"))
      (map #(s/split % #"\t"))
      (reduce (fn [acc [k v]]
                (assoc acc k (keyword v))) {})))
(defn words
  [content]
  (->
   (s/replace content #"<[^>]+>" "")
   (s/replace #"\n" " ")
   (s/replace #"[,|.|;|:|-|=|+]" "")
   s/lower-case
   (s/split #" ")))

#_(words  "Sets a sentiment score to a string. The higher the positive number, the more
  positive the sentiment; the lower the negative number, the more negative the
  sentiment.")


(defn
  sentiment
  [lexicon aggregation-func content]
  (->> content
       words
  (map lexicon)
  (remove nil?)
  aggregation-func))


(defn nrc-emotion-lexicon
  "Returns a map with one of the following emotions:
  #{:surprise :joy :positive :negative :anticipation :anger :sadness :trust :fear}"
  [emotion]
  (->> nrc-lexicon
       (filter (comp #(= % emotion) second))
       (into {})))

(defn add-sentiment
  [lexicon f ds]
  (map (partial sentiment lexicon f)
       (:content ds)))

(comment
 (def mess-w-sentiments
   (->
    (tc/dataset "data/prepared-messages.csv"  {:key-fn keyword})
 (tc/add-column :message-word-count #(map (fn [content]
                                            (->> content
                                                 words
                                                 (remove nil?)
                                                    count))  (:content %)))
    (tc/add-column :afinn-sentiment (partial add-sentiment afinn-lexicon (partial reduce +)))
    (tc/add-column :trust (partial add-sentiment (nrc-emotion-lexicon :trust) count))
    (tc/add-column :surprise (partial add-sentiment (nrc-emotion-lexicon :surprise) count))
    (tc/add-column :joy (partial add-sentiment (nrc-emotion-lexicon :joy) count))
    (tc/add-column :positive (partial add-sentiment (nrc-emotion-lexicon :positive) count))
    (tc/add-column :negative (partial add-sentiment (nrc-emotion-lexicon :negative) count))
    (tc/add-column :anticipation (partial add-sentiment (nrc-emotion-lexicon :anticipation) count))
    (tc/add-column :anger (partial add-sentiment (nrc-emotion-lexicon :anger) count))
    (tc/add-column :sadness (partial add-sentiment (nrc-emotion-lexicon :sadness) count))
    (tc/add-column :fear (partial add-sentiment (nrc-emotion-lexicon :fear) count))))


 (-> mess-w-sentiments
     (tc/select-columns [:surprise :joy :positive :negative :anticipation :anger :sadness :trust :fear :message-word-count :sender-id]))  
 (-> mess-w-sentiments
     (tc/group-by :topic)
     (tc/aggregate {:word-count #(fun/sum (:message-word-count %))
                    :afinn-sentiment #(fun/sum (:afinn-sentiment %))})
     (tc/add-column :sentiment #(fun// (:afinn-sentiment %) (:word-count %)))
     (tc/order-by :sentiment :desc)
     (tc/head))

  (-> mess-w-sentiments
     (tc/group-by :topic)
     (tc/aggregate {:word-count #(fun/sum (:message-word-count %))
                    :afinn-sentiment #(fun/sum (:afinn-sentiment %))})
     (tc/add-column :sentiment #(fun// (:afinn-sentiment %) (:word-count %)))
     (tc/order-by :sentiment :desc)
     (tc/tail))
 )









