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
   (s/replace #"[,|.|;|:|-]" "")
   (s/replace #"[,|.|;|:|-|=|+]" "")
   s/lower-case
   (s/split #" ")))

(words  "Sets a sentiment score to a string. The higher the positive number, the more
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

(->
 (tc/dataset "data/prepared-messages.csv")
 (tc/add-column :message-word-count #(map (fn [content]
                                            (->> content
                                                 words
                                                 (remove nil?)
                                                 count)) (get % "content")))
 (tc/add-column :afinn-sentiment #(map (partial sentiment afinn-lexicon (partial reduce +)) (get % "content")))
 (tc/add-column :trust #(map (partial sentiment (nrc-emotion-lexicon :trust) (partial count)) (get % "content"))) 
 (tc/add-column :surprise #(map (partial sentiment (nrc-emotion-lexicon :surprise) (partial count)) (get % "content"))) 
 (tc/add-column :joy #(map (partial sentiment (nrc-emotion-lexicon :joy) (partial count)) (get % "content"))) 
 (tc/add-column :positive #(map (partial sentiment (nrc-emotion-lexicon :positive) (partial count)) (get % "content"))) 
 (tc/add-column :negative #(map (partial sentiment (nrc-emotion-lexicon :negative) (partial count)) (get % "content"))) 
 (tc/add-column :anticipation #(map (partial sentiment (nrc-emotion-lexicon :anticipation) (partial count)) (get % "content"))) 
 (tc/add-column :anger #(map (partial sentiment (nrc-emotion-lexicon :anger) (partial count)) (get % "content"))) 
 (tc/add-column :sadness #(map (partial sentiment (nrc-emotion-lexicon :sadness) (partial count)) (get % "content"))) 
 (tc/add-column :fear #(map (partial sentiment (nrc-emotion-lexicon :fear) (partial count)) (get % "content")))
 (tc/select-columns [:surprise :joy :positive :negative :anticipation :anger :sadness :trust :fear :message-word-count]))






