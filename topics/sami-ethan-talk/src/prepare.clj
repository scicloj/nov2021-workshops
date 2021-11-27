(ns prepare)

(require '[scicloj.notespace.v4.api :as notespace]
         '[scicloj.kindly.kind :as kind])


(comment
  (notespace/restart! #_{:open-browser? true})
  ,)

(def raw-data (read-string
               (slurp "./zulip-scicloj.txt")))

(take 1 raw-data)

(require '[tablecloth.api :as tc]
         '[tablecloth.time.api :as time]
         '[tech.v3.datatype :refer [emap] :as dtype]
         '[tech.v3.datatype.functional :as fun]
         '[tech.v3.datatype.datetime :as dtype-dt]
         '[clojure.string :as s])

(-> raw-data first keys)

(def messages
  (-> raw-data
      (tc/dataset)
      (tc/select-columns [:subject
                             :sender_id
                             :timestamp
                             :content
                             :display_recipient])
      (tc/rename-columns {:display_recipient :stream
                             :subject           :topic
                             :sender_id         :sender-id})))

^kind/dataset
(tc/head messages)

^kind/dataset
(tc/info messages)

(defn secs-since-different-sender [sender gap-duration]
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

(defn add-conversation-flow-features [ds]
  (let [prompt-response-threshold (* 60 60 12)]
    (-> ds
        (tc/group-by [:topic :stream])
        (tc/add-column :same-sender-as-last?
                       #(let [sender-id (:sender-id %)]
                          (fun/eq sender-id (fun/shift sender-id 1))))
        (tc/add-column :secs-since-last
                       #(let [timestamp (:timestamp %)]
                          (fun/- timestamp (fun/shift timestamp 1))))
        (tc/add-column :secs-since-diff-sender
                       #(secs-since-different-sender
                         (:sender-id %) (:secs-since-last %)))
        ;; in a sequence of msgs by the same user, let's drop all buth
        ;; the first, i.e. those that have `nil` :values in this row
        ;; created above.
        (tc/drop-rows (complement :secs-since-diff-sender))
        (tc/add-column :prompt-response?
                       #(fun/< (:secs-since-diff-sender %)
                               prompt-response-threshold))
        (tc/add-column :secs-until-next-response
                       #(fun/shift (:secs-since-diff-sender %) -1))
        (tc/add-column :next-response-prompt?
                       #(fun/< (fun/shift (:secs-since-diff-sender %) -1)
                               prompt-response-threshold))
        (tc/add-column :active?
                       #(emap
                         (fn [next-response-prompt? prompt-response?]
                           (if next-response-prompt? true
                               (if prompt-response? true false)))
                         :boolean
                         (:next-response-prompt? %)
                         (:prompt-response? %)))
        (tc/ungroup))))

(defn add-time-features [ds]
  (-> ds
      (tc/add-column
       :date-time
       #(emap
         (fn [seconds-ts]
           (time/milliseconds->anytime (* 1000 seconds-ts) :local-date-time))
         :local-date-time
         (:timestamp %)))
      (tc/add-columns {:local-date #(emap
                                     (fn [t]
                                       (time/convert-to t :local-date))
                                     :local-date (:date-time %))
                       :month (fn [ds]
                                (emap #(time/month % {:as-number? true})
                                      :int32
                                      (:date-time ds)))
                       :dayofweek (fn [ds]
                                    (emap #(time/dayofweek % {:as-number? true})
                                          :int32
                                          (:date-time ds)))
                       :hour #(emap time/hour :int32 (:date-time %))
                       :year #(emap time/year :int32 (:date-time %))})))

(defn add-message-content-features [ds]
  (-> ds
      (tc/add-columns {:keyword-? (fn [ds]
                                    (emap #(s/includes? % "?")
                                          :boolean
                                          (:content ds)))})
      (tc/drop-columns [:content])))


(def messages-with-features
 (-> messages
    add-time-features
    add-conversation-flow-features
    add-message-content-features
    ))

^kind/dataset
(tc/head messages-with-features)


;; (table/write! messages-active? "prepped-data.csv")















(require '[scicloj.viz.api :as viz])
(require '[aerial.hanami.templates :as ht])

(-> messages-active?
    (table/group-by [:year :month])
    (table/aggregate {:active? #(-> % :active? count)})
    (table/order-by [:year :month])
    (viz/data)
    (viz/type ht/line-chart)
    (viz/x :month)
    (viz/y :active?)
    (viz/color "year")
    (viz/viz))


(comment
 (->> messages
;;;; specific analysis from here
      #_(table/add-column :active-and-not-same-sender?
                          #(map (fn [ac ssal] (and ac (not ssal)))
                                (:active? %) (:same-sender-as-last? %)))
      (table/select-rows (complement :same-sender-as-last?))
      (table/group-by [:subject :active?])
      (table/aggregate {:count #(-> % :active? count)
                        :first-post #(-> % :timestamp first)
                        :last-post #(-> % :timestamp last)
                        :mean-gap (comp #(/ % 3600)
                                        fun/mean :secs-since-diff-sender)})
      #_(table/select-rows (comp not not :active?)) ;; is this a tablecloth bug? if i enter the keyword it filters out everything
      #_(table/order-by :count :desc)
      (table/order-by :subject :active? :desc)

      #_(table/aggregate-columns [:active? :timestamp] [#(count %)
                                                        #(first %)])

      #_(table/select-rows (comp #(= % "preferred notebook") :subject))
      #_(table/drop-columns [:id :type :stream_id :timestamp :sender_id #_:secs-since-last :same-sender-as-last? :content])
      #_(table/select-columns [:sender_full_name :secs-since-diff-sender :active?])
      #_(table/select-columns [:topic :active? :same-sender-as-last? :active-and-not-same-sender?])
      (vary-meta merge {:print-column-max-width 50}))

;; was the next after a quick response.
;; messeges
;; check if the forward looking is consistence.
;; looking at. messages. descriptive statistics.
;; introduction, ourselves, get the quickstart going what our goals are in terms of the talk.
;; fastmath
;; scicloj-ml
;; hanami
;; fun
;; slide of libraries. we may draw upon.
;; 


)
(comment

  (def test-data
    [["a" 2]
     ["b" 3]
     ["b" 2]
     ["a" 3]
     ["b" 2]
     ["c" 3]
     ["c" 3]
     ["b" 2]])
  ;; adding since last sender time (adding times until sender changes)
  (def test-data
    [["a" 2 2]
     ["b" 3 0]
     ["b" 2 5]
     ["a" 3 3]
     ["b" 2 2]
     ["c" 3 0]
     ["c" 3 6]
     ["b" 2 2]])

  ;; shifting time since sender change with one to look forward
  (def test-data
    [["a" 2 2 0]
     ["b" 3 0 5]
     ["b" 2 5 3]
     ["a" 3 3 2]
     ["b" 2 2 0]
     ["c" 3 0 6]
     ["c" 3 6 2]
     ["b" 2 2 nil]])

  ;; test if next sender wait is under 5, ie post is active,
  ;; as i generates prompt response
  (def test-data
    [["a" 2 2 0   true]
     ["b" 3 0 5   false]
     ["b" 2 5 3   true]
     ["a" 3 3 2   true]
     ["b" 2 2 0   true]
     ["c" 3 0 6   false]
     ["c" 3 6 2   true]
     ["b" 2 2 nil nil]])

  ;;
  (def test-data
    [["a" 2 2 0   true]
     ["b" 3 0 5   false]
     ["b" 2 5 3   true]
     ["a" 3 3 2   true]
     ["b" 2 2 0   true]
     ["c" 3 0 6   false]
     ["c" 3 6 2   true]
     ["b" 2 2 nil nil]])

  (reverse
   (reduce (fn [acc current]
             (let [last (first acc)
                   last-sender (first last)
                   last-gap-duration (second last)
                   current-sender (first current)
                   current-gap-duratiion (second current)]
               (if (= current-sender last-sender)
                 (conj (rest acc) [last-sender 0] [current-sender (+ current-gap-duratiion
                                                                     last-gap-duration)])
                 (conj acc current)))) '() test-data))
;;(["a" 2] ["b" 0] ["b" 5] ["a" 3] ["b" 2] ["c" 0] ["c" 6] ["b" 2])

  (def test-data-col-name
    ["a" "b" "b" "a" "b" "c" "c" "b"])
  (def test-data-col-gap-duration
    [2 3 2 3 2 3 3 2]))

