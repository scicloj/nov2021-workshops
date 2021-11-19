(ns fiddle)

(comment
  ;;---- starting the repl (start in command line in the folder where you are saving this file)
  clojure -Sdeps "$(curl -sL https://bit.ly/johnsdeps)" -M:ad-hoc/data-science:alpha/hot-load:repl/cider -r
  ;;---- once started just cider-connect-clj to the port displayed in terminal ^^^
  )

(require '[clojure.tools.deps.alpha.repl :refer [add-libs]])

(add-libs '{scicloj/notespace {:mvn/version "4-alpha-13"}})

(require '[scicloj.notespace.v4.api :as notespace]
         '[scicloj.kindly.kind :as kind])

(comment
  (notespace/restart!)

  (notespace/restart-events!)
  (notespace/stop!)

  (scicloj.notespace.v4.frontend.engine/stop!)

  )

;;;;
(add-libs '{scicloj/tablecloth {:mvn/version "6.025"}})



(require '[tablecloth.api :as table])

(require '[tech.v3.datatype.functional :as fun])


;;(def raw-data (read-string (slurp "../zulip-scicloj.txt")))
(def raw-data (read-string (slurp "./zulip-scicloj.txt")))

(-> raw-data first keys)

(take 1 raw-data)

(def data (->> raw-data
               (map #(select-keys %
                                  [:id
                                   :stream_id
                                   :subject
                                   :sender_id
                                   :sender_full_name
                                   :type
                                   :timestamp
                                   :content]))
               (#(table/dataset %
                                #_{:parser-fn
                                   {:timestamp [:local-date-time
                                                (fn [el] (dtdt/milliseconds->datetime
                                                         :local-date-time (* 1000 el)))]}}))))

^kind/dataset
(table/head data)

^kind/dataset
(table/info data)

^kind/dataset
(-> data
    (table/select-rows (comp #(= "local data science courses" %) :subject)))

(table/head data-with-diff)

(def prompt-response-threshold (* 60 60 12))

(def test-data
  [["a" 2]
   ["b" 3]
   ["b" 2]
   ["a" 3]
   ["b" 2]
   ["c" 3]
   ["c" 3]
   ["b" 2]])




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
 [2 3 2 3 2 3 3 2])

(-> {:1 test-data-col-name
     :2 test-data-col-gap-duration}
    table/dataset
    (table/group-by :1)
    (table/group-by :2)
    )

    (table/group-by )

(map #(identity [%1 %2]) test-data-col-name test-data-col-gap-duration)
;;(["a" 2] ["b" 3] ["b" 2] ["a" 3] ["b" 2] ["c" 3] ["c" 3] ["b" 2])

(defn secs-since-different-sender
  [sender gap-duration]
 (->>
  (map #(identity [%1 %2]) sender gap-duration)
  (reduce (fn [acc current]
            (let [last (first acc)
                  last-sender (first last)
                  last-gap-duration (second last)
                  current-sender (first current)
                  current-gap-duratiion (second current)]
              (if (= current-sender last-sender)
                (conj (rest acc) [last-sender 0] [current-sender (+ current-gap-duratiion
                                                                    last-gap-duration)])
                (conj acc current)))) '())
  reverse
  (map second)))

(count
 (secs-since-different-sender test-data-col-name test-data-col-gap-duration))

(count test-data-col-gap-duration)




(-> data
    (table/order-by :timestamp)
    (table/group-by :subject)
    ;; create a flag to point when the previous message was posted by the same user
    (table/add-column :same-sender-as-last?
                      #(let [sender-id (:sender_id %)]
                         (fun/eq sender-id (fun/shift sender-id 1))))
    (table/add-column :secs-since-last
                      #(let [timestamp (:timestamp %)]
                         (fun/- timestamp (fun/shift timestamp 1))))
    (table/add-column :secs-since-diff-sender
                      #(secs-since-different-sender (:sender_id %) (:secs-since-last %)))
    (table/add-column :prompt-response?
                      ;; #(fun/< (:secs-since-last %)
                      #(fun/< (:secs-since-diff-sender %)
                              prompt-response-threshold))
    (table/add-column :next-response-prompt?
                      ;; #(fun/< (fun/shift (:secs-since-last %) -1)
                      #(fun/< (fun/shift (:secs-since-diff-sender %) -1)
                              prompt-response-threshold))
    ;; calculate the time since post by a user that is other than the current poster
    (table/add-column :active-conversation?
                      #(tech.v3.datatype/emap
                        ;; (fn [same-sender? secs-since-last prompt-response?]
                        ;;   (if-not same-sender?
                        ;;     (< secs-since-last threshold)
                        ;;     (or (= 0 secs-since-last)
                        ;;         (= prompt-response? true))))
                        (fn [next-response-prompt?]
                          (if next-response-prompt? true false))
                        :boolean
                        (:next-response-prompt? %)
                        ))
    ;; mark all posts active or inactive based on time since poster switch
    ;; mark all points when a conversation is finished?
    ;; decide heuristics: how long is a break so that a conversation turns inactive
    ;; 3 hours?
    (table/ungroup)

    ;;;; specific analysis from here

    #_(table/add-column :active-and-not-same-sender? #(map (fn [ac ssal] (and ac (not ssal)))
                                                         (:active-conversation? %)
                                                         (:same-sender-as-last? %)))
    (table/select-rows (comp not :same-sender-as-last?))
    (table/group-by [:subject :active-conversation?])
    (table/aggregate {:count #(-> % :active-conversation? count)
                      :first-post #(-> % :timestamp first)
                      :last-post #(-> % :timestamp last)})

    #_(table/aggregate-columns [:active-conversation? :timestamp] [#(count %)
                                                                 #(first %)])

    #_(table/select-rows (comp #(= % "preferred notebook") :subject))
    #_(table/drop-columns [:id :type :stream_id :timestamp :sender_id #_:secs-since-last :same-sender-as-last? :content])
    #_(table/select-columns [:sender_full_name :secs-since-diff-sender :active-conversation?])
    #_(table/select-columns [:topic :active-conversation? :same-sender-as-last? :active-and-not-same-sender?])
    (vary-meta merge {:print-column-max-width 50})
    )
