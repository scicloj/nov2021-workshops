(ns datavis-tutorials.hanami
  (:require [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.api :as kindly]
            [scicloj.kindly.kind :as kind]
            [aerial.hanami.common :as hc]
            [aerial.hanami.templates :as ht]
            [scicloj.viz.api :as viz]
            [tablecloth.api :as tc]))


;; # Hanami tutorial

;; ## Using Notespace

;; For more details, see the [Notespace v4 tutorial](https://scicloj.github.io/notespace/doc/scicloj/notespace/v4/tutorial-test/index.html).

;; ### (re)start

(comment
  (notespace/restart!
   {:open-browser? true}))

;; ### troubleshooting

(comment
  (notespace/restart-events!))

;; ## Some data to visualize

;; A random walk as a dataset:

(def random-walk-dataset
  (let [n 40]
    (-> {:x (range n)
         :y (->> #(- (rand) 0.5)
                 (repeatedly n)
                 (reductions +))}
        (tc/dataset {:dataset-name "random-walk"}))))

random-walk-dataset

;; A random walk as a collection of maps:

(-> random-walk-dataset
    (tc/rows :as-maps))

;; ## Writing a Vega-Lite spec

(def random-walk-vl-spec
  {:encoding
   {:y {:field "y", :type "quantitative"},
    :x {:field "x", :type "quantitative"},
    :tooltip
    [{:field "x", :type "quantitative"} {:field "y", :type "quantitative"}]},
   :mark       {:type "line"},
   :width      400,
   :height     100,
   :data
   {:values (-> random-walk-dataset
                (tc/rows :as-maps))}})

;; We can tell Notespace to render as Vega using a metadata tag at the code:

^kind/vega
random-walk-vl-spec

;; We can also do it by marking the resulting value's metadata like this:
(-> random-walk-vl-spec
    (kindly/consider kind/vega))

;; ## Writing a Vega-Lite spec using Hanami

(def random-walk-vl-spec-using-hanami
  (hc/xform ht/line-chart
            :DATA (-> random-walk-dataset
                      (tc/rows :as-maps))
            :HEIGHT 100))

random-walk-vl-spec-using-hanami

(-> random-walk-vl-spec-using-hanami
    (kindly/consider kind/vega))

;; ## Using Hanami through viz.clj

(-> random-walk-dataset
    (viz/data)
    (viz/type ht/line-chart)
    (viz/viz {:HEIGHT 100}))

:bye
