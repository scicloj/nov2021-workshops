(ns inferme-tutorials.plot
  (:require [cljplot.core :as plot]
            [cljplot.build :as b]))

;; Adapting the `inferme.plot` functions for our convenience.

(def default-height 600)
(def default-width 600)

(defn- maybe-add-title
  [plot title]
  (if-not title
    plot
    (b/add-label plot :top title {:font-size 16 :font-style :bold})))

(defn frequencies
  ([data] (frequencies data {}))
  ([data {:keys [x-label y-label sort? title width height]
          :or   {y-label "probability"
                 x-label "data"
                 sort?   true
                 width default-width
                 height default-height}}]
   (-> (plot/xy-chart {:width width :height height}
                      (b/series [:grid]
                                [:frequencies data
                                 {:range? true :sort? sort? :pmf? true}])
                      (b/update-scale :x :fmt (fn [v] (if (nil? v) "/nil/" v)))
                      (b/add-axes :bottom)
                      (b/add-axes :left)
                      (b/add-label :bottom x-label)
                      (b/add-label :left y-label)
                      (maybe-add-title title))
       :buffer)))

(defn histogram
  ([data] (histogram data {}))
  ([data {:keys [x-label y-label bins title width height]
          :or   {y-label "probability"
                 x-label "data"
                 width default-width
                 height default-height}}]
   (-> (plot/xy-chart {:width width :height height}
                      (b/series [:grid]
                                [:histogram data
                                 {:bins bins :density? true :padding-out -0.1 :stroke? false}]
                                [:density data {:color [10 10 10 80] :area? true}])
                      (b/update-scale :x :fmt (fn [v] (if (nil? v) "/nil/" v)))
                      (b/add-axes :bottom)
                      (b/add-axes :left)
                      (b/add-label :bottom x-label)
                      (b/add-label :left y-label)
                      (maybe-add-title title))
       :buffer)))


(defn lag
  ([d] (lag d {}))
  ([d {:keys [width height]
       :or   {width  default-width
              height default-height}}]
   (-> (plot/xy-chart {:width width :height height}
                      (b/series [:grid nil {:position [0 1]}] [:acf d {:lags 50 :position [0 1] :label "ACF"}]
                                [:grid nil {:position [0 0]}] [:pacf d {:lags 50 :position [0 0] :label "PACF"}])
                      (b/update-scales :x :fmt int)
                      (b/add-axes :bottom)
                      (b/add-axes :left)
                      (b/add-label :bottom "lag")
                      (b/add-label :left "autocorrelation"))
       (plot/show))))
