(ns statistics-is-easy.workshop-1
  (:require [aerial.hanami.templates :as ht]
            [aerial.hanami.common :as hc]
            [kixi.stats.distribution :as kstatsd]
            [kixi.stats.test :as ktest]
            [fastmath.stats :as fstat]
            [kixi.stats.core :as kcore]
            [scicloj.notespace.v4.api :as notespace]
            [scicloj.notespace.v4.config :as config]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.api :as kindly]
            [scicloj.viz.api :as viz]))

^kind/hidden
(comment
  (notespace/restart! {})
  (config/set! {:notes?     true
                :header?    false
                :last-eval? false})
  (notespace/restart-events!)
  (notespace/stop!))

;# An introduction to statistical inference

;## Goals
;- Derive conclusions from first principles 
;- Use randomization techniques known collectively as resampling to derive information from the experiment being conducted
;- To answer questions on how accurate a measurement is likely to be and could it have happened by mistake
;- Some real world applications
;- Practical considerations

;
;;## Is a coin fair if I told you that I observed 15 heads in 17 coin tosses  
;
;Create a large number of buckets to draw from  and split them up based on the p parameter. Any random drawing that draws from the left side is
; considered a success
(defn apply-prob
  [p n]
  (let [draw-space (-> (/ 1 p) (* 1000))]
    (reduce (fn [success draw]
              (if (>= (* p draw-space)  draw)
                (inc success)
                success))
            0
            (repeatedly n #(rand-int draw-space)))))
(apply-prob 0.5 17)
;
;Flip the coin n(17) times (one experiment) Keep track of heads. Repeat this experiment 10000 times
(-> (map (fn [s] {:x s :y 1}) (repeatedly 10000 #(apply-prob 0.5 17)))
    viz/data
    (viz/x :x {:XTITLE "fair coin tosses"})
    (viz/y :y {:YTITLE "number of samples" :YAGG "sum"})
    (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
    (viz/type ht/bar-chart)
    viz/viz)
;
;Do the same by using a binomial distribution from the kixi stats library
;
;A **binomial distribution** with parameters n and p is the discrete probability distribution of the number of p successes in a sequence of n
; independent experiments
;
(-> (map (fn [s] {:x s :y 1}) (kstatsd/sample 10000 (kstatsd/binomial {:n 17 :p 0.5})))
    viz/data
    (viz/x :x {:XTITLE "number of heads out of 17 tosses"})
    (viz/y :y {:YTITLE "number of samples" :YAGG "sum"})
    (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
    (viz/type ht/bar-chart)
    viz/viz)
;Render both distributions, one where the mean is 8 and other where the mean is the observed value
;
(defn render-ab
  [a-prob b-prob trials]
  (-> (hc/xform ht/layer-chart
                :LAYER [(-> (map (fn [s] {:x s :y 1}) (repeatedly 10000 #(apply-prob a-prob trials)))
                            viz/data
                            (viz/x :x {:XTITLE "A"})
                            (viz/y :y {:YTITLE "number of samples" :YAGG "sum"})
                            (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
                            (viz/type ht/bar-chart)
                            viz/viz)
                        (-> (map (fn [s] {:x s :y 1}) (repeatedly 10000 #(apply-prob b-prob trials)))
                            viz/data
                            (viz/x :x {:XTITLE "B"})
                            (viz/y :y {:YTITLE "" :YAGG "sum"})
                            (viz/color  {:value "red"})
                            (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
                            (viz/type ht/bar-chart)
                            viz/viz)])
      (kindly/consider kind/vega)))

(render-ab 0.5 (float (/ 15 17)) 17)

;Is the coin fair. Encode the experiment
(defn experiment
  [observed trials success-probability num-bootstraps]
  (let [experimental-successes (repeatedly num-bootstraps #(apply-prob success-probability trials))
        count-good (count (filter #(>= % observed) experimental-successes))]
    {:count-good count-good
     :num-bootstraps num-bootstraps
     :observed-probability (float (/ count-good num-bootstraps))}))
(experiment 15 17 0.5 10000)

;Do the same thing  as a z test
(def population 17)
(-> {:mu (* population 0.5) :sd (Math/sqrt (* population 0.5 0.5))}
    (ktest/simple-z-test {:mean 15 :n 17})
    ktest/p-value)
;### Other examples
;Take the instance of an online game company that introduces a new feature where the normal retention rate is 70% after introducing the feature 
;we observe that 38 of 97 people revisit the site. Is there a real problem with this feature?
(render-ab 0.7 (float (/ 38 97)) 97)
(experiment 70 100 (float (/ 38 97)) 10000)

;
;When doing an A/B experiment the normal click through rate(Number of users clicking impressions)  is 2%. 
; The adtech company has changed the way ads are being selected for display and observes that of the 900 impressions presented 30 users clicked into the ads. 
; Is the new selection system better
(render-ab 0.02 (float (/ 30 900)) 900)
(experiment 30 900 0.02 10000)
; ### Test to check the effectiveness of a drug
; To check the effectiveness of a drug we test it against a placebo, the numbers indicate the measured improvement
(def placebo [54 51 58 44 55 52 42 47 58 46])
(def drug [54 73 53 70 73 68 52 65 65])

;Measure the observed difference in mean between the drug and the placebo. 
(defn avg
  [s]
  (-> (apply + s)
      (/ (count s))))

(defn avg-diff
  [s1 s2]
  (Math/ceil (- (avg s1) (avg s2))))
(avg-diff drug placebo)
;
;Was this effect seen by pure chance.
;
;Lets run an experiment where many many times we shuffle the labels and check the difference in the average for each shuffled drug. 
;Each time we see a value greater than the observed we increment a counter. 

(defn shuffled-avg-diff
  [sample population]
  (let [all-together (shuffle (concat sample population))
        new-sample (take (count sample) all-together)
        new-population (drop (count sample) all-together)]
    (avg-diff new-sample new-population)))

(shuffled-avg-diff drug placebo)

(->  (repeatedly 10000 #(shuffled-avg-diff drug placebo))
     ((fn [data] (map (fn [r] {:x r :y 1}) data)))
     viz/data
     (viz/x :x {:XTITLE "difference between means"})
     (viz/y :y {:YTITLE "bootstrap samples" :YAGG "sum"})
     (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
     (viz/type ht/bar-chart)
     viz/viz)
;
(defn drug-experiment
  [num-bootstraps drug placebo]
  (let [observed-diff (avg-diff drug placebo)
        experiment-diff (repeatedly num-bootstraps #(shuffled-avg-diff drug placebo))
        count-good (->> experiment-diff
                        (filter #(>= % observed-diff))
                        count)
        observed-probability (float (/ count-good num-bootstraps))]
    {:num-bootstraps num-bootstraps
     :count-good count-good
     :observed-probability observed-probability}))
(drug-experiment 10000 drug placebo)
;Lets look at the results by using a T-test with the kixi stats library.
(def summary-stats
  {:mean kcore/mean
   :sd kcore/standard-deviation
   :n kcore/count})

(def summary-stats-rf
  (redux.core/fuse summary-stats))

;; Then calculate the pair of summaries and pass to t-test:

(-> (ktest/t-test (transduce identity summary-stats-rf placebo)
                  (transduce identity summary-stats-rf drug))
    (ktest/p-value))
;### Why we shuffle
;
;The idea here is if the drug had no real effect then the placebo would often give more improvement than the drug.
;
;By shuffling we are simulating a situation in which some placebo measurements replace the drug measurements. 
;
;If the observed average difference is matched or exceeded then the drug may have no visible effect beyond the placebo
;
;We will look at this by looking at the more _degenerate_ case where we artificially create the difference from a population 
;We take  random widely varying values for a population and simply add the observed difference to each value for the drug.
(def placebo-measure-2 [56 348 162 420 440 250 389 476 288 456])
(def drug-measure-2 (mapv #(+ % 13) placebo-measure-2))

(->  (repeatedly 10000 #(shuffled-avg-diff drug-measure-2 placebo-measure-2))
     ((fn [data] (map (fn [r] {:x r :y 1}) data)))
     viz/data
     (viz/x :x {:XTITLE "difference between means"})
     (viz/y :y {:YTITLE "bootstrap samples" :YAGG "sum"})
     (assoc :TOOLTIP [{:field "x" :type "quantitative"} {:field :y :aggregate :YAGG}])
     (viz/type ht/bar-chart)
     viz/viz)

;Rerun the experiment with the new values and check the significance
(drug-experiment 10000 drug-measure-2 placebo-measure-2)

;## Confidence intervals
;Is the drug effective (How large is the effect). Lets say the average survival on the placebo is 5 years, and the 
;drug increases the survival on an average by 3 days. The difference between 5 years and 3 days may be significant
;but it is not a large effect
;
;To measure the confidence interval we 
;1. bootstrap - choose uniformly at random with replacement for the drug and the placebo,
;2. Add the observed difference for this instance of the values to the result
;3. Repeat 1 and 2 many many times
;4. Sort the results and pick the ends based on the desired confidence interval
;
;One example of the drug vector created by bootstrapping
drug
(repeatedly (count drug) #(rand-nth drug))

(defn bootstrap-avg
  [sample]
  (-> (repeatedly (count sample) #(rand-nth sample))
      avg))

(defn confidence-intervals
  [percentile num-bootstraps drug placebo tails]
  (let [edge (/ (- 1 percentile) tails)
        lower-index (Math/ceil (* edge num-bootstraps))
        upper-index (Math/floor (* (- 1 edge) num-bootstraps))
        diffs (-> (repeatedly num-bootstraps #(- (bootstrap-avg drug) (bootstrap-avg placebo)))
                  sort)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (first diffs)
     :max (last diffs)
     :median (float (nth diffs (int (/ (count diffs) 2))))}))
(confidence-intervals 0.9 10000 drug placebo 2)

(def xrule-chart
  (-> (assoc-in ht/xrule-layer [:encoding :x2] {:field :X2})
      (assoc :data ht/data-options)))

(defn render-ci
  [ci]
  (-> (hc/xform ht/layer-chart
                :LAYER [(hc/xform xrule-chart
                                  :X "min"
                                  :XTYPE "quantitative"
                                  :X2 "max"
                                  :VALDATA [ci]
                                  :XZERO false)
                        (-> (hc/xform ht/bar-chart
                                      :X "lower"
                                      :YBIN hc/RMV
                                      :VALDATA [ci]
                                      :TOOLTIP hc/RMV
                                      :SIZE 14)
                            (update :encoding dissoc :y)
                            (assoc-in [:encoding :x2] {:field "upper"}))
                        (-> (hc/xform ht/point-chart
                                      :X "median"
                                      :YBIN hc/RMV
                                      :VALDATA [ci]
                                      :SIZE 14
                                      :MCOLOR "red"
                                      :TOOLTIP hc/RMV)
                            (update :encoding dissoc :y)
                            (assoc :tooltip [{:field "avg"}]))]
                :HEIGHT 40)
      (kindly/consider kind/vega)))
(render-ci (confidence-intervals 0.90 10000 drug placebo 2))
;## Power
;The power of a test is the probability of rejecting the null hypothesis when it is false. The power statistic is normally used to determine the minimum sample size required to get a statistically significant result   
;
(defn is-significant?
  [{:keys [observed-probability]}]
  (<= observed-probability 0.05))

(defn power
  [experiment & [p]]
  (->> (range 1000) ;;do the following 1000 times
       (pmap #(experiment %)) ;;run the experiment
       (filter is-significant?) ;;filter out successes - experiments where the null hypothesis is false
       count ;;count them
       ;;See if it rejects the null hypothesis atleast power times number of tries
       ((fn [c] (if p (< (* p 1000) c) (/ c 1000))))))
;
;For the  case of the drug experiment we are simply running the test(drug-experiment) a 1000 times and counting all the times we see a statistically significant p-value 
(delay (power (fn [exp-num] (drug-experiment 10000 drug placebo))))
;;Can i reject the null hypothesis with 80% probability
(delay (power (fn [exp-num] (drug-experiment 10000 drug placebo)) 0.8))
;##  Outliers 
;; Suppose we were measuring the confidence interval of average salaries as a precursor to the evaluation of does college 
;; education affect salary. But we have Bill Gates' salary in the mix
(def salaries [200 69 141 45 154 169 142 198 178 197 1000000 166 188 178 129 87 151 101 187 154])
;;rank transformation use indexes instead of ranks
(map-indexed  (fn [i s] [i s]) (sort salaries))
(def salaries-bill-gates [200 69 141 45 154 169 142 198 178 197 166 188 178 129 87 151 101 187 154])

(defn ci-outlier
  [salaries]
  (let [edge (/ (- 1 0.9) 2)
        lower-index (Math/ceil (* edge 10000))
        upper-index (Math/floor (* (- 1 edge) 10000))
        diffs (-> (repeatedly 10000 #(bootstrap-avg salaries))
                  sort)]
    {:lower (float (nth diffs lower-index))
     :upper (float (nth diffs upper-index))
     :min (float (first diffs))
     :max (float (last diffs))
     :median (float (nth diffs (int (/ (count diffs) 2))))}))
(render-ci (ci-outlier salaries))
(render-ci (ci-outlier salaries-bill-gates))

;## Other evauation methods
;-  Chi-Squared - Measure the deviation of observed data for multiple categories from expectation or test the independence of two variables
;- Fischers Exact test - like chi squared for four categories and expected counts are below 10
; - ANOVA - How different groups are when  independent variable/s is/are changed(One way , multi way)
; - Linear regression - Predict future values based on the values observed
; - Linear corelation - How well a variable  can predict another if a linear relationship exists
; - Multiple Testing - What if the 5% of the samples fall in the statistically significant range would we consider such a result in that range to be statistically significant

;## Practical considerations
;- Bootstrapping - When sample sizes are small (under 100) we may under estimate the size of the confidence interval and a significance test may work better
;- Significance/shuffle/permutations can be used for as few as 3 points
;- Strategies for evaluating treatments - CI, significance test, shuffle like significance, resample
;- Bootstrapping should be used with caution when there are outliers
;- Neither bootstrapping nor sampling should be used if the sample is not representative
;- Resampling should be used with care when data exhibits serial dependence.

;## References
;- Statistics is easy - Dennis Shasha, Manda Wilson
;- Bootstrap methods and their application - D.V. Hinkley, A.C. Davidson

;## Terminology used in the namespace
; - Binomial distribution: the binomial distribution with parameters n and p is the discrete probability distribution of the number of successes in a sequence of n independent experiments
; - Null hypothesis - The normal boring expectation of an observation.
; - Significance: (p-value) The ability to say wether an observation has not happened due to random chance. A very small p-value(probability) indicates that the observation observed is not probable so the null hypothesis can be rejected. Done by shuffling 
; - Bootstrapping: Drawing from a sample at random with replacement
; - Confidence interval: The range of values of the measure we expect our test statistic is likely to take
; - Power: The probability that a significance test witll reject the null hypothesis
; - Z-Test - The test used to compare two means used as a measure to create the p-value to accept or reject the null hypothesis when the standard deviation is known
; - Students T - Test - Similar to the z-test  Used to compare the means for a smaller number of samples with the assumption that the distribution is normal and the standard deviation is unknown
