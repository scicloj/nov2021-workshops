(ns inferme-tutorials.intro
  (:require [fastmath.core :as math] ; generateme
            [fastmath.random :as random]
            [fastmath.stats :as stats]
            [inferme.core :refer :all] ; generateme
            [inferme-tutorials.plot :as plot]
            [inferme.plot]
            [cljplot.core] ; generateme
            [scicloj.notespace.v4.api :as notespace] ; scicloj
            [scicloj.notespace.v4.view :as notespace.view]
            [scicloj.notespace.v4.config :as notespace.config]
            [scicloj.kindly.api :as kindly]
            [scicloj.kindly.kind :as kind]
            [fitdistr.core :as fitdistr] ; generateme
            [fitdistr.distributions]))


;; # Intro to probabilistic modelling with Inferme

;; ## Setup

;; Clone [Inferme](github.com/generateme/inferme/) and install it locally by `lein install`.

;; Notespace setup:
(defonce notespace-started
  (notespace/restart!))

^kind/hidden
(comment
  ;; Customize and troubleshoot Notespace (WIP):
  (notespace/restart-events!)
  (notespace.config/set! {:notes?     true
                          :header?    true
                          :last-eval? true})
  (notespace.config/set! {:header?    true
                          :last-eval? true}))

;; Adapt Inferme's plotting to Notespace:

(intern 'cljplot.core 'show :buffer)

;; ## Background

;; ### Redefence

;; * [Probabilistic Models of Cognition by Goodman, Tenenbaum, and the ProbMods Contributors](http://probmods.org/)

;; * [Probabilistic Programming and Bayesian Methods for Hackers by Cam Davidson Pilon](https://github.com/CamDavidsonPilon/Probabilistic-Programming-and-Bayesian-Methods-for-Hackers)

;; ### The Bayesian inference landscape in Clojure

;; * [anglican](https://probprog.github.io/anglican/index.html)

;; * [bayadera](https://github.com/uncomplicate/bayadera)

;; * [distributions](https://github.com/michaellindon/distributions)

;; * [metaprob](https://github.com/probcomp/metaprob)

;; * [inferme](https://github.com/generateme/inferme)

;; * [daphne](https://github.com/plai-group/daphne)

;; ### Libraries used in this session

;; * [inferme](https://github.com/generateme/inferme)

;; * [fastmath](https://github.com/generateme/fastmath)

;; * [fitdistr](https://github.com/generateme/fitdistr)

;; * [cljplot](https://github.com/generateme/cljplot)

;; ### (pseudo) random numbers with Fastmath

(random/frand)

(repeatedly 3 random/frand)

(let [rng (random/rng :isaac 1337)]
  (random/frandom rng))

(repeatedly 3
            #(let [rng (random/rng :isaac 1337)]
               (random/frandom rng)))

(def rng1 (random/rng :isaac 1337))
(random/frandom rng1)
(random/frandom rng1)
(random/frandom rng1)

;; ## Basic notions

;; ### Probability space

(def N 1000)

(def Omega (set (shuffle (range N))))

;; ### Events

(def Odd
  (->> Omega
       (filter (fn [omega]
                 (< (* 3 omega) N)))
       set))

(count Omega)

(count Odd)

;; ### Probability

(defn P [A]
  (-> A
      count
      float
      (/ N)))

(P Odd)

;; ### Random variables

;; r.v. -- a function from the probability space to numbers

(defn X [omega]
  (math/sq omega))

(X 34)

(->> Omega
     (map X)
     (take 3))

(defn Y [omega]
  (let [rng (random/rng :isaac omega)]
    (random/frandom rng)))

(->> Omega
     (map Y)
     (take 9))

(defn Z [omega]
  (let [rng (random/rng :isaac omega)]
    (random/grandom rng))) ; Gassian

(->> Omega
     (map Z)
     (take 9))

;; ### Events of a random variable

(def Y-is-less-than-half
  (->> Omega
       (filter (fn [omega]
                 (< (Y omega) 1/2)))
       set))

(P Y-is-less-than-half)


(->> Omega
     (filter (comp #(< % 1/2)
                   Y))
     set
     P)


#_(->> Omega
       (filter (comp pred
                     Y))
       set
       P)


;; ### Distributions of random variables

(->> Omega
     (map X)
     plot/histogram)

(->> Omega
     (map Y)
     plot/histogram)

(->> Omega
     (map (comp math/sq Y))
     plot/histogram)

(->> Omega
     (map Z)
     plot/histogram)

;; ### Conditional probability

(def Y-is-more-than-third
  (->> Omega
       (filter (fn [omega]
                 (> (Y omega) 1/3)))
       set))

(P Y-is-less-than-half)

(P Y-is-more-than-third)

(P (filter Y-is-less-than-half
           Y-is-more-than-third))

;; 1/3 < Y < 1/2
;; 1/2-1/3 = 1/6


;; What is the probability that
;; Y is more than 1/3
;; conditioned on
;; Y being less than 1/2 ?

(/ (P (filter Y-is-less-than-half Y-is-more-than-third))
   (P Y-is-less-than-half))

;; ### Conditional distribution

(->> Omega
     (filter Y-is-less-than-half)
     (map Y)
     plot/histogram)

(->> Omega
     (filter Y-is-less-than-half)
     (map (comp math/sq Y))
     plot/histogram)

;; ## Modeling with Inferme

(defmodel model1
  [Y (:uniform-real)]
  (model-result [(condition (< Y 1/2))]
                {:Y2 (math/sq Y)}))

(def results1
  (delay
    (infer :rejection-sampling model1)))

(-> @results1
    (trace :Y)
    plot/histogram
    delay)

(-> @results1
    (trace :Y2)
    plot/histogram
    delay)

(-> @results1
    (trace :Y)
    (->> (map (fn [y] (> y 1/3))))
    frequencies
    delay)

(-> @results1
    (trace :Y)
    (->> (map (fn [y] (> y 1/3))))
    plot/frequencies
    delay)

;; ## Binomial distribution

(defmodel binomial-model-1
  []
  (let [coin (distr :bernoulli {:p 0.5})
        flips (repeatedly 3 #(random/sample coin))
        total (reduce + flips)]
    (model-result []
                  {:total total})))

(-> (infer :forward-sampling binomial-model-1 {:samples 10000})
    (trace :total)
    frequencies
    delay)

(-> (infer :forward-sampling binomial-model-1 {:samples 10000})
    (trace :total)
    plot/frequencies
    delay)

(defmodel binomial-model-2
  []
  (let [total (random/sample (distr :binomial {:trials 3
                                               :p      0.5}))]
    (model-result []
                  {:total total})))

(-> (infer :forward-sampling binomial-model-2 {:samples 10000})
    (trace :total)
    plot/frequencies
    delay)

;; ## Getting to know a coin -- a Bayesian approach

(def coin1-trials 5)
(def coin1-heads 2)
(def coin1-tails (- coin1-trials coin1-heads))

(defmodel coin-model-1
  [p (:uniform-real)] ; prior
  (let [coin-flipping (distr :binomial {:trials coin1-trials
                                        :p p})]
    (model-result [(observe1 coin-flipping coin1-heads)])))

(def coin-model-1-result
  (-> (infer :metropolis-hastings coin-model-1 {:samples 10000
                                                :thin 100
                                                :steps [0.2]})
      delay))

(-> @coin-model-1-result
    :acceptance-ratio)

(-> @coin-model-1-result
    (trace :p)
    (->> (take 9)))

(-> @coin-model-1-result
    (trace :p)
    plot/lag
    delay)

(-> @coin-model-1-result
    (trace :p) ; posterior
    plot/histogram
    delay)

(-> @coin-model-1-result
    (trace :p)
    stats/mean
    delay)

(-> @coin-model-1-result
    (trace :p)
    (->> (fitdistr/fit :mle :beta))
    delay)

(defmodel coin-model-2
  [p (:beta {:alpha 10
             :beta 10})]
  (let [coin-flipping (distr :binomial {:trials coin1-trials
                                        :p      p})]
    (model-result [(observe1 coin-flipping coin1-heads)])))

(def coin-model-2-result
  (-> (infer :metropolis-hastings coin-model-2 {:samples 10000
                                                :thin 100
                                                :steps [0.2]})
      delay))

(-> @coin-model-2-result
    :acceptance-ratio)

(-> @coin-model-2-result
    (trace :p)
    plot/lag
    delay)

(defmodel coin-model-3
  [p (:beta {:alpha (+ 10 coin1-heads)
             :beta  (+ 10 coin1-tails)})]
  (model-result []))

(def coin-model-3-result
  (-> (infer :metropolis-hastings coin-model-2 {:samples 10000})
      delay))


^kind/hiccup
[:div
 [:p "by sampling"]
 (-> @coin-model-2-result
     (trace :p)
     (concat [0 1])
     (plot/histogram {:height 200})
     kindly/to-hiccup)
 [:p "by conjugate"]
 (-> @coin-model-3-result
     (trace :p)
     (concat [0 1])
     (plot/histogram {:height 200})
     kindly/to-hiccup)]


;; ## Heirarchy (WIP)

(def trials 10)
(def successes [2 3 6 9])
(def group [:A :A :B :B])

(defmodel heirarchical-model-1
  [a-alpha (:uniform-real 5)
   a-beta (:uniform-real 5)
   b-alpha (:uniform-real 5)
   b-beta (:uniform-real 5)
   p0 (:beta {:alpha a-alpha :beta a-beta})
   p1 (:beta {:alpha a-alpha :beta a-beta})
   p2 (:beta {:alpha b-alpha :beta b-beta})
   p3 (:beta {:alpha b-alpha :beta b-beta})]
  (model-result [(observe1 (distr :binomial {:trials trials :p p0})
                           (successes 0))
                 (observe1 (distr :binomial {:trials trials :p p1})
                           (successes 1))
                 (observe1 (distr :binomial {:trials trials :p p2})
                           (successes 2))
                 (observe1 (distr :binomial {:trials trials :p p3})
                           (successes 3))]))


(def heirarchical-model-results-1
  (-> (infer :metropolis-hastings heirarchical-model-1
             {:samples 10000
              :thin    100})
      delay))

(-> @heirarchical-model-results-1
    (trace :p0)
    plot/histogram
    delay)


