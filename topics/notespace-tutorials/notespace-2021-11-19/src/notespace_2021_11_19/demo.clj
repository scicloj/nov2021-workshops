

; # Notespace v4 tutorial

; ## Setup

(ns notespace-2021-11-19.demo
  (:require [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.api :as kindly]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.kindness :as kindness]))

;; To start (or restart) Notespace, use the following call:
(comment
  (notespace/restart!))

;; Browse localhost:1903 to see the browser view.

;; ## The relevant landscape

;; #### data visualization & literate programming

;; - Gorilla-REPL, Clojupyter, IClojure, Nextjournal

;; - Org-babel-Clojure, RMarkdown-Clojure

;; - Marginalia, Oz, Notespace, Clerk

;; - Saite

;; - Reveal, Portal

;; - Pink-Gorilla: Goldly, Reval

;; - REPL-Tooling, Chlorine, Clover

;; #### Your namespace as a notebook

;; - Oz
;; - Notespace
;; - Clerk
;; - Godly

;; ## Notespace principles

;; - community driven

;; - flows with the REPL

;; ## Usage

;; - documentation -- [scicloj.ml](https://github.com/scicloj/scicloj.ml)

;; - testable docs / literate testing -- [clojisr](https://github.com/scicloj/clojisr)

;; - visual documsnts -- [clojisr-examples](https://github.com/scicloj/clojisr-examples)

;; - study, exploration -- [image-demo-1](https://scicloj.github.io/ml-study/projects/image-demo-1/docs/image-demo-1/session2-1/) from the ml-study group


;; ## Interaction

;; - file change

;; - namespace load

;; - region eval

;; - tabs

;; ## Basic examples

(+ 1 2)

(def x 9)

(rand)

;; ## Extended Hiccup (Gorilla-UI)

^kind/hiccup
[:p/vega
 {:description "A simple bar chart with embedded data."
  :data        {:values [{:a "A" :b 28} {:a "B" :b 55} {:a "C" :b 43}
                         {:a "D" :b (+ 91 (rand-int 9))}
                         {:a "E" :b 81} {:a "F" :b 53}]}
  :mark        :bar
  :encoding    {:x {:field :a :type :nominal :axis {:labelAngle 0}}
                :y {:field :b :type :quantitative}}}]

;; ## Specifying note kinds

;; The notion of note kinds is very similar to the one we had at v3. This needs to be documented more carefully, but for now, here are a few examples.

;; ### by a metadata tag at the source code

^kind/hiccup
[:p/sparklinespot
 {:data      (->> #(- (rand) 0.5)
                  (repeatedly 99)
                  (reductions +))
  :svgHeight 50}]

;; ### by varying the metadata of the returned value

(-> {:description "A simple bar chart with embedded data."
     :height 50
     :data        {:values [{:a "A" :b 28} {:a "B" :b 55} {:a "C" :b 43}
                            {:a "D" :b (+ 91 (rand-int 9))} {:a "E" :b 81} {:a "F" :b 53}
                            {:a "G" :b 19} {:a "H" :b 87} {:a "I" :b 52}]}
     :mark        :bar
     :encoding    {:x {:field :a :type :nominal :axis {:labelAngle 0}}
                   :y {:field :b :type :quantitative}}}
    (kindly/consider kind/vega))

;; ### by implementing the Kindness protocol

(deftype BigBigBigText [text]
  kindness/Kindness
  (->behaviour [this]
    {:render-src?   true
     :value->hiccup (fn [value]
                      [:big [:big [:big (.text value)]]])}))

(BigBigBigText. "hi!")

;; ## Delays

;; When the evaluation value is a Clojure [delay](https://clojuredocs.org/clojure.core/delay), will render by dereferencing the delay.

(delay
  (Thread/sleep 500)
  (+ 1 2))

;; We encourage the user to put slow computations in `delay` blocks. This way, evaluating the whole namespace is fast, and slowness is experienced only in the context of evaluating specific parts of it for rendering.

;; ## Troubleshooting

;; Notespace my run into bugs and unreliable states.

;; One useful practice in such a situation is restarting its event system:

(comment
  (notespace/restart-events!))

;; A more complete restart would be restarting the whole Notespace, including the webserver.

(comment
  (notespace/restart!))

;; After this kind of complete restart, a browser refresh will also be needed.


;; ## Challenges

;; ### Ergonomics

;; - reason about the situation

;; - represent what is known

;; ### Compatibility

;; - kind

;; - view representation

;; - resource management

