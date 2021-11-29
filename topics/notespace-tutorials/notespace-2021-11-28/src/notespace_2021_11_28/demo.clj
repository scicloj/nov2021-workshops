;; # Notespace v4

;; Nov. 28th, 2021

:_
;; # Setup

(ns notespace-2021-11-28.demo
  (:require [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.api :as kindly]
            [scicloj.kindly.kind :as kind]
            [scicloj.kindly.kindness :as kindness]))

;; To start (or restart) Notespace, use the following call:
(comment
  (notespace/restart!))

;; Browse localhost:1903 to see the browser view.

:_

;; # scope

;; * data visualization

;; * literate programming

;; * reproducible research

:_

;; # Why

;; - documentation -- [scicloj.ml](https://github.com/scicloj/scicloj.ml)

;; - testable docs / literate testing -- [clojisr](https://github.com/scicloj/clojisr) -- [codgen](https://scicloj.github.io/clojisr/doc/clojisr/v1/codegen-test/)

;; - visual documents -- [clojisr-examples](https://github.com/scicloj/clojisr-examples) -- [ridgeline](https://scicloj.github.io/clojisr-examples/doc/clojisr-examples/graph-gallery/ridgeline/)

;; - reproducible research -- [notespace-sicmutils-example](https://github.com/scicloj/notespace-sicmutils-example) -- [the double pendulum](https://scicloj.github.io/notespace-sicmutils-example/docs/notespace-sicmutils-example/double-pendulum/)

;; - study, exploration -- [ml-study](https://clojurians.zulipchat.com/#narrow/stream/264992-ml-study) -- [image processing](https://scicloj.github.io/ml-study/projects/image-demo-1/docs/image-demo-1/session2-1/) from the ml-study group -- [data visualizations](https://www.youtube.com/watch?v=2tGk1Jh7dJs)

:_


;; # Landscape

;; * browser-based notebooks: **Gorilla-REPL, Clojupyter, IClojure, Nextjournal, Maria.cloud**

;; * literate programming in plaintext: **Org-babel-Clojure, RMarkdown-Clojure**

;; * a unique browser UI: **Saite**

;; * sending visualizations from the REPL - **Reveal, Portal**

;; * a whole stack for diverse UI experiences: **Pink-Gorilla (Goldly, Reval)**

;; * visualizations inside the editor: **REPL-Tooling, Chlorine, Clover**

;; * namespace-as-a-notebook: **Marginalia, Oz, Notespace, Clerk, Goldly**

:_

;; # ns --> notebook

;; - Oz
;; - Notespace
;; - Clerk
;; - Goldly

:_

;; # principles

;; Notespace:

;; - community driven

;; - flows with the REPL

:_

;; # Interaction

;; - file change

;; - namespace load

;; - region eval

;; - tabs

:_

;; # demo

;; $\alpha^2$ (using KaTeX)


^kind/hiccup
[:div
 [:p/math "\\alpha^2"]
 [:p/math "\\beta^2"]]


(+ 1 2333)


(def x 9)

(+ 1 1 99999)

(rand)



;; editscript

;;

;; what is know / unknown, how we represent it

:_




;; # Hiccup

;; (extended with Pink-Gorilla/Gorilla-UI)

^kind/hiccup
[:div [:h1 "section 1"]
 [:p "hello"]]

^kind/hiccup
[:div
 [:p "hi"]
 [:p/vega
 {:description "A simple bar chart with embedded data."
  :data        {:values [{:a "A" :b 28} {:a "B" :b 55}
                         {:a "C" :b 43}
                         {:a "D" :b (+ 91 (rand-int 9))}
                         {:a "E" :b 81} {:a "F" :b 53}]}
  :mark        :bar
  :encoding    {:x {:field :a :type :nominal :axis
                    {:labelAngle 0}}
                :y {:field :b :type :quantitative}}}]]

;; # code kind

^kind/hiccup
[:p/sparklinespot
 {:data      (->> #(- (rand) 0.5)
                  (repeatedly 99)
                  (reductions +))
  :svgHeight 50}]

^kind/hiccup
(->> (range 9 99 10)
     (map (fn [i]
            [:p/sparklinespot
             {:data      (->> #(- (rand) 0.5)
                              (repeatedly i)
                              (reductions +))
              :svgHeight 50}]))
     (into [:div]))


;; # value kind

(-> {:description "A simple bar chart with embedded data."
     :height 50
     :data        {:values [{:a "A" :b 28} {:a "B" :b 55}
                            {:a "C" :b 43}
                            {:a "D" :b (+ 91 (rand-int 9))}
                            {:a "E" :b 81} {:a "F" :b 53}
                            {:a "G" :b 19} {:a "H" :b 87}
                            {:a "I" :b 52}]}
     :mark        :bar
     :encoding    {:x {:field :a :type :nominal :axis {:labelAngle 0}}
                   :y {:field :b :type :quantitative}}}
    (kindly/consider kind/vega))

;; # kindness

(deftype BigBigBigText [text]
  kindness/Kindness
  (->behaviour [this]
    {:render-src?   true
     :value->hiccup (fn [value]
                      [:big [:big [:big (.text value)]]])}))

(BigBigBigText. "hi!")

;; # Delays

;; When the evaluation value is a Clojure [delay](https://clojuredocs.org/clojure.core/delay), will render by dereferencing the delay.

(delay
  (Thread/sleep 3500)
  (+ 1 233333))

:_

;; # Restart

;; Useful for troubleshooting:

;; Restarting the event system:

(comment
  (notespace/restart-events!))

;; Restarting the whole syste (needs browser refresh):

(comment
  (notespace/restart!))

:_

;; # Challenges

;; - ergonomics

;; - stability

;; - compatibility: kinds of vis., representing visual output, managing resources

;; - where to go now?

:_



