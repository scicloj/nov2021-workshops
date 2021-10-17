(ns datavis-tutorials.hanami
  (:require [scicloj.notespace.v4.api :as notespace]
            [scicloj.kindly.api :as kindly]
            [scicloj.kindly.kind :as kind]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.webserver :as clerk-webserver]
            [nextjournal.beholder :as beholder]
            [clojure.java.browse :as browse]
            [aerial.hanami.common :as hc :refer [RMV]]
            [aerial.hanami.templates :as ht]
            [scicloj.viz.api :as viz]
            [tablecloth.api :as tc]))

;; # Hanami tutorial

;; ## Using Notespace

;; For more details, see the [Notespace v4 tutorial](https://scicloj.github.io/notespace/doc/scicloj/notespace/v4/tutorial-test/index.html).

;; ### (re)starting

(comment
  (notespace/restart!
   {:open-browser? true}))

;; ### troubleshooting

(comment
  (notespace/restart-events!))

;; ## Using Clerk

;; ### Starting

(comment
  (def port 7777)
  (clerk-webserver/start! {:port port})
  (def filewatcher
    (beholder/watch #(clerk/file-event %) "src"))
  (browse/browse-url (str "http://localhost:" port))
  (clerk/show! "src/datavis_tutorials/hanami.clj"))

;; ### Stopping
(comment
  (beholder/stop filewatcher))

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
   :background "floralwhite",
   :data
   {:values (-> random-walk-dataset
                (tc/rows :as-maps))}})

;; ## Showing a Vega-Lite plot

;; ### Notespace

;; We can tell Notespace to render as Vega using a metadata tag at the code:
;; `^kind/vega`
;; `random-walk-vl-spec`
;; (commented out to avoid a conflict with Clerk)

;; We can also do it by marking the resulting value's metadata like this:
(-> random-walk-vl-spec
    (kindly/consider kind/vega))

;; ### Clerk

(-> random-walk-vl-spec
    clerk/vl)

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

;; At Notespace, this should automatically render as a plot.

(-> random-walk-dataset
    (viz/data)
    (viz/type ht/line-chart)
    (viz/viz {:HEIGHT 100}))

;; ## Hanami rules

;; This text has been copied from Jon Anthony's Saite tutorials, 2021-01-23.
;;
;;; A walk through 'exercise' with templates, substitution keys and
;;; transformations using them.
;;
;;; `hc/xform` is the general transformation function. It actually can work on any nested set of collections. Typically the outer is a map, but need not be.
;;; It has three signatures, one of which is variadic:
;;; `hc/xform [coll]`, `hc/xform [coll submap]`, `hc/xform [coll k v & kvs]`.
;;
;;; `hc/get-default` takes a key and returns its value in the default
;;; substitution key map AKA 'submap'.  Or `nil` if not there.
;;
;;; uses only the default submap
(hc/xform {:a :X})

(hc/get-default :X)

(get @hc/_defaults :X)

;;; Values given as k/v pairs override any in the default submap
(hc/xform {:a :X} :X "foo")

;;; Next, we first get {:a {:y :Y}}, which is then recursively transformed
(hc/xform {:a :X} :X {:y :Y})

(hc/get-default :Y)

;;; If a value is neither in the default submap nor given as a k/v pair,
;;; it is taken as the literal value
(hc/xform {:a :Saite})

(hc/get-default :Saite)

;;; Again, giving it a value in the k/v pairs will use that value
(hc/xform {:a :Saite} :Saite 12)

(hc/get-default :FOO)

(hc/xform {:a :FOO} :FOO #{1 2})

{:a :FOO}

hc/RMV

;;; Here we see an example of the third transformation rule in action
;;; (https://github.com/jsa-aerial/hanami#basic-transformation-rules)
;;; {:a :FOO} --> {:a  #{}} --> {:a RMV} --> {}
;;; hc/RMV is the special Specter value NONE, which when encountered
;;; in a (Specter) transform means to remove the associated item.
(hc/xform {:a :FOO} :FOO #{})

;;; Many (most) default substitution key values are RMV. This is so the key
;;; they are a value for will be removed from anywhere they appear, unless
;;; a specific value is given for them.
;;; {:a :FOO} --> {:a {:scale :XSCALE}} --> {:a {:scale RMV}} -->
;;; {:a {}} --> {:a RMV} --> {}
(hc/get-default :XSCALE)

(hc/xform {:a :FOO} :FOO {:scale :XSCALE})

;;; We can use the default data template fragment to show this basic idea in
;;; action. The fragment has 4 items:
ht/data-options

;;; Three of the associated substitution keys default to RMV
(hc/get-default :NDATA)

(hc/get-default :UDATA)

(hc/get-default :DFMT)

(hc/get-default :VALDATA)

;;; :VALDATA is an interesting case as its default value is a function
;;; (//github.com/jsa-aerial/hanami#function-values-for-substitution-keys)
;;; This function looks up :FDATA and :DATA in the current submap passed to
;;; it. If they are RMV, it returns RMV. hc/_defaults is the default submap.

(hc/get-default :VALDATA)

(hc/get-default :DATA)

(hc/get-default :FDATA)

((hc/get-default :VALDATA) @hc/_defaults)

;;; If :DATA has a value it returns that.
((hc/get-default :VALDATA) {:DATA [{"one" 1} {"two" 2} {"three" 3}]})

;; ;;; ON SERVER SIDE ONLY: If :FDATA has a value the :VALDATA function looks up 
;; ;;; :FDATA's substitution key function
;; ;;; (https://github.com/jsa-aerial/hanami#subtitution-key-functions) and
;; ;;; passes it the value of :FDATA, and the current submap. This function will
;; ;;; resolve the value of :FDATA to a file (or error) and fetch its content.
;; ;;; On the client this always returns RMV. Place cursor at right paren.
;; ;;; Ctrl-X Ctrl-E runs on client. Ctrl-X J runs on server (will give file
;; ;;; not found error - unless you have a real json file there...)
;; #_((hc/get-default :VALDATA)
;;    (merge @hc/_defaults {:FDATA "~/.saite/Data/my-data.json"}))


;;; OK, after that diversion, back to how the third rule of xform and having
;;; RMV for most defaults works in our favor in getting legal VG/VGL field
;;; values while using very generic fragments in definitions. So, we can now
;;; see that with just the defaults, all four items in data-options will
;;; get RMV and so all four will be removed:
ht/data-options

(hc/xform ht/data-options)

;;; But if we give one a legal value we get a legal data field value
(hc/xform ht/data-options :DATA [{"one" 1} {"two" 2} {"three" 3}])

(hc/xform ht/data-options :UDATA "data/cars.json")

;;; Yes, if you give more than one, you will get an ILLEGAL value. But then
;;; you deserve what you get...


;;; Because of this, we can just use hc/data-options as the default value of
;;; the data field in a view (see Vega/Vega-Lite for information on views)
;;; view-base is a fragment that is used in the definition of any view in
;;; templates.
ht/view-base

;;; As you see, there is a lot of stuff and most of it substitution keys which
;;; themselves have a lot of stuff. Take :ENCODING for example
(hc/get-default :ENCODING)

;;; Again, a lot of stuff, most of it more substitution keys. But we are
;;; bottoming out - _most_ of these have base values - many RMV. If we
;;; transform this using just the defaults, we get a legal (default) encoding
(hc/xform {:encoding :ENCODING})

;; ;;; Let's look at another pair in view-base, the :usermeta :USERDATA pair
;; ;;; The :usermeta field is a special field that the authors of VG/VGL (the
;; ;;; members of the Interactive Data Lab at UWash in Seattle) have included in
;; ;;; the schemas of VG and VGL. They understood user apps may want to encode
;; ;;; special data in a legal VG/VGL spec and this field supports that use
;; ;;; case. Generally, that data will be some form of _control_ data. That's
;; ;;; exactly how it is used in Saite. Out of the box, Hanami has :USERDATA
;; ;;; as RMV, so :usermeta will just be removed. But Saite gives it a value of
;; ;;; various control data.
;; (hc/get-default :USERDATA)

;; ;;; Again, many values are substitution keys themselves with their own default
;; ;;; values.  :msgop and :session-name are really server things and don't
;; ;;; concern us when working via the client.  In the 'Tabs' tab and 'Gallery'
;; ;;; tab of this document, you will see values given for :LEFT, :TOP, :FID, :VID
;; ;;; which control how the associated picture frame and vis are displayed.
;; (hc/xform (hc/get-default :USERDATA))


;;; Now that we've seen :ENCODING and :USERDATA, let's go back to view-base and
;;; see how these work to give a nearly complete Vega-Lite specification when
;;; transformed. Put the cursor at the 'e' in view-base and Ctrl-X Ctrl-E to
;;; get its value. Then use Ctrl-X Ctrl-C to get the transformed value.
(hc/xform ht/view-base)

;;; That's pretty close to being a legal view spec, but it doesn't have a data
;;; value and so would throw an error in VGL. We can fix that by giving a
;;; data source
(hc/xform ht/view-base :UDATA "data/cars.json")

;; ;;; OK, the only thing left to get a legal spec is to add in a mark spec. Marks
;; ;;; basically define how VG/VGL render the view - line plot, scatter plot, bar
;; ;;; chart, area chart, etc etc. There are several out-of-box 'chart/plot'
;; ;;; templates that add a mark around view-base. Here are a couple. Put the
;; ;;; cursor at the end and Ctrl-X Ctrl-E. Notice the mark spec again has several
;; ;;; fields with substitution key values. Only the 'type' field is concrete.
ht/point-chart

ht/bar-chart

;; ;;; Now we are ready to get a complete legal Vega-Lite specification. We will
;; ;;; use the point-chart template which will give a scatter plot. So, we need
;; ;;; a data source and this one has it's x field as the string "Horsepower" and
;; ;;; its y field as "Miles_per_Gallon". That's all we really need, but to get
;; ;;; a nice color faceting, we also ask the "Origin" field be color faceted.
;; ;;; You can see this exact chart rendered in the 'Tabs' tab demo (where we
;; ;;; add frame and visual ids to control its placement in the doc body)
(hc/xform
 ht/point-chart
 :UDATA "data/cars.json"
 :X "Horsepower" :Y "Miles_per_Gallon" :COLOR "Origin")

;; ;;; we are pretty much at the end here. The last thing to cover concerns what
;; ;;; some have voiced a desire for - changing the default style of using
;; ;;; :UPPERCASE keywords for substitution keys. Nothing in any of the
;; ;;; transformation process requires this and you can make them be any style
;; ;;; or form you like. The following is a little exercise in how you can
;; ;;; automate that change
;; ;;; We already saw that hc/_defaults is the default submap. As you can see,
;; ;;; there are a lot of such default keys.
;; (deref hc/_defaults)
;; ;;; We will need clojure.set and clojure.string for the exercise. Like any
;; ;;; form, requires can be evaluated. Just put the cursor on the right paren
;; ;;; like any other example, and Ctrl-X Ctrl-E these
;; (require '[clojure.set :as set])
;; (require '[clojure.string :as str])
;; ;;; We will turn our uppercase keys into lower case strings. mydefs is a map
;; ;;; from the uppercase keywords to their lower case strings. If you eval this
;; ;;; you will see the first (random) 10 cases. Comment the take out and
;; ;;; uncomment each xform in turn to see how the templates can be transformed
;; ;;; to use these lower case substitution keys.
;; ;;; This is just an example, more likely version would use namespace qualified
;; ;;; lowercase keywords.
;; (let [mydefs (->> hc/_defaults deref
;;                (mapv (fn[[k v]](vector (-> k name str/lower-case) k)))
;;                (into {}) set/map-invert)]
;;   (take 10 mydefs)
;;   ;; Convert the current default substitution _keys_ to be our new lowercase
;;   ;; string keys. You could then reset! hc/_defaults to this map to a merge of
;;   ;; it with the default hc/_defaults to have both types of keys.
;;   #_(hc/xform @hc/_defaults mydefs)
;;   ;; Use it to perform same conversion of view-base and point-chart as above
;;   #_(hc/xform ht/view-base (hc/xform @hc/_defaults mydefs))
;;   #_(hc/xform ht/point-chart (hc/xform @hc/_defaults mydefs)))



:bye
