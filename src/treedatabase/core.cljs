(ns ^:figwheel-always treedatabase.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cuerdas.core :as str]
            [com.rpl.specter :as s]
            [devtools.core :as devtools]
            ))

(devtools/set-pref! :install-sanity-hints true) ; this is optional
(devtools/install!)

;(.log js/console (range 200))

(enable-console-print!)

(println "<-----------------Start of core")

;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Hello world"}))

(defn length>0 [string]
  (> (.-length string) 0))

(defn list-of-path-segments [paths]
  (map #(filter length>0 %) (map #(str/split % #"/") paths)))

(defn get-sub-path [path item]
  (when item
    (let [location (. path lastIndexOf item)
          location-plus-one (+ location 1)
          next-slash (. path indexOf "/" location-plus-one)
          sub-string (if (= next-slash -1) path (. path substring 0 next-slash))
          ]
      ;(println path location location-plus-one next-slash)
      sub-string)))

(defn extract-id-by-type [type full-path]
  (when full-path
    (let [path (get-sub-path full-path type)
          last-slash (. path lastIndexOf "/")
          ;_ (println "path:" path "last-slash:" last-slash)
          ;_ (println "length:" (.-length path))
          sub-string (if (= last-slash -1) path (. path substring (+ last-slash 1) (.-length path)))]
      sub-string)))

(defn valid-path? [prefix map]
  ;(println "valid-path?->map:" map)
  (let [path (:path map)]
    (when path
      ;(println "valid-path?->path:" path)
      (let [list-of-path-segments (first (list-of-path-segments (list path)))
            ;_ (println "valid-path?->list-of-path-segments:" list-of-path-segments)
            last (last list-of-path-segments)
            ;_ (println "valid-path?->last:" last)
            ]
        (when
          (. last startsWith prefix)
          path)
        )
      )
    )
  )

(defn merge-nodes [current-node new-data]
  ;(println "current-node:" current-node "new-data:" new-data)
  (merge current-node new-data)
  )

(defn process-working-data [initial-map working-data]
  (println "working-data:" working-data)
  (let [list-of-path-segments (:list-of-path-segments working-data)
        _ (println list-of-path-segments)
        lookup-map (:lookup-map working-data)
        ;_ (println lookup-map )
        ]
    (loop [current-state initial-map
           path-segments list-of-path-segments]
      (let [path-segment (first path-segments)
            ;_ (println path-segment)
            last-map (get lookup-map (last path-segment))
            ;_ (println "last-map:" last-map "current-state:" current-state)
            ;new-state (build-tree-from-paths-map current-state path-segment last-map)
            new-state (update-in current-state path-segment merge-nodes last-map)
            _ (println "new-state:" new-state)
            ]
        (if (empty? path-segments)
          current-state
          (recur new-state (rest path-segments))
          )
        )
      )
    )
  )

;; TODO need a tree database

(def treedatabase {"Shops"
                   {"shop0001"
                    {"veh0001"
                     {"job0001"
                      {:path "/Shops/shop0001/veh0001/job0001"}}}}}
  )
(.log js/console treedatabase)

;(pprint "treedatabase:" treedatabase)
(println (s/select [(s/keypath "Shops")(s/keypath "shop0001")] treedatabase))
(.log js/console (s/select [(s/keypath "Shops")(s/keypath "shop0001")] treedatabase))

;(println (process-working-data
; {}
; {:list-of-path-segments (list (list "Shops" "shop0001"))
; :lookup-map {"shop0001" {:test :result}}}))

;; TODO can we replace find-leaf with a s/select?
;; TODO need a place where we can call this to test
;(println (s/select [s/ALL :a even?] [{:a 1} {:a 2} {:a 4} {:a 3}]))
;(println (s/setval (s/srange 2 4)
;                   [99]
;                   [0 1 2 3 4 5 6 7 8 9]))
;(println (s/setval (s/keypath "Shops")
;                   {"shop0001" {}}
;                   {"Shops" {}}
;                   ))
;(println (s/setval nil
;                   {"Shops" {}}
;                   {}
;                   ))

;(.log js/console (str "Hey Seymore! what is goin' on? " (js/Date.)))

(println "<-----------------No wuckin furries, mate")

(om/root
  (fn [data owner]
    (reify om/IRender
      (render [_]
        (dom/h1 nil (:text data)))))
  app-state
  {:target (. js/document (getElementById "app"))})

(defn on-js-reload []
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;(swap! app-state update-in [:__figwheel_counter] inc)
  )
