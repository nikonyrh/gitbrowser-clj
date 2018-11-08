(ns gitbrowser-clj.core
    (:require [gitbrowser-clj.util]
              
              [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]))

; lein clean && lein figwheel
; (ns gitbrowser-clj.core)

; SKIP_BUILD=1 ./build.sh

(comment
  (require '(gitbrowser-clj.util)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let []
  (defn home-page []
    [:div]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mount-root []
  (reagent/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (secretary/dispatch! path))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (mount-root))
