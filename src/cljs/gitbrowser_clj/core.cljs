(ns gitbrowser-clj.core
    ; https://github.com/r0man/cljs-http
    (:require-macros [cljs.core.async.macros :refer [go]])
    
    (:require [gitbrowser-clj.util :as u]
              
              [reagent.core :as reagent :refer [atom]]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]
              [cljs-http.client :as http]
              [cljs.core.async :refer [chan <!]]))

; lein clean && lein figwheel
; (ns gitbrowser-clj.core)

; SKIP_BUILD=1 ./build.sh

(def state
  {:repos     (atom nil)
   :repo-refs (atom nil)})


(let [{:keys [repos repo-refs]} state
      ref-url #(str % "?to=10")]
  
  (defn deref-state []
    (zipmap (->> state keys)
            (->> state vals (map deref))))
  
  (defn reload-refs! [repo]
    (let [url (-> repo :urls :refs ref-url)]
      (go (->> (http/get url) <! :body :response :refs
               (swap! repo-refs assoc (:name repo))))))
  
  (defn reload-repos! []
    (reset! repos [])
    (reset! repo-refs {})
    
    (go (let [repos-response (->> (http/get "/repos") <! :body :response :repos)]
          (reset! repos repos-response)
          (doseq [repo repos-response]
            (reload-refs! repo))))))


(reload-repos!)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn epoch->str [epoch] (-> epoch (* 1000) js/Date. .toISOString))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let [{:keys [repos repo-refs]} state]
  (defn home-page []
    [:ul
      (doall
        (for [{:keys [name]} @repos
              :let [key (str name "/" (-> @repo-refs (get name) first :hash))]]
          ^{:key key}
          [:li [:b name]
           [:pre
            (clojure.string/join "\n"
              (for [{:keys [msg ref-type name hash time]} (get @repo-refs name)
                    :let [ref-type (case ref-type "tag" " tag  " ref-type)
                          datetime (epoch->str time)]]
                (clojure.string/join " | " [hash datetime ref-type name])))]]))]))
      
     



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
