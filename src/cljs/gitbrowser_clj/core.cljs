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

(defonce state
  {:page      (atom {:name :home})
   :repos     (atom nil)
   :repo-refs (atom nil)})


(let [{:keys [repos repo-refs]} state
      ref-url #(str % "?to=10")]
  
  (defn deref-state []
    (zipmap (->> state keys)
            (->> state vals (map deref))))
  
  (defn reload-refs! [repo]
    (go (->> repo :urls :refs ref-url http/get <! :body :response :refs
             (swap! repo-refs assoc (:name repo)))))
  
  (defn reload-repos! []
    (reset! repos [])
    (reset! repo-refs {})
    
    (go (let [repos-response (->> "/repos" http/get <! :body :response :repos)]
          (reset! repos repos-response)
          (doseq [repo repos-response]
            (reload-refs! repo))))))

(defonce _
  [(reload-repos!)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn epoch->str [epoch] (-> epoch (* 1000) js/Date. .toISOString
                             (clojure.string/replace "T" " ")
                             (clojure.string/replace ".000Z" "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let [{:keys [page repos repo-refs]} state]
  (defn render []
    (case (:name @page)
      :home
      ^{:key [:home]}
      [:ul
        (str @page)
        (doall
          (for [repo-name (map :name @repos)]
            ^{:key [repo-name (-> @repo-refs (get repo-name) first :hash)]}
            [:li [:b [:a {:href (str "/" repo-name)} repo-name]]
             [:pre
              (for [{:keys [msg ref-type name hash time urls]} (get @repo-refs repo-name)
                    :let [ref-t (case ref-type
                                  "tag"    [:font {:color "#FF0088"} "T"]
                                  "branch" [:font {:color "#00FF00"} "B"])
                          datetime (epoch->str time)
                          name     (clojure.string/replace name #".+/" "")]]
                ^{:key [repo-name hash]}
                [:span (-> [[:a {:href (:self urls) :target "_blank"} (subs hash 0 10)]
                            datetime ref-t name]
                           (interleave (repeat " | ")) butlast)
                   "\n"])]]))]
      
      :repo
      (let [{:keys [repo]} @page]
        ^{:key [:repo repo]}
        [:div {:style {:padding-left "1em"}}
          [:h2 repo]
          "TODO"])))
  
  ; (secretary/dispatch! "/")
  (secretary/defroute "/" []
    (reset! page {:name :home}))
  
  (secretary/defroute "/:repo" [repo]
    (reset! page {:name :repo :repo repo})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FUU what a hack...
(defn re-render! []
  (reagent/render [render] (.getElementById js/document "app")))

(let [timeout (js/setTimeout re-render! 500)]
  (defn init! []
    (accountant/configure-navigation!
      {:nav-handler
       (fn [path]
         (secretary/dispatch! path))
       :path-exists?
       (fn [path]
         (secretary/locate-route path))})
    (accountant/dispatch-current!)
    
    (js/clearTimeout timeout)
    (re-render!)))

