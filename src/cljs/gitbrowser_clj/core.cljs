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
      ref-url #(str % "?to=100")]
  
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
  (defn ref-list [repo-name n-refs]
    [:pre {:style {:margin-left "1em"}}
      (for [{:keys [msg ref-type name hash time urls]} (->> repo-name (get @repo-refs) (take n-refs))
            :let [ref-t
                  [:b (case ref-type
                        "tag"    [:font {:color "#AF0"} "TAG"]
                        "branch" [:font {:color "#0F6"} "BRA"])]
                  datetime (epoch->str time)
                  name     (clojure.string/replace name #".+/" "")]]
        ^{:key [repo-name hash]}
        (into [:span]
          (-> [[:a {:href (:self urls) :class "external" :target "_blank"} (subs hash 0 10)]
               datetime ref-t name]
            (interleave (repeat " | ")) butlast vec (conj "\n"))))])
  
  (defn render []
    (case (:name @page)
      :home
      ^{:key [:home]}
      (into [:p]
        (for [repo-name (map :name @repos)]
          ^{:key [repo-name (-> @repo-refs (get repo-name) first :hash)]}
          [:span
            [:h3 "> " [:a {:href (str "/" repo-name)} repo-name]]
            (conj (ref-list repo-name 10) "...")]))
      
      :repo
      (let [{:keys [repo]} @page]
        ^{:key [:repo repo]}
        [:p
          [:h3 [:a {:href "/"} "<"] " " repo]
          (ref-list repo 1e3)])))
  
  ; (secretary/dispatch! "/")
  (secretary/defroute "/" []
    (reset! page {:name :home}))
  
  (secretary/defroute "/:repo" [repo]
    (.scrollTo js/window 0 0)
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

