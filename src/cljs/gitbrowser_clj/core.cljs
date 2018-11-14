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
      [:b "COMMIT HASH   | YYYY-MM-DD HH:MM:SS | TYPE   | REF NAME\n"]
      (for [{:keys [msg ref-type name hash time urls]} (->> repo-name (get @repo-refs) (take n-refs))]
        ^{:key [repo-name hash]}
        (into [:span]
          (-> [(let [short-hash (subs hash 0 11)]
                 [:span
                   [:a {:href (str "/ui/repo/" repo-name "/commits/" hash)} short-hash] " "
                   [:a {:href (:self urls) :class "external" :target "_blank"} "^"]])
               
               (epoch->str time)
               
               [:b (case ref-type
                     "tag"    [:font {:color "#AF0"} "TAG   "]
                     "branch" [:font {:color "#0F6"} "BRANCH"])]
               
               (let [short-name (clojure.string/replace name #".+/" "")]
                 [:a {:href (str "/ui/repo/" repo-name "/commits/" hash)} short-name])]
            (interleave (repeat " | ")) butlast vec (conj "\n"))))])
  
  
  (defn render-home []
    ^{:key [:home]}
    (into [:p]
      (for [repo-name (map :name @repos)]
        ^{:key [repo-name (-> @repo-refs (get repo-name) first :hash)]}
        [:span
          [:h3 {:class "indicator"} "> " [:a {:href (str "/ui/repo/" repo-name)} repo-name]]
          (conj (ref-list repo-name 10) "...")])))
  
  
  (defn render-repo []
    (let [{:keys [repo]} @page]
      ^{:key [:repo repo]}
      [:p
        [:h3 [:a {:href "/"} "<"] " " repo]
        (ref-list repo 1e3)]))
  
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn render []
    (case (:name @page)
      :home (render-home)
      :repo (render-repo)))
  
  
  ; (secretary/dispatch! "/")
  (secretary/defroute "/" []
    (reset! page {:name :home}))
  
  (secretary/defroute "/ui/repo/:repo" [repo]
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

