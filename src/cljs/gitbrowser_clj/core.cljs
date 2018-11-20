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
  {:page            (atom {:name :home})
   :repos           (atom nil)
   :repo-refs       (atom nil)
   :hash->commit    (atom nil)
   :commit->parents (atom nil)})


(let [{:keys [repos repo-refs hash->commit commit->parents]} state]
  
  (defn deref-state []
    (zipmap (->> state keys)
            (->> state vals (map deref))))
  
  
  (defn reload-refs! [repo]
    (go (let [repo-name (:name repo)
              refs      (-> repo :urls :refs (str "?to=100") http/get <! :body :response :refs)]
          (swap! repo-refs assoc (:name repo) refs)
          (doseq [ref refs]
            (swap! hash->commit assoc [repo-name (:hash ref)] ref)))))
  
  
  (defn reload-repos! []
    (reset! repos [])
    (reset! repo-refs {})
    (reset! hash->commit {})
    (reset! commit->parents {})
    
    (go (let [response (-> "/repos" http/get <! :body :response :repos)]
          (reset! repos response)
          (doseq [repo response]
            (reload-refs! repo)))))
  
  
  (defn reload-commit! [repo-name hash]
  ; (.log js/console (clojure.string/join " / " ["reload-commit!" repo-name hash]))
    (swap! commit->parents dissoc [repo-name hash])
    
    (go 
      (let [current-hash->commit @hash->commit]
        (when-let [commit (get current-hash->commit [repo-name hash])]
          (let [parents (-> commit :urls :parents (str "?to=50") http/get <! :body :response :parents)
                
                new-commits
                (for [commit parents
                      :let  [commit-key [repo-name (:hash commit)]]
                      :when (not (contains? current-hash->commit commit-key))]
                  [commit-key commit])]
            
            (swap! commit->parents assoc [repo-name hash] parents)
            
            (when-not (empty? new-commits)
              (swap! hash->commit merge (into {} new-commits)))))))))


(defonce _
  [(reload-repos!)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn epoch->str [epoch] (-> epoch (* 1000) js/Date. .toISOString
                             (clojure.string/replace "T" " ")
                             (clojure.string/replace ".000Z" "")))

(defn short-hash [hash]
  (subs hash 0 11))

(defn str-subs [s max-len]
  (let [padding (apply str (repeat max-len " "))]
    (if (<= (count s) max-len)
      (-> s (str padding) (subs 0 max-len))
      (-> s (subs 0 (- max-len 2)) (str "..")))))

(defn my-interleave [coll sep]
  (-> (filter some? coll) (interleave (repeat sep)) butlast))

(defn commit-col [urls repo-name hash & [ref-name]]
  [:span
   [:a {:href (:self urls) :class "external" :target "_blank"} "^"] " "
   [:a {:href (str "/ui/repo/" repo-name "/commits/" hash)} (short-hash hash)]
   (when ref-name (str "   " (-> ref-name (clojure.string/replace #".+/" "") (str-subs 16))))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(let [{:keys [page repos repo-refs commit->parents]} state]
  
  (defn ref-list [repo-name n-refs]
    [:pre {:style {:margin-left "1em"}}
      [:b "YYYY-MM-DD HH:MM:SS | TYPE   | ^ COMMIT HASH @ REF NAME         | MSG\n"]
      (for [{:keys [msg ref-type name hash time urls]} (->> repo-name (get @repo-refs) (take n-refs))]
        ^{:key [repo-name hash]}
        (into [:span]
          (-> [(epoch->str time)
               
               [:b (case ref-type
                     "tag"    [:font {:color "#AF0"} "TAG   "]
                     "branch" [:font {:color "#0F6"} "BRANCH"])]
               
               (commit-col urls repo-name hash name)
               
               (-> (re-seq #"[^\r\n]+" msg) first)] ; (str-subs 80))]
              (my-interleave " | ") vec (conj "\n"))))])
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn render-home []
    ^{:key [:home]}
    (into [:div]
      (for [repo-name (map :name @repos)]
        ^{:key [repo-name (-> @repo-refs (get repo-name) first :hash)]}
        [:span
          [:h3 {:class "inactive"} "> " [:a {:href (str "/ui/repo/" repo-name)} repo-name]]
          (conj (ref-list repo-name 5) "...")])))
  
  
  (defn render-repo []
    (let [{:keys [repo]} @page]
      ^{:key [:repo repo]}
      [:div
        [:h3 {:class "inactive"} [:a {:href "/"} "<"] " " repo]
        (ref-list repo 1e3)]))
  
  
  (defn render-commit []
    (let [{:keys [repo-name hash]} @page
          
          li           [:li {:style {:font-family "monospace" :whitespace "pre" :margin-left "1em"}}]
          th           "YYYY-MM-DD HH:MM:SS | ^ COMMIT HASH | MSG"
          body-style   {:style {:display "inline-block" :max-width "160ex"
                                :margin-left (-> th count (+ 3) (str "ex")) :margin-bottom "1em"}}
          inner-style  {:style {:font-size "80%"}}
          
          parents       (get @commit->parents [repo-name hash])
          max-body-rows 6]
      ^{:key [:commit repo-name hash]}
      [:div
        [:h3 {:class "inactive"}
          [:a {:href "/"} "<"] " "
          [:a {:href (str "/ui/repo/" repo-name)} repo-name] " / " (short-hash hash)]
       [:ul {:style {:list-style-type "none" :padding-left "0px"}}
        (cons
          (conj li [:b th "\n"])
          (for [{:keys [msg hash time urls]} parents
                :let [[msg-title _ & msg-body] (clojure.string/split msg #"\r?\n")]]
            (into li
              (-> [(epoch->str time)
                   
                   (commit-col urls repo-name hash)
                   
                   [:span msg-title [:br]
                     (when msg-body
                       [:span body-style
                        (into [:span inner-style]
                          (-> (if (-> msg-body count (> max-body-rows))
                                (concat (take (dec max-body-rows) msg-body) ["..."])
                                msg-body)
                              (my-interleave [:br])))])]]
                  (my-interleave " | ")))))]]))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (defn render []
    (case (:name @page)
      :home   (render-home)
      :repo   (render-repo)
      :commit (render-commit)))
  
  
  ; (secretary/dispatch! "/")
  (secretary/defroute "/" []
    (reset! page {:name :home}))
  
  (secretary/defroute "/ui/repo/:repo" [repo]
    (.scrollTo js/window 0 0)
    (reset! page {:name :repo :repo repo}))
  
  (secretary/defroute "/ui/repo/:repo-name/commits/:hash" [repo-name hash]
    (.scrollTo js/window 0 0)
    (reload-commit! repo-name hash)
    (reset! page {:name :commit :repo-name repo-name :hash hash})))

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

