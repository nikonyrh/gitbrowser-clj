(ns gitbrowser-clj.handler
  (:require [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [not-found resources]]
            [hiccup.page :refer [include-js include-css html5]]
            [gitbrowser-clj.middleware :refer [wrap-middleware]]
            [config.core :refer [env]]
            
            [nikonyrh-utilities-clj.core :as u]
            [gitbrowser-clj.backend :refer [load-repo]]))

(set! *warn-on-reflection* true)

(def mount-target
  [:div#app
      [:h3 "ClojureScript has not been compiled!"]
      [:p "please run " [:b "lein figwheel"] " in order to start the compiler"]])

(defn head []
  [:head
   [:meta {:charset "utf-8"}]
   [:meta {:name "viewport" :content "width=device-width, initial-scale=1"}]
   (include-css (if (env :dev) "/css/site.css" "/css/site.min.css"))])

(defn loading-page []
  (html5
    (head)
    [:body {:class "body-container"} mount-target (include-js "/js/app.js")]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce repos (atom {}))

(defonce _init_
  (future
    (->> ["/home/wrecked/vendor/elasticsearch"
          "/home/wrecked/vendor/spark"
          "/home/wrecked/vendor/git"]
         (pmap (juxt #(->> % (re-seq #"[^\\/]+$") first) load-repo))
         (into (sorted-map))
         (reset! repos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro json [response]
  `(let [t0# (System/nanoTime)
         response# ~response
         response# (if (seq? response#) (doall response#) response#)
         t1# (System/nanoTime)]
    {:headers {"Content-Type" "application/json"}
     :body    (-> {:response response# :took-ms (-> t1# (- t0#) (* 1e-6))} clojure.data.json/write-str (str "\n"))}))


(defn repo-info [repo]
  {:name repo
   :urls {:self (u/my-format "/repos/{%s:repo}")
          :refs (u/my-format "/repos/{%s:repo}/refs")}})

(defn commit-info [repo commit]
  (let [hash (:hash commit)]
    (assoc commit :urls
      {:self    (u/my-format "/repos/{%s:repo}/commits/{%s:hash}")
       :parents (u/my-format "/repos/{%s:repo}/commits/{%s:hash}/parents")})))


(defn to-ints [values]
  (for [[v default] values] (if v (Integer. v) default)))

(defn make-filter [query & key-path]
  (if-not query identity
    (let [query (clojure.string/lower-case query)]
      (partial filter #(-> % (get-in key-path) clojure.string/lower-case (.contains query))))))


(defroutes routes
  (GET "/" [] (loading-page))
  
  (GET "/repos" []
    (json {"repos" (->> @repos keys (map repo-info))}))
  
  (GET "/repos/:repo" [repo]
    (json {"repo" (merge (repo-info repo)
                    {:n-refs (-> @repos (get repo) :refs count)})}))
  
  (GET "/repos/:repo/refs" [repo from to query]
    (let [[from to] (to-ints [[from 0] [to 100]])]
      (json {"refs" (-> @repos (get repo) :refs ((make-filter query :name))
                        (->> (drop from) (take to) (map (partial commit-info repo))))})))
  
  (GET "/repos/:repo/commits/:hash" [repo hash to query]
    (let [{:keys [commits]} (@repos repo)]
      (json {"commit" (->> hash commits (commit-info repo))})))
  
  (GET "/repos/:repo/commits/:hash/parents" [repo hash to query]
    (let [[to] (to-ints [[to 100]])
          {:keys [hash->parents commits]} (@repos repo)]
      (json {"parents" (->> hash hash->parents (map commits) ((make-filter query :msg)) (map (partial commit-info repo)) (take to))})))
  
  (resources "/")
  (not-found "Not Found"))


(def app (-> #'routes wrap-middleware))
