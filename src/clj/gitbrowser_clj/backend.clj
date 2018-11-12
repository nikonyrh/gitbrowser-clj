(ns gitbrowser-clj.backend
  (:require [clj-jgit.porcelain :as jgit]
            [clj-jgit.querying  :as jgit.q]
            [clj-jgit.internal  :as jqit.i]
            [nikonyrh-utilities-clj.core :as u]
            [clojure.pprint :refer [pprint]])
  
  (:import  [org.eclipse.jgit.api Git]
            [org.eclipse.jgit.lib AnyObjectId]
            [org.eclipse.jgit.diff DiffFormatter DiffEntry]
            [org.eclipse.jgit.util.io DisabledOutputStream]
            [org.eclipse.jgit.diff RawTextComparator]
            [org.eclipse.jgit.revwalk RevCommit]))

(set! *warn-on-reflection* true)

(defmacro sym-hashmap [& symbols]
  `(zipmap ~(mapv keyword symbols) ~(vec symbols)))
; (macroexpand-1 '(sym-hashmap a b c))


(defn repo->refs [^org.eclipse.jgit.api.Git repo hash->commit commit->hash]
  (vec (for [[ref-type items]
             [[:branch (-> repo .branchList (.setListMode org.eclipse.jgit.api.ListBranchCommand$ListMode/REMOTE) .call)]
              [:tag    (-> repo .tagList .call)]]
             
             item items
             :let [name (.getName item)]
             :when (not= name "refs/remotes/origin/HEAD")
             :let [hash (case (-> item type str) ; well this is kinda hacky
                          "class org.eclipse.jgit.lib.ObjectIdRef$PeeledNonTag"
                          (-> item .getObjectId .name)
                          
                          "class org.eclipse.jgit.lib.ObjectIdRef$PeeledTag"
                          (-> item .getPeeledObjectId .name)
                          
                          "class org.eclipse.jgit.internal.storage.file.RefDirectory$LooseUnpeeled"
                          (-> item .getObjectId .name)
                          
                          "class org.eclipse.jgit.internal.storage.file.RefDirectory$LooseNonTag"
                          (-> item .getObjectId .name)
                          
                          "class org.eclipse.jgit.internal.storage.file.RefDirectory$LoosePeeledTag"
                          (-> item .getPeeledObjectId .getName))]]
                          
                        ; item)]]
           (sym-hashmap ref-type name hash))))



(defn load-repo [path]
  (let [repo            (jgit/load-repo path)
        commit->hash    (fn [^RevCommit commit] (.name commit))
        hash->commit    (fn [^String hash]
                          (jgit.q/find-rev-commit repo (jqit.i/new-rev-walk repo) hash))
        
        commits
        (u/hashfor [commit (-> repo .log .all .call)]
          (let [hash (commit->hash commit)]
            [hash
             {:hash      hash
              :parents   (->> commit .getParents (mapv commit->hash))
              :msg       (->> commit .getFullMessage)
              :time      (->> commit .getCommitTime)
              :comm-name (->> commit .getCommitterIdent .getName)
              :auth-name (->> commit .getAuthorIdent    .getName)}]))
        
        refs (->> (repo->refs repo hash->commit commit->hash)
                  
                  ; git repo has an odd refs/tags/junio-gpg-pub with hash of 7214aea37915ee2c4f6369eb9dea520aec7d855b :o
                  (filter (comp commits :hash))
                  
                  (map #(merge % (-> % :hash commits (dissoc :parents))))
                  (sort-by (comp - :time commits :hash)))
        
        hash->parents
        (fn [hash]
          (if-not (commits hash)
            ()
            (->> [#{} #{hash}]
                 (iterate
                   (fn [[seen-hashes round-hashes]]
                     (let [new-hashes
                           (for [hash round-hashes
                                 p (-> hash commits :parents)
                                 :when (not (seen-hashes p))]
                             p)]
                       [(into seen-hashes new-hashes) (seq new-hashes)])))
                 (map second)
                 (take-while some?)
                 (apply concat))))]
    
    (sym-hashmap path name repo refs commits hash->commit hash->parents)))





(comment
  (def repo (load-repo "/home/wrecked/vendor/git"))
  
  (->> repo :refs (filter (comp (:commits repo) :hash)) (sort-by (comp - :time (:commits repo) :hash)) first)
  (->> repo :refs (remove (comp (:commits repo) :hash)) first)
  (-> repo :commits (get "238e487ea943f80734cc6dad665e7238b8cbc7ff"))

  (->> "238e487ea943f80734cc6dad665e7238b8cbc7ff" ((:hash->parents repo)) (take 100000) count time)
  (->> repo :refs (map (juxt identity (comp (:commits repo) :hash))) (take 10))
  
  (->> repo :refs (map :hash) (filter (comp not string?)) (map type) frequencies)
  (->> repo :refs (map :hash) (filter (comp not string?)) first type str)
  (->> repo :refs (map :hash) (filter (comp not string?)) first .getPeeledObjectId .getName)
  
  (->> repo :repo jgit/git-log first)
  (->> repo :refs first)
  (->> repo :refs (map :name) frequencies (sort-by (comp - val)) (take 10)))
  
