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



