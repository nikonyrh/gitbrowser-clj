(ns ^:figwheel-no-load gitbrowser-clj.dev
  (:require
    [gitbrowser-clj.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)
