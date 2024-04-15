(ns kit.ipcalc.env
  (:require
    [clojure.tools.logging :as log]
    [kit.ipcalc.dev-middleware :refer [wrap-dev]]))

(def defaults
  {:init       (fn []
                 (log/info "\n-=[ipcalc starting using the development or test profile]=-"))
   :start      (fn []
                 (log/info "\n-=[ipcalc started successfully using the development or test profile]=-"))
   :stop       (fn []
                 (log/info "\n-=[ipcalc has shut down successfully]=-"))
   :middleware wrap-dev
   :opts       {:profile       :dev
                :persist-data? true}})
