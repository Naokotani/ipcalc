(ns kit.ipcalc.env
  (:require [clojure.tools.logging :as log]))

(def defaults
  {:init       (fn []
                 (log/info "\n-=[ipcalc starting]=-"))
   :start      (fn []
                 (log/info "\n-=[ipcalc started successfully]=-"))
   :stop       (fn []
                 (log/info "\n-=[ipcalc has shut down successfully]=-"))
   :middleware (fn [handler _] handler)
   :opts       {:profile :prod}})
