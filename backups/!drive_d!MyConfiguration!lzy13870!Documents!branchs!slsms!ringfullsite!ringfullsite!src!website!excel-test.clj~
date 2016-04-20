;; Copyright (c) Stuart Sierra, 2012. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

(ns website.excel-test
  (:use [dk.ative.docjure.spreadsheet]
        [clojure.pprint]
        [clojure.java.io])
  (:require [clojure.data.json :refer [read-str write-str]]
            [website.encrypto]
   )
  )

;; (for [inputstring (->> (load-workbook "/Users/apple/Downloads/数据报表_20160329143013209.xlsx")
;;                        (select-sheet "Table1")
;;                        (select-columns {:A :inputstring})) :when (and inputstring (not (= (:inputstring inputstring) "inputstring")))]
;;   (let []
;;     (->> (clojure.data.json/read-str (:inputstring inputstring) :key-fn keyword) :request :body :memberId)))


(defn not-empty? [ent]
  (not (empty? ent)))
(def member-ids
  "会员ID"
  (set
   (filter not-empty?
           (for [inputstring (->> (load-workbook "/Users/apple/Downloads/数据报表_20160329143013209.xlsx")
                                  (select-sheet "Table1")
                                  (select-columns {:A :inputstring}))
                 :when (and inputstring (not (= (:inputstring inputstring) "inputstring")))]
             (let []
               ;; (->> (clojure.data.json/read-str (:inputstring inputstring) :key-fn keyword) :request :body :memberId)
               (try (:memberId (:body (:request (read-str (:inputstring inputstring) :key-fn keyword))))
                    (catch Exception ex (.getMessage ex))))))))


