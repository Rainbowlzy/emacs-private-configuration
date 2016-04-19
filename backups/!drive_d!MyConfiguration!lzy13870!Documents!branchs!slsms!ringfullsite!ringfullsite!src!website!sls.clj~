(ns website.sls
  (:use [ring.adapter.jetty]
        [ring.util.response]
        [clojure.pprint]
        [clj.qrgen]
        [net.cgrand.enlive-html]
        [selmer.parser])
  (:import [com.mongodb MongoOptions ServerAddress]
           [org.bson.types ObjectId]
           [com.mongodb DB WriteConcern])
  (:require [net.cgrand.enlive-html :as html]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clj-http.client :as client]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [compojure.core :refer [GET POST defroutes]]
            [monger.core :as mg]
            [monger.collection :as mc]
            [selmer.parser :refer [render-file]]))

(defn global-response [resp]
  (-> resp response (content-type "text/html; charset=UTF-8")))

(defn home [req] (render-file "home.html"
                      {
                       :title "Home"
                       :nav-items [{:label "Home" :url "/"}
                                   {:label "New Product" :url "/new-product"}
                                   {:label "About" :url "/about"}]
                       :product-list-header ["图片" "名称" "数量" "尺寸" "型号"  "颜色"  "详情"]
                       :product-list (let [conn (mg/connect)
                                           db   (mg/get-db conn "sls")
                                           u "sls"
                                           p (.toCharArray (str 666666))
                                           coll "products"]
                                       (mc/find-maps db coll))
                       }))

(defroutes main-routes
  (route/files "/")
  (GET "/" [req] (home req))
  (GET "/new-product" [req] (render-file "new-product.html" {:title (json/write-str req)}))
  (GET "/about" [] (response "This is a private system."))
  (route/not-found "Page not found")
  )

(def app (compojure.core/routes main-routes))

;; (defn handler [req]
;;   (->
;;    ;; (response (reduce str (post-page sample-post)))
;;    ;; (file-response "readme.html" {:root "public"})
;;    ;; (response "hello")
;;    (response (main-page req))
;;    (content-type "text/html; charset=UTF-8")))

(defn launch []
   (run-jetty app {:port 18080 :join? false :route "public"}))

(defn -main
  "启动web应用程序"
  ^{:static false
    :dynamic true}
  [& args]
  (def server (launch)))

;; sudo mongod

(let [conn (mg/connect)
      db   (mg/get-db conn "sls")
      u "sls"
      p (.toCharArray (str 666666))
      coll "products"]
  ;; (mg/authenticate db u p)
  ;; (mc/insert-and-return db "products" {:name "ksj" :color "黄" :count 50 :size 40 :label "default"})
  ;; (mc/find db coll {:count {"$gt" 10 "$lt" 99}})
  ;; (mc/remove db coll {:count {"$lt" 20}})
  ;; (mc/remove db coll)
  ;; (mc/update db coll {:count {"$gt" 0}} {:count 20 :owner "高洪"} {:upsert true})
  ;; (pprint (mc/find-maps db coll))
  )
(defn restart []
  (.stop server)
  (.start server)
  )

;; (str (as-file (from "http://10.1.201.72:18080")))
;; /var/folders/j0/v760t6w94d50bql8hlh3rxfc0000gn/T/QRCode5030151942762847936.png
;; D:\MyConfiguration\lzy13870\AppData\Local\Temp\QRCode1100735308893157262.png

;; (defn url-attack [] "http://www.baidu.com/baidu.php?url=ssRK00jnh8orQ_g9YOLug-S1m1Sqa7NOHE5osXkcszJquhfwOT5FcREBamf9qfcKjyXkDjl0b7apsAgZY9oBZb65YK7bznpZW8qUHcHF3ccH74QOscntp1_6AJgKa4pKB9IYsUf.7D_iHF8xnhA94wEYL_SNK-deQbfHgI3ynDgg6msw5I7AMHdd_NR2A5jkq8ZFqTrHllgw_E9tGbSNK-deQbmTMdWi1PjNz8smX5dxAS2FnvZWtonrHGEsfq8QjkSyHjblubltXQjkSyMHz4rMG34nheuztIdMugbzTEZF83e5ZGzmTMHvGYTjGo_5Z4mThedlTrHIt5s3The3IhZF8qISZFY3tyZWtVrM-zI5HkzuPv1-3eorzEFb4XrHIkvX5HblqoAVPXzOk8_eAThqPvlZoWmYlXgFYq5ZFbLUrgW8_e2thH-34PLZu3qrHoXkvyNq-----xHEer1IvUdPHV2XgZJyAp7WFYvyu70.U1Yk0ZDqV_1c8fKY5UUnzQb0pyYqnW0Y0ATqmhwln0KdpHdBmy-bIfKspyfqnHb0mv-b5HR40AdY5HDsnHIxn10sn-tknjD1g1nsnW00pvbqn0KzIjY3njT0uy-b5HD3rj6sg1DYPH7xnH6zPj7xnHbdPH9xnH6kn1PxnHTsnj7xnHRYrj9xnHD4nHT0mhbqnW0Yg1DdPfKVm1Y3rjc4n103Pdtknj7xnHnvrjnsPHcvndts0Z7spyfqn0Kkmv-b5H00ThIYmyTqn0KEIhsq0A7B5HKxn0K-ThTqn0KsTjYknjRsnW03PWTv0A4vTjYsQW0snj0snj0s0AdYTjYs0AwbUL0qn0KzpWYs0Aw-IWdsmsKhIjYs0ZKC5H00ULnqn0KBI1YknfK8IjYs0ZPl5fKYIgnqnHc1PjR3n1R1n1m4P1nzP10zrHR0ThNkIjYkPjR4P1fLP16LrHfv0ZPGujdBuj9WPjDLPH0snWnvuHR40AP1UHYkwjm3n1wDnj0znYD1wW-j0A7W5HD0TA3qn0KkUgfqn0KkUgnqn0KlIjYs0AwYpyfqn0K9TLKWm1Ys0ZNspy4Wm1Ys0APzm1YdP1bdP6&us=0.0.0.0.0.0.0&us=0.0.0.0.0.0.13&ck=6203.20.1459747842434.0.0.516.212.0&shh=www.baidu.com&sht=baiduhome_pg")
;; (future (client/get (url-attack)))

(println "loaded.")
