(ns lsnet.views
  (:require [hathcock.db :as hdb]
            [lsnet.page :as lp]
            [chesty.db :as db]
            [clojure.string :as str]
            [krulak.util :as util]
            [chesty.api.validators :as avs]
            [cheshire.core :as json]
            [com.ashafa.clutch :as clu]
            [chesty.db.views :as v]
            [chesty.memcached :as cm]
            [ring.util.response :as response]
            [krulak.simple-email :as kse]
            [chesty.api :as api]))

(defn js-redirect [href]
  (str "<script>window.location.href='" href "'</script>"))

(defn save-product [request]
  (let [params (:params request)
        params (-> params
                   (select-keys [:content :images :title :category :quantity :newprice :normprice :sku :visible :custom])
                   (update-in [:title]
                              #(or % (-> params :content (str/split % "\n")
                                         (->> (remove str/blank?)) first))))
        {:keys [content title]} params
        uri (str (:category params) \/ (util/to-url-slug title))
        post (merge
              (hdb/post
               :uris [uri]
               :content-html content)
              params)]
    (db/put-doc post))
  "/new-product")

(defn update-product [request]
  (let [params (:params request)
        id (:id params)
        _  (prn "id:" id)
        params (-> params
                   (select-keys [:content :images :title :category :quantity :newprice :normprice :sku :visible :custom])
                   (update-in [:title]
                              #(or % (-> params :content (str/split % "\n")
                                         (->> (remove str/blank?)) first))))
        {:keys [content title]} params
        uri (str (:category params) \/ (util/to-url-slug title))
        params (merge
                {:uris [uri]
                 :content-html content}
                params)]
    (-> (clu/get-document id)
        (clu/update-document params))
    (str "/view-product/" id)))

(defn delete-product [request id]
  (clu/delete-document (clu/get-document id))
  (js-redirect "/admin-products"))

(defn delete-category [request id]
  (let [cats (hdb/categories)
        sorted-cats (sort-by :position cats)
        to-update (remove nil? (map #(if (not (= id (:_id %))) %) sorted-cats))
        updated-cats (map #(assoc-in % [:position] (str (inc (.indexOf to-update %)))) to-update)]
    (clu/bulk-update updated-cats)
    (clu/delete-document (clu/get-document id))
    (js-redirect "/categories")))

(defn delete-header-footer [request id]
  (let [header-footer (clu/get-document id)
        loc (:location header-footer)
        hfs (sort-by :position 
                     (remove nil? 
                             (map #(if (and (= loc (:location %)) 
                                            (not (= id (:_id %)))) %) (hdb/site-header-footers))))
        updated-hfs (map #(assoc-in % [:position] (str (inc (.indexOf hfs %)))) hfs)]
    (clu/bulk-update updated-hfs)
    (clu/delete-document (clu/get-document id))
    (js-redirect "/header-footer")))

(defn position-mechanic [old new maps]
  (let [pos-fn (fn [f m] (update-in m [:position] #(str (f (read-string %)))))
        to-change (take (Math/abs (- new old)) (drop (- (min new old) 1) maps))]
    (if (neg? (- new old))
      (map #(pos-fn inc %) to-change)
      (map #(pos-fn dec %) to-change))))

(defn save-category [request]
  (let [params (:params request)
        cats (hdb/categories)
        old (+ (count cats) 1)
        new (read-string (:position params))
        params (-> params
                   (select-keys [:category :visible :position]))
        post (merge
              (hdb/post-cat)
              params)
        updated-cats (if (not (= old new)) (position-mechanic old new cats))]
    (if updated-cats (clu/bulk-update updated-cats))
    (db/put-doc post))
  "/categories")

(defn update-category [request]
  (let [params (:params request)
        id (:id params)
        new-pos (read-string (:position params))
        cats (hdb/categories)        
        old-pos (read-string (:position (first (filter #(= id (:_id %)) cats))))
        cats (sort-by :position (remove nil? (map #(if (not (= id (:_id %))) %) cats)))
        params (-> params
                   (select-keys [:category :visible :position]))
        changed-cats (position-mechanic old-pos new-pos cats)]
    (if changed-cats (clu/bulk-update changed-cats))
    (clu/update-document (clu/get-document id) params))
  "/categories")

(defn sort-cat-by-alpha [request]
  (let [cats (hdb/categories)
        sorted-cats (sort-by :category cats)
        updated-cats (map #(assoc-in % [:position] (str (inc (.indexOf sorted-cats %)))) sorted-cats)]
    (clu/bulk-update updated-cats))
  (js-redirect "/categories"))

(defn save-header-footer [request]
  (let [params (:params request)
        params (-> params
                   (select-keys [:content :location :visible :position]))
        post (merge
              (hdb/header-footer)
              params)]
    (db/put-doc post))
  "/header-footer")

(defn update-header-footer [request]
  (let [params (:params request)
        id (:id params)
        loc (:location params)
        new-pos (read-string (:position params))
        hfs (hdb/site-header-footers)
        old-pos (read-string (:position (first (filter #(= id (:_id %)) hfs))))
        hfs (sort-by :position 
                     (remove nil? (map #(if (and (= loc (:location %)) 
                                                 (not (= id (:_id %)))) %) hfs)))       
        params (-> params
                   (select-keys [:content :location :visible :position]))
        changed-hfs (position-mechanic old-pos new-pos hfs)]
    (if changed-hfs (clu/bulk-update changed-hfs))
    (-> (clu/get-document id)
        (clu/update-document params)))
  "/header-footer")

(defn save-user [request]
  (let [params (:params request)
        salt "8dmf7t"
        pw (:password params)
        cpw (:passwordconfirm params)
        params (-> params
                   (select-keys [:password :name]))
        post (merge
              (hdb/admin-doc)
              params)]
    (if (and (= pw cpw) (> (count (:name params)) 4) (> (count (:password params)) 4)) (db/put-doc post)
        (if (not (= pw cpw)) "$('#login_error').html('Passwords must match')"
            (if (or (not (> (count (:name params)) 4)) (not (> (count (:password params)) 4)))
              "$('#login_error').html('Username and password must be longer than 4 characters.')"
              "window.location.href = '/staff'")))))

(defn login-check [request]
  (let [wb-cred (:params request)
        db-cred (hdb/admin-for-name (:name wb-cred))
        response (if (and (= (:name wb-cred) (:name db-cred)) 
                          (= (:password wb-cred) (:password db-cred)))
                   {:status 200
                    :headers {"Content-Type" "application/json; charset-utf-8"}
                    :body "/staff"
                    :user (:name db-cred)
                    :cookie (:password db-cred)}
                   {:status 403
                    :headers {"Content-Type" "application/json; charset-utf-8"}
                    :body "Username/Password combination was not valid."})]
    (json/generate-string response)))

(defn user-secure [request]
  (let [ck-cred (:params request)
        db-cred (hdb/admin-for-name (:name ck-cred))
        response (if (and (= (:name ck-cred) (:name db-cred))
                          (= (:password db-cred) (:logincookie ck-cred)))
                   {:status 200
                    :headers {"Content-Type" "application/json; charset-utf-8"}
                    :body "Logged in. Proceeding."
                    :uri (:uri ck-cred)}
                   {:status 403
                    :headers {"Content-Type" "application/json; charset-utf-8"}
                    :body "Not logged in. Redirecting."})]
    (json/generate-string response)))

(defn product-email-template [name email msg])

(defn send-product-email [request]
  (prn "request: " request)
  (let [site-doc (:chesty/site request)
        params (:params request)
        {:keys [mail-to email-subject]} (:email-params site-doc)
        {:keys [name email notes admintext url]} params
        email {:from (:email params) 
               :to mail-to
               :subject email-subject 
               :body {:html (str admintext "<p>Notes: " notes "</p><p>Name: " name "</p><p>Email: " email)
                      :text (str admintext "<p>Notes: " notes "</p><p>Name: " name "</p><p>Email: " email)}}
        
        ses (:ses site-doc)]
    (kse/send-email ses email)
    (str url)))

(defn view-front-page [request]
  (lp/front-page request (hdb/newest-products {:limit 200}) (hdb/categories) (hdb/site-header-footers)))

(defn view-new-product-page [request]
  (lp/new-product-page request (hdb/categories)))

(defn view-categories-page [request]
  (lp/categories-page request (hdb/categories)))

(defn view-admin-home [request]
  (lp/admin-home request))

(defn view-admin-products [request]
  (lp/admin-products request (hdb/newest-products {:limit 200}) (hdb/categories)))

(defn view-product-page [request id]
  (lp/edit-product-page request (clu/get-document id) (hdb/categories)))

(defn view-edit-category-page [request id]
  (lp/edit-category-page request (clu/get-document id) (hdb/categories)))

(defn view-new-user-page [request]
  (lp/new-user-page request))

(defn view-login-page [request]
  (lp/login-page request))

(defn view-header-footer-page [request]
  (lp/header-footer-page request (hdb/site-header-footers)))

(defn view-edit-header-footer-page [request id]
  (lp/edit-header-footer-page request (clu/get-document id) (hdb/site-header-footers)))
