(ns om-undo-tree.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [om.core :as om :include-macros true]
            [sablono.core :as html :refer-macros [html]]
            [cljs.core.async :refer [<! chan]]))

(enable-console-print!)

(defn log
  "logs cljs stuff as js stuff for inspection"
  [& args]
  (.log js/console (clj->js (map clj->js args))))

(defn history-node [state index]
  {:index index
   :state state
   :count 0
   :children []})

(def app-state (atom {:text "Hello world!"
                      :counter 0}))

(def app-history (atom {:current-path []
                        :history (history-node app-state 0)}))

(defn add-child [history path state]
  (let [children-path (conj path :children)
        current-children (get-in history children-path)
        new-children (if-let [existing-child (first (filter #(= (:state %) state) current-children))]
                       (assoc current-children (:index existing-child) (update-in existing-child [:count] inc))
                       (conj current-children (history-node state (count current-children))))]
    (assoc-in history children-path new-children)))

(defn get-current-location [{:keys [current-path history]}]
  (get-in history current-path))

(defn push-history [state]
  (swap! app-history (fn [{:keys [current-path history] :as new}]
                       (let [current-children (:children (get-current-location history))
                             child-exists (filter #(= (:state %) state) current-children)

                             with-child (assoc new :history (add-child history current-path state))
                             num-children (count (:children (get-current-location with-child)))
                             new-path (conj current-path :children (- num-children 1))]
                         (assoc with-child :current-path new-path)))))

(defn update-history
  [{:keys [path old-state new-state tag] :as tx-data}]
  (let [history @app-history]
    (when (not= new-state old-state)
      (push-history new-state))))

(defn undo []
  (when-not (zero? (count (:current-path @app-history)))
    (swap! app-history (fn [history]
                         (update-in history [:current-path] #(apply vector (drop 2 %)))))
    (reset! app-state (:state (get-current-location @app-history)))))

(defn app-view [app owner]
  (reify
    om/IRender
    (render [_]
      (html [:div {:class "app"}
             [:div
              (:text app)]
             [:button {:on-click #(om/transact! app :counter inc)}
              "Increment!"]
             [:div
              (:counter app)]
             [:button {:on-click undo}
              "Undo"]]))))

(om/root
 app-view
 app-state
 {:target (. js/document (getElementById "app"))
  :tx-listen update-history
  ;; :instrument
  ;; (fn [f cursor m]
  ;;   (om/build* editor [f cursor m]))
  })

;; (defn pr-map-cursor [cursor]
;;   (pr-str
;;    (into cljs.core.PersistentHashMap.EMPTY
;;          (om/value cursor))))

;; (defn handle-change [e cursor owner]
;;   (let [value (.. e -target -value)]
;;     (try
;;       (let [data (reader/read-string value)]
;;         (if (= (set (keys @cursor)) (set (keys data)))
;;           (do
;;             (om/transact! cursor (fn [_] data))
;;             (om/set-state! owner :value value))
;;           (om/update-state! owner :value identity)))
;;       (catch :default ex
;;         (om/set-state! owner :value value)))))

;; (defn pr-map-cursor [cursor]
;;   (pr-str
;;    (into cljs.core.PersistentHashMap.EMPTY
;;          (om/value cursor))))

;; (defn editor [[_ cursor :as original] owner opts]
;;   (reify
;;     om/IInitState
;;     (init-state [_]
;;       {:value (pr-map-cursor cursor)
;;        :editing false})
;;     om/IRenderState
;;     (render-state [_ {:keys [editing value]}]
;;       (html (:div {:class "editor"}
;;                   (:div {}
;;                         [:label {:class "inspector"} "path:"]
;;                         [:code nil (pr-str (om/path cursor))])
;;                   (:div {}
;;                         [:label {:class "inspector"} "value:"]
;;                         [:input {:class "edit"
;;                               :value (if editing
;;                                        value
;;                                        (pr-map-cursor cursor))
;;                               :onFocus (fn [e]
;;                                          (om/set-state! owner :editing true)
;;                                          (om/set-state! owner :value
;;                                                         (pr-map-cursor (second (om/get-props owner)))))
;;                               :onBlur (fn [e] (om/set-state! owner :editing false))
;;                               :onChange #(handle-change % cursor owner)}])
;;                   (apply om/build* original))))))
