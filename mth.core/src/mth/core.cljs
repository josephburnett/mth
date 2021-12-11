(ns ^:figwheel-hooks mth.core
  (:require
   [goog.dom :as gdom]
   [reagent.core :as reagent :refer [atom]]
   [reagent.dom :as rdom]))

(defn multiply [a b] (* a b))


;; Descriptions of the practice problems.

(def practices
  [{:key :adding-double-digits
    :description "Adding double digit numbers"}])


;; Functions to create new problems.

(defmulti new-problem :key)

(defn make-choices [n]
  (let [lower (max 0 (- n 5))
        upper (+ n 5)
        incorrect-choices (for [m (range lower upper)
                                :when (not= m n)] m)]
    (->> incorrect-choices
         (shuffle)
         (take 4)
         (cons n)
         (shuffle))))

(defmethod new-problem :adding-double-digits [practice]
  (let [a (+ 10 (rand-int 89))
        b (+ 10 (rand-int 89))
        answer (+ a b)
        choices (make-choices answer)]
    (merge practice {:a a :b b :answer answer :choices choices})))


;; Functions to update app state.

(defonce app-state (atom {}))

(defn click-practice [practice]
  (let [state @app-state]
    (reset!
     app-state
     (-> state
         (assoc :practice practice)
         (assoc :problem (new-problem practice))))))

(defn click-response [response]
  (let [state @app-state]
    (reset!
     app-state
     (if (= (get-in state [:problem :answer]) response)
       (assoc state :problem (new-problem (:practice state))) ; correct
       (assoc-in state [:problem :response] response)))))     ; incorrect


;; Functions to display the app and current problem.

(defn get-app-element []
  (gdom/getElement "app"))

(defn render-choice [choice]
  [:li {:on-click #(click-response choice)} choice])
                                        
(defn render-choices [problem]
  (into [:ul] (for [c (:choices problem)]
                (render-choice c))))

(defmulti render-problem :key)

(defmethod render-problem :adding-double-digits [problem]
  [:div
   [:span (:a problem) "+" (:b problem) "="]
   [render-choices problem]])

(defn render-practice [practice]
  [:div
   [:span [:a {:on-click #(click-practice practice)} (:description practice)]]])

(defn render-practice-selection []
  (into [:div] (map render-practice practices)))

(defn app []
  (let [state @app-state]
    [:div
     [:h1 (:text @app-state)]
     (if (:problem state)
       [render-problem (:problem state)]
       [render-practice-selection])]))

(defn mount [el]
  (rdom/render [app] el))

(defn mount-app-element []
  (when-let [el (get-app-element)]
    (mount el)))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element)
)
