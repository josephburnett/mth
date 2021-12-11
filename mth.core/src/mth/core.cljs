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

(def instructions "Choose a math practice:")

(defonce app-state (atom {:text instructions}))

(defn click-practice [practice]
  (let [state @app-state]
    (reset!
     app-state
     (-> state
         (assoc :practice practice)
         (assoc :problem (new-problem practice))
         (assoc :text (:description practice))))))

(defn correct [state]
  (let [score (+ 3 (:score state))]
    (-> state
        (assoc :score score)
        (assoc :problem (new-problem (:practice state)))
        (assoc :text (str "Correct! +3. Your score is " (str score) ".")))))

(defn incorrect [state response]
  (let [score (max 0 (- (:score state) 1))]
    (-> state
        (assoc :score score)
        (assoc-in [:problem :response] response)
        (assoc :text (str "Incorrect. -1. Your score is " (str score) ".")))))

(defn click-response [response]
  (let [state @app-state]
    (reset!
     app-state
     (if (= (get-in state [:problem :answer]) response)
       (correct state)
       (incorrect state response)))))


;; Functions to display the app and current problem.

(defn get-app-element []
  (gdom/getElement "app"))

(defn render-choice [choice]
  [:li
   [:h1 [:a {:style {:color "blue"}
             :on-click #(click-response choice)} choice]]])
                                        
(defn render-choices [problem]
  [:ul {:style {:list-style "none"}}
   (for [c (:choices problem)]
     (render-choice c))])

(defmulti render-problem :key)

(defmethod render-problem :adding-double-digits [problem]
  [:div
   [:span {:style {:font-size "60px"}} (:a problem) " + " (:b problem) " ="]
   [render-choices problem]])

(defn render-practice [practice]
  [:div
   [:h1 [:a {:style {:color "blue"}
             :on-click #(click-practice practice)} (:description practice)]]])

(defn render-practice-selection []
  [:div
   [:ul {:style {:list-style "none"}}
    (map render-practice practices)]])

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
